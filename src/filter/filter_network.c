/*
 * filter_network.c
 * $Id: filter_network.c,v 1.18 2000/02/14 13:23:40 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <regex.h>
#include <errno.h>
#include "util.h"
#include "sem.h"
#include "filter.h"
#include "filter_methods.h"
#include "filter_mm.h"



struct filternetspec {
	char **lines;
	int nr_lines;
};



/* ok, we have to launch the network back-to-front
 * (i.e. it is a _directed_ network)
 * we do this recursievely - ugh(?)
 */
int filternetwork_launch(filter_network_t *net)
{
	sigset_t sigs;

	if (FILTERNETWORK_IS_RUNNING(net))
		return -1;

	/* block EPIPE */
	sigemptyset(&sigs);
	sigaddset(&sigs, SIG_BLOCK);
	sigprocmask(SIG_BLOCK, &sigs, NULL);

	/* init state */
	if (!(net->launch_context = _launchcontext_alloc()))
		return -1;

	DPRINTF("initting nodes\n");
	if (net->node.ops->init(FILTER_NODE(net)) == -1)
		goto out;

	DPRINTF("launching nodes\n");
	if (net->node.ops->launch(FILTER_NODE(net)) == -1)
		goto out;

	DPRINTF("waiting for nodes to complete initialization\n");
	sem_op(net->launch_context->semid, 0, -net->launch_context->nr_threads);

	if (ATOMIC_VAL(net->launch_context->result) != 0)
		goto out;
	DPRINTF("all nodes ready.\n");

	return 0;

 out:
	DPRINTF("error.\n");
	net->node.ops->postprocess(FILTER_NODE(net));
	_launchcontext_free(net->launch_context);
	net->launch_context = NULL;

	return -1;
}

/* wait for the network to finish processing */
int filternetwork_wait(filter_network_t *net)
{
	int res;

       	if (!FILTERNETWORK_IS_RUNNING(net))
		return -1;

	res = net->node.ops->wait(FILTER_NODE(net));
	DPRINTF("net result is %i\n", res);

	net->node.ops->postprocess(&net->node);
	_launchcontext_free(net->launch_context);
	return res;
}

/* kill the network */
void filternetwork_terminate(filter_network_t *net)
{
	net->node.ops->postprocess(FILTER_NODE(net));
	_launchcontext_free(net->launch_context);
}





/* API */

filter_network_t *filternetwork_new(const char *name)
{
	filter_network_t *net = NULL;
	filter_t *f = NULL;

	if (!name)
		return NULL;
	if (!(f = _filter_alloc(name, name, FILTER_FLAG_NETWORK)))
	        return NULL;
	if (!(net = FILTER_NETWORK(_filter_instantiate(f, name))))
		goto err;

	return net;

 err:
	_filter_free(f);
	return NULL;
}

void filternetwork_delete(filter_network_t *net)
{
	filter_t *f;

	if (FILTERNETWORK_IS_RUNNING(net))
		return;

	f = net->node.filter;
	_network_free(net);
	if (!is_hashed_filter(f))
		_filter_free(net->node.filter);
}

filter_node_t *filternetwork_add_node(filter_network_t *net,
				      const char *filter, const char *name)
{
	filter_t *f;
	filter_node_t *n;
	const char *nm = name;

	if (!net || !filter)
		return NULL;
	if (!(f = filter_get(filter)))
		return NULL;
	if (!nm)
		nm = hash_unique_name_node(f->name, net);
	if (!(n = _filter_instantiate(f, nm)))
		return NULL;

	/* Add node to networks node list and to the hash using name/net.
	 */
	n->net = net;
	list_add_node(n, net);
	hash_add_node(n);
	net->nr_nodes++;

	return n;
}

void filternetwork_delete_node(filter_node_t *node)
{
	if (FILTERNODE_IS_RUNNING(node))
		return;

	list_remove_node(node);
	hash_remove_node(node);
	node->net->nr_nodes--;
	_node_free(node);
}

filter_pipe_t *filternetwork_add_connection(filter_node_t *source, const char *source_port,
					    filter_node_t *dest, const char *dest_port)
{
	filter_portdesc_t *in, *out;
	filter_pipe_t *p;

	if (!source || !source_port || !dest || !dest_port
	    || source->net != dest->net)
		return NULL;
	if (FILTERNODE_IS_RUNNING(source))
		return NULL;

	if (!(in = filter_get_outputdesc(source->filter, source_port))
	    || !(out = filter_get_inputdesc(dest->filter, dest_port)))
		return NULL;

	if (!(FILTER_PORT_IS_AUTOMATIC(in->type))
	    && filternode_get_output(source, in->label))
	        return NULL;
	if (!(FILTER_PORT_IS_AUTOMATIC(out->type))
	    && filternode_get_input(dest, out->label))
	        return NULL;

	if (!(p = _pipe_alloc()))
		return NULL;
	p->in_name = out->label;
	p->out_name = in->label;
	p->source = source;
	p->dest = dest;
	if (source->filter->connect_out(source, source_port, p) == -1)
		goto _err;
	if (dest->filter->connect_in(dest, dest_port, p) == -1)
		goto _err;

	/* add the pipe to all port lists/hashes.
	 * connect_out/in may have mucked with p->dest/source, so
	 * we have to use that instead of dest/source directly. */
	list_add_input(p, p->dest);
	hash_add_input(p, p->dest);
	list_add_output(p, p->source);
	hash_add_output(p, p->source);

	/* signal input changes to destination node */
	if (dest->filter->fixup_pipe(p->dest, p) == -1)
	        goto _err_fixup;

	p->dest->nr_inputs++;
	p->source->nr_outputs++;

	return p;

 _err_fixup:
	list_remove_input(p);
	hash_remove_input(p);
	list_remove_output(p);
	hash_remove_output(p);
 _err:
	_pipe_free(p);
	return NULL;
}

void filternetwork_break_connection(filter_pipe_t *p)
{
	if (FILTERNODE_IS_RUNNING(p->source))
		return;

	/* disconnect the pipe */
	list_remove_input(p);
	hash_remove_input(p);
	list_remove_output(p);
	hash_remove_output(p);

	/* notify the connected nodes */
	p->source->filter->fixup_break_out(p->source, p);
	p->dest->filter->fixup_break_in(p->dest, p);

	/* kill the pipe */
	_pipe_free(p);
}


int filternode_set_param(filter_node_t *n, const char *label, void *val)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;

	if (!n || !label || !val)
		return -1;
	if (!(pdesc = filter_get_paramdesc(n->filter, label)))
		return -1;
	if (!(param = filternode_get_param(n, label))) {
		if (!(param = _param_alloc(pdesc)))
			return -1;
		hash_add_param(param, n);
		list_add_param(param, n);
	}

	switch (FILTER_PARAMTYPE(pdesc->type)) {
	case FILTER_PARAMTYPE_INT:
		param->val.i = *(int *)val;
		break;
	case FILTER_PARAMTYPE_FLOAT:
		param->val.f = *(float *)val;
		break;
	case FILTER_PARAMTYPE_SAMPLE:
		param->val.sample = *(SAMPLE *)val;
		break;
	case FILTER_PARAMTYPE_FILE:
		param->val.file = *(fileid_t *)val;
		break;
	case FILTER_PARAMTYPE_STRING:
		param->val.string = *(char **)val;
		break;
	}

	return n->filter->fixup_param(n, label);
}

int filternode_set_paramstring(filter_node_t *n, const char *label,
			       const char *val)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;
	int res = -1;

	if (!n || !label || !val)
		return -1;
	if (!(pdesc = filter_get_paramdesc(n->filter, label)))
		return -1;
	if (!(param = filternode_get_param(n, label))) {
		if (!(param = _param_alloc(pdesc)))
			return -1;
		hash_add_param(param, n);
		list_add_param(param, n);
	}

	switch (FILTER_PARAMTYPE(pdesc->type)) {
	case FILTER_PARAMTYPE_INT:
		res = sscanf(val, " %i ", &param->val.i);
		break;
	case FILTER_PARAMTYPE_FLOAT:
		res = sscanf(val, " %f ", &param->val.f);
		break;
	case FILTER_PARAMTYPE_SAMPLE: /* FIXME: this is SAMPLE type specific */
		res = sscanf(val, " %f ", (float *)&param->val.sample);
		break;
	case FILTER_PARAMTYPE_FILE:
		res = sscanf(val, " %i ", &param->val.file);
		break;
	case FILTER_PARAMTYPE_STRING:
		if ((param->val.string = strdup(val)))
			res = 1;
		break;
	}

	if (res == 1)
	        res = n->filter->fixup_param(n, label);

	return res;
}

int filternode_get_paramstring(filter_node_t *n, const char *label,
			       char *val, ssize_t s)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;
	int res = -1;

	if (!n || !label || !val)
		return -1;
	if (!(pdesc = filter_get_paramdesc(n->filter, label)))
		return -1;
	if (!(param = filternode_get_param(n, label)))
		return -1;

	switch (FILTER_PARAMTYPE(pdesc->type)) {
	case FILTER_PARAMTYPE_INT:
		res = snprintf(val, s, "%i", param->val.i);
		break;
	case FILTER_PARAMTYPE_FLOAT:
		res = snprintf(val, s, "%f", param->val.f);
		break;
	case FILTER_PARAMTYPE_SAMPLE: /* FIXME: this is SAMPLE type specific */
		res = snprintf(val, s, "%f", param->val.sample);
		break;
	case FILTER_PARAMTYPE_FILE:
		res = snprintf(val, s, "%i", param->val.file);
		break;
	case FILTER_PARAMTYPE_STRING:
		res = snprintf(val, s, "%s", param->val.string);
		break;
	}

	return res >=0 ? 0 : -1;
}



/* filternetwork filters / loading / saving
 * BIG FIXME!
 */

filter_t *filternetwork_get_filter(filter_network_t *net)
{
	return net->node.filter;
}

static struct filter_network_mapping *create_map(const char *label,
						 const char *node)
{
	struct filter_network_mapping *map;

	if (!(map = ALLOC(struct filter_network_mapping)))
		return NULL;
	map->label = label;
	map->node = node;

	return map;
}

filter_paramdesc_t *filternetwork_add_param(filter_network_t *net,
		      const char *node, const char *param,
              	      const char *label, const char *desc)
{
	filter_node_t *n;
	filter_paramdesc_t *d;

	if (is_hashed_filter(net->node.filter))
		return NULL;
	if (!(n = filternetwork_get_node(net, node)))
		return NULL;
	if (!(d = filter_get_paramdesc(n->filter, param)))
		return NULL;
	if (!(d = filter_add_param(net->node.filter, strdup(label),
				   strdup(desc), d->type)))
		return NULL;

	d->private = create_map(strdup(param), strdup(node));

	return d;
}

filter_portdesc_t *filternetwork_add_input(filter_network_t *net,
		     const char *node, const char *port,
		     const char *label, const char *desc)
{
	filter_node_t *n;
	filter_portdesc_t *d;

	if (is_hashed_filter(net->node.filter))
		return NULL;
	if (!(n = filternetwork_get_node(net, node)))
		return NULL;
	if (!(d = filter_get_inputdesc(n->filter, port)))
		return NULL;
	if (!(d = filter_add_input(net->node.filter, strdup(label),
				   strdup(desc), d->type)))
		return NULL;

	d->private = create_map(strdup(port), strdup(node));

	return d;
}

filter_portdesc_t *filternetwork_add_output(filter_network_t *net,
		      const char *node, const char *port,
		      const char *label, const char *desc)
{
	filter_node_t *n;
	filter_portdesc_t *d;

	if (is_hashed_filter(net->node.filter))
		return NULL;
	if (!(n = filternetwork_get_node(net, node)))
		return NULL;
	if (!(d = filter_get_outputdesc(n->filter, port)))
		return NULL;
	if (!(d = filter_add_output(net->node.filter, strdup(label),
				    strdup(desc), d->type)))
		return NULL;

	d->private = create_map(strdup(port), strdup(node));

	return d;
}


/* I would like to have the following syntax:
(filternetwork name
   (node p ping
      (export-input in pingin "input of ping"
         (export-param gain gain "gain of bla"))
      (export-output out pingout "out of ping")
      (export-param rate pingrate "hasdk")
      (set-param foo 5))
   (node n null)
# connections follow
   (connect p out n in
      (set-param gain 7))
   (connect n out p in)
   (set-param pingrate 10)
)
 * now do this with regular expressions...
 * "parens": '[[:space:]]*\((([^()]*|\()*)\) 
 */

/* Expression parsing using regular expressions and returning an
 * argv like result (by modifying the argument!).
 * Example use:
 *   '[[:space:]]*\([[:space:]]*([[:alnum:]]+)[[:space:]]+([[:alnum:]]+)[[:space:]]+([[:alnum:]]+)[[:space:]]+"([^"]*)"[[:space:]]*\)'
 * will match ' ( param node parm "description of param")'
 * and results in argv:
 *   argv[0] == "param"
 *   argv[1] == "node"
 *   argv[2] == "parm"
 *   argv[3] == "description of param"
 * Note that as extra \0s are inserted in the source string room for them
 * has to be ensured in the regular expression. Watch out for
 * sub-subexpressions, too - they can screw up your subexpression result!
 * Returns pointer to after parsed string (or NULL on error).
 *
 * Other example for "step-by-step" match:
 * 1. match '(command ....) comment\n'
 *   regexp: '[[:space:]]*\([[:space:]]*([[:alnum:]]+)[[:space:]]+(("[^"]*"|[^)"]+)+)[[:space:]]*\).*$'
 *   with argv[0] == command, argv[1] == ...
 * 2. special match the rest of the args to the command using argv[1].
 */
char *glame_parse(char *str, const char *exp, char **argv, int nargvmask)
{
	regex_t e;
	regmatch_t *m;
	int i;

	if (regcomp(&e, exp, REG_EXTENDED|REG_NEWLINE) != 0)
		return NULL;
	if (!(m = (regmatch_t *)malloc(sizeof(regmatch_t)*(e.re_nsub+1))))
		goto out;
	if (regexec(&e, str, e.re_nsub+1, m, 0) != 0)
		goto out;

	for (i=1; i<=e.re_nsub; i++) {
		if (m[i].rm_so == -1
		    || m[i].rm_so == m[i].rm_eo-1
		    || nargvmask & (1<<(i-1))) {
			argv[i-1] = NULL;
		} else {
			argv[i-1] = &str[m[i].rm_so];
			str[m[i].rm_eo] = '\0';
		}
	}
	str = &str[m[0].rm_eo];

	free(m);
	regfree(&e);
	return str;

 out:
	free(m);
	regfree(&e);
	return NULL;
}

int filternetwork_save(filter_network_t *net, const char *filename)
{
	filter_node_t *n;
	filter_pipe_t *p;
	filter_portdesc_t *d;
	filter_paramdesc_t *pd;
	char val[256];
	FILE *fd;


	if (!(fd = fopen(filename, "w")))
		return -1;

	filternetwork_foreach_node(net, n) {
		fprintf(fd, "n %s %s\n", n->filter->name, n->name);
	}

	filternetwork_foreach_node(net, n) {
		filternode_foreach_output(n, p) {
			fprintf(fd, "c %s %s %s %s\n", p->source->name,
				p->out_name, p->dest->name, p->in_name);
		}
	}

	filternetwork_foreach_node(net, n) {
		filter_foreach_inputdesc(n->filter, d) {
			if (filternode_get_input(n, d->label))
				continue;
			fprintf(fd, "i %s %s %s-i_%s %s\n", n->name, d->label,
				n->name, d->label, d->description);
		}
		filter_foreach_outputdesc(n->filter, d) {
			if (filternode_get_output(n, d->label))
				continue;
			fprintf(fd, "o %s %s %s-o_%s %s\n", n->name, d->label,
				n->name, d->label, d->description);
		}
		filter_foreach_paramdesc(n->filter, pd) {
			fprintf(fd, "p %s %s %s-%s %s\n", n->name, pd->label,
				n->name, pd->label, pd->description);
			if (filternode_get_param(n, pd->label)) {
				filternode_get_paramstring(n, pd->label, val, 255);
				fprintf(fd, "s %s-%s %s\n", n->name, pd->label, val);
			}
		}
	}


	fclose(fd);

	return 0;
}


filter_t *filternetwork_load(const char *filename)
{
	struct stat sbuf;
	FILE *fd;
	char c, node1[256], name1[256], node2[256], name2[256], desc[256];
	const char *p;
	char line[1024], **lines;
	int nr_lines;
	filter_network_t *net = NULL;
	filter_t *f;
	struct filternetspec *spec;

	if (stat(filename, &sbuf) == -1
	    || !S_ISREG(sbuf.st_mode))
		return NULL;
	if (!(fd = fopen(filename, "r")))
		return NULL;

	if (!(p = strrchr(filename, '/')))
		p = filename;
	if (!(net = filternetwork_new(strdup(p))))
	        goto err;

	/* read the file contents line by line and
	 * store this lines into a buffer of line-pointers
	 * linked through the filters private field.
	 */
	nr_lines = 0;
	lines = NULL;

	while (fgets(line, 1023, fd)) {
		lines = realloc(lines, (nr_lines+1)*sizeof(char *));
		lines[nr_lines++] = strdup(line);
	}

	spec = ALLOC(struct filternetspec);
	spec->lines = lines;
	spec->nr_lines = nr_lines;
	net->node.filter->private = spec;

	fclose(fd);
	f = filternetwork_get_filter(net);
	filternetwork_delete(net);

	return f;

 err:
	fclose(fd);
	filternetwork_delete(net);
	/* FIXME - filter_delete() umm... */
	free(f);
	return NULL;
}
