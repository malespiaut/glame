/*
 * filter_network.c
 * $Id: filter_network.c,v 1.32 2000/03/20 09:42:44 richi Exp $
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




/* filter node API.
 */

static void set_param(filter_param_t *param, void *val)
{
	switch (FILTER_PARAMTYPE(param->desc->type)) {
	case FILTER_PARAMTYPE_INT:
		param->val.i = *(int *)val;
		break;
	case FILTER_PARAMTYPE_FLOAT:
		param->val.f = *(float *)val;
		break;
	case FILTER_PARAMTYPE_SAMPLE:
		param->val.sample = *(SAMPLE *)val;
		break;
	case FILTER_PARAMTYPE_STRING:
		free(param->val.string);
		param->val.string = strdup((char *)val);
		break;
	case FILTER_PARAMTYPE_LIST:
	        param->val.list = *(int *)val;
		break;
	}
}

char *filterparam_to_string(filter_param_t *param)
{
	char buf[512];

	if (!param)
		return NULL;

	switch (FILTER_PARAMTYPE(param->desc->type)) {
	case FILTER_PARAMTYPE_INT:
		snprintf(buf, 511, "%i", param->val.i);
		break;
	case FILTER_PARAMTYPE_FLOAT:
		snprintf(buf, 511, "%f", param->val.f);
		break;
	case FILTER_PARAMTYPE_SAMPLE:
		/* FIXME: this is SAMPLE type specific */
		snprintf(buf, 511, "%f", param->val.sample);
		break;
	case FILTER_PARAMTYPE_STRING:
		snprintf(buf, 511, "\"%s\"", param->val.string);
		break;
	case FILTER_PARAMTYPE_LIST:
	        snprintf(buf, 511, "%i", param->val.list);
		break;
	default:
		return NULL;
	}

	return strdup(buf);
}

void *filterparamval_from_string(filter_paramdesc_t *pdesc, const char *val)
{
	filter_param_t param;
	char s[512];
	void *m;
	int res;

	if (!pdesc || !val)
		return NULL;

	switch (FILTER_PARAMTYPE(pdesc->type)) {
	case FILTER_PARAMTYPE_INT:
		res = sscanf(val, " %i ", &param.val.i);
		break;
	case FILTER_PARAMTYPE_FLOAT:
		res = sscanf(val, " %f ", &param.val.f);
		break;
	case FILTER_PARAMTYPE_SAMPLE: /* FIXME: this is SAMPLE type specific */
		res = sscanf(val, " %f ", &param.val.sample);
		break;
	case FILTER_PARAMTYPE_STRING:
		if ((res = sscanf(val, " \"%511[^\"]\" ", s)) == 1) {
			return strdup(s);
		} else if ((res = sscanf(val, " %511[^\"] ", s)) == 1) {
		        return strdup(s);
		}
		break;
	case FILTER_PARAMTYPE_LIST:
		res = sscanf(val, " %i ", &param.val.list);
		break;
	default:
		return NULL;
	}
	if (res != 1)
		return NULL;

	if (!(m = malloc(sizeof(param.val))))
		return NULL;
	memcpy(m, &param.val, sizeof(param.val));

	return m;
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
	set_param(param, val);

	return n->filter->fixup_param(n, NULL, label, param);
}

int filterpipe_set_sourceparam(filter_pipe_t *p, const char *label, void *val)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;

	if (!p || !label || !val)
		return -1;
	if (!(pdesc = filterportdesc_get_paramdesc(p->source_port, label)))
		return -1;
	if (!(param = filterpipe_get_sourceparam(p, label))) {
		if (!(param = _param_alloc(pdesc)))
			return -1;
		hash_add_sourceparam(param, p);
		list_add_sourceparam(param, p);
	}
	set_param(param, val);

	/* FIXME: ummm, need fixup_pipesourceparam???? */
	return p->source->filter->fixup_param(p->source, p, label, param);
}

int filterpipe_set_destparam(filter_pipe_t *p, const char *label, void *val)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;

	if (!p || !label || !val)
		return -1;
	if (!(pdesc = filterportdesc_get_paramdesc(p->dest_port, label)))
		return -1;
	if (!(param = filterpipe_get_destparam(p, label))) {
		if (!(param = _param_alloc(pdesc)))
			return -1;
		hash_add_destparam(param, p);
		list_add_destparam(param, p);
	}
	set_param(param, val);

	/* FIXME: ummm, need fixup_pipedestparam???? */
	return p->dest->filter->fixup_param(p->dest, p, label, param);
}




/* filter network API.
 */

int filternetwork_launch(filter_network_t *net)
{
	sigset_t sigs;

	if (!net || FILTERNETWORK_IS_RUNNING(net))
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

	DPRINTF("all nodes launched.\n");

	return 0;

 out:
	DPRINTF("error.\n");
	filternetwork_terminate(net);

	return -1;
}

int filternetwork_start(filter_network_t *net)
{
	if (!net || !FILTERNETWORK_IS_RUNNING(net))
		return -1;

	sem_op(net->launch_context->semid, 0,
	       -net->launch_context->nr_threads);
	if (ATOMIC_VAL(net->launch_context->result) != 0)
		goto out;

	return 0;

out:
	DPRINTF("Network error. Terminating.\n");
	filternetwork_terminate(net);
	return -1;
}

int filternetwork_pause(filter_network_t *net)
{
	if (!net || !FILTERNETWORK_IS_RUNNING(net))
		return -1;

	sem_op(net->launch_context->semid, 0,
	       +net->launch_context->nr_threads);

	return 0;
}

int filternetwork_wait(filter_network_t *net)
{
	int res;

	if (!net || !FILTERNETWORK_IS_RUNNING(net))
		return -1;

	res = net->node.ops->wait(FILTER_NODE(net));

	net->node.ops->postprocess(&net->node);
	_launchcontext_free(net->launch_context);
	net->launch_context = NULL;

	return res;
}

void filternetwork_terminate(filter_network_t *net)
{
	if (!net || !FILTERNETWORK_IS_RUNNING(net))
		return;
	atomic_set(&net->launch_context->result, 1);
	sem_zero(net->launch_context->semid, 0);

	/* "safe" cleanup */
	filternetwork_wait(net);
}


filter_network_t *filternetwork_new()
{
	filter_network_t *net = NULL;
	filter_t *f = NULL;

	if (!(f = _filter_alloc(FILTER_FLAG_NETWORK)))
	        return NULL;
	if (!(net = FILTER_NETWORK(_filter_instantiate(f, "network"))))
		goto err;
	net->node.filter->private = net;

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
		_filter_free(f);
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
	hash_add_node(n);
	list_add_node(n);
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
	filter_portdesc_t *out, *in;
	filter_pipe_t *p;

	if (!source || !source_port || !dest || !dest_port
	    || source->net != dest->net)
		return NULL;
	if (FILTERNODE_IS_RUNNING(source))
		return NULL;

	/* are there ports with the specified names? */
	if (!(out = filter_get_outputdesc(source->filter, source_port))
	    || !(in = filter_get_inputdesc(dest->filter, dest_port)))
		return NULL;

	/* are there already connections to the ports and do
	 * they not have the AUTOMATIC flag set? */
	if (!(FILTER_PORT_IS_AUTOMATIC(out->type))
	    && filternode_get_output(source, out->label))
	        return NULL;
	if (!(FILTER_PORT_IS_AUTOMATIC(in->type))
	    && filternode_get_input(dest, in->label))
	        return NULL;

	if (!(p = _pipe_alloc(out, in)))
		return NULL;
	p->in_name = in->label;
	p->out_name = out->label;
	p->source = source;
	p->dest = dest;
	if (source->filter->connect_out(source, source_port, p) == -1)
		goto _err;

	/* do we support the requested pipe type? */
	if (!FILTER_PORT_IS_COMPATIBLE(in->type, p->type))
		return NULL;

	if (dest->filter->connect_in(dest, dest_port, p) == -1)
		goto _err;

	/* add the pipe to all port lists/hashes.
	 * connect_out/in may have mucked with p->dest/source, so
	 * we have to use that instead of dest/source directly. */
	list_add_input(p, p->dest);
	hash_add_input(p, p->dest);
	list_add_output(p, p->source);
	hash_add_output(p, p->source);
	p->dest->nr_inputs++;
	p->source->nr_outputs++;

	/* signal input changes to destination node */
	dest->filter->fixup_pipe(p->dest, p);

	return p;

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
	p->source->nr_outputs--;
	list_remove_output(p);
	hash_remove_output(p);
	p->dest->nr_inputs--;

	/* notify the connected nodes */
	p->source->filter->fixup_break_out(p->source, p);
	p->dest->filter->fixup_break_in(p->dest, p);

	/* kill the pipe */
	_pipe_free(p);
}



/* filternetwork filters / loading / saving
 * BIG FIXME!
 */

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

	if (!net || !node || !param || !label)
		return NULL;
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

	if (!net || !node || !port || !label)
		return NULL;
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

	if (!net || !node || !port || !label)
		return NULL;
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
 * (let* ((net (filternetwork_new))
 *        (nodename (filternetwork_add_node net "filter" "nodename")))
 *    (filternode_set_param ....)
 *    (filternetwork_add_input ...)
 *    (filternetwork_add_output ...)
 *    (filternetwork_add_param ...)
 *    (filternode_set_param ....) ; wrapped ones
 *    (let ((pipe (filternetwork_add_connection node "port" node "port)))
 *        (filterpipe_set_sourceparam ...)
 *        (filterpipe_set_destparam ...))
 *    net)
 */
char *filternetwork_to_string(filter_network_t *net)
{
	char *buf, *val;
	int len;
	filter_node_t *n;
	filter_portdesc_t *portd;
	filter_paramdesc_t *paramd;
	filter_param_t *param;
	filter_pipe_t *fpipe;

	if (!net)
		return NULL;

	/* first alloc a big-enough buffer FIXME - br0ken. */
	if (!(buf = (char *)malloc(64*1024)))
		return NULL;
	len = 0;

	/* generate the network start part */
	len += sprintf(&buf[len], "(let *((net (filternetwork_new))\n");

	/* iterate over all nodes in the network creating
	 * node create commands. */
	filternetwork_foreach_node(net, n) {
		len += sprintf(&buf[len], "\t(%s (filternetwork_add_node net \"%s\" \"%s\"))\n",
			       n->name, n->filter->name, n->name);
	}
	/* ((net .. */
	len += sprintf(&buf[len-1], ")\n") - 1;

	/* first create the parameter set commands for the
	 * nodes. */
	filternetwork_foreach_node(net, n) {
		filternode_foreach_param(n, param) {
			val = filterparam_to_string(param);
			len += sprintf(&buf[len], "   (filternode_set_param %s \"%s\" %s)\n",
				       n->name, param->label, val);
			free(val);
		}
	}

	/* create the port/parameter export commands */
	filternetwork_foreach_node(net, n) {
		/* iterate over all exported inputs/outputs
		 * and params possibly creating export commands. */
		filter_foreach_inputdesc(net->node.filter, portd) {
			/* check, if exported output is "our" one */
			if (strcmp(n->name, filterdesc_map_node(portd)) != 0
			    || !filter_get_inputdesc(n->filter, filterdesc_map_label(portd)))
				continue;
			len += sprintf(&buf[len], "   (filternetwork_add_input net %s \"%s\" \"%s\" \"%s\")\n",
				       n->name, filterdesc_map_label(portd),
				       portd->label, portd->description);
			/* port parameters are magically exported
			 * too - they are set "directly" using the
			 * filterpipe_set_param() call - only querying
			 * those parameter descriptors is little bit
			 * messy ATM.
			 */
		}
		filter_foreach_outputdesc(net->node.filter, portd) {
			/* check, if exported output is "our" one */
			if (strcmp(n->name, filterdesc_map_node(portd)) != 0
			    || !filter_get_outputdesc(n->filter, filterdesc_map_label(portd)))
				continue;
			len += sprintf(&buf[len], "   (filternetwork_add_output net %s \"%s\" \"%s\" \"%s\")\n",
				       n->name, filterdesc_map_label(portd),
				       portd->label, portd->description);
			/* port parameters - dto. */
		}
		filter_foreach_paramdesc(net->node.filter, paramd) {
			/* check, if exported param is "our" one */
			if (strcmp(n->name, filterdesc_map_node(paramd)) != 0
			    || !filter_get_paramdesc(n->filter, filterdesc_map_label(paramd)))
				continue;
			len += sprintf(&buf[len], "   (filternetwork_add_param net %s \"%s\" \"%s\" \"%s\")\n",
				       n->name, filterdesc_map_label(paramd),
				       paramd->label, paramd->description);
		}
	}

	/* iterate over all connections and create connect
	 * commands. */
	filternetwork_foreach_node(net, n) {
		filternode_foreach_input(n, fpipe) {
			len += sprintf(&buf[len], "   (let ((pipe (filternetwork_add_connection %s \"%s\" %s \"%s\")))\n",
				       fpipe->source->name, fpipe->out_name,
				       fpipe->dest->name, fpipe->in_name);

			/* iterate over all pipe dest parameters creating
			 * parameter set commands. */
			filterpipe_foreach_destparam(fpipe, param) {
				val = filterparam_to_string(param);
				len += sprintf(&buf[len], "\t(filterpipe_set_destparam pipe \"%s\" %s)\n",
					       param->label, val);
				free(val);
			}

			/* iterate over all pipe source parameters creating
			 * parameter set commands. */
			filterpipe_foreach_sourceparam(fpipe, param) {
				val = filterparam_to_string(param);
				len += sprintf(&buf[len], "\t(filterpipe_set_sourceparam pipe \"%s\" %s)\n",
					       param->label, val);
				free(val);
			}
			
			/* (let ((pipe... */
			len += sprintf(&buf[len-1], ")\n") - 1;
		}
	}

	/* (let* ... */
	len += sprintf(&buf[len], "   net)\n");

	return buf;
}


#if 0
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
void glame_parse_init(char **argv, int argc)
{
	int i;
	for (i=0; i<argc; i++)
		argv[i] = NULL;
}

void glame_parse_free(char **argv, int argc)
{
	int i;
	for (i=0; i<argc; i++) {
		free(argv[i]);
		argv[i] = NULL;
	}
}

char *glame_parse(char *str, const char *exp, char **argv, int nargvmask)
{
	regex_t e;
	regmatch_t *m;
	int i;

	if (regcomp(&e, exp, REG_EXTENDED|REG_NEWLINE) != 0)
		return NULL;
	if (!(m = (regmatch_t *)malloc(sizeof(regmatch_t)*(e.re_nsub+1))))
		goto out;
	if (regexec(&e, str, e.re_nsub+1, m, 0) != 0) {
		DPRINTF("parse error.\n");
		goto out;
	}

	for (i=1; i<=e.re_nsub; i++) {
		if (m[i].rm_so == -1
		    || m[i].rm_so == m[i].rm_eo
		    || nargvmask & (1<<(i-1))) {
			argv[i-1] = NULL;
		} else {
			argv[i-1] = (char *)malloc(m[i].rm_eo-m[i].rm_so+1);
			strncpy(argv[i-1], &str[m[i].rm_so],
				m[i].rm_eo-m[i].rm_so);
			argv[i-1][m[i].rm_eo-m[i].rm_so] = '\0';
      			/* DPRINTF("%i) \"%s\"\n", i-1, argv[i-1]); */
		}
	}
	str = &str[m[0].rm_eo];
	/* DPRINTF("-> \"%s\"\n", str); */

	free(m);
	regfree(&e);
	return str;

 out:
	free(m);
	regfree(&e);
	return NULL;
}


#define CMD       "([[:alpha:]-]+)"
#define IDENT     "([[:alpha:]][[:alnum:]_-]*)"
#define IDENT_OPT "([[:alpha:]]?|[[:alpha:]][[:alnum:]_-]*)"
#define ARG       "([[:alnum:]._-]+|\"[^\"]*\")"
#define ARG_OPT   "([[:alnum:]._-]*|\"[^\"]*\")"

#define FNREGEXP_ARGCOMMAND "[[:space:]]*\\([[:space:]]*"CMD"[[:space:]]+"IDENT"[[:space:]]+"ARG"[[:space:]]*"ARG_OPT"[[:space:]]*\\)|[[:space:]]*\\)"

#define FNREGEXP_NODE "[[:space:]]*"IDENT"[[:space:]]+"IDENT
#define FNREGEXP_CONNECT "[[:space:]]*"IDENT"[[:space:]]+"IDENT"[[:space:]]+"IDENT"[[:space:]]+"IDENT"[[:space:]]*"
#define FNREGEXP_SETPARAM "[[:space:]]*"IDENT"[[:space:]]+"ARG"[[:space:]]*\\)"

#define FNREGEXP_COMMAND "[[:space:]]*\\([[:space:]]*"CMD"[[:space:]]+|[[:space:]]*\\)"

#define FNREGEXP_FILTERNETWORK "[[:space:]]*\\([[:space:]]*filternetwork[[:space:]]+"IDENT"[[:space:]]+"ARG"[[:space:]]+"

static int parse_node(filter_network_t *net, char **buf)
{
	char *argv[8];
	char *b;
	filter_node_t *n;
	void *val;

	glame_parse_init(argv, 8);

	/* parse " name filter" */
	if (!(b = glame_parse(*buf, FNREGEXP_NODE, argv, 0)))
		return -1;
	*buf = b;
	if (!(n = filternetwork_add_node(net, argv[1], argv[0]))) {
		DPRINTF("error in adding node (filter %s)\n", argv[1]);
		goto err;
	}

	do {
	        glame_parse_free(argv, 8);

		/* parse node specific commands with args */
		if (!(b = glame_parse(*buf, FNREGEXP_ARGCOMMAND, argv, 0)))
			return -1;
		*buf = b;
		if (!argv[0]) {
			return 0;
		} else if (strcmp(argv[0], "export-input") == 0) {
			if (!filternetwork_add_input(net, n->name, argv[1],
						     argv[2], argv[3]))
				goto err;
		} else if (strcmp(argv[0], "export-output") == 0) {
			if (!filternetwork_add_output(net, n->name, argv[1],
						      argv[2], argv[3]))
				goto err;
		} else if (strcmp(argv[0], "export-param") == 0) {
			if (!filternetwork_add_param(net, n->name, argv[1],
						     argv[2], argv[3]))
				goto err;
		} else if (strcmp(argv[0], "set-param") == 0) {
			if (!(val = filterparamval_from_string(filter_get_paramdesc(n->filter, argv[1]), argv[2])))
				goto err;
			filternode_set_param(n, argv[1], val);
			free(val);
		} else
			goto err;
	} while (1);

	return 0;

err:
	DPRINTF("%s\n", *buf);
	glame_parse_free(argv, 8);
	return -1;
}

static int parse_connect(filter_network_t *net, char **buf)
{
	char *argv[8];
	char *b;
	filter_pipe_t *p;
	void *val;

	glame_parse_init(argv, 8);

	/* parse " n1 p1 n2 p2" */
	if (!(b = glame_parse(*buf, FNREGEXP_CONNECT, argv, 0)))
		return -1;
	*buf = b;
	if (!(p = filternetwork_add_connection(filternetwork_get_node(net, argv[0]), argv[1],
					       filternetwork_get_node(net, argv[2]), argv[3]))) {
		DPRINTF("unable to add connection %s::%s - %s::%s\n", argv[0], argv[1],
			argv[2], argv[3]);
		return -1;
	}

	do {
		glame_parse_free(argv, 8);

		/* parse connection specific commands with args */
		if (!(b = glame_parse(*buf, FNREGEXP_ARGCOMMAND, argv, 0)))
			goto err;
		*buf = b;
		if (!argv[0]) {
			return 0;
		} else if (strcmp(argv[0], "set-sourceparam") == 0) {
			if (!(val = filterparamval_from_string(filterportdesc_get_paramdesc(p->source_port, argv[1]), argv[2])))
				goto err;
			filterpipe_set_sourceparam(p, argv[1], val);
			free(val);
		} else if (strcmp(argv[0], "set-destparam") == 0) {
			if (!(val = filterparamval_from_string(filterportdesc_get_paramdesc(p->dest_port, argv[1]), argv[2])))
				goto err;
			filterpipe_set_destparam(p, argv[1], val);
			free(val);
		} else
			goto err;
	} while (1);
	return 0;

err:
	DPRINTF("%s\n", *buf);
	glame_parse_free(argv, 8);
	return -1;
}

static int parse_setparam(filter_network_t *net, char **buf)
{
	char *argv[8];
	char *b;
	void *val;

	glame_parse_init(argv, 8);

	/* parse " param value)" */
	if (!(b = glame_parse(*buf, FNREGEXP_SETPARAM, argv, 0)))
		return -1;
	*buf = b;

	if (!(val = filterparamval_from_string(filter_get_paramdesc(FILTER_NODE(net)->filter, argv[0]), argv[1]))) {
	        glame_parse_free(argv, 8);
		return -1;
	}
	filternode_set_param(FILTER_NODE(net), argv[0], val);
	free(val);
	glame_parse_free(argv, 8);

	return 0;
}

static int parse_command(filter_network_t *net, char **buf)
{
	char *argv[8];
	char *b;

	glame_parse_init(argv, 8);

	/* parse "(command " or ")" */
	if (!(b = glame_parse(*buf, FNREGEXP_COMMAND, argv, 0)))
		return -1;
	*buf = b;
	if (!argv[0]) {
		return 0;
	} else if (strcmp(argv[0], "node") == 0) {
		if (parse_node(net, buf) == -1)
			goto err;
	} else if (strcmp(argv[0], "connect") == 0) {
		if (parse_connect(net, buf) == -1)
			goto err;
	} else if (strcmp(argv[0], "set-param") == 0) {
		if (parse_setparam(net, buf) == -1)
			goto err;
	} else
		goto err;

	glame_parse_free(argv, 8);
	return 1;

err:
	DPRINTF("%s\n", *buf);
	glame_parse_free(argv, 8);
	return -1;
}

filter_network_t *string2net(char *buf, filter_network_t *net)
{
	char *argv[8];
	int freenet = 0, res;

	glame_parse_init(argv, 8);

	/* parse "(filternetwork name" */
	if (!(buf = glame_parse(buf, FNREGEXP_FILTERNETWORK, argv, 0)))
		goto err;
	if (!net) {
		if (!(net = filternetwork_new(argv[0], argv[1]))) {
			DPRINTF("unable to create network\n");
			goto err;
		}
		freenet = 1;
	}

	do {
		res = parse_command(net, &buf);
	} while (res > 0);
	if (res == -1)
		goto err_net;

	glame_parse_free(argv, 8);
	return net;

err_net:
	if (freenet)
		filternetwork_delete(net);
err:
	glame_parse_free(argv, 8);
	return NULL;
}

filter_network_t *filternetwork_from_string(const char *buf)
{
	filter_network_t *net;
	char *str;

	if (!buf || !(str = strdup(buf)))
		return NULL;

	net = string2net(str, NULL);
	free(str);
	return net;
}
#endif
