/*
 * filter_network.c
 * $Id: filter_network.c,v 1.17 2000/02/11 14:42:15 nold Exp $
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
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <errno.h>
#include "filter.h"
#include "util.h"
#include "sem.h"

#define STATE_UNDEFINED 0
#define STATE_PREPROCESSED 1
#define STATE_INITIALIZED 2
#define STATE_PRELAUNCHED 3
#define STATE_LAUNCHED 4


/* prototypes of internal functions */
static int preprocess_node(filter_node_t *n);
static int init_node(filter_node_t *n);
static int launch_node(filter_node_t *n);
static void postprocess_node(filter_node_t *n);
static int wait_node(filter_node_t *n);

static int preprocess_network(filter_node_t *n);
static int init_network(filter_node_t *n);
static int launch_network(filter_node_t *n);
static void postprocess_network(filter_node_t *n);
static int wait_network(filter_node_t *n);


struct filter_node_operations filter_node_ops = {
	.preprocess = preprocess_node,
	.init = init_node,
	.launch = launch_node,
	.postprocess = postprocess_node,
	.wait = wait_node,
};

struct filter_node_operations filter_network_ops = {
	.preprocess = preprocess_network,
	.init = init_network,
	.launch = launch_network,
	.postprocess = postprocess_network,
	.wait = wait_network,
};



static int preprocess_node(filter_node_t *n)
{
	filter_pipe_t *p;

	if (!n || n->state == STATE_PREPROCESSED)
		return 0;
	if (n->state != STATE_UNDEFINED)
		return -1;
	n->state = STATE_PREPROCESSED;

	/* init the pipe fds to "error" */
	list_foreach_input(n, p)
		p->dest_fd = -1;
        list_foreach_output(n, p)
		p->source_fd = -1;

	return 0;
}
static int preprocess_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;

	if (preprocess_node(n) == -1)
		return -1;

	filternetwork_foreach_node(net, n)
		if (n->ops->preprocess(n) == -1)
			return -1;

	return 0;
}

static int init_node(filter_node_t *n)
{
	filter_pipe_t *p;
	int fds[2];

	if (!n || n->state == STATE_INITIALIZED)
		return 0;
	if (n->state != STATE_PREPROCESSED)
		return -1;
	n->state = STATE_INITIALIZED;

	list_foreach_output(n, p) {
		if (pipe(fds) == -1)
			return -1;
		p->source_fd = fds[1];
		p->dest_fd = fds[0];
	}

	return 0;
}
static int init_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;

	if (init_node(n) == -1)
		return -1;

	filternetwork_foreach_node(net, n)
		if (n->ops->init(n) == -1)
			return 1;

	return 0;
}

static void *launcher(void *node)
{
	struct sembuf sop;
	filter_node_t *n = (filter_node_t *)node;

	DPRINTF("%s launched\n", n->filter->name);

	if (n->filter->f(n) == 0)
		pthread_exit(NULL);

	DPRINTF("%s had failure\n", n->filter->name);

	/* set result */
	atomic_inc(&n->launch_context->result);

	/* increment filter ready semaphore */
	sem_op(n->launch_context->semid, 0, 1);

	pthread_exit((void *)-1);

	return NULL;
}
static int launch_node(filter_node_t *n)
{
	if (!n || n->state >= STATE_PRELAUNCHED)
		return 0;

	n->state = STATE_PRELAUNCHED;

	/* launch filter thread */
	if (pthread_create(&n->thread, NULL, launcher, n) != 0)
		return -1;
	n->launch_context->nr_threads++;

	n->state = STATE_LAUNCHED;

	return 0;
}
static int launch_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;

	filternetwork_foreach_node(net, n)
		if (n->ops->launch(n) == -1)
			return -1;

	return 0;
}

/* kill threads, close pipes and free pending buffers */
static void postprocess_node(filter_node_t *n)
{
	filter_pipe_t *p;

	if (!n || n->state == STATE_UNDEFINED)
		return;

	if (n->state == STATE_LAUNCHED
	    && pthread_cancel(n->thread) != ESRCH) {
		pthread_join(n->thread, NULL);
		n->state = STATE_INITIALIZED;
	}

	list_foreach_input(n, p)
		if (p->source_fd != -1)
			close(p->source_fd);

	list_foreach_output(n, p)
		if (p->dest_fd != -1)
			close(p->dest_fd);

	n->state = STATE_UNDEFINED;
}
static void postprocess_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;
	filter_buffer_t *buf;

	postprocess_node(n);

	filternetwork_foreach_node(net, n)
		n->ops->postprocess(n);

	while ((buf = filternetwork_first_buffer(net))) {
		atomic_set(&buf->refcnt, 1);
		fbuf_unref(buf);
	}
}

static int wait_node(filter_node_t *n)
{
	int res, filter_ret;

	DPRINTF("Waiting for %s.\n", n->filter->name);
	while ((res = pthread_join(n->thread, 
				   (void **)&filter_ret)) != 0
	       && (res != ESRCH))
		;

	return filter_ret;
}
static int wait_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;
	int res = 0;

	filternetwork_foreach_node(net, n)
		if (n->ops->wait(n) == -1)
			res = -1;

	return res;
}


/* ok, we have to launch the network back-to-front
 * (i.e. it is a _directed_ network)
 * we do this recursievely - ugh(?)
 */
int filternetwork_launch(filter_network_t *net)
{
	sigset_t sigs;

	/* block EPIPE */
	sigemptyset(&sigs);
	sigaddset(&sigs, SIG_BLOCK);
	sigprocmask(SIG_BLOCK, &sigs, NULL);

	if (net->node.launch_context->state != STATE_UNDEFINED)
		return -1;

	/* init state */
	net->node.launch_context->state = STATE_LAUNCHED;
	net->node.launch_context->nr_threads = 0;
	atomic_set(&net->node.launch_context->result, 0);
	sem_zero(net->node.launch_context->semid, 0);

	if (net->node.ops->preprocess(&net->node) == -1)
		goto out;

	if (net->node.ops->init(&net->node) == -1)
		goto out;

	if (net->node.ops->launch(&net->node) == -1)
		goto out;

	sem_op(net->node.launch_context->semid, 0, 
			-net->node.launch_context->nr_threads);

	if (ATOMIC_VAL(net->node.launch_context->result) != 0)
		return -1;

	return 0;

 out:
	net->node.ops->postprocess(&net->node);
	net->node.launch_context->state = STATE_UNDEFINED;

	return -1;
}

/* wait for the network to finish processing */
int filternetwork_wait(filter_network_t *net)
{
	int res;

	if (net->node.launch_context->state != STATE_LAUNCHED)
		return -1;

	res = net->node.ops->wait(&net->node);
	DPRINTF("net result is %i\n", res);

	net->node.ops->postprocess(&net->node);
	net->node.launch_context->state = STATE_UNDEFINED;
	return res;
}

/* kill the network */
void filternetwork_terminate(filter_network_t *net)
{
	net->node.ops->postprocess(&net->node);
	net->node.launch_context->state = STATE_UNDEFINED;
}



static int filternode_init(filter_node_t *n, const char *name,
			   filter_network_t *net)
{
	n->flags = 0;
	n->net = net;
	n->name = name;
	n->ops = &filter_node_ops;
	n->filter = NULL;

	n->nr_params = 0;
	n->nr_inputs = 0;
	n->nr_outputs = 0;
	INIT_LIST_HEAD(&n->net_list);
	INIT_LIST_HEAD(&n->params);
	INIT_LIST_HEAD(&n->inputs);
	INIT_LIST_HEAD(&n->outputs);

	if (net)
		n->launch_context = net->node.launch_context;

	return 0;
}

static int filternetwork_init(filter_network_t *net, const char *name,
			      filter_network_t *pnet)

{
	if (filternode_init(&net->node, name, pnet) == -1)
		return -1;

	net->node.flags = FILTER_NODEFLAG_NETWORK;
	net->node.ops = &filter_network_ops;

	net->nr_nodes = 0;
	INIT_LIST_HEAD(&net->nodes);

	return 0;
}


/* API */

filter_network_t *filternetwork_new(const char *name)
{
	filter_network_t *net = NULL;
	filter_launchcontext_t *lc = NULL;

	if (!name)
		return NULL;

	if (!(net = ALLOC(filter_network_t)))
		return NULL;
	if (!(lc = ALLOC(filter_launchcontext_t)))
		goto _err;

	if (filternetwork_init(net, name, NULL) == -1)
		goto _err;

	if ((lc->semid = semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) == -1)
		goto _err;
	INIT_LIST_HEAD(&lc->buffers);
	ATOMIC_INIT(lc->result, 0);
	lc->state = STATE_UNDEFINED;
	net->node.launch_context = lc;

	return net;

 _err:
	free(net);
	free(lc);
	return NULL;
}

void filternetwork_delete(filter_network_t *net)
{
	filter_node_t *n;

	while ((n = filternetwork_first_node(net)))
		filternode_delete(n);

	if (net->node.launch_context) {
		ATOMIC_RELEASE(net->node.launch_context->result);
		sem_remove(net->node.launch_context->semid);
	}

	free(net);
}

filter_node_t *filternetwork_add_node(filter_network_t *net, const char *filter, const char *name)
{
	filter_t *f;
	filter_node_t *n;
	const char *nm = name;

	if (!net || !filter)
		return NULL;
	if (!(f = hash_find_filter(filter)))
		return NULL;
	if (!nm)
		nm = hash_unique_name_node(f->name, net);

	if (f->flags & FILTER_FLAG_NETWORK) {
		if (!(n = (filter_node_t *)ALLOC(filter_network_t)))
			return NULL;
		if (filternetwork_init((filter_network_t *)n, nm, net) == -1)
			return NULL;
	} else {
		if (!(n = ALLOC(filter_node_t)))
			return NULL;
		if (filternode_init(n, nm, net) == -1)
			return NULL;
	}

	/* basic init node */
	n->filter = f;

	/* call custom init() */
	if (f->init
	    && f->init(n) == -1)
		return NULL;

	/* add node to networks net and input node lists
	 * it will be discarded if connected. Also add it
	 * to the hash using name/net.
	 */
	list_add(&n->net_list, &net->nodes);
	hash_add_node(n);
	net->nr_nodes++;

	return n;
}

void filternode_delete(filter_node_t *node)
{
	filter_pipe_t *p;
	filter_paramdesc_t *d;
	filter_param_t *param;

	if (node->state > STATE_PREPROCESSED)
		return;

	if (node->flags & FILTER_NODEFLAG_NETWORK)
		filternetwork_delete((filter_network_t *)node);

	/* remove node from network */
	if (node->net) {
		list_del(&node->net_list);
		hash_remove_node(node);
		node->net->nr_nodes--;
	}

	/* break all connections, destroy parameters */
	while ((p = list_gethead_input(node)))
		filternetwork_break_connection(p);
	while ((p = list_gethead_output(node)))
		filternetwork_break_connection(p);

	list_foreach_paramdesc(node->filter, d)
		if ((param = hash_find_param(d->label, node))) {
			hash_remove_param(param);
			free(param);
		}

	/* call the cleanup routine, if one provided */
	if (node->filter->cleanup)
		node->filter->cleanup(node);

	/* destroy node */
	free(node);
}

filter_pipe_t *filternetwork_add_connection(filter_node_t *source, const char *source_port,
					    filter_node_t *dest, const char *dest_port)
{
	filter_portdesc_t *in, *out;
	filter_pipe_t *p;

	if (!source || !source_port || !dest || !dest_port
	    || source->net != dest->net)
		return NULL;

	if (!(in = hash_find_outputdesc(source_port, source->filter))
	    || !(out = hash_find_inputdesc(dest_port, dest->filter)))
		return NULL;

	if (!(FILTER_PORT_IS_AUTOMATIC(in->type))
	    && hash_find_output(in->label, source))
	        return NULL;
	if (!(FILTER_PORT_IS_AUTOMATIC(out->type))
	    && hash_find_input(out->label, dest))
	        return NULL;

	if (!(p = ALLOC(filter_pipe_t)))
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
	list_del_input(p);
	hash_remove_input(p);
	list_del_output(p);
	hash_remove_output(p);
 _err:
	free(p);
	return NULL;
}

void filternetwork_break_connection(filter_pipe_t *p)
{
	/* disconnect the pipe */
	list_del_input(p);
	hash_remove_input(p);
	list_del_output(p);
	hash_remove_output(p);

	/* notify the connected nodes */
	p->source->filter->fixup_break_out(p->source, p);
	p->dest->filter->fixup_break_in(p->dest, p);

	/* kill the pipe */
	free(p);
}


int filternode_setparam(filter_node_t *n, const char *label, void *val)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;

	if (!n || !label || !val)
		return -1;
	if (!(pdesc = hash_find_paramdesc(label, n->filter)))
		return -1;
	if (!(param = hash_find_param(label, n))) {
		if (!(param = ALLOC(filter_param_t)))
			return -1;
		param->label = pdesc->label;
		hash_add_param(param, n);
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

int filternode_setparamstring(filter_node_t *n, const char *label,
			      const char *val)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;
	int res = -1;

	if (!n || !label || !val)
		return -1;
	if (!(pdesc = hash_find_paramdesc(label, n->filter)))
		return -1;
	if (!(param = hash_find_param(label, n))) {
		if (!(param = ALLOC(filter_param_t)))
			return -1;
		param->label = pdesc->label;
		hash_add_param(param, n);
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

int filternode_getparamstring(filter_node_t *n, const char *label,
			      char *val, ssize_t s)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;
	int res = -1;

	if (!n || !label || !val)
		return -1;
	if (!(pdesc = hash_find_paramdesc(label, n->filter)))
		return -1;
	if (!(param = hash_find_param(label, n)))
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

static int fnf_param(filter_t *f, filter_network_t *net,
		     char *node, char *label,
		     char *extern_label, char *desc)
{
	filter_node_t *n;
	filter_paramdesc_t *d;

	if (!(n = hash_find_node(node, net)))
		return -1;
	if (!(d = hash_find_paramdesc(label, n->filter)))
		return -1;
	if (!(d = filter_add_param(f, strdup(extern_label),
				   strdup(desc), d->type)))
		return -1;

	d->private = create_map(strdup(label), strdup(node));

	return 0;
}

static int fnf_input(filter_t *f, filter_network_t *net,
		     char *node, char *label,
		     char *extern_label, char *desc)
{
	filter_node_t *n;
	filter_portdesc_t *d;

	if (!(n = hash_find_node(node, net)))
		return -1;
	if (!(d = hash_find_inputdesc(label, n->filter)))
		return -1;
	if (!(d = filter_add_input(f, strdup(extern_label),
				   strdup(desc), d->type)))
		return -1;

	d->private = create_map(strdup(label), strdup(node));

	return 0;
}

static int fnf_output(filter_t *f, filter_network_t *net,
		      char *node, char *label,
		      char *extern_label, char *desc)
{
	filter_node_t *n;
	filter_portdesc_t *d;

	if (!(n = hash_find_node(node, net)))
		return -1;
	if (!(d = hash_find_outputdesc(label, n->filter)))
		return -1;
	if (!(d = filter_add_output(f, strdup(extern_label),
				    strdup(desc), d->type)))
		return -1;

	d->private = create_map(strdup(label), strdup(node));

	return 0;
}


int filternetwork_filter_init(filter_node_t *n)
{
	FILE *fd;
	char c, node1[256], name1[256], node2[256], name2[256], desc[256];
	filter_network_t *net = (filter_network_t *)n;


	if (!(fd = fopen((char *)n->filter->private, "r")))
		return -1;


	while (fscanf(fd, "%c", &c) == 1) {
		switch (c) {
		case 'n':
			fscanf(fd, " %s %s ", node1, name1);
			if (!filternetwork_add_node(net, node1, strdup(name1)))
				goto err;
			break;
		case 'c':
			fscanf(fd, " %s %s %s %s ", node1, name1,
			       node2, name2);
			if (!filternetwork_add_connection(hash_find_node(node1, net),
							  name1, hash_find_node(node2, net), name2))
				goto err;
			break;
		case 'i': /* skip */
			fgets(desc, 256, fd);
			break;
		case 'o': /* skip */
			fgets(desc, 256, fd);
			break;
		case 'p': /* skip */
			fgets(desc, 256, fd);
			break;
		case 's':
			fscanf(fd, " %s %s ", name1, desc);
			filternode_setparamstring(&net->node, name1, desc);
			break;
		default:
			break;
		}
	}

	fclose(fd);
	return 0;

 err:
	fclose(fd);
	filternetwork_delete(net);
	return -1;
}

void filternetwork_filter_cleanup(filter_node_t *n)
{
	filternetwork_delete((filter_network_t *)n);
}

int filternetwork_filter_f(filter_node_t *n)
{
	PANIC("uh? filternetwork launched??");
	return -1;
}

int filternetwork_filter_connect_out(filter_node_t *source, const char *port,
				     filter_pipe_t *p)
{
	filter_portdesc_t *d;
	struct filter_network_mapping *m;
	filter_node_t *n;

	if (!(d = hash_find_outputdesc(port, source->filter)))
		return -1;
	m = (struct filter_network_mapping *)d->private;
	if (!(n = hash_find_node(m->node, source)))
		return -1;
	p->out_name = m->label;
	p->source = n;

	return 0;
}

int filternetwork_filter_connect_in(filter_node_t *dest, const char *port,
				    filter_pipe_t *p)
{
	filter_portdesc_t *d;
	struct filter_network_mapping *m;
	filter_node_t *n;

	if (!(d = hash_find_inputdesc(port, dest->filter)))
		return -1;
	m = (struct filter_network_mapping *)d->private;
	if (!(n = hash_find_node(m->node, dest)))
		return -1;
	p->in_name = m->label;
	p->dest = n;

	return 0;
}

int filternetwork_filter_fixup_param(filter_node_t *node, const char *name)
{
	filter_paramdesc_t *d;
	filter_param_t *p;
	struct filter_network_mapping *m;
	filter_node_t *n;

	if (!(p = hash_find_param(name, node)))
		return -1;
	if (!(d = hash_find_paramdesc(name, node->filter)))
		return -1;
	m = (struct filter_network_mapping *)d->private;
	if (!(n = hash_find_node(m->node, node)))
		return -1;
	return filternode_setparam(n, m->label, &p->val);
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
		list_foreach_output(n, p) {
			fprintf(fd, "c %s %s %s %s\n", p->source->name,
				p->out_name, p->dest->name, p->in_name);
		}
	}

	filternetwork_foreach_node(net, n) {
		list_foreach_inputdesc(n->filter, d) {
			if (hash_find_input(d->label, n))
				continue;
			fprintf(fd, "i %s %s %s-i_%s %s\n", n->name, d->label,
				n->name, d->label, d->description);
		}
		list_foreach_outputdesc(n->filter, d) {
			if (hash_find_output(d->label, n))
				continue;
			fprintf(fd, "o %s %s %s-o_%s %s\n", n->name, d->label,
				n->name, d->label, d->description);
		}
		list_foreach_paramdesc(n->filter, pd) {
			fprintf(fd, "p %s %s %s-%s %s\n", n->name, pd->label,
				n->name, pd->label, pd->description);
			if (hash_find_param(pd->label, n)) {
				filternode_getparamstring(n, pd->label, val, 255);
				fprintf(fd, "s %s-%s %s\n", n->name, pd->label, val);
			}
		}
	}


	fclose(fd);

	return 0;
}

filter_t *filternetwork_load(const char *filename)
{
	FILE *fd;
	filter_t *f = NULL;
	char c, node1[256], name1[256], node2[256], name2[256], desc[256];
	const char *p;
	filter_network_t *net = NULL;

	if (!(fd = fopen(filename, "r")))
		return NULL;

	if (!(p = strrchr(filename, '/')))
		p = filename;


        if (!(f = filter_alloc(strdup(p), NULL, filternetwork_filter_f)))
		return NULL;
	if (!(net = ALLOC(filter_network_t)))
	        goto err;
	filternetwork_init(net, NULL, NULL);

	f->flags = FILTER_FLAG_NETWORK;
	f->private = strdup(filename);
	f->init = filternetwork_filter_init;
	f->cleanup = filternetwork_filter_cleanup;
	f->connect_out = filternetwork_filter_connect_out;
	f->connect_in = filternetwork_filter_connect_in;
	f->fixup_param = filternetwork_filter_fixup_param;

	while (fscanf(fd, "%c", &c) == 1) {
		switch (c) {
		case 'n':
			fscanf(fd, " %s %s ", node1, name1);
			if (!filternetwork_add_node(net, node1, strdup(name1)))
				goto err;
			break;
		case 'c':
			fscanf(fd, " %s %s %s %s ", node1, name1,
			       node2, name2);
			if (!filternetwork_add_connection(hash_find_node(node1, net),
							  name1, hash_find_node(node2, net), name2))
				goto err;
			break;
		case 'i':
			fscanf(fd, " %s %s %s ", node1, name1, name2);
			fgets(desc, 255, fd);
			if (fnf_input(f, net, node1, name1, name2, desc) == -1)
				goto err;
			break;
		case 'o':
			fscanf(fd, " %s %s %s ", node1, name1, name2);
			fgets(desc, 255, fd);
			if (fnf_output(f, net, node1, name1, name2, desc) == -1)
				goto err;
			break;
		case 'p':
			fscanf(fd, " %s %s %s ", node1, name1, name2);
			fgets(desc, 255, fd);
			if (fnf_param(f, net, node1, name1, name2, desc) == -1)
				goto err;
			break;
		case 's':
			fgets(desc, 256, fd);
			break;
		default:
			DPRINTF("wrong command %c\n", c);
			break;
		}
	}

	fclose(fd);
	filternetwork_delete(net);

	return f;

 err:
	fclose(fd);
	filternetwork_delete(net);
	/* FIXME - filter_delete() umm... */
	free(f);
	return NULL;
}




