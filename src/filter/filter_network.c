/*
 * filter_network.c
 * $Id: filter_network.c,v 1.10 2000/02/05 15:59:26 richi Exp $
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
#include <sys/ipc.h>
#include <sys/sem.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <errno.h>
#include "filter.h"
#include "util.h"


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


static void filternetwork_cleanup(filter_network_t *net)
{
	filter_buffer_t *buf;

	if (net->state != STATE_UNDEFINED)
		return;

	while ((buf = filternetwork_first_buffer(net))) {
		atomic_set(&buf->refcnt, 1);
		fbuf_unref(buf);
	}
}


/* ok, we have to launch the network back-to-front
 * (i.e. it is a _directed_ network)
 * we do this recursievely - ugh(?)
 */
int filternetwork_launch(filter_network_t *net)
{
	filter_node_t *n;
	sigset_t sigs;
	struct sembuf sop;

	/* block EPIPE */
	sigemptyset(&sigs);
	sigaddset(&sigs, SIG_BLOCK);
	sigprocmask(SIG_BLOCK, &sigs, NULL);

	if (net->state != STATE_UNDEFINED)
		return -1;

	/* init state */
	atomic_set(&net->result, 0);
	net->state = STATE_LAUNCHED;

	filternetwork_foreach_input(net, n)
		if (preprocess_node(n) == -1)
			goto out;

	filternetwork_foreach_input(net, n)
		if (init_node(n) == -1)
			goto out;

	filternetwork_foreach_input(net, n)
		if (launch_node(n) == -1)
			goto out;

	sop.sem_num = 0;
	sop.sem_op = -net->nr_nodes;
	sop.sem_flg = 0;
	semop(net->semid, &sop, 1);
	if (ATOMIC_VAL(net->result) != 0)
		goto out;

	return 0;

 out:
	filternetwork_foreach_input(net, n)
		postprocess_node(n);

	filternetwork_cleanup(net);

	net->state = STATE_UNDEFINED;

	return -1;
}

/* wait for the network to finish processing */
int filternetwork_wait(filter_network_t *net)
{
	int res, wait_again;
	filter_node_t *n;

	if (net->state != STATE_LAUNCHED)
		return -1;
	net->state = STATE_UNDEFINED;

	do {
		int filter_ret = 0;
		wait_again = 0;
		
		filternetwork_foreach_node(net, n) {
			DPRINTF("Waiting for %s.\n", n->filter->name);
			if ((res = pthread_join(n->thread, 
			                        (void **)&filter_ret)) != 0
			    && (res != ESRCH)) {
				wait_again = 1;
			}
			if (filter_ret  == -1) {
				DPRINTF("Output error caught. "
					"Terminating network.\n");
				wait_again = 0;
				filternetwork_terminate(net);
			}
		}
	} while (wait_again);

	filternetwork_foreach_input(net, n)
		postprocess_node(n);

	filternetwork_cleanup(net);

	DPRINTF("net result is %i\n", ATOMIC_VAL(net->result));

	return ATOMIC_VAL(net->result);
}

/* kill the network */
void filternetwork_terminate(filter_network_t *net)
{
	filter_node_t *n;

	filternetwork_foreach_input(net, n)
		postprocess_node(n);

	filternetwork_cleanup(net);
}



/* we preprocess the network recursively - so it has to be
 * non cyclic.
 * preprocessing does memory allocation for the file-descriptors */
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

	/* recurse on output filters */
	list_foreach_output(n, p)
		if (preprocess_node(p->dest) == -1)
			return -1;

	return 0;
}

/* we initialize the network recursively.
 * initialization does alloc the pipes */
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
	
	/* recurse on output filters */
	list_foreach_output(n, p)
		if (init_node(p->dest) == -1)
			return -1;

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
	atomic_inc(&n->net->result);

	/* increment filter ready semaphore */
	sop.sem_num = 0;
	sop.sem_op = 1;
	sop.sem_flg = 0;
	semop(n->net->semid, &sop, 1);

	pthread_exit((void *)-1);

	return NULL;
}

/* we launch the network recursively, but we have to do
 * this back to front (really??) */
static int launch_node(filter_node_t *n)
{
	filter_pipe_t *p;

	if (!n || n->state >= STATE_PRELAUNCHED)
		return 0;

	n->state = STATE_PRELAUNCHED;

	/* recurse on output filters */
	list_foreach_output(n, p)
		if (launch_node(p->dest) == -1)
			return -1;

	/* launch filter thread */
	if (pthread_create(&n->thread, NULL, launcher, n) != 0)
		return -1;

	n->state = STATE_LAUNCHED;

	return 0;
}

/* we postprocess the network recursively - so it has to be
 * non cyclic.
 * postprocessing does memory deallocation and fd-closing */
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

	/* recurse on output filters */
	list_foreach_output(n, p)
		postprocess_node(p->dest);
}



/* API */

filter_network_t *filternetwork_new(const char *name)
{
	filter_network_t *net;

	if (!name)
		return NULL;

	if (!(net = ALLOC(filter_network_t)))
		return NULL;

	net->node.name = name;

	net->nr_nodes = 0;
	INIT_LIST_HEAD(&net->nodes);
	INIT_LIST_HEAD(&net->inputs);
	INIT_LIST_HEAD(&net->buffers);

	net->state = STATE_UNDEFINED;

	ATOMIC_INIT(net->result, 0);
	if ((net->semid = semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) == -1) {
		free(net);
		return NULL;
	}

	return net;
}

void filternetwork_delete(filter_network_t *net)
{
	filter_node_t *n;

	while ((n = filternetwork_first_node(net)))
		filternode_delete(n);

	ATOMIC_RELEASE(net->result);
	semctl(net->semid, 0, IPC_RMID, 0);

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

	if (!(n = ALLOC(filter_node_t)))
		return NULL;

	/* basic init node */
	n->net = net;
	n->name = nm;
	n->filter = f;
	n->nr_params = 0;
	n->nr_inputs = 0;
	n->nr_outputs = 0;

	/* call custom init() */
	if (f->init)
		if (!(n = f->init(n)))
			return NULL;

	/* init the list after init() to allow copy & reallocate */
	INIT_LIST_HEAD(&n->neti_list);
	INIT_LIST_HEAD(&n->net_list);
	INIT_LIST_HEAD(&n->params);
	INIT_LIST_HEAD(&n->inputs);
	INIT_LIST_HEAD(&n->outputs);

	/* add node to networks net and input node lists
	 * it will be discarded if connected. Also add it
	 * to the hash using name/net.
	 */
	list_add(&n->neti_list, &net->inputs);
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
		list_del(&node->neti_list);
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

	/* remove the source filter from the net output filter list
	 * and the dest filter from the net input filter list.
	 */
	list_del(&dest->neti_list);
	INIT_LIST_HEAD(&dest->neti_list);

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

	n->filter->fixup_param(n, label);

	return 0;
}


int filternetwork_save(filter_network_t *net, const char *filename)
{
	/* FIXME */
	return -1;
}

filter_t *filternetwork_load(const char *filename)
{
	/* FIXME */
	return NULL;
}




