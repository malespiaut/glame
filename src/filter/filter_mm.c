/*
 * filter_mm.c
 * $Id: filter_mm.c,v 1.16 2000/10/09 16:24:02 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther
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
 *
 * Allocators with basic init && deallocators with recursive cleanup.
 */

#include <string.h>
#include "util.h"
#include "glame_sem.h"
#include "filter.h"
#include "filter_methods.h"
#include "filter_ops.h"
#include "filter_mm.h"



filter_portdesc_t *_portdesc_alloc(filter_t *filter, const char *label,
				   int type, const char *desc)
{
	filter_portdesc_t *d;

	if (!(d = ALLOC(filter_portdesc_t)))
		return NULL;
	INIT_LIST_HEAD(&d->list);
	INIT_HASH_HEAD(&d->hash);
	d->label = strdup(label);
	d->type = type;
	d->description = strdup(desc);
	d->filter = filter;
	filterpdb_init(&d->params, NULL);
	if (d->label && d->description)
		return d;

	free((char *)d->label);
	free((char *)d->description);
	free(d);
	return NULL;
}

void _portdesc_free(filter_portdesc_t *d)
{
	if (!d)
		return;
	filterpdb_delete(&d->params);
	free((char *)d->label);
	free((char *)d->description);
	free(d);
}


filter_pipe_t *_pipe_alloc(filter_portdesc_t *sourceport,
			   filter_portdesc_t *destport)
{
	filter_pipe_t *p;

	if (!(p = ALLOC(filter_pipe_t)))
		return NULL;
	INIT_LIST_HEAD(&p->input_list);
	INIT_HASH_HEAD(&p->input_hash);
	INIT_LIST_HEAD(&p->output_list);
	INIT_HASH_HEAD(&p->output_hash);
	p->source_fd = -1;
	p->dest_fd = -1;

	/* Init & copy of source/dest parameters is delayed!
	 * -- see filter_network.c::filternetwork_add_connection() */
	p->source_port = sourceport;
	p->dest_port = destport;

	/* init emitter - redirector installation is delayed!
	 * -- see filter_network.c::filternetwork_add_connection() */
	INIT_GLSIG_EMITTER(&p->emitter);

	return p;
}

void _pipe_free(filter_pipe_t *p)
{
	if (!p)
		return;

	/* first signal pipe deletion */
	glsig_emit(&p->emitter, GLSIG_PIPE_DELETED, p);

	/* delete source & dest params */
	filterpdb_delete(&p->source_params);
	filterpdb_delete(&p->dest_params);

	/* delete signal handlers */
	glsig_delete_all_handlers(&p->emitter);
	free(p);
}


filter_t *_filter_alloc(int flags)
{
	filter_t *f;

	if (!(f = ALLOC(filter_t)))
		return NULL;
	f->plugin = NULL;
	f->flags = flags;
	f->f = NULL;
	f->init = NULL;
	f->connect_out = filter_default_connect_out;
	f->connect_in = filter_default_connect_in;
	f->set_param = filter_default_set_param;
	if (flags & FILTER_FLAG_NETWORK) {
		f->f = filter_network_f;
		f->init = filter_network_init;
		f->connect_out = filter_network_connect_out;
		f->connect_in = filter_network_connect_in;
		f->set_param = filter_network_set_param;
	}
	INIT_GLSIG_EMITTER(&f->emitter);
	filterpdb_init(&f->params, NULL);
	INIT_LIST_HEAD(&f->inputs);
	INIT_LIST_HEAD(&f->outputs);
	f->priv = NULL;
	return f;
}

void _filter_free(filter_t *f)
{
	filter_portdesc_t *portd;

	if (!f)
		return;
	filterpdb_delete(&f->params);
	while ((portd = filter_first_input_portdesc(f))) {
		hash_remove_portdesc(portd);
		list_remove_portdesc(portd);
		_portdesc_free(portd);
	}
	while ((portd = filter_first_output_portdesc(f))) {
		hash_remove_portdesc(portd);
		list_remove_portdesc(portd);
		_portdesc_free(portd);
	}
	glsig_delete_all_handlers(&f->emitter);
	free(f);
}


filter_launchcontext_t *_launchcontext_alloc()
{
	filter_launchcontext_t *c;

	if (!(c = ALLOC(filter_launchcontext_t)))
		return NULL;
	if ((c->semid = glame_semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) == -1) {
		free(c);
		return NULL;
	}
	{
		union semun sun;
		sun.val = 0;
		glame_semctl(c->semid, 0, SETVAL, sun);
	}
	ATOMIC_INIT(c->result, 0);
	c->nr_threads = 0;
	if (c->semid != -1)
		return c;

	free(c);
	return NULL;
}

void _launchcontext_free(filter_launchcontext_t *c)
{
	if (!c)
		return;
	ATOMIC_RELEASE(c->result);
	{
		union semun sun;
		glame_semctl(c->semid, 0, IPC_RMID, sun);
	}
	free(c);
}


static int _node_init(filter_node_t *n, const char *name)
{
	INIT_LIST_HEAD(&n->list);
	INIT_HASH_HEAD(&n->hash);
	n->priv = NULL;
	n->ops = &filter_node_ops;
	INIT_GLSIG_EMITTER(&n->emitter);
	filterpdb_init(&n->params, n);
	n->nr_inputs = 0;
	INIT_LIST_HEAD(&n->inputs);
	n->nr_outputs = 0;
	INIT_LIST_HEAD(&n->outputs);
	INIT_LIST_HEAD(&n->buffers);
	if (!(n->name = strdup(name)))
		return -1;
	return 0;
}

static void __node_free(filter_node_t *n)
{
	filter_pipe_t *pipe;

	filterpdb_delete(&n->params);
	while ((pipe = filternode_first_input(n))) {
		hash_remove_input(pipe);
		list_remove_input(pipe);
		hash_remove_output(pipe);
		list_remove_output(pipe);
		_pipe_free(pipe);
	}
	while ((pipe = filternode_first_output(n))) {
		hash_remove_input(pipe);
		list_remove_input(pipe);
		hash_remove_output(pipe);
		list_remove_output(pipe);
	       	_pipe_free(pipe);
	}

	glsig_delete_all_handlers(&n->emitter);
	free((char *)n->name);
	free(n);
}

void _node_free(filter_node_t *n)
{
	if (!n)
		return;

	/* first signal deletion */
	glsig_emit(&n->emitter, GLSIG_NODE_DELETED, n);

	if (n->filter->flags & FILTER_FLAG_NETWORK)
		_network_free((filter_network_t *)n);
	else
		__node_free(n);
}

static int _network_init(filter_network_t *net)
{
	net->node.ops = &filter_network_ops;
	net->nr_nodes = 0;
	INIT_LIST_HEAD(&net->nodes);
	net->launch_context = NULL;
	return 0;
}

void _network_free(filter_network_t *net)
{
	filter_node_t *n;

	if (!net)
		return;
	while ((n = filternetwork_first_node(net))) {
		hash_remove_node(n);
		list_remove_node(n);
		_node_free(n);
	}
	__node_free(&net->node);
}


filter_node_t *_filter_instantiate(filter_t *f, const char *name)
{
	filter_node_t *n;

	if (f->flags & FILTER_FLAG_NETWORK) {
		if (!(n = FILTER_NODE(ALLOC(filter_network_t))))
			return NULL;
	} else {
		if (!(n = ALLOC(filter_node_t)))
			return NULL;
	}

	if (_node_init(n, name) == -1)
		goto err;
	if (f->flags & FILTER_FLAG_NETWORK)
		if (_network_init(FILTER_NETWORK(n)) == -1)
			goto err;

	/* install signal redirector */
	glsig_add_redirector(&n->emitter, ~0, &f->emitter);

	/* copy params */
	filterpdb_copy(&n->params, &f->params);
	n->filter = f;

	if (f->init && f->init(n) == -1)
		goto err;

	return n;

 err:
	_node_free(n);
	return NULL;
}

