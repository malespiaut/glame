/*
 * filter_mm.c
 * $Id: filter_mm.c,v 1.1 2000/02/14 13:23:40 richi Exp $
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
#include "filter.h"
#include "filter_methods.h"
#include "filter_ops.h"
#include "filter_mm.h"



filter_paramdesc_t *_paramdesc_alloc(const char *label, int type,
				     const char *desc)
{
	filter_paramdesc_t *d;

	if (!(d = ALLOC(filter_paramdesc_t)))
		return NULL;
	d->label = strdup(label);
	d->type = type;
	d->description = strdup(desc);
	if (d->label && d->description)
		return d;

	free((char *)d->label);
	free((char *)d->description);
	free(d);
	return NULL;
}

void _paramdesc_free(filter_paramdesc_t *d)
{
	if (!d)
		return;
	free((char *)d->label);
	free((char *)d->description);
	free(d);
}


filter_param_t *_param_alloc(filter_paramdesc_t *d)
{
	filter_param_t *p;

	if (!(p = ALLOC(filter_param_t)))
		return NULL;
	p->label = d->label;
	return p;
}

void _param_free(filter_param_t *p)
{
	free(p);
}


filter_portdesc_t *_portdesc_alloc(const char *label, int type,
				   const char *desc)
{
	filter_portdesc_t *d;

	if (!(d = ALLOC(filter_portdesc_t)))
		return NULL;
	d->label = strdup(label);
	d->type = type;
	d->description = strdup(desc);
	d->nr_params = 0;
	INIT_LIST_HEAD(&d->params);
	if (d->label && d->description)
		return d;

	free((char *)d->label);
	free((char *)d->description);
	free(d);
	return NULL;
}

void _portdesc_free(filter_portdesc_t *d)
{
	filter_paramdesc_t *p;

	if (!d)
		return;
	while ((p = filterportdesc_first_paramdesc(d))) {
		hash_remove_paramdesc(p);
		list_remove_paramdesc(p);
		_paramdesc_free(p);
	}
	free((char *)d->label);
	free((char *)d->description);
	free(d);
}


filter_pipe_t *_pipe_alloc()
{
	filter_pipe_t *p;

	if (!(p = ALLOC(filter_pipe_t)))
		return NULL;
	p->source_fd = -1;
	p->dest_fd = -1;
	p->nr_params = 0;
	INIT_LIST_HEAD(&p->params);
	return p;
}

void _pipe_free(filter_pipe_t *p)
{
	filter_param_t *param;

	if (!p)
		return;
	while ((param = filterpipe_first_param(p))) {
		hash_remove_param(param);
		list_remove_param(param);
		_param_free(param);
	}
	free(p);
}


filter_t *_filter_alloc(const char *name, const char *description, int flags)
{
	filter_t *f;

	if (!(f = ALLOC(filter_t)))
		return NULL;
	f->name = strdup(name);
	f->description = strdup(description);
	f->flags = flags;
	f->f = NULL;
	f->init = NULL;
	f->cleanup = NULL;
	f->connect_out = filter_default_connect_out;
	f->connect_in = filter_default_connect_in;
	f->fixup_param = filter_default_fixup_param;
	f->fixup_pipe = filter_default_fixup_pipe;
	f->fixup_break_in = filter_default_fixup_break_in;
	f->fixup_break_out = filter_default_fixup_break_out;
	if (flags & FILTER_FLAG_NETWORK) {
		f->f = filter_network_f;
		f->init = filter_network_init;
		f->cleanup = filter_network_cleanup;
		f->connect_out = filter_network_connect_out;
		f->connect_in = filter_network_connect_in;
		f->fixup_param = filter_network_fixup_param;
	}
	f->nr_params = 0;
	INIT_LIST_HEAD(&f->params);
	f->nr_inputs = 0;
	INIT_LIST_HEAD(&f->inputs);
	f->nr_outputs = 0;
	INIT_LIST_HEAD(&f->outputs);
	f->private = NULL;
	hash_init_filter(f);
	INIT_LIST_HEAD(&f->list);
	if (f->name && f->description)
		return f;

	free((char *)f->name);
	free((char *)f->description);
	free(f);
	return NULL;
}

void _filter_free(filter_t *f)
{
	filter_paramdesc_t *paramd;
	filter_portdesc_t *portd;

	if (!f)
		return;
	while ((paramd = filter_first_paramdesc(f))) {
		hash_remove_paramdesc(paramd);
		list_remove_paramdesc(paramd);
		_paramdesc_free(paramd);
	}
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
	free((char *)f->name);
	free((char *)f->description);
	free(f);
}


filter_launchcontext_t *_launchcontext_alloc()
{
	filter_launchcontext_t *c;

	if (!(c = ALLOC(filter_launchcontext_t)))
		return NULL;
	if ((c->semid = semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) != -1)
		sem_zero(c->semid, 0);
	INIT_LIST_HEAD(&c->buffers);
	ATOMIC_INIT(c->result, 0);
	c->nr_threads = 0;
	if (c->semid != -1)
		return c;

	free(c);
	return NULL;
}

void _launchcontext_free(filter_launchcontext_t *c)
{
	filter_buffer_t *fb;

	if (!c)
		return;
	while ((fb = filterlaunchcontext_first_buffer(c))) {
		list_remove_buffer(fb);
		_buffer_free(fb);
	}
	ATOMIC_RELEASE(c->result);
	semctl(c->semid, 0, IPC_RMID, (union semun)0);
	free(c);
}


static int _node_init(filter_node_t *n, const char *name)
{
	n->private = NULL;
	n->ops = &filter_node_ops;
	n->nr_params = 0;
	INIT_LIST_HEAD(&n->params);
	n->nr_inputs = 0;
	INIT_LIST_HEAD(&n->inputs);
	n->nr_outputs = 0;
	INIT_LIST_HEAD(&n->outputs);
	if (!(n->name = strdup(name)))
		return -1;
	return 0;
}

static void __node_free(filter_node_t *n)
{
	filter_param_t *param;
	filter_pipe_t *pipe;

	while ((param = filternode_first_param(n))) {
		hash_remove_param(param);
		list_remove_param(param);
		_param_free(param);
	}
	while ((pipe = filternode_first_input(n))) {
		hash_remove_input(pipe);
		list_remove_input(pipe);
		hash_remove_output(pipe);
		list_remove_output(pipe);
		pipe->source->filter->fixup_break_out(pipe->source, pipe);
		_pipe_free(pipe);
	}
	while ((pipe = filternode_first_output(n))) {
		hash_remove_input(pipe);
		list_remove_input(pipe);
		hash_remove_output(pipe);
		list_remove_output(pipe);
		pipe->dest->filter->fixup_break_out(pipe->dest, pipe);
		_pipe_free(pipe);
	}

	/* call the cleanup routine, if one provided */
	if (n->filter && n->filter->cleanup)
		n->filter->cleanup(n);

	free((char *)n->name);
	free(n);
}

void _node_free(filter_node_t *n)
{
	if (!n)
		return;
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
	n->filter = f;

	if (f->init && f->init(n) == -1)
		goto err;

	return n;

 err:
	_node_free(n);
	return NULL;
}

