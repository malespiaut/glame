/*
 * filter_mm.c
 * $Id: filter_mm.c,v 1.17 2000/10/28 13:45:48 richi Exp $
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


/* "iterator" for filter sub-nodes deletion. */
#define filter_first_node(net) list_gethead(&(net)->nodes, \
        filter_t, list)


filter_pipe_t *_pipe_alloc(filter_port_t *sourceport,
			   filter_port_t *destport)
{
	filter_pipe_t *p;

	if (!(p = ALLOC(filter_pipe_t)))
		return NULL;
	INIT_LIST_HEAD(&p->source_list);
	INIT_LIST_HEAD(&p->dest_list);
	p->source_fd = -1;
	p->dest_fd = -1;

	/* Init & copy of source/dest parameters is delayed!
	 * -- see filter_network.c::filternetwork_add_connection() */
	p->source = sourceport;
	p->dest = destport;

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
	filterparamdb_delete(&p->source_params);
	filterparamdb_delete(&p->dest_params);

	/* delete signal handlers */
	glsig_delete_all(&p->emitter);
	free(p);
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


/* Allocate pristine filternode/network. */
filter_t *_filter_alloc(int type)
{
	filter_t *f;

	if (!(f = ALLOC(filter_t)))
		return NULL;

	f->type = type;

	f->net = NULL;
	INIT_HASH_HEAD(&f->hash);
	INIT_LIST_HEAD(&f->list);
	f->name = NULL;

	f->plugin = NULL;
	if (type & FILTERTYPE_NETWORK) {
		f->f = filter_network_f;
		f->init = NULL;
		f->connect_out = filter_network_connect_out;
		f->connect_in = filter_network_connect_in;
		f->set_param = filter_network_set_param;
	} else {
		f->f = NULL;
		f->init = NULL;
		f->connect_out = filter_default_connect_out;
		f->connect_in = filter_default_connect_in;
		f->set_param = filter_default_set_param;
	}
	filterportdb_init(&f->ports, f);

	f->priv = NULL;

	f->glerrno = 0;
	f->glerrstr = NULL;

	INIT_GLSIG_EMITTER(&f->emitter);

	if (type & FILTERTYPE_NETWORK) {
		f->ops = &filter_network_ops;
	} else {
		f->ops = &filter_node_ops;
	}

	filterparamdb_init(&f->params, f);

	f->state = STATE_UNDEFINED;
	INIT_LIST_HEAD(&f->buffers);

	f->nr_nodes = 0;
	INIT_LIST_HEAD(&f->nodes);
	f->launch_context = NULL;

	return f;
}

void _filter_free(filter_t *f)
{
	filter_t *n;

	if (!f)
		return;

	/* first signal deletion */
	glsig_emit(&f->emitter, GLSIG_FILTER_DELETED, f);

	while ((n = filter_first_node(f))) {
		hash_remove_node(n);
		list_remove_node(n);
		_filter_free(n);
	}

	filterportdb_delete(&f->ports);
	filterparamdb_delete(&f->params);

	glsig_delete_all(&f->emitter);
	free((char *)f->name);

	free(f);
}


filter_t *_filter_instantiate(filter_t *f)
{
	filter_t *n, *node, *source, *dest;
	filter_pipe_t *pipe, *p;
	filter_port_t *port;

	/* allocate new structure. */
	if (!(n = _filter_alloc(f->type & ~FILTERTYPE_PLUGIN)))
		return NULL;

	/* copy all the stuff. */
	n->name = NULL;
	n->f = f->f;
	n->init = f->init;
	n->connect_out = f->connect_out;
	n->connect_in = f->connect_in;
	n->set_param = f->set_param;

	n->priv = f->priv;

	glsig_copy_handlers(&n->emitter, &f->emitter);
	glsig_copy_redirectors(&n->emitter, &f->emitter);
	filterparamdb_copy(&n->params, &f->params);
	filterportdb_copy(&n->ports, &f->ports);

	/* copy nodes */
	filter_foreach_node(f, node) {
		if (filter_add_node(n, _filter_instantiate(node), node->name) == -1)
			goto err;
	}

	/* and connections */
	/* second create the connections (loop through all outputs)
	 * and copy pipe parameters */
	filter_foreach_node(f, node) {
	    filterportdb_foreach_port(filter_portdb(node), port) {
		if (!filterport_is_output(port))
			continue;
		filterport_foreach_pipe(port, pipe) {
			source = filter_get_node(n, filterport_filter(filterpipe_source(pipe))->name);
			dest = filter_get_node(n, filterport_filter(filterpipe_dest(pipe))->name);
			if (!(p = filterport_connect(filterportdb_get_port(filter_portdb(source), filterport_label(filterpipe_source(pipe))),
						     filterportdb_get_port(filter_portdb(dest), filterport_label(filterpipe_dest(pipe))))))
				goto err;
			filterparamdb_copy(filterpipe_sourceparamdb(p),
					   filterpipe_sourceparamdb(pipe));
			filterparamdb_copy(filterpipe_destparamdb(p),
					   filterpipe_destparamdb(pipe));
		}
	    }
	}

	if (n->init && n->init(n) == -1)
		goto err;

	return n;

 err:
	_filter_free(n);
	return NULL;
}

