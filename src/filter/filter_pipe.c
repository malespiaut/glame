/*
 * filter_pipe.h
 * $Id: filter_pipe.c,v 1.8 2001/05/11 11:45:41 richi Exp $
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
 */

#include "filter.h"
#include "filter_port.h"
#include "filter_pipe.h"


/* filter pipe addition/removal to filter port pipes list. */
#define list_add_pipe_source(p, port) do { list_add(&(p)->source_list, \
        &(port)->pipes); (port)->nr_pipes++; } while (0)
#define list_add_pipe_dest(p, port) do { list_add(&(p)->dest_list, \
        &(port)->pipes); (port)->nr_pipes++; } while (0)
#define list_remove_pipe_source(p) do { list_del(&(p)->source_list); \
        (p)->source->nr_pipes--; } while (0)
#define list_remove_pipe_dest(p) do { list_del(&(p)->dest_list); \
        (p)->dest->nr_pipes--; } while (0)


static filter_pipe_t *_pipe_alloc(filter_port_t *sourceport,
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
	 * -- see filterport_connect() */
	p->source = sourceport;
	p->dest = destport;

	p->connection = NULL;

	/* init emitter - redirector installation is delayed!
	 * -- see filterport_connect() */
	INIT_GLSIG_EMITTER(&p->emitter);

	return p;
}

static void _pipe_free(filter_pipe_t *p)
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

	/* kill connection */
	if (p->connection) {
		list_del(&p->connection->list);
		free(p->connection);
	}

	free(p);
}


/* Connection management API.
 */

filter_pipe_t *filterport_connect(filter_port_t *source, filter_port_t *dest)
{
	filter_pipe_t *p = NULL;
	struct fconnection *c;

	if (!source || !dest
	    || !filterport_filter(source) || !filterport_filter(dest)
	    || filterport_filter(source) == filterport_filter(dest)
	    || !filterport_is_output(source)
	    || !filterport_is_input(dest))
		return NULL;
	if (FILTER_IS_LAUNCHED(filterport_filter(source))
	    || FILTER_IS_LAUNCHED(filterport_filter(dest)))
		return NULL;

	/* do we support the out/in port type combination? */
	if (!FILTER_PORTS_ARE_COMPATIBLE(dest->type, source->type))
		goto _err;

	/* Alloc the pipe. */
	if (!(p = _pipe_alloc(source, dest)))
		return NULL;
        /* Yes, this does work. */
	p->type = filterport_type(dest)|filterport_type(source);

	/* Try to establish the connections. */
	if (filterport_filter(source)->connect_out(filterport_filter(source), source, p) == -1)
		goto _err;
	if (filterport_filter(dest)->connect_in(filterport_filter(dest), dest, p) == -1)
		goto _err;

	/* Now we have source & dest fixed - so we can finally
	 * init the parameter dbs and copy the parameters from
	 * the port descriptors. Note that while we associate
	 * the params with the connect_in/out mucked ports, we
	 * copy the (default) parameters from the unmucked ports
	 * - this is a hack introduced for filternetwork parameter
	 * handling that could/should go away for maximum flexibility. */
	filterparamdb_init(&p->source_params, filterport_filter(filterpipe_source(p)));
	filterparamdb_copy(&p->source_params, &source->params);
	filterparamdb_init(&p->dest_params, filterport_filter(filterpipe_dest(p)));
	filterparamdb_copy(&p->dest_params, &dest->params);

	/* Also the signal redirector can be installed now. Note
	 * that GLSIG_PIPE_CHANGED is redirected to the destination node
	 * only! This simplifies signal handling a lot as it matches
	 * the semantics of the old fixup_pipe() method. */
	glsig_add_redirector(filterpipe_emitter(p), ~0, filter_emitter(filterport_filter(filterpipe_dest(p))));
	glsig_add_redirector(filterpipe_emitter(p), ~GLSIG_PIPE_CHANGED, filter_emitter(filterport_filter(filterpipe_source(p))));

	/* add the pipe to all port lists/hashes.
	 * connect_out/in may have mucked with p->dest/source, so
	 * we have to use that instead of int/out directly. */
	list_add_pipe_dest(p, filterpipe_dest(p));
	list_add_pipe_source(p, filterpipe_source(p));

	/* as everything is set up now, we need to register the initial
	 * connection request in the sources filter connection list. */
	c = ALLOC(struct fconnection);
	INIT_LIST_HEAD(&c->list);
	c->pipe = p;
	c->source_filter = filter_name(filterport_filter(source));
	c->source_port = filterport_label(source);
	c->dest_filter = filter_name(filterport_filter(dest));
	c->dest_port = filterport_label(dest);
	list_add(&c->list, &filterport_filter(source)->connections);
	p->connection = c;

	/* signal pipe changes */
	glsig_emit(filterpipe_emitter(p), GLSIG_PIPE_CHANGED, p);

	return p;

 _err:
	_pipe_free(p);
	return NULL;
}

void filterpipe_delete(filter_pipe_t *p)
{
	if (FILTER_IS_PLUGIN(filterport_filter(filterpipe_source(p)))
	    || FILTER_IS_PLUGIN(filterport_filter(filterpipe_dest(p)))
	    || FILTER_IS_LAUNCHED(filterport_filter(filterpipe_source(p))))
		return;

	/* disconnect the pipe */
	list_remove_pipe_source(p);
	list_remove_pipe_dest(p);

	/* kill the pipe */
	_pipe_free(p);
}


filter_port_t *filterpipe_connection_source(filter_pipe_t *fp)
{
	filter_t *net, *source = NULL;

	net = filterport_filter(filterpipe_source(fp))->net;
	while (net) {
		source = filter_get_node(net, fp->connection->source_filter);
		if (source)
			break;
		net = net->net;
	}
	if (!source)
		return NULL;
	return filterportdb_get_port(filter_portdb(source),
				     fp->connection->source_port);
} 

filter_port_t *filterpipe_connection_dest(filter_pipe_t *fp)
{
	filter_t *net, *dest = NULL;

	net = filterport_filter(filterpipe_dest(fp))->net;
	while (net) {
		dest = filter_get_node(net, fp->connection->dest_filter);
		if (dest)
			break;
		net = net->net;
	}
	if (!dest)
		return NULL;
	return filterportdb_get_port(filter_portdb(dest),
				     fp->connection->dest_port);
}


static pthread_mutex_t _fpifbmx = PTHREAD_MUTEX_INITIALIZER;
static void _filterpipe_is_feedback_cleanup(filter_t *n)
{
	filter_port_t *port;
	filter_pipe_t *p;

	if (!(n->type & 32))
		return;
	n->type &= ~32;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (!filterport_is_output(port))
			continue;
		filterport_foreach_pipe(port, p) {
			_filterpipe_is_feedback_cleanup(filterport_filter(filterpipe_dest(p)));
		}
	}
}
static int _filterpipe_is_feedback(filter_pipe_t *pipe, filter_t *n)
{
	filter_port_t *port;
	filter_pipe_t *p;

	if (n->type & 32)
		return 0;
	n->type |= 32;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (!filterport_is_output(port))
			continue;
		filterport_foreach_pipe(port, p) {
			if (pipe == p)
				return 1;
			if (_filterpipe_is_feedback(pipe, filterport_filter(filterpipe_dest(p))) == 1)
				return 1;
		}
	}

	return 0;
}
int filterpipe_is_feedback(filter_pipe_t *pipe)
{
	int res;

	pthread_mutex_lock(&_fpifbmx);
	res = _filterpipe_is_feedback(pipe, filterport_filter(filterpipe_dest(pipe)));
	_filterpipe_is_feedback_cleanup(filterport_filter(filterpipe_dest(pipe)));
	pthread_mutex_unlock(&_fpifbmx);

	DPRINTF("%p %s feedback\n", pipe, res ? "has" : "has not");

	return res;
}
