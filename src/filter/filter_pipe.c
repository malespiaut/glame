/*
 * filter_pipe.h
 * $Id: filter_pipe.c,v 1.3 2000/12/18 09:51:55 richi Exp $
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
	free(p);
}

void _connection_delete(glsig_handler_t *h, long sig, va_list va)
{
	struct fconnection *c;

	c = (struct fconnection *)glsig_handler_private(h);
	list_del(&c->list);
	free(c);
}


/* Connection management API.
 */

filter_pipe_t *filterport_connect(filter_port_t *source, filter_port_t *dest)
{
	filter_pipe_t *p;
	struct fconnection *c;

	if (!source || !dest
	    || !filterport_filter(source) || !filterport_filter(dest)
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
	/* install a signalhandler for GLSIG_PIPE_DELETED which removes
	 * the connection from this list. */
	glsig_add_handler(filterpipe_emitter(p), GLSIG_PIPE_DELETED,
			  _connection_delete, c);

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
