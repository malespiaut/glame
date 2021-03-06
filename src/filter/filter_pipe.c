/*
 * filter_pipe.h
 * $Id: filter_pipe.c,v 1.15 2004/10/23 13:09:22 richi Exp $
 *
 * Copyright (C) 2000, 2001, 2002 Richard Guenther
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
#define glame_list_add_pipe_source(p, port) do { glame_list_add(&(p)->source_list, \
        &(port)->pipes); (port)->nr_pipes++; } while (0)
#define glame_list_add_pipe_dest(p, port) do { glame_list_add(&(p)->dest_list, \
        &(port)->pipes); (port)->nr_pipes++; } while (0)
#define glame_list_remove_pipe_source(p) do { glame_list_del(&(p)->source_list); \
        (p)->source->nr_pipes--; } while (0)
#define glame_list_remove_pipe_dest(p) do { glame_list_del(&(p)->dest_list); \
        (p)->dest->nr_pipes--; } while (0)


static filter_pipe_t *_pipe_alloc(filter_port_t *sourceport,
				  filter_port_t *destport)
{
	filter_pipe_t *p;

	if (!(p = ALLOC(filter_pipe_t)))
		return NULL;
	GLAME_INIT_LIST_HEAD(&p->source_list);
	GLAME_INIT_LIST_HEAD(&p->dest_list);
	GLAME_INIT_LIST_HEAD(&p->list);
	p->source_fd = -1;
	p->dest_fd = -1;

	/* Init & copy of source/dest parameters is delayed!
	 * -- see filterport_connect() */
	p->source = sourceport;
	p->dest = destport;
	p->real_source = sourceport;
	p->real_dest = destport;

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
	if (!glame_list_empty(&p->list))
		glame_list_del(&p->list);

	free(p);
}


/* Connection management API.
 */

filter_pipe_t *filterport_connect(filter_port_t *source, filter_port_t *dest)
{
	filter_pipe_t *p = NULL;

	if (!source || !dest
	    || !filterport_filter(source) || !filterport_filter(dest)
	    || filterport_filter(source) == filterport_filter(dest)
	    || !filterport_is_output(source)
	    || !filterport_is_input(dest)
	    || !filterport_filter(source)->net
	    || filterport_filter(source)->net != filterport_filter(dest)->net)
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
	if (source->connect(source, p) == -1)
		goto _err;
	if (dest->connect(dest, p) == -1)
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
	glsig_add_redirector(filterpipe_emitter(p), ~0, filterport_emitter(filterpipe_dest(p)));
	glsig_add_redirector(filterpipe_emitter(p), ~GLSIG_PIPE_CHANGED, filterport_emitter(filterpipe_source(p)));

	/* add the pipe to all port lists/hashes.
	 * connect_out/in may have mucked with p->dest/source, so
	 * we have to use that instead of int/out directly. */
	glame_list_add_pipe_dest(p, filterpipe_dest(p));
	glame_list_add_pipe_source(p, filterpipe_source(p));

	/* as everything is set up now, we need to register the initial
	 * connection request in the sources filter connection list. */
	p->real_source = source;
	p->real_dest = dest;
	glame_list_add(&p->list, &filterport_filter(source)->connections);

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
	glame_list_remove_pipe_source(p);
	glame_list_remove_pipe_dest(p);

	/* kill the pipe */
	_pipe_free(p);
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

	return res;
}
