/*
 * filter_port.c
 * $Id: filter_port.c,v 1.10 2002/03/24 19:20:24 richi Exp $
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

#include <string.h>
#include "filter.h"
#include "filter_pipe.h"
#include "filter_port.h"


/* Clever GLSIG_PIPE_CHANGED handler for the input side of
 * an unmanaged (not gone trough custom connect()) pipe.
 * Allows for simple method-/handler-less filters.
 */
static void filter_handle_pipe_change(glsig_handler_t *h, long sig, va_list va)
{
	filter_t *n;
	filter_pipe_t *in;
	filter_pipe_t *out;
	filter_port_t *port;
	int index;

	GLSIGH_GETARGS1(va, in);
	n = filterport_filter(filterpipe_dest(in));

	/* Find out "index" of port of pipe that just changed. */
	index = 0;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (!filterport_is_input(port))
			continue;
		if (port == filterpipe_dest(in))
			break;
		index++;
	}

	/* Find corresponding output port. */
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (!filterport_is_output(port))
			continue;
		if (index == 0)
			break;
		index--;
	}
	if (!port) {
		DPRINTF("You probably want to fixup yourself\n");
		return;
	}

	/* Update output pipe properties. */
	filterport_foreach_pipe(port, out) {
		if (out->type == FILTER_PIPETYPE_UNDEFINED)
			out->type = in->type;
		if (memcmp(&out->u, &in->u, sizeof(out->u)) == 0)
			continue;
		out->u = in->u;
		glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	}
}

static int default_connect_input(filter_port_t *port, filter_pipe_t *pipe)
{
        /* We accept everything. Default checks are done by the
	 * filterport_connect function. But as all ports are now
	 * "automatic" we as default do accept one connection only. */
	if (filterport_get_pipe(port))
		return -1;

	/* As this is an unmanaged connection, we have to provide
	 * a default handler for the GLSIG_PIPE_CHANGED signal,
	 * so install one. */
	glsig_add_handler(filterpipe_emitter(pipe), GLSIG_PIPE_CHANGED,
			  filter_handle_pipe_change, NULL);

	return 0;	
}

static int default_connect_output(filter_port_t *port, filter_pipe_t *pipe)
{
	filter_pipe_t *in = NULL;
	filter_port_t *inp;
	filter_t *n;
	int index;

	/* As with default connect in method, we accept one
	 * connection only in this "default" mode. */
	if (filterport_get_pipe(port))
		return -1;

	n = filterport_filter(port);

	/* Find index of output port. */
	index = 0;
	filterportdb_foreach_port(filter_portdb(n), inp) {
		if (!filterport_is_output(inp))
			continue;
		if (inp == port)
			break;
		index++;
	}

	/* Find corresponding input port to get pipe information from. */
	filterportdb_foreach_port(filter_portdb(n), inp) {
		if (!filterport_is_input(inp))
			continue;
		if (index == 0)
			break;
		index--;
	}
	if (!inp) {
		DPRINTF("You want to connect outputs yourself for %s\n", filter_name(n));
		return -1;
	}

	/* No connection? Postpone initialization. */
	if (!(in = filterport_get_pipe(inp)))
		return 0;

	pipe->type = in->type;
	pipe->u = in->u;

	return 0;
}

static int redirect_connect_output(filter_port_t *outp, filter_pipe_t *p)
{
	const char *map_node, *map_label;
	filter_port_t *port;
	filter_t *source, *node;

	source = filterport_filter(outp);
	map_node = filterport_get_property(outp, FILTERPORT_MAP_NODE);
	map_label = filterport_get_property(outp, FILTERPORT_MAP_LABEL);
	if (!map_node || !map_label)
		return -1;

	if (!(node = filter_get_node(source, map_node))
	    || !(port = filterportdb_get_port(filter_portdb(node), map_label))
	    || !filterport_is_output(port))
		return -1;
	p->source = port;

	return port->connect(port, p);
}

static int redirect_connect_input(filter_port_t *inp, filter_pipe_t *p)
{
	const char *map_node, *map_label;
	filter_port_t *port;
	filter_t *dest, *node;

	dest = filterport_filter(inp);
	map_node = filterport_get_property(inp, FILTERPORT_MAP_NODE);
	map_label = filterport_get_property(inp, FILTERPORT_MAP_LABEL);
	if (!map_node || !map_label)
		return -1;

	if (!(node = filter_get_node(dest, map_node))
	    || !(port = filterportdb_get_port(filter_portdb(node), map_label))
	    || !filterport_is_input(port))
		return -1;
	p->dest = port;

	return port->connect(port, p);
}


static filter_port_t *portdb_alloc_item()
{
	filter_port_t *p;

	if (!(p = (filter_port_t *)malloc(sizeof(filter_port_t))))
		return NULL;
	gldb_init_item(&p->entry);
	glsdb_init(&p->properties);
	INIT_GLSIG_EMITTER(&p->emitter);

	p->type = FILTER_PORTTYPE_ANY;
	p->flags = FILTER_PORTFLAG_INPUT;
	p->connect = NULL;

	/* Init the parameter database is deferred until add. */

	GLAME_INIT_LIST_HEAD(&p->pipes);
	p->nr_pipes = 0;

	return p;
}

static void portdb_op_delete(gldb_item_t *item)
{
	filter_port_t *p = (filter_port_t *)item;
	filter_pipe_t *pipe;

	/* Signal port deletion. */
	glsig_emit(&p->emitter, GLSIG_PORT_DELETED, p);

	/* First remove all connections. */
	while ((pipe = filterport_get_pipe(p)))
		filterpipe_delete(pipe);

	/* Delete all other stuff. But notify the associated
	 * filter about the change before deleting the signal
	 * handlers. */
	glsdb_delete(&p->properties);
	glsig_emit(&p->emitter, GLSIG_FILTER_CHANGED,
		   ((filter_portdb_t *)p->entry.db)->node);
	glsig_delete_all(&p->emitter);
}

static gldb_item_t *portdb_op_copy(gldb_item_t *source)
{
	filter_port_t *d;
	filter_port_t *s = (filter_port_t *)source;

	if (!(d = portdb_alloc_item()))
		return NULL;

	/* Copy property db. */
	glsdb_copy(&d->properties, &s->properties);

	/* Copy signal handlers, not the redirectors. */
	glsig_copy_handlers(&d->emitter, &s->emitter);

	/* Copy type/flags. Parameter db copying is deferred until add. */
	d->type = s->type;
	d->flags = s->flags;
	d->connect = s->connect;

	/* We do NOT copy the pipes! This needs to be done
	 * manually, as both ends (ports) have to exist and
	 * the connect methods need to be called.
	 */

	return &d->entry;
}

static int portdb_op_add(gldb_t *db, gldb_item_t *i, gldb_item_t *source)
{
	filter_port_t *p = (filter_port_t *)i;
	filter_t *node = ((filter_portdb_t *)db)->node;

	/* We can and should not try to do anything with
	 * items of a db with no node associated. But we
	 * may consider this case an error. */
	if (!node)
		DERROR("Adding port to anonymous (NULL node) portdb!");

	/* We need to initialize the parameter db now as we know the
	 * filter. Also we need do the deferred copy of the paramdb,
	 * if source!=NULL. */
	filterparamdb_init(&p->params, node);
	if (source)
		filterparamdb_copy(&p->params, &((filter_port_t *)source)->params);

	/* Add a redirector to the associated node. */
	glsig_add_redirector(&p->emitter, ~0, &node->emitter);

	return 0;
}

static struct gldb_ops ops = { portdb_op_delete,
			       portdb_op_copy,
			       portdb_op_add };



/* API stuff.
 */

void filterportdb_init(filter_portdb_t *db, filter_t *f)
{
	gldb_init(&db->db, &ops);
	db->node = f;
}


/* DB part. */

filter_port_t *filterportdb_add_port(filter_portdb_t *db, const char *label,
				     int type, int flags, ...)
{
	filter_port_t *p;
	va_list va;
	const char *key, *prop;

	if (!db || FILTER_IS_PLUGIN(db->node) || !label)
		return NULL;

	if (!(p = portdb_alloc_item(db->node)))
		return NULL;
	p->type = type;
	p->flags = flags;
	if (flags == FILTER_PORTFLAG_INPUT)
		p->connect = default_connect_input;
	else if (flags == FILTER_PORTFLAG_OUTPUT)
		p->connect = default_connect_output;

	if (gldb_add_item(&db->db, &p->entry, label) == -1) {
		portdb_op_delete(&p->entry);
		free(p);
		return NULL;
	}

	/* Process the va and add the specified key/value pairs
	 * to the property database. */
	va_start(va, flags);
	while ((key = va_arg(va, const char *)) != FILTERPORT_END) {
		prop = va_arg(va, const char *);
		filterport_set_property(p, key, prop);
	}
	va_end(va);

	/* Signal the associated filter its change. */
	glsig_emit(&p->emitter, GLSIG_FILTER_CHANGED, db->node);

	return p;
}

int filterport_redirect(filter_port_t *source, filter_port_t *dest)
{
	if (!source || !dest || !filterport_filter(dest)
	    || filterport_is_input(source) != filterport_is_input(dest)
	    || !FILTER_PORTS_ARE_COMPATIBLE(filterport_type(source),
					    filterport_type(dest)))
		return -1;

	filterport_set_property(source, FILTERPORT_MAP_NODE,
				filter_name(filterport_filter(dest)));
	filterport_set_property(source, FILTERPORT_MAP_LABEL,
				filterport_label(dest));
	if (filterport_is_input(source))
		source->connect = redirect_connect_input;
	else if (filterport_is_output(source))
		source->connect = redirect_connect_output;

	return 0;
}

void filterport_delete(filter_port_t *port)
{
	const char *map_node, *map_label;
	filter_t *node, *net = filterport_filter(port);
	filter_port_t *dport;

	if (!port || FILTER_IS_PLUGIN(net))
		return;

	/* If connections to this port get redirected, delete all
	 * pipes at the destination that originate here. */
	if ((map_node = filterport_get_property(port, FILTERPORT_MAP_NODE))
	    && (map_label = filterport_get_property(port,
						    FILTERPORT_MAP_LABEL))
	    && (node = filter_get_node(net, map_node))
	    && (dport = filterportdb_get_port(filter_portdb(node),
					      map_label))) {
		filter_pipe_t *pipe;
	again:
		filterport_foreach_pipe(dport, pipe) {
			if (pipe->real_source == port
			    || pipe->real_dest == port) {
				filterpipe_delete(pipe);
				goto again;
			}
		}
	}

	gldb_delete_item(&port->entry);
}
