/*
 * filter_methods.c
 * $Id: filter_methods.c,v 1.26 2001/04/06 18:16:46 nold Exp $
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
 *
 * This file contains
 * - filter default methods
 * - network filter methods
 */

#include <stdio.h>
#include <string.h>
#include "util.h"
#include "filter.h"


/* Clever GLSIG_PIPE_CHANGED handler for the input side of
 * an unmanaged (not gone trough custom connect_in()) pipe.
 * Allows for simple method-/handler-less filters.
 */
static void filter_handle_pipe_change(glsig_handler_t *h, long sig, va_list va)
{
	filter_t *n;
	filter_pipe_t *in;
	filter_pipe_t *out;
	filter_port_t *port;

	GLSIGH_GETARGS1(va, in);
	n = filterport_filter(filterpipe_dest(in));

	/* Input pipe format change is easy for us as we know
	 * nothing about internal connections between
	 * inputs and outputs.
	 * So the rule of dumb is to update all output
	 * pipe formats to the format of the input
	 * pipe we just got. We also have to forward
	 * the change to every output slot, of course.
	 * We need to stop the fixup as soon as a failure
	 * occours - the pipe may be broken.
	 * FIXME: this does "simple" check if anything would
	 * change using memcmp to prevent endless loops with
	 * cyclic networks.
	 */
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_input(port))
			continue;
		filterport_foreach_pipe(port, out) {
			/* Dont overwrite "foreign" pipes. */
			if (out->type != FILTER_PIPETYPE_UNDEFINED
			    && out->type != in->type)
				continue;
			/* Prevent endless pipe change loops. */
			if (out->type == in->type
			    && memcmp(&out->u, &in->u, sizeof(out->u)) == 0) {
				continue;
			}
			out->type = in->type;
			out->u = in->u;
			glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
		}
	}
}


/* Standard filter default methods.
 */

/* Default output connect method.
 * Output pipe type is copied from corresponding input
 * (or uninitialized).
 */
int filter_default_connect_out(filter_t *n, filter_port_t *outp,
			       filter_pipe_t *p)
{
	filter_pipe_t *in = NULL;
	filter_port_t *inp;

	/* As with default connect in method, we accept one
	 * connection only in this "default" mode. */
	if (filterport_get_pipe(outp))
		return -1;

	/* For the following stuff we need an existing input pipe
	 * with a valid type. */
	filterportdb_foreach_port(filter_portdb(n), inp) {
		if (filterport_is_output(inp))
			continue;
		filterport_foreach_pipe(inp, in) {
			/* We may use in to copy pipe properties if
			 * in has a defined type and the pipe we are
                         * copying to has the same type or undefined
                         * type. */
			if (in->type != FILTER_PIPETYPE_UNDEFINED
			    && (in->type == p->type
				|| p->type == FILTER_PIPETYPE_UNDEFINED)) {
				p->type = in->type;
				p->u = in->u;
			}
		}
	}

	/* Nothing suitable? Oh well... */
	return 0;
}

int filter_default_connect_in(filter_t *n, filter_port_t *inp,
			      filter_pipe_t *p)
{
        /* We accept everything. Default checks are done by the
	 * filternetwork_add_conection function. But as all
	 * ports are now "automatic" we as default do accept one
	 * connection only. */
	if (filterport_get_pipe(inp))
		return -1;

	/* As this is an unmanaged connection, we have to provide
	 * a default handler for the GLSIG_PIPE_CHANGED signal,
	 * so install one. */
	glsig_add_handler(&p->emitter, GLSIG_PIPE_CHANGED,
			  filter_handle_pipe_change, NULL);

	return 0;
}

int filter_default_set_param(filter_t *n, filter_param_t *param,
			     const void *val)
{
	/* We do not reject any parameter change by default.
	 */
	return 0;
}


/* Filternetwork filter methods.
 */
int filter_network_f(filter_t *n)
{
	PANIC("uh? filternetwork launched??");
	return -1;
}

int filter_network_connect_out(filter_t *source, filter_port_t *outp,
			       filter_pipe_t *p)
{
	const char *map_node, *map_label;
	filter_port_t *port;
	filter_t *node;

	map_node = filterport_get_property(outp, FILTERPORT_MAP_NODE);
	map_label = filterport_get_property(outp, FILTERPORT_MAP_LABEL);
	if (!map_node || !map_label)
		return -1;

	if (!(node = filter_get_node(source, map_node))
	    || !(port = filterportdb_get_port(filter_portdb(node), map_label))
	    || !filterport_is_output(port))
		return -1;
	p->source = port;

	return node->connect_out(node, port, p);
}

int filter_network_connect_in(filter_t *dest, filter_port_t *inp,
			      filter_pipe_t *p)
{
	const char *map_node, *map_label;
	filter_port_t *port;
	filter_t *node;

	map_node = filterport_get_property(inp, FILTERPORT_MAP_NODE);
	map_label = filterport_get_property(inp, FILTERPORT_MAP_LABEL);
	if (!map_node || !map_label)
		return -1;

	if (!(node = filter_get_node(dest, map_node))
	    || !(port = filterportdb_get_port(filter_portdb(node), map_label))
	    || !filterport_is_input(port))
		return -1;
	p->dest = port;

	return node->connect_in(node, port, p);
}

int filter_network_set_param(filter_t *node, filter_param_t *param,
			     const void *val)
{
	filter_t *n;
	const char *map_node, *map_label;
	filter_param_t *p;

	/* pipe parameter setting does not go through the wrapped funcs */
	map_node = filterparam_get_property(param, FILTERPARAM_MAP_NODE);
	map_label = filterparam_get_property(param, FILTERPARAM_MAP_LABEL);
	if (!map_node || !map_label)
		return -1;

	if (!(n = filter_get_node(node, map_node))) {
		DPRINTF("No such node %s\n", map_node);
		return -1;
	}
	if (!(p = filterparamdb_get_param(filter_paramdb(n), map_label))) {
		DPRINTF("No such param %s\n", map_label);
		return -1;
	}
	return filterparam_set(p, val);
}
