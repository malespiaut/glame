/*
 * filter_methods.c
 * $Id: filter_methods.c,v 1.20 2000/08/14 08:48:06 richi Exp $
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
#include "util.h"
#include "filter.h"


/* Clever GLSIG_PIPE_CHANGED handler for the input side of
 * an unmanaged (not gone trough custom connect_in()) pipe.
 * Allows for simple method-/handler-less filters.
 */
static void filter_handle_pipe_change(glsig_handler_t *h, long sig, va_list va)
{
	filter_node_t *n;
	filter_pipe_t *in;
	filter_pipe_t *out;

	GLSIGH_GETARGS1(va, in);
	n = in->dest;

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
	filternode_foreach_output(n, out) {
		if (out->type == in->type
		    && memcmp(&out->u, &in->u, sizeof(out->u)) == 0)
			continue;
		out->type = in->type;
		out->u = in->u;
		glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	}
}


/* Standard filter default methods.
 */

/* Default output connect method.
 * Output pipe type is copied from corresponding input
 * (or uninitialized).
 */
int filter_default_connect_out(filter_node_t *n, const char *port,
			       filter_pipe_t *p)
{
	filter_pipe_t *in = NULL;

	/* As with default connect in method, we accept one
	 * connection only in this "default" mode. */
	if (filternode_get_output(n, port))
		return -1;

	/* For the following stuff we need an existing input pipe
	 * with a valid type. */
	filternode_foreach_input(n, in)
		if (in->type != FILTER_PIPETYPE_UNDEFINED)
			goto found;
	return 0;

 found:
	/* If we dont have a pipe type already (still 0), try to guess
	 * one from the existing input. */
	if (p->type == FILTER_PIPETYPE_UNDEFINED)
		p->type = in->type;

	/* A source port has to provide pipe data info,
	 * we copy from the existing input. */
	p->u = in->u;

	return 0;
}

int filter_default_connect_in(filter_node_t *n, const char *port,
			      filter_pipe_t *p)
{
        /* We accept everything. Default checks are done by the
	 * filternetwork_add_conection function. But as all
	 * ports are now "automatic" we as default do accept one
	 * connection only. */
	if (filternode_get_input(n, port))
		return -1;

	/* As this is an unmanaged connection, we have to provide
	 * a default handler for the GLSIG_PIPE_CHANGED signal,
	 * so install one. */
	glsig_add_handler(&p->emitter, GLSIG_PIPE_CHANGED,
			  filter_handle_pipe_change, NULL);

	return 0;
}

int filter_default_set_param(filter_node_t *n, filter_param_t *param,
			     const void *val)
{
	/* We do not reject any parameter change by default.
	 */
	return 0;
}


/* Filternetwork filter methods.
 */

int filter_network_init(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;
	filter_network_t *templ;
	filter_node_t *node, *source, *dest;
	filter_pipe_t *pipe, *p;

	/* empty network? */
	if (!(templ = (filter_network_t *)n->filter->private))
		return 0;

	/* copy network from templ to net */

	/* first create all nodes and copy set parameters */
	filternetwork_foreach_node(templ, n) {
		if (!(node = filternetwork_add_node(net, plugin_name(n->filter->plugin), n->name)))
			return -1;
		filterpdb_copy(filternode_pdb(node), filternode_pdb(n));
	}

	/* second create the connections (loop through all outputs)
	 * and copy pipe parameters */
	filternetwork_foreach_node(templ, n) {
		filternode_foreach_output(n, pipe) {
			source = filternetwork_get_node(net, pipe->source->name);
			dest = filternetwork_get_node(net, pipe->dest->name);
			if (!(p = filternetwork_add_connection(source, pipe->out_name, dest, pipe->in_name)))
				return -1;
			filterpdb_copy(filterpipe_sourcepdb(p),
				       filterpipe_sourcepdb(pipe));
			filterpdb_copy(filterpipe_destpdb(p),
				       filterpipe_destpdb(pipe));
		}
	}

	return 0;
}

int filter_network_f(filter_node_t *n)
{
	PANIC("uh? filternetwork launched??");
	return -1;
}

int filter_network_connect_out(filter_node_t *source, const char *port,
			       filter_pipe_t *p)
{
	filter_portdesc_t *d;
	struct filter_network_mapping *m;
	filter_node_t *n;

	if (!(d = filter_get_outputdesc(source->filter, port)))
		return -1;
	m = (struct filter_network_mapping *)d->private;
	if (!(n = filternetwork_get_node(source, m->node)))
		return -1;
	p->out_name = m->label;
	p->source = n;

	return n->filter->connect_out(p->source, p->out_name, p);
}

int filter_network_connect_in(filter_node_t *dest, const char *port,
			      filter_pipe_t *p)
{
	struct filter_network_mapping *m;
	filter_node_t *n;

	m = (struct filter_network_mapping *)p->dest_port->private;
	if (!(n = filternetwork_get_node(dest, m->node)))
		return -1;
	p->in_name = m->label;
	p->dest = n;

	return n->filter->connect_in(p->dest, p->in_name, p);
}

int filter_network_set_param(filter_node_t *node, filter_param_t *param,
			     const void *val)
{
	filter_node_t *n;
	const char *map_node, *map_label;
	filter_param_t *p;

	/* pipe parameter setting does not go through the wrapped funcs */
	map_node = filterparam_get_property(param, FILTERPARAM_MAP_NODE);
	map_label = filterparam_get_property(param, FILTERPARAM_MAP_LABEL);
	if (!map_node || !map_label)
		return -1;

	if (!(n = filternetwork_get_node(FILTER_NETWORK(node), map_node))) {
		DPRINTF("No such node %s\n", map_node);
		return -1;
	}
	if (!(p = filterpdb_get_param(filternode_pdb(n), map_label))) {
		DPRINTF("No such param %s\n", map_label);
		return -1;
	}
	return filterparam_set(p, val);
}
