/*
 * filter_methods.c
 * $Id: filter_methods.c,v 1.7 2000/03/14 14:29:26 richi Exp $
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


/* Standard filter default methods.
 */

/* Default output connect method.
 * Output pipe type is copied from corresponding input
 * (or uninitialized).
 */
int filter_default_connect_out(filter_node_t *n, const char *port,
			       filter_pipe_t *p)
{
	filter_pipe_t *in;
	int t;

	/* fill in a sane pipe type - check for possible match */
	t = (p->source_port->type & p->dest_port->type) & ~FILTER_PORTTYPE_AUTOMATIC;
	if (!t)
	        return -1;
	p->type = 1<<(ffs(t)-1);

	/* a source port has to provide pipe data info.
	 * we copy from the first input port if any. */
	if ((in = filternode_first_input(n))) {
		p->type = in->type;
		p->u = in->u;
	}

	return 0;
}

int filter_default_connect_in(filter_node_t *n, const char *port,
			      filter_pipe_t *p)
{
        /* We accept everything. Default checks are done by the
	 * filternetwork_add_conection function.
         */
	return 0;
}

int filter_default_fixup_param(filter_node_t *n, filter_pipe_t *p,
			       const char *name, filter_param_t *param)
{
	/* Parameter change? In the default method
	 * we know nothing about parameters, so we
	 * cant do anything about it.
	 * Forwarding is useless, too.
	 */
	return 0;
}

int filter_default_fixup_pipe(filter_node_t *n, filter_pipe_t *in)
{
	filter_pipe_t *out;

	/* Pipe format change is easy for us as we know
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
		if (out->type == in->type && memcmp(&out->u, &in->u, sizeof(out->u)) == 0)
			continue;
		out->type = in->type;
		out->u = in->u;
		if (out->dest->filter->fixup_pipe(out->dest, out) == -1)
			return -1;
	}

	return 0;
}

void filter_default_fixup_break_in(filter_node_t *n, filter_pipe_t *in)
{
	/* we dont know nothing about relationships between input
	 * and output ports, so anything here would be senseless. */
	return;
}

void filter_default_fixup_break_out(filter_node_t *n, filter_pipe_t *out)
{
	/* we dont know nothing about relationships between input
	 * and output ports, so anything here would be senseless. */
	return;
}



/* Filternetwork filter methods.
 */

extern filter_network_t *string2net(char *buf, filter_network_t *net);
int filter_network_init(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;
	char *buf;

	/* empty network? */
	if (!(buf = (char *)n->filter->private))
		return 0;

	if (!string2net(buf, net))
	        return -1;

	return 0;
}

void filter_network_cleanup(filter_node_t *n)
{
	/* NOP? */
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

	return 0;
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

	return 0;
}

int filter_network_fixup_param(filter_node_t *node, filter_pipe_t *p,
			       const char *name, filter_param_t *param)
{
	filter_paramdesc_t *d;
	struct filter_network_mapping *m;
	filter_node_t *n;

	/* pipe parameter setting does not go through the wrapped funcs */
	d = param->desc;
	m = (struct filter_network_mapping *)d->private;
	if (!(n = filternetwork_get_node(node, m->node)))
		return -1;
	if (FILTER_PARAMTYPE(d->type) == FILTER_PARAMTYPE_STRING)
	        return filternode_set_param(n, m->label, param->val.string);
	else
	        return filternode_set_param(n, m->label, &param->val);
}

/* fixup_pipe && fixup_break_in && fixup_break_out do not have to be
 * special at the moment.
 */

