/*
 * filter_methods.c
 * $Id: filter_methods.c,v 1.1 2000/02/14 13:23:40 richi Exp $
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
	filter_portdesc_t *out;
	filter_pipe_t *in;

	/* is there a port with the right name? */
	if (!(out = filter_get_outputdesc(n->filter, port)))
		return -1;

	/* fill in a sane pipe type */
	p->type = FILTER_PIPETYPE_DEFAULT(out->type);

	/* a source port has to provide pipe data info.
	 * we copy from the first input port if any. */
	if ((in = filternode_first_input(n))) {
		p->type = in->type;
		p->u = in->u;
	}

	return 0;
}

/* Default input connect method.
 * We accept all input types.
 */
int filter_default_connect_in(filter_node_t *n, const char *port,
				     filter_pipe_t *p)
{
	filter_portdesc_t *in;

	/* is there a port with the right name? */
	if (!(in = filter_get_inputdesc(n->filter, port)))
		return -1;

	/* do we support the requested pipe type? */
	if (!FILTER_PORT_IS_COMPATIBLE(in->type, p->type))
		return -1;

	return 0;
}

int filter_default_fixup_param(filter_node_t *n, const char *name)
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
	 */
	filternode_foreach_output(n, out) {
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

int filter_network_init(filter_node_t *n)
{
	FILE *fd;
	char c, node1[256], name1[256], node2[256], name2[256], desc[256];
	filter_network_t *net = (filter_network_t *)n;

	/* empty network? */
	if (!n->filter->private)
		return 0;

	if (!(fd = fopen((char *)n->filter->private, "r")))
		return -1;

	while (fscanf(fd, "%c", &c) == 1) {
		switch (c) {
		case 'n':
			fscanf(fd, " %s %s ", node1, name1);
			if (!filternetwork_add_node(net, node1, name1))
				goto err;
			break;
		case 'c':
			fscanf(fd, " %s %s %s %s ", node1, name1,
			       node2, name2);
			if (!filternetwork_add_connection(filternetwork_get_node(net, node1),
							  name1, filternetwork_get_node(net, node2), name2))
				goto err;
			break;
		case 'i': /* skip */
			fgets(desc, 256, fd);
			break;
		case 'o': /* skip */
			fgets(desc, 256, fd);
			break;
		case 'p': /* skip */
			fgets(desc, 256, fd);
			break;
		case 's':
			fscanf(fd, " %s %s ", name1, desc);
			filternode_set_paramstring(&net->node, name1, desc);
			break;
		default:
			break;
		}
	}

	fclose(fd);
	return 0;

 err:
	fclose(fd);
	filternetwork_delete(net);
	return -1;
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
	filter_portdesc_t *d;
	struct filter_network_mapping *m;
	filter_node_t *n;

	if (!(d = filter_get_inputdesc(dest->filter, port)))
		return -1;
	m = (struct filter_network_mapping *)d->private;
	if (!(n = filternetwork_get_node(dest, m->node)))
		return -1;
	p->in_name = m->label;
	p->dest = n;

	return 0;
}

int filter_network_fixup_param(filter_node_t *node, const char *name)
{
	filter_paramdesc_t *d;
	filter_param_t *p;
	struct filter_network_mapping *m;
	filter_node_t *n;

	if (!(p = filternode_get_param(node, name)))
		return -1;
	if (!(d = filter_get_paramdesc(node->filter, name)))
		return -1;
	m = (struct filter_network_mapping *)d->private;
	if (!(n = filternetwork_get_node(node, m->node)))
		return -1;
	return filternode_set_param(n, m->label, &p->val);
}

/* fixup_pipe && fixup_break_in && fixup_break_out do not have to be
 * special at the moment.
 */
