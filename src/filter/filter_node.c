/*
 * filter_node.c
 * $Id: filter_node.c,v 1.1 2000/01/20 14:54:19 richi Exp $
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
 */

#include <sys/types.h>
#include <signal.h>
#include <stdlib.h>
#include <pthread.h>
#include "util.h"
#include "filter.h"
#include "filter_hash.h"


filter_node_t *filternode_add(filter_network_t *net, const char *name)
{
	filter_t *filter;
	filter_node_t *n;

	if (!net || !name)
		return NULL;
	if (!(filter = hash_find_filter(name)))
		return NULL;

	if (!(n = ALLOC(filter_node_t)))
		return NULL;

	/* init node */
	n->net = net;
	INIT_LIST_HEAD(&n->neti_list);
	INIT_LIST_HEAD(&n->neto_list);
	n->filter = filter;
	if (filter->nr_params > 0
	    && !(n->params = ALLOCN(filter->nr_params, union filter_param)))
		goto _nomem;
	n->nr_inputs = filter->nr_inputs;
	if (filter->nr_inputs > 0
	    && !(n->inputs = ALLOCN(filter->nr_inputs, filter_pipe_t *)))
		goto _nomem;
	n->nr_outputs = filter->nr_outputs;
	if (filter->nr_outputs > 0
	    && !(n->outputs = ALLOCN(filter->nr_outputs, filter_pipe_t *)))
		goto _nomem;

	if (filter->init)
		filter->init(n);

	/* add node to networks input and output node lists
	 * it will be discarded if connected.
	 */
	list_add(&n->neti_list, &net->inputs);
	list_add(&n->neto_list, &net->outputs);

	return n;

 _nomem:
	free(n->params);
	free(n->inputs);
	free(n->outputs);
	free(n);

	return NULL;
}

int filternode_connect(filter_node_t *source, char *source_port,
		       filter_node_t *dest, char *dest_port)
{
	filter_pipe_t *p;

	if (!source || !source_port || !dest || !dest_port
	    || source->net != dest->net)
		return -1;

	if (!(p = ALLOC(filter_pipe_t)))
		return -1;

	p->source = source;
	p->dest = dest;
	if (source->filter->connect_out(source, source_port, p) == -1)
		goto _err;
	if (dest->filter->connect_in(dest, dest_port, p) == -1)
		goto _err;

	/* fill the slots */
	source->outputs[p->source_port] = p;
	dest->inputs[p->dest_port] = p;

	/* signal input changes to destination node */
	if (dest->filter->fixup(dest, p->dest_port) == -1)
	        goto _err_fixup;

	/* remove the source filter from the net output filter list
	 * and the dest filter from the net input filter list.
	 */
	list_del(&source->neto_list);
	INIT_LIST_HEAD(&source->neto_list);
	list_del(&dest->neti_list);
	INIT_LIST_HEAD(&dest->neti_list);

	return 0;

 _err_fixup:
	source->outputs[p->source_port] = NULL;
	dest->inputs[p->dest_port] = NULL;
 _err:
	free(p);
	return -1;
}


/* helper to find a slot */
int _filterconnect_find_port(struct filter_port_desc *ports, int cnt,
			     const char *port)
{
	int i;

	for (i=0; i<cnt; i++)
		if (strcmp(ports[i].label, port) == 0)
			return i;

	return -1;
}
/* helper to find a slot & assign it */
int _filterconnect_assign_port(struct filter_port_desc *pdesc, int cnt,
			       const char *port,
			       filter_pipe_t ***slots, int *nr_slots)
{
	int slot;

	/* find slot */
	if ((slot = _filterconnect_find_port(pdesc, cnt, port)) == -1)
		return -1;

	/* automatic ports are special */
	if (pdesc->flags & FILTER_PORTFLAG_AUTOMATIC) {
		while ((*slots)[slot] != NULL) {
			slot++;
			if (slot >= *nr_slots) {
				*slots = (filter_pipe_t **)realloc(*slots, slot*sizeof(void *));
				(*slots)[slot] = NULL;
				(*nr_slots)++;
			}
		}
	} else if ((*slots)[slot] != NULL)
		return -1;

	return slot;
}


/* Default output connect method.
 * Output pipe type is copied from corresponding input
 * (or uninitialized).
 */
int filter_default_connect_out(filter_node_t *n, const char *port,
			       filter_pipe_t *p)
{
	int i;

	/* try to find port and assign a slot */
	i = filterconnect_assignslot_output(n, port);
	if (i == -1)
		return -1;

	/* fill pipe */
	p->source_port = i;
	p->type = FILTER_PIPETYPE_UNINITIALIZED;

	/* source port has to provide pipe data info.
	 * we copy from the corresponding input port. */
	if (n->nr_inputs > i && n->inputs[i]) {
		p->type = n->inputs[i]->type;
		p->u = n->inputs[i]->u;
	}

	return 0;
}

/* Default input connect method.
 * We accept all input types.
 */
int filter_default_connect_in(filter_node_t *n, const char *port,
			      filter_pipe_t *p)
{
	int i;

	/* try to find port and assign a slot */
	i = filterconnect_assignslot_input(n, port);
	if (i == -1)
		return -1;

	/* fill pipe */
	p->dest_port = i;

	return 0;
}

/* Default fixup method.
 * Does update (copy) one-to-one and automatic connections
 * and forwards the fixup. Obviously does not know anything
 * about parameters.
 */
int filter_default_fixup(filter_node_t *n, int input_slot)
{
	int i;

	/* parameter change? - in the default method
	 * we know nothing about parameters, so we
	 * cant do anything about it.
	 * forwarding is useless, too.
	 */
	if (input_slot == -1)
		return 0;

	/* if its an automatic one, check all inputs for
	 * being of the same type and parameters.
	 * for further processing set the input_slot to
	 * the first of the automatic ones, too.
	 */
	if (input_slot >= n->filter->nr_inputs
	    || n->filter->inputs[input_slot].flags & FILTER_PORTFLAG_AUTOMATIC) {
		input_slot = n->filter->nr_inputs-1;
		for (i=input_slot+1; i<n->nr_inputs; i++) {
			if (!n->inputs[i])
				continue;
			if (n->inputs[i]->type != n->inputs[input_slot]->type
			    || memcmp(&n->inputs[i]->u, &n->inputs[input_slot]->u, sizeof(n->inputs[i]->u)))
				return -1;
		}
	}

	/* if the corresponding output slot is not connected
	 * all is done.
	 */
	if (input_slot >= n->nr_outputs
	    || !n->outputs[input_slot])
		return 0;

	/* check if the output slot is an automatic one.
	 * if so, update all (connected) automatic output
	 * slots wrt to the changed input and forward the
	 * fixup.
	 * else just copy & forward to one output.
	 */
	if (input_slot >= n->filter->nr_outputs
	    || n->filter->outputs[input_slot].flags & FILTER_PORTFLAG_AUTOMATIC) {
		for (i=input_slot; i<n->nr_outputs; i++) {
			if (!n->outputs[i])
				continue;
			n->outputs[i]->type = n->inputs[input_slot]->type;
			n->outputs[i]->u = n->inputs[input_slot]->u;

			if (n->outputs[i]->dest->filter->fixup(n->outputs[i]->dest, n->outputs[i]->dest_port) == -1)
				return -1;
		}
	} else {
		n->outputs[input_slot]->type = n->inputs[input_slot]->type;
		n->outputs[input_slot]->u = n->inputs[input_slot]->u;
		if (n->outputs[input_slot]->dest->filter->fixup(n->outputs[input_slot]->dest, n->outputs[input_slot]->dest_port) == -1)
			return -1;
	}

	return 0;
}

