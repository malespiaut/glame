/*
 * filter_node.c
 * $Id: filter_node.c,v 1.4 2000/01/27 12:53:35 richi Exp $
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

	n->nr_params = 0;
	INIT_LIST_HEAD(&n->params);

	n->nr_inputs = 0;
	n->nr_outputs = 0;
	INIT_LIST_HEAD(&n->inputs);
	INIT_LIST_HEAD(&n->outputs);

	if (filter->init)
		filter->init(n);

	/* add node to networks input and output node lists
	 * it will be discarded if connected.
	 */
	list_add(&n->neti_list, &net->inputs);
	list_add(&n->neto_list, &net->outputs);

	return n;
}

int filternode_connect(filter_node_t *source, char *source_port,
		       filter_node_t *dest, char *dest_port)
{
	filter_port_desc_t *in, *out;
	filter_pipe_t *p;

	if (!source || !source_port || !dest || !dest_port
	    || source->net != dest->net)
		return -1;

	if (!(in = hash_find_outputdesc(source_port, source->filter))
	    || !(out = hash_find_inputdesc(dest_port, dest->filter)))
		return -1;

	if (!(p = ALLOC(filter_pipe_t)))
		return -1;
	p->in_name = in->label;
	p->out_name = out->label;
	p->source = source;
	p->dest = dest;
	if (source->filter->connect_out(source, source_port, p) == -1)
		goto _err;
	if (dest->filter->connect_in(dest, dest_port, p) == -1)
		goto _err;

	/* add the pipe to all port lists/hashes.
	 * connect_out/in may have mucked with p->dest/source, so
	 * we have to use that instead of dest/source directly. */
	list_add_input(p, p->dest);
	hash_add_input(p, p->dest);
	list_add_output(p, p->source);
	hash_add_output(p, p->source);

	/* signal input changes to destination node */
	if (dest->filter->fixup(p->dest, p) == -1)
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
	list_del_input(p);
	hash_remove_input(p);
	list_del_output(p);
	hash_remove_output(p);
 _err:
	free(p);
	return -1;
}


/* Default output connect method.
 * Output pipe type is copied from corresponding input
 * (or uninitialized).
 */
int filter_default_connect_out(filter_node_t *n, const char *port,
			       filter_pipe_t *p)
{
	filter_port_desc_t *out;
	filter_pipe_t *in;

	/* is there a port with the right name? */
	if (!(out = hash_find_outputdesc(port, n->filter)))
		return -1;

	/* do we have a connection to this port already and are
	 * we not a multiple connection port? */
	if (!(out->flags & FILTER_PORTFLAG_AUTOMATIC)
	    && hash_find_output(port, n))
		return -1;

	/* fill pipe */
	p->type = FILTER_PIPETYPE_UNINITIALIZED;

	/* a source port has to provide pipe data info.
	 * we copy from the first input port if any. */
	if ((in = list_gethead_input(n))) {
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
	filter_port_desc_t *in;

	/* is there a port with the right name? */
	if (!(in = hash_find_inputdesc(port, n->filter)))
		return -1;

	/* do we have a connection to this port already and are
	 * we not a multiple connection port? */
	if (!(in->flags & FILTER_PORTFLAG_AUTOMATIC)
	    && hash_find_input(port, n))
		return -1;

	return 0;
}

int filter_default_fixup(filter_node_t *n, filter_pipe_t *in)
{
	filter_pipe_t *out;

	/* Parameter change? In the default method
	 * we know nothing about parameters, so we
	 * cant do anything about it.
	 * Forwarding is useless, too.
	 */
	if (in == NULL)
		return 0;

	/* Pipe format change is easy for us as we know
	 * nothing about internal connections between
	 * inputs and outputs.
	 * So the rule of dumb is to update all output
	 * pipe formats to the format of the input
	 * pipe we just got. We also have to forward
	 * the change to every output slot, of course.
	 */
	list_foreach_output(n, out) {
		out->type = in->type;
		out->u = in->u;
		if (out->dest->filter->fixup(out->dest, out) == -1)
			return -1;
	}

	return 0;
}


int filternode_setparam_f(filter_node_t *n, const char *label, fileid_t file)
{
	filter_param_t *param;
	filter_param_desc_t *pdesc;

	if (!(param = hash_find_param(label, n))) {
		if (!(pdesc = hash_find_paramdesc(label, n->filter)))
			return -1;
		if (!(param = ALLOC(filter_param_t)))
			return -1;
		param->label = pdesc->label;
		hash_add_param(param, n);
	}
	param->val.file = file;

	return 0;
}

