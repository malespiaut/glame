/*
 * filter_node.c
 * $Id: filter_node.c,v 1.9 2000/02/03 18:21:21 richi Exp $
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


filter_node_t *filternetwork_add_node(filter_network_t *net, const char *filter, const char *name)
{
	filter_t *f;
	filter_node_t *n;

	if (!net || !filter)
		return NULL;
	if (!(f = hash_find_filter(filter)))
		return NULL;

	if (!(n = ALLOC(filter_node_t)))
		return NULL;

	/* init node */
	n->net = net;
	INIT_LIST_HEAD(&n->neti_list);
	INIT_LIST_HEAD(&n->net_list);
	n->name = name;
	n->filter = f;

	n->nr_params = 0;
	INIT_LIST_HEAD(&n->params);

	n->nr_inputs = 0;
	n->nr_outputs = 0;
	INIT_LIST_HEAD(&n->inputs);
	INIT_LIST_HEAD(&n->outputs);

	if (f->init)
		f->init(n);

	/* add node to networks input and output node lists
	 * it will be discarded if connected.
	 */
	list_add(&n->neti_list, &net->inputs);
	list_add(&n->net_list, &net->nodes);

	return n;
}

filter_pipe_t *filternetwork_add_connection(filter_node_t *source, const char *source_port,
					    filter_node_t *dest, const char *dest_port)
{
	filter_portdesc_t *in, *out;
	filter_pipe_t *p;

	if (!source || !source_port || !dest || !dest_port
	    || source->net != dest->net)
		return NULL;

	if (!(in = hash_find_outputdesc(source_port, source->filter))
	    || !(out = hash_find_inputdesc(dest_port, dest->filter)))
		return NULL;

	if (!(p = ALLOC(filter_pipe_t)))
		return NULL;
	p->in_name = out->label;
	p->out_name = in->label;
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
	if (dest->filter->fixup_pipe(p->dest, p) == -1)
	        goto _err_fixup;

	p->dest->nr_inputs++;
	p->source->nr_outputs++;

	/* remove the source filter from the net output filter list
	 * and the dest filter from the net input filter list.
	 */
	list_del(&dest->neti_list);
	INIT_LIST_HEAD(&dest->neti_list);

	return p;

 _err_fixup:
	list_del_input(p);
	hash_remove_input(p);
	list_del_output(p);
	hash_remove_output(p);
 _err:
	free(p);
	return NULL;
}



int filternode_setparam(filter_node_t *n, const char *label, void *val)
{
	filter_param_t *param;
	filter_paramdesc_t *pdesc;

	if (!(pdesc = hash_find_paramdesc(label, n->filter)))
		return -1;
	if (!(param = hash_find_param(label, n))) {
		if (!(param = ALLOC(filter_param_t)))
			return -1;
		param->label = pdesc->label;
		hash_add_param(param, n);
	}

	switch (FILTER_PARAMTYPE(pdesc->type)) {
	case FILTER_PARAMTYPE_INT:
		param->val.i = *(int *)val;
		break;
	case FILTER_PARAMTYPE_FLOAT:
		param->val.f = *(float *)val;
		break;
	case FILTER_PARAMTYPE_SAMPLE:
		param->val.sample = *(SAMPLE *)val;
		break;
	case FILTER_PARAMTYPE_FILE:
		param->val.file = *(fileid_t *)val;
		break;
	case FILTER_PARAMTYPE_STRING:
		param->val.string = *(char **)val;
		break;
	}

	n->filter->fixup_param(n, label);

	return 0;
}

