/*
 * filter.c
 * $Id: filter.c,v 1.4 2000/01/27 10:30:30 richi Exp $
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

#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "filter.h"


extern int filter_default_connect_out(filter_node_t *n, const char *port,
				      filter_pipe_t *p);
extern int filter_default_connect_in(filter_node_t *n, const char *port,
				     filter_pipe_t *p);
extern int filter_default_fixup(filter_node_t *n, filter_pipe_t *in);



filter_t *filter_alloc(const char *name, int (*func)(filter_node_t *))
{
	filter_t *f;

	if (!name || !func)
		return NULL;
	if (hash_find_filter(name))
		return NULL;
	if (!(f = ALLOC(filter_t)))
		return NULL;
	hash_init_filter(f);

	if (!(f->name = strdup(name)))
		goto _nomem;

	INIT_LIST_HEAD(&f->params);
	INIT_LIST_HEAD(&f->inputs);
	INIT_LIST_HEAD(&f->outputs);
	f->nr_params = 0;
	f->nr_inputs = 0;
	f->nr_outputs = 0;

	f->f = func;

	/* fill in default methods */
	f->connect_out = filter_default_connect_out;
	f->connect_in = filter_default_connect_in;
	f->fixup = filter_default_fixup;

	return f;

 _nomem:
	free(f->name);
	free(f);
	return NULL;
}

int filter_add(filter_t *filter)
{
	if (hash_find_filter(filter->name))
		return -1;

	hash_add_filter(filter);

	return 0;
}

int filter_add_input(filter_t *filter, char *label, int flags)
{
	filter_port_desc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return -1;
	if (hash_find_inputdesc(label, filter))
		return -1;
	if (!(desc = ALLOC(filter_port_desc_t)))
		return -1;

	desc->label = strdup(label);
	desc->flags = flags;
	hash_add_inputdesc(desc, filter);
	list_add_inputdesc(desc, filter);
	filter->nr_inputs++;

	return 0;
}

int filter_add_output(filter_t *filter, char *label, int flags)
{
	filter_port_desc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return -1;
	if (hash_find_outputdesc(label, filter))
		return -1;
	if (!(desc = ALLOC(filter_port_desc_t)))
		return -1;

	desc->label = strdup(label);
	desc->flags = flags;
	hash_add_outputdesc(desc, filter);
	list_add_outputdesc(desc, filter);
	filter->nr_outputs++;

	return 0;
}

int filter_add_param(filter_t *filter, char *label, char *type, int flags)
{
	filter_param_desc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return -1;
	if (hash_find_paramdesc(label, filter))
		return -1;
	if (!(desc = ALLOC(filter_param_desc_t)))
		return -1;

	desc->label = strdup(label);
	desc->type = strdup(type);
	desc->flags = flags;
	hash_add_paramdesc(desc, filter);
	list_add_paramdesc(desc, filter);
	filter->nr_params++;

	return 0;
}


/* include all static filter headers here */
extern int drop(filter_node_t *n);
extern int one2n(filter_node_t *n);
extern int dup(filter_node_t *n);
extern int mix(filter_node_t *n);
extern int null(filter_node_t *n);
extern int ping(filter_node_t *n);
extern int ping_fixup(filter_node_t *n, filter_pipe_t *in);
extern int file_in(filter_node_t *n);
extern int file_out(filter_node_t *n);
extern int volume_adjust(filter_node_t *n);

int filter_init()
{
	filter_t *f;

	if (!(f = filter_alloc("drop", drop))
	    || filter_add_input(f, "in", FILTER_PORTFLAG_AUTOMATIC) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("one2n", one2n))
	    || filter_add_input(f, "in", 0) == -1
	    || filter_add_output(f, "out", FILTER_PORTFLAG_AUTOMATIC) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("dup", dup))
	    || filter_add_input(f, "in", 0) == -1
	    || filter_add_output(f, "out", 0) == -1
	    || filter_add_output(f, "out", 0) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("mix", mix))
	    || filter_add_input(f, "in", FILTER_PORTFLAG_AUTOMATIC) == -1
	    || filter_add_output(f, "mixed", 0) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("null", null))
	    || filter_add_input(f, "in", FILTER_PORTFLAG_AUTOMATIC) == -1
	    || filter_add_output(f, "out", FILTER_PORTFLAG_AUTOMATIC) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("ping", ping))
	    || filter_add_input(f, "in", 0) == -1
	    || filter_add_output(f, "out", 0) == -1)
		return -1;
	f->fixup = ping_fixup;
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("file_in", file_in))
	    || filter_add_param(f, "file", "%i", 0) == -1
	    || filter_add_output(f, "out", 0) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("file_out", file_out))
	    || filter_add_param(f, "file", "%i", FILTER_PARAMFLAG_OUTPUT) == -1
	    || filter_add_input(f, "in", 0) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("volume_adjust", volume_adjust))
	    || filter_add_param(f, "factor", "%f", 0) == -1
	    || filter_add_input(f, "in", 0) == -1
	    || filter_add_output(f, "out", 0) == -1
	    || filter_add(f) == -1)
		return -1;

	return 0;
}
