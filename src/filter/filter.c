/*
 * filter.c
 * $Id: filter.c,v 1.3 2000/01/24 11:43:22 richi Exp $
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
extern int filter_default_fixup(filter_node_t *n, int input_slot);



filter_t *filter_alloc(const char *name, int (*func)(filter_node_t *),
		       int nr_params, int nr_inputs, int nr_outputs)
{
	filter_t *f;

	if (hash_find_filter(name))
		return NULL;

	if (!(f = ALLOC(filter_t)))
		return NULL;
	hash_init_filter(f);
	
	if (!(f->name = strdup(name))
	    || (nr_params>0
		&& !(f->params = ALLOCN(nr_params, struct filter_param_desc)))
	    || (nr_inputs>0
		&& !(f->inputs = ALLOCN(nr_inputs, struct filter_port_desc)))
	    || (nr_outputs>0
		&& !(f->outputs = ALLOCN(nr_outputs, struct filter_port_desc))))
		goto _nomem;
	f->nr_params = nr_params;
	f->nr_inputs = nr_inputs;
	f->nr_outputs = nr_outputs;

	f->f = func;

	/* fill in default methods */
	f->connect_out = filter_default_connect_out;
	f->connect_in = filter_default_connect_in;
	f->fixup = filter_default_fixup;

	return f;

 _nomem:
	free(f->name);
	free(f->params);
	free(f->inputs);
	free(f->outputs);
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


/* include all static filter headers here */
extern int drop(filter_node_t *n);
extern int one2n(filter_node_t *n);
extern int dup(filter_node_t *n);
extern int mix(filter_node_t *n);
extern int null(filter_node_t *n);
extern int ping(filter_node_t *n);
extern int ping_connect_out(filter_node_t *n, const char *port,
			    filter_pipe_t *p);
extern int ping_fixup(filter_node_t *n, int slot);
extern int file_in(filter_node_t *n);
extern int file_out(filter_node_t *n);
extern int volume_adjust(filter_node_t *n);

int filter_init()
{
	filter_t *f;

	if (!(f = filter_alloc("drop", drop, 0, 1, 0)))
		return -1;
	f->inputs[0].label = "in";
	f->inputs[0].flags = FILTER_PORTFLAG_AUTOMATIC;
        if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("one2n", one2n, 0, 1, 1)))
		return -1;
	f->inputs[0].label = "in";
	f->outputs[0].label = "out";
	f->outputs[0].flags = FILTER_PORTFLAG_AUTOMATIC;
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("dup", dup, 0, 1, 2)))
		return -1;
	f->inputs[0].label = "in";
	f->outputs[0].label = "out1";
	f->outputs[1].label = "out2";
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("mix", mix, 0, 1, 1)))
		return -1;
	f->inputs[0].label = "in";
	f->inputs[0].flags = FILTER_PORTFLAG_AUTOMATIC;
	f->outputs[0].label = "mixed";
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("null", null, 0, 1, 1)))
		return -1;
	f->inputs[0].label = "in";
	f->inputs[0].flags = FILTER_PORTFLAG_AUTOMATIC;
	f->outputs[0].label = "out";
	f->outputs[0].flags = FILTER_PORTFLAG_AUTOMATIC;
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("ping", ping, 0, 1, 1)))
		return -1;
	f->connect_out = ping_connect_out;
	f->fixup = ping_fixup;
	f->inputs[0].label = "in";
	f->outputs[0].label = "out";
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("file_in", file_in, 1, 0, 1)))
		return -1;
	f->params[0].label = "file";
	f->params[0].type = "%i";
	f->outputs[0].label = "out";
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("file_out", file_out, 1, 1, 0)))
		return -1;
	f->params[0].label = "file";
	f->params[0].type = "%i";
	f->params[0].flags = FILTER_PARAMFLAG_OUTPUT;
	f->inputs[0].label = "in";
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("volume_adjust", volume_adjust, 1, 1, 1)))
		return -1;
	f->params[0].label = "factor";
	f->params[0].type = "%f";
	f->inputs[0].label = "in";
	f->outputs[0].label = "out";
	if (filter_add(f) == -1)
		return -1;

	return 0;
}
