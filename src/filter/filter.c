/*
 * filter.c
 * $Id: filter.c,v 1.8 2000/02/01 09:35:55 mag Exp $
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


/* global list of registered filters */
static struct list_head filter_list;


extern int filter_default_connect_out(filter_node_t *n, const char *port,
				      filter_pipe_t *p);
extern int filter_default_connect_in(filter_node_t *n, const char *port,
				     filter_pipe_t *p);
extern int filter_default_fixup(filter_node_t *n, filter_pipe_t *in);



filter_t *filter_alloc(const char *name, const char *description,
		       int (*func)(filter_node_t *))
{
	filter_t *f;

	if (!name || !func)
		return NULL;
	if (hash_find_filter(name))
		return NULL;
	if (!(f = ALLOC(filter_t)))
		return NULL;
	hash_init_filter(f);
	INIT_LIST_HEAD(&f->list);

	f->name = name;
	f->description = description;

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
}

int filter_add(filter_t *filter)
{
	if (hash_find_filter(filter->name))
		return -1;

	hash_add_filter(filter);
	list_add_filter(filter);

	return 0;
}

int filter_add_input(filter_t *filter, const char *label,
		     const char *description, int type)
{
	filter_portdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return -1;
	if (hash_find_inputdesc(label, filter))
		return -1;
	if (!(desc = ALLOC(filter_portdesc_t)))
		return -1;

	desc->label = label;
	desc->type = type;
	desc->description = description;
	hash_add_inputdesc(desc, filter);
	list_add_inputdesc(desc, filter);
	filter->nr_inputs++;

	return 0;
}

int filter_add_output(filter_t *filter, const char *label,
		      const char *description, int type)
{
	filter_portdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return -1;
	if (hash_find_outputdesc(label, filter))
		return -1;
	if (!(desc = ALLOC(filter_portdesc_t)))
		return -1;

	desc->label = label;
	desc->description = description;
	desc->type = type;
	hash_add_outputdesc(desc, filter);
	list_add_outputdesc(desc, filter);
	filter->nr_outputs++;

	return 0;
}

int filter_add_param(filter_t *filter, const char *label,
		     const char *description, int type)
{
	filter_paramdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return -1;
	if (hash_find_paramdesc(label, filter))
		return -1;
	if (!(desc = ALLOC(filter_paramdesc_t)))
		return -1;

	desc->label = label;
	desc->description = description;
	desc->type = type;
	hash_add_paramdesc(desc, filter);
	list_add_paramdesc(desc, filter);
	filter->nr_params++;

	return 0;
}


/* include all static filter headers here */
extern int basic_register();
extern int channel_io_register();
extern int audio_io_register();
extern int debug_register();
extern int mix(filter_node_t *n);
extern int volume_adjust(filter_node_t *n);
extern int waveform_register();

int filter_init()
{
	filter_t *f;

	INIT_LIST_HEAD(&filter_list);

	/* initialize basic filters */
	if (basic_register() == -1)
		return -1;

	/* initialize channel/file input & output filters */
	if (channel_io_register() == -1)
		return -1;

	/* initialize audio input & output filters */
	if (audio_io_register() == -1)
		return -1;

	/* initialize debug & profile filters */
	if (debug_register() == -1)
		return -1;

	/* initialize waveform filters */
	if (waveform_register() == -1)
		return -1;

	if (!(f = filter_alloc("mix", "mix n channels", mix))
	    || filter_add_input(f, "in", "input stream",
				FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add_output(f, "mixed", "mixed stream",
				 FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("volume_adjust", "scale samples",
			       volume_adjust))
	    || filter_add_param(f, "factor", "scale factor",
				FILTER_PARAMTYPE_FLOAT) == -1
	    || filter_add_input(f, "in", "input stream",
				FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add_output(f, "out", "output stream",
				 FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;

	return 0;
}


filter_t *filter_next(filter_t *f)
{
	struct list_head *lh;

	if (!f)
		lh = &filter_list;
	else
		lh = &f->list;

	lh = lh->next;
	if (lh == &filter_list)
		return NULL;

	return list_entry(lh, filter_t, list);
}
