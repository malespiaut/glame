/*
 * filter.c
 * $Id: filter.c,v 1.11 2000/02/05 15:59:26 richi Exp $
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


/* filter default methods */

/* Default output connect method.
 * Output pipe type is copied from corresponding input
 * (or uninitialized).
 */
static int filter_default_connect_out(filter_node_t *n, const char *port,
				      filter_pipe_t *p)
{
	filter_portdesc_t *out;
	filter_pipe_t *in;

	/* is there a port with the right name? */
	if (!(out = hash_find_outputdesc(port, n->filter)))
		return -1;

	/* do we have a connection to this port already and are
	 * we not a multiple connection port? */
	if (!FILTER_PORT_IS_AUTOMATIC(out->type)
	    && hash_find_output(port, n))
		return -1;

	/* fill in a sane pipe type */
	p->type = FILTER_PIPETYPE_DEFAULT(out->type);

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
static int filter_default_connect_in(filter_node_t *n, const char *port,
				     filter_pipe_t *p)
{
	filter_portdesc_t *in;

	/* is there a port with the right name? */
	if (!(in = hash_find_inputdesc(port, n->filter)))
		return -1;

	/* do we have a connection to this port already and are
	 * we not a multiple connection port? */
	if (!FILTER_PORT_IS_AUTOMATIC(in->type)
	    && hash_find_input(port, n))
		return -1;

	/* do we support the requested pipe type? */
	if (!FILTER_PORT_IS_COMPATIBLE(in->type, p->type))
		return -1;

	return 0;
}

static int filter_default_fixup_param(filter_node_t *n, const char *name)
{
	/* Parameter change? In the default method
	 * we know nothing about parameters, so we
	 * cant do anything about it.
	 * Forwarding is useless, too.
	 */
	return 0;
}
static int filter_default_fixup_pipe(filter_node_t *n, filter_pipe_t *in)
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
	list_foreach_output(n, out) {
		out->type = in->type;
		out->u = in->u;
		if (out->dest->filter->fixup_pipe(out->dest, out) == -1)
			return -1;
	}

	return 0;
}
static void filter_default_fixup_break_in(filter_node_t *n, filter_pipe_t *in)
{
	/* we dont know nothing about relationships between input
	 * and output ports, so anything here would be senseless. */
	return;
}
static void filter_default_fixup_break_out(filter_node_t *n, filter_pipe_t *out)
{
	/* we dont know nothing about relationships between input
	 * and output ports, so anything here would be senseless. */
	return;
}


/* the API functions */

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
	f->fixup_param = filter_default_fixup_param;
	f->fixup_pipe = filter_default_fixup_pipe;
	f->fixup_break_in = filter_default_fixup_break_in;
	f->fixup_break_out = filter_default_fixup_break_out;

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

filter_portdesc_t *filter_add_input(filter_t *filter, const char *label,
				    const char *description, int type)
{
	filter_portdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return NULL;
	if (hash_find_inputdesc(label, filter))
		return NULL;
	if (!(desc = ALLOC(filter_portdesc_t)))
		return NULL;

	desc->label = label;
	desc->type = type;
	desc->description = description;
	hash_add_inputdesc(desc, filter);
	list_add_inputdesc(desc, filter);
	filter->nr_inputs++;

	return desc;
}

filter_portdesc_t *filter_add_output(filter_t *filter, const char *label,
				     const char *description, int type)
{
	filter_portdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return NULL;
	if (hash_find_outputdesc(label, filter))
		return NULL;
	if (!(desc = ALLOC(filter_portdesc_t)))
		return NULL;

	desc->label = label;
	desc->description = description;
	desc->type = type;
	hash_add_outputdesc(desc, filter);
	list_add_outputdesc(desc, filter);
	filter->nr_outputs++;

	return desc;
}

filter_paramdesc_t *filter_add_param(filter_t *filter, const char *label,
				     const char *description, int type)
{
	filter_paramdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return NULL;
	if (hash_find_paramdesc(label, filter))
		return NULL;
	if (!(desc = ALLOC(filter_paramdesc_t)))
		return NULL;

	desc->label = label;
	desc->description = description;
	desc->type = type;
	hash_add_paramdesc(desc, filter);
	list_add_paramdesc(desc, filter);
	filter->nr_params++;

	return desc;
}


/* include all static filter headers here */
extern int basic_register();
extern int channel_io_register();
extern int audio_io_register();
extern int debug_register();
extern int mix(filter_node_t *n);
extern int volume_adjust(filter_node_t *n);
extern int waveform_register();
extern int read_file_register();

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

	/* initialize read_file filter */
	if (read_file_register() == -1)
		return -1;
	
	if (!(f = filter_alloc("mix", "mix n channels", mix))
	    || !filter_add_input(f, "in", "input stream",
				 FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, "mixed", "mixed stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("volume_adjust", "scale samples",
			       volume_adjust))
	    || !filter_add_param(f, "factor", "scale factor",
				 FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_input(f, "in", "input stream",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, "out", "output stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f) == -1)
		return -1;

	return 0;
}

