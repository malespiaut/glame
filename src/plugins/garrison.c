/*
 * garrison.c
 *
 * Copyright (C) 2000 Jim Garrison
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

#include "filter.h"
#include <math.h>

/* This filter positions a mono sound in the stereo field */

static int pan_f(filter_node_t *n)
{
	filter_param_t *pan;	/* between -1 and 1 */
	filter_pipe_t *in, *l_out, *r_out;
	filter_buffer_t *l_buf, *r_buf;
	int i;

	if (!(pan = filternode_get_param(n, "pan"))
	    || !(in = filternode_get_input(n, PORTNAME_IN))
	    || !(l_out = filternode_get_output(n, PORTNAME_OUT))
	    || !(r_out = filternode_next_output(l_out)))
		return -1;

	filterpipe_sample_hangle(l_out) = FILTER_PIPEPOS_LEFT;
	filterpipe_sample_hangle(r_out) = FILTER_PIPEPOS_RIGHT;

	FILTER_AFTER_INIT;

	while (pthread_testcancel(), l_buf = sbuf_get(in)) {
		l_buf = sbuf_make_private(l_buf);
		r_buf = sbuf_alloc(sbuf_size(l_buf), n);
		for (i = 0; i < sbuf_size(l_buf); i++) {
			sbuf_buf(r_buf)[i] = sbuf_buf(l_buf)[i] 
				* (filterparam_val_float(pan) + 1.0) / 2;
			sbuf_buf(l_buf)[i] *= 
				(-(filterparam_val_float(pan)) + 1.0) / 2;
		}
		sbuf_queue(l_out, l_buf);
		sbuf_queue(r_out, r_buf);
	}

	sbuf_queue(l_out, l_buf);
	sbuf_queue(r_out, l_buf);

	FILTER_BEFORE_CLEANUP;

	return 0;
}

#if 0

/* Mixes and balances effected and uneffected signals */

static int bal_f(filter_node_t *n)
{
	return 0;
}

/* Another shot at the infamous echo filter */

static int echo3_f(filter_node_t *n)
{
	filter_param_t *feedback, *delay;
	filter_pipe_t *in, *out;
	feedback_fifo_t fifo;
	filter_buffer_t *buf, *dbuf = NULL;
	int i;

	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	feedback = filternode_get_param(n, "feedback");
	delay = filternode_get_param(n, "delay");
	INIT_FEEDBACK_FIFO(fifo);

	FILTER_AFTER_INIT;

	while (pthread_testcancel(), 
	    ((buf = sbuf_make_private(sbuf_get(in))) || (dbuf))) {
		
		sbuf_queue(out, dbuf);
		
	}

	FILTER_BEFORE_CLEANUP;

	return 0;
}

/* This takes a parameter and turns it into a stream. Will be useful
   when combined with my new echo and tremolo effects (soon to come) */

static int p2s_f (filter_node_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	SAMPLE val;
	int i;

	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return -1;

	param = filternode_get_param(n, "val");
	val = filterparam_val_sample(param);

	buf = sbuf_make_private(sbuf_alloc(SIZE, n));
	for (i = 0; i < SIZE; i++)
		sbuf_buf(buf)[i] = val;

	FILTER_AFTER_INIT;

	while (1) {		/**** FIXME ****/
		if (filterparam_val_sample(param) != val) {
			sbuf_unref(buf);
			val = filterparam_val_sample(param);
			buf = sbuf_make_private(sbuf_alloc(SIZE, n));
			for (i = 0; i < SIZE; i++)
				sbuf_buf(buf)[i] = val;
		}
		sbuf_ref(buf);
		sbuf_queue(out, buf);
	}

	FILTER_BEFORE_CLEANUP;

	sbuf_unref(buf);

	return 0;
}

/* 
 * Multiplies each sample from two input channels.  Useful for tremolo.
 */

static int ringmod_f (filter_node_t *n)
{
	filter_pipe_t *in, *auxin, *out;
	filter_buffer_t *buf, *auxbuf;

	if ((auxin = filternode_get_input(n, "aux_in"))
	    || (in = filternode_get_input(n, PORTNAME_IN))
	    || (out = filternode_get_output(n, PORTNAME_OUT)))
		return -1;

	FILTER_AFTER_INIT;

	/* FIXME */

	FILTER_BEFORE_CLEANUP;

	return 0;
}

#endif

int garrison_register()
{
	filter_t *f;

	/***** pan filter *****/
	if ((f = filter_alloc("pan", "Positions a mono audio stream in the stereo field", pan_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, PORTNAME_IN, "input stream to pan", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, PORTNAME_OUT, "output streams", FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC)))
		return -1;
	if (!(filter_add_param(f, "pan", "position in stereo field", FILTER_PARAMTYPE_FLOAT)))
		return -1;
	if (filter_add(f))
		return -1;

#if 0
	/***** balance filter *****/
	if ((f = filter_alloc("Balance", "Mixes wet/dry signals", bal_f)) == NULL)
		return -1;
	if (filter_add(f))
		return -1;

	/***** echo3 filter *****/
	if ((f = filter_alloc("Echo 3", "echo", echo3_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, "delay", "delay time (in samples)", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_param(f, "feedback", "feedback ratio", FILTER_PARAMTYPE_FLOAT)))
		return -1;
	if (filter_add(f))
		return -1;

	/***** parameter to stream *****/
	if ((f = filter_alloc("p2s", "Converts parameter to stream", p2s_f)) == NULL)
		return -1;
	if (!(filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_param(f, "val", "value to output", FILTER_PARAMTYPE_FLOAT)))
		return -1;
	if (filter_add(f))
		return -1;

	/***** ring modulator filter *****/
	if ((f = filter_alloc("ringmod", "Ring Modulator", ringmod_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_input(f, "aux_in", "aux input", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (filter_add(f))
		return -1;
#endif

	return 0;
}
