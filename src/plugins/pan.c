/*
 * pan.c
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

/* This filter positions a mono sound in the stereo field */

static int pan_f(filter_node_t *n)
{
	filter_param_t *pan;	/* [-pi, pi] */
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
				* (filterparam_val_float(pan) + FILTER_PIPEPOS_RIGHT) / 2;
			sbuf_buf(l_buf)[i] *= 
				(filterparam_val_float(pan) + FILTER_PIPEPOS_LEFT) / -2;
		}
		sbuf_queue(l_out, l_buf);
		sbuf_queue(r_out, r_buf);
	}

	sbuf_queue(l_out, l_buf);
	sbuf_queue(r_out, l_buf);

	FILTER_BEFORE_CLEANUP;

	return 0;
}

int pan_register()
{
	filter_t *f;

	if (((f = filter_alloc(pan_f)) == NULL)
	    || !filter_add_input(f, PORTNAME_IN, "input stream to pan", FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "two output streams", FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC)
	    || !filter_add_param(f, "pan", "position in stereo field [-pi, pi]", FILTER_PARAMTYPE_FLOAT)
	    || filter_add(f, "pan", "Positions a mono audio stream in the stereo field"))
		return -1;

	return 0;
}
