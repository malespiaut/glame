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

/* This effect inverts the phase of a signal.  It can be used to correct
   phase problems.  Hopefully this will eventually make it into basic.c
   since it is an extremely simple effect and commonly used. */

static int invert_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	int i;

	in = filternode_get_input(n, PORTNAME_IN);	
	out = filternode_get_output(n, PORTNAME_OUT);	
	if (!in || !out)
		return -1;

	FILTER_AFTER_INIT;

	while (pthread_testcancel(), buf = sbuf_get(in)) {
		sbuf_make_private(buf);
		for (i = 0; i < sbuf_size(buf); i++) {
			sbuf_buf(buf)[i] = -(sbuf_buf(buf)[i]);
		}
		sbuf_queue(out, buf);
	}

	sbuf_queue(out, buf);

	FILTER_BEFORE_CLEANUP;

	return 0;
}

/* This filter positions a mono sound in the stereo field */

static int pan_f(filter_node_t *n)
{
	filter_param_t *pan;	/* between -1 and 1 */
	filter_pipe_t *in, *l_out, *r_out;
	filter_buffer_t *l_buf, *r_buf;
	int i;

	if (!(pan = filternode_get_param(n, "pan")))
		return -1;
	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		return -1;
	if (!(l_out = filternode_get_output(n, "left_out")))
		return -1;
	if (!(r_out = filternode_get_output(n, "right_out")))
		return -1;

	FILTER_AFTER_INIT;

	while (pthread_testcancel(), l_buf = sbuf_get(in)) {
		sbuf_make_private(l_buf);
		r_buf = sbuf_alloc(sbuf_size(l_buf), n);
		for (i = 0; i < sbuf_size(l_buf); i++) {
			sbuf_buf(r_buf)[i] = sbuf_buf(l_buf)[i] * (pan->val.f + 1.0);
			sbuf_buf(l_buf)[i] *= -(pan->val.f) - 1.0;
		}
		sbuf_queue(l_out, l_buf);
		sbuf_queue(r_out, r_buf);
	}

	sbuf_queue(l_out, l_buf);
	sbuf_queue(r_out, l_buf);

	FILTER_BEFORE_CLEANUP;

	return 0;
}

/* This filter is a complete filter for routing needs.  It has the
   functionality of "mix" and "one2n" filters combined into one. */

static int route_f(filter_node_t *n)
{
#if 0
	filter_buffer_t *buf;
	filter_pipe_t *in, *out;

	FILTER_AFTER_INIT;

	do {
		filternode_foreach_input(n, in) {
			
		}
		filternode_foreach_output(n, out) {
			sbuf_ref(buf);
			sbuf_queue(out, buf);
		}
	} while (pthread_testcancel(), buf);

	FILTER_BEFORE_CLEANUP;

#endif
	return 0;
}

int garrison_register()
{
	filter_t *f;

	/***** invert filter *****/
	if ((f = filter_alloc("Phase Inverter", "Inverses the phase of the audio signal", invert_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, "in", "input stream to invert", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, "out", "inverted output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (filter_add(f))
		return -1;

	/***** pan filter *****/
	if ((f = filter_alloc("Pan", "Positions a mono audio stream in the stereo field", pan_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, "in", "input stream to pan", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, "left_out", "left output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, "right_out", "right output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_param(f, "pan", "position in stereo field", FILTER_PARAMTYPE_FLOAT)))
		return -1;
	if (filter_add(f))
		return -1;

#if 0
	/***** route filter *****/
	if ((f = filter_alloc("Route", "Mixes and splits audio signals", route_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, "in", "input", FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, "out", "output", FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (filter_add(f))
		return -1;
#endif

	return 0;
}
