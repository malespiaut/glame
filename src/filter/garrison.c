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

	in = hash_find_input("input", n);	
	out = hash_find_output("output", n);	
	if (!in || !out)
		return -1;

	FILTER_AFTER_INIT;

	do {
		buf = fbuf_get(in);
		fbuf_make_private(buf);
		for (i = 0; i < fbuf_size(buf); i++) {
			fbuf_buf(buf)[i] = -fbuf_buf(buf)[i];
		}
		fbuf_queue(out, buf);
	} while (pthread_testcancel(), buf);

	FILTER_BEFORE_CLEANUP;

	return 0;
}

static int pan_f(filter_node_t *n)
{
#if 0
	
#endif
	return 0;
}

int garrison_register()
{
	filter_t *f;

	/***** invert filter *****/
	if ((f = filter_alloc("Phase Inverter", "Inverses the phase of the audio signal", invert_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, "input", "input stream to invert", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, "output", "inverted output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (filter_add(f))
		return -1;

	/***** pan filter *****/
	if ((f = filter_alloc("Pan", "Positions a mono audio stream in the stereo field", pan_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, "input", "input stream to pan", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, "left output", "left output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, "right output", "right output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (filter_add(f))
		return -1;

	return 0;
}
