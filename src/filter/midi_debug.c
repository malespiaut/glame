/*
 * midi_debug.c
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
 */

#include <stdio.h>
#include "filter.h"
#include "midi.h"

static int input2mbuf_f (filter_node_t *n)
{
	filter_buffer_t *buf;
	filter_pipe_t *out;
	int a, b, c;
	
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return -1;
	
	FILTER_AFTER_INIT;
	
	do {
		FILTER_CHECK_STOP;
		buf = mbuf_alloc(1, n);
		printf("MIDI Byte (\"status data1 data2\"): ");
		scanf(" %d %d %d", &a, &b, &c);
		MIDI_BYTE_STATUS(mbuf_buf(buf)[0]) = (unsigned char) a;
		MIDI_BYTE_DATA1(mbuf_buf(buf)[0]) = (unsigned char) b;
		MIDI_BYTE_DATA2(mbuf_buf(buf)[0]) = (unsigned char) c;
		MIDI_BYTE_FLAGS(mbuf_buf(buf)[0]) = MIDI_EVENT_FLAG_NOQUEUE;
	} while (1);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	
	return 0;
}

extern int midi_debug_register()
{
	filter_t *f;
	
	if (!(f = filter_alloc("input_to_mbuf", "for debug puposes", input2mbuf_f))
	    || !filter_add_output(f, PORTNAME_OUT, "midi out", FILTER_PORTTYPE_MIDI)
	    || filter_add(f))
		return -1;
	
	return 0;
}