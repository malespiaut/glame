/*
 * midi_basic.c
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

static int midi_mix_f (filter_node_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;

	FILTER_AFTER_INIT;

	/* FIXME */

	FILTER_BEFORE_CLEANUP;

	return 0;
}

extern int midi_basic_register()
{
	filter_t *f;

	if (!(f = filter_alloc("midi_mix", "mixes midi pipes", midi_mix_f))
	    || !filter_add_input(f, PORTNAME_IN, "midi in",
				FILTER_PORTTYPE_MIDI|FILTER_PORTTYPE_AUTOMATIC)
	    || !filter_add_output(f, PORTNAME_OUT, "midi out",
				FILTER_PORTTYPE_MIDI)
	    || !filter_add(f))
		return -1;

	return 0;
}
