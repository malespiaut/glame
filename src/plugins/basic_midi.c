/*
 * basic_midi.c
 *
 * Copyright (C) 2000 Jim Garrison, Richard Guenther
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

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include "filter.h"

static int midi_mix_f (filter_node_t *n)
{
	filter_buffer_t *buf;
	filter_pipe_t **inputs, *p, *output;
	int active_channels, i, maxfd;
	fd_set channels;

	if (!(inputs = ALLOCN(filternode_nrinputs(n), filter_pipe_t *)))
		return -1;
	if (!(output = filternode_get_output(n, PORTNAME_OUT)))
		return -1;

	/* put all input connections into an easy accessable
	 * array - we can use it for active connection
	 * tracking, too. */
	active_channels = 0;
	filternode_foreach_input(n, p)
		inputs[active_channels++] = p;


	FILTER_AFTER_INIT;

	while (pthread_testcancel(), active_channels>0) {
		/* wait for pipe activity */
		FD_ZERO(&channels);
		maxfd = 0;
		for (i=0; i<n->nr_inputs; i++)
			if (inputs[i]) {
				FD_SET(inputs[i]->dest_fd, &channels);
				if (inputs[i]->dest_fd > maxfd)
					maxfd = inputs[i]->dest_fd;
			}
		if (select(maxfd+1, &channels, NULL, NULL, NULL) <= 0)
			continue;

		/* just forward all pending buffers */
		for (i=0; i<n->nr_inputs; i++)
			if (inputs[i]
			    && FD_ISSET(inputs[i]->dest_fd, &channels)) {
				if (!(buf = mbuf_get(inputs[i]))) {
					inputs[i] = NULL;
					active_channels--;
				}
				mbuf_queue(output, buf);
			}
	}

	FILTER_BEFORE_CLEANUP;

	free(inputs);

	return 0;
}

extern int basic_midi_register()
{
	filter_t *f;

	if (!(f = filter_alloc(midi_mix_f))
	    || !filter_add_input(f, PORTNAME_IN, "midi in",
				FILTER_PORTTYPE_MIDI|FILTER_PORTTYPE_AUTOMATIC)
	    || !filter_add_output(f, PORTNAME_OUT, "midi out",
				FILTER_PORTTYPE_MIDI)
	    || filter_add(f, "midi_mix", "mixes midi pipes"))
		return -1;

	return 0;
}
