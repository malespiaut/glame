/*
 * null.c
 * $Id: null.c,v 1.1 2000/01/20 14:54:19 richi Exp $
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

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "filter.h"
#include "util.h"


/* this does a "null operation" on any number of input channels
 * routing them to the appropriate output channel. it does
 * this asynchronly, though (so its a nice example of multiple
 * asynchronous inputs and outputs).
 */
int null(filter_node_t *n)
{
	filter_buffer_t *in;
	int active_channels, *active, i, maxfd;
	fd_set channels;

	if (n->nr_inputs != n->nr_outputs)
		return -1;

	active_channels = n->nr_inputs;
	if (!(active = ALLOCN(n->nr_inputs, int)))
		return -1;
	for (i=0; i<n->nr_inputs; i++)
		if (n->inputs[i] && n->outputs[i])
			active[i] = 1;
		else
			active[i] = 0;

	while (pthread_testcancel(), active_channels>0) {
		/* wait for pipe activity */
		FD_ZERO(&channels);
		maxfd = 0;
		for (i=0; i<n->nr_inputs; i++)
			if (active[i]) {
				FD_SET(n->inputs[i]->dest_fd, &channels);
				if (n->inputs[i]->dest_fd > maxfd)
					maxfd = n->inputs[i]->dest_fd;
			}
		if (select(maxfd+1, &channels, NULL, NULL, NULL) <= 0)
			continue;

		/* just forward all pending buffers */
		for (i=0; i<n->nr_inputs; i++) {
			if (!active[i]
			    || !FD_ISSET(n->inputs[i]->dest_fd, &channels))
				continue;
			/* get buffer and forward it also EOFs!*/
			in = fbuf_get(n->inputs[i]);
			fbuf_queue(n->outputs[i], in);
			if (!in) {
				active[i] = 0;
				active_channels--;
			}
		}
	}

	free(active);

	return 0;
}


