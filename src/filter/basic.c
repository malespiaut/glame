/*
 * basic.c
 * $Id: basic.c,v 1.12 2000/02/20 15:25:55 richi Exp $
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
 *
 * This file contains basic filters which do not depend on the actual
 * used protocol. Contained are
 * - drop
 * - one2n
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"


/* drop is a wastebucket for stream data. it does throw away
 * any number of input channels, producing nothing. it does
 * this asynchronly, though (so its a nice example of multiple
 * asynchronous inputs).
 */
static int drop_f(filter_node_t *n)
{
	filter_buffer_t *in;
	filter_pipe_t **inputs, *p;
	int active_channels, i, maxfd;
	fd_set channels;

	if (!(inputs = ALLOCN(filternode_nrinputs(n), filter_pipe_t *)))
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

		/* just unref all pending buffers */
		for (i=0; i<n->nr_inputs; i++)
			if (inputs[i]
			    && FD_ISSET(inputs[i]->dest_fd, &channels)) {
				if (!(in = fbuf_get(inputs[i]))) {
					inputs[i] = NULL;
					active_channels--;
				}
				fbuf_unref(in);
			}
	}

	FILTER_BEFORE_CLEANUP;

	free(inputs);

	return 0;
}


/* one2n is another channel routing filter:
 * n - times duplication of one input channel */
static int one2n_f(filter_node_t *n)
{
	filter_buffer_t *buf;
	filter_pipe_t *in, *out;

	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		return -1;

	FILTER_AFTER_INIT;

	/* get_buffer returns NULL, if there will be no more
	 * data - i.e. NULL is an EOF mark.
	 * the pthread_testcancel is important (do it first to
	 * avoid deadlocks)! */
	do {
		/* we get the input buffer referenced for us by
		 * our source. */
	        buf = fbuf_get(in);

		/* forward the input buffer n times */
		filternode_foreach_output(n, out) {
			/* we need to get a reference for our
			 * destination and then queue the buffer
			 * in the destinations pipe. */
			fbuf_ref(buf);
			fbuf_queue(out, buf);
		}

		/* now we drop our own reference of the
		 * buffer as we are ready with it now. */
		fbuf_unref(buf);
	} while (pthread_testcancel(), buf);


	FILTER_BEFORE_CLEANUP;

	return 0;
}



/* Registry setup of all contained filters
 */
int basic_register()
{
	filter_t *f;

	if (!(f = filter_alloc("drop", "drops n streams", drop_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_ANY)
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("one2n", "replicates one input n times", one2n_f))
	    || !filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_ANY)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				  FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_ANY)
	    || filter_add(f) == -1)
		return -1;

	return 0;
}
