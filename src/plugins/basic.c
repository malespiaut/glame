/*
 * basic.c
 * $Id: basic.c,v 1.4 2000/03/21 09:37:17 richi Exp $
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
#include "glplugin.h"


PLUGIN_DESCRIPTION(basic, "protocol independend fbuf filters (drop, one2n)")
PLUGIN_SET(basic, "drop one2n")


/* Drop is a wastebucket for stream data. it does throw away
 * any number of input channels, producing nothing. It does
 * this asynchronly to avoid deadlocks due to blocking reads
 * and writes (so its a nice example of multiple asynchronous
 * inputs - but still rather simple).
 */
static int drop_f(filter_node_t *n)
{
	filter_buffer_t *in;
	filter_pipe_t **inputs, *p;
	int active_channels, i, maxfd;
	fd_set channels;

	if (!(inputs = ALLOCN(filternode_nrinputs(n), filter_pipe_t *)))
		FILTER_ERROR_RETURN("no memory");

	/* put all input connections into an easy accessable
	 * array - we can use it for active connection
	 * tracking, too. */
	active_channels = 0;
	filternode_foreach_input(n, p)
		inputs[active_channels++] = p;


	FILTER_AFTER_INIT;

	while (active_channels>0) {
	        FILTER_CHECK_STOP;

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

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(inputs);

	FILTER_RETURN;
}

PLUGIN_DESCRIPTION(drop, "throw away all data")
PLUGIN_PIXMAP(drop, "dumpster.xpm")
int drop_register()
{
	filter_t *f;

	if (!(f = filter_alloc(drop_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_ANY)
	    || filter_add(f, "drop", "drops n streams") == -1)
		return -1;
	return 0;
}





/* one2n is another channel routing filter, it does n - times duplication
 * of one input channel. To avoid deadlocks, this has again be done totally
 * asynchron, both from the input and the output ends. So inbetween buffering
 * in a queue is done per output. */
static int one2n_f(filter_node_t *n)
{
	typedef struct {
		filter_pipe_t *out;
		feedback_fifo_t fifo;
	} one2n_param_t;
	filter_buffer_t *buf;
	filter_pipe_t *in, *out;
	one2n_param_t *p;
	int maxfd, i, nr, eof, empty, res;
	fd_set rset, wset;

	nr = filternode_nroutputs(n);
	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	if (!(p = ALLOCN(nr, one2n_param_t)))
	        FILTER_ERROR_RETURN("no memory");

	/* init struct */
	i = 0;
	filternode_foreach_output(n, out) {
		INIT_FEEDBACK_FIFO(p[i].fifo);
		p[i++].out = out;
	}

	FILTER_AFTER_INIT;

	/* In the following loop you may miss the EOF send - but its
	 * there, EOFs just get queued like regular buffers. Loop termination
	 * is at input EOF and all send queues empty time.
	 * - FIXME - do read throttling via max(bytes in a queue) and not
	 *   adding the in fd to the rset.
	 */
	eof = 0;
	do {
	        FILTER_CHECK_STOP;

		/* set up fd sets for select */
		maxfd = 0;
		FD_ZERO(&rset);
		if (!eof) {
			FD_SET(in->dest_fd, &rset);
			maxfd = in->dest_fd;
		}
		empty = 1;
		FD_ZERO(&wset);
		for (i=0; i<nr; i++) {
			if (!has_feedback(&p[i].fifo))
				continue;
			empty = 0;
			FD_SET(p[i].out->source_fd, &wset);
			if (p[i].out->source_fd > maxfd)
				maxfd = p[i].out->source_fd;
		}
		if (eof && empty)
			break;
		res = select(maxfd+1, &rset, empty ? NULL : &wset, NULL, NULL);
		if (res == -1)
			perror("select");
		if (res <= 0)
			continue;

		/* do we have input? - queue in each feedback buffer,
		 * be clever with the references, too. */
		if (FD_ISSET(in->dest_fd, &rset)) {
			buf = fbuf_get(in);
			for (i=0; i<nr-1; i++) {
				fbuf_ref(buf);
				add_feedback(&p[i].fifo, buf);
			}
			if (nr >= 1)
				add_feedback(&p[nr-1].fifo, buf);
			else
				fbuf_unref(buf);
		}

		/* foreach output check, if we are ready to queue
		 * a buffer. */
		for (i=0; i<nr; i++) {
			if (!has_feedback(&p[i].fifo)
			    || !FD_ISSET(p[i].out->source_fd, &wset))
				continue;
			fbuf_queue(p[i].out, get_feedback(&p[i].fifo));
		}
	} while (1);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

PLUGIN_DESCRIPTION(one2n, "replicate stream")
PLUGIN_PIXMAP(one2n, "default.png")
int one2n_register()
{
	filter_t *f;

	if (!(f = filter_alloc(one2n_f))
	    || !filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_ANY)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				  FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_ANY)
	    || filter_add(f, "one2n", "replicates one input n times") == -1)
		return -1;
	return 0;
}
