/*
 * basic.c
 * $Id: basic.c,v 1.11 2000/02/17 16:16:07 richi Exp $
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
 * This file contains a set of filters which is the basis for
 * constructing networks. All filters are generic with respect
 * to their port types.
 * Contained filters are
 * - drop
 * - one2n
 * - mix
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




/* this is slightly more complex, it does work with
 * any number of input channels and one output channel. 
 * FIXME This mixer should have a gain value per channel,
 * but at this stage I don't know, how to easily add a parameter
 * per channel
 */

static int mix_f(filter_node_t *n)
{
	filter_pipe_t  *p, **pin=NULL, *pout;
	filter_buffer_t **in=NULL, *out, *lastout;
	int i, eofs, *pos=NULL, opos, res=-1, rate;
	SAMPLE s;

	/* We require at least one connected input and
	 * a connected output. Also all input pipes have
	 * to match in the sample rate!
	 */
	if (filternode_nrinputs(n) == 0)
		return -1;
	if (!(pout = filternode_get_output(n, PORTNAME_OUT)))
		return -1;
	rate = filterpipe_sample_rate(filternode_get_input(n, PORTNAME_IN));
	filternode_foreach_input(n, p)
		if (filterpipe_sample_rate(p) != rate)
			return -1;

	/* init */
	eofs = 0;
	if (!(in = ALLOCN(filternode_nrinputs(n), filter_buffer_t *)))
			return -1;
	if (!(pos = ALLOCN(filternode_nrinputs(n), int)))
		goto _cleanup;
	if (!(pin = ALLOCN(filternode_nrinputs(n), filter_pipe_t *)))
		goto _cleanup;

	FILTER_AFTER_INIT;
	
	/* get first input buffers from all channels */
	i=0;
	filternode_foreach_input(n, p) {
		pin[i] = p;
		if (!(in[i] = sbuf_get(pin[i])))
			eofs++;
		pos[i] = 0;
		i++;
	}

	/* get first output buffer */
	out = sbuf_alloc(GLAME_WBUFSIZE, n);
	opos = 0;

	while (pthread_testcancel(), (eofs != n->nr_inputs)) {
		s = 0;
		for (i=0; i<n->nr_inputs; i++) {
			/* check, if we need a new input buffer */
			if (in[i] && (sbuf_size(in[i]) == pos[i])) {
				/* unref the processed buffer */
			        sbuf_unref(in[i]);
				/* get new buffer */
				if (!(in[i] = sbuf_get(pin[i])))
					eofs++;
				pos[i] = 0;
			}
			/* sum the channels */
			if(in[i]){
				s += sbuf_buf(in[i])[pos[i]];
				pos[i]++;
			}
		}

		/* check, if we need a new output buffer */
		if (sbuf_size(out) == opos) {
			/* submit the (full) output buffer.
			 * we have already a reference to out (ours! - and
			 * we wont release it */
			sbuf_queue(pout, out);
			/* alloc new buffer */
			out = sbuf_alloc(GLAME_WBUFSIZE, n);
			opos = 0;
		}

		/* write the sample  */
		sbuf_buf(out)[opos++] = s/(n->nr_inputs);
	}

	/* submit the last pending output buffer - but truncate
	 * it to the actual necessary length */
	lastout = sbuf_alloc(opos, n);
	memcpy(sbuf_buf(lastout), sbuf_buf(out), sizeof(SAMPLE)*opos);
	sbuf_unref(out);
	sbuf_queue(pout, lastout);

	FILTER_BEFORE_CLEANUP;
	/* cleanup - FIXME, we need to do this with a
	 * pthread_cleanup_pop() - and previous
	 * pthread_cleanup_push() - ugh! */
	res=0;
_cleanup:
	if (in) free(in);
	if (pos) free(pos);
        if (pin) free(pin);

	return res;
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

        if (!(f = filter_alloc("mix", "mix n channels", mix_f))
            || !filter_add_input(f, PORTNAME_IN, "input stream",
                                FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE) 
            || !filter_add_output(f, PORTNAME_OUT, "mixed stream",
                                FILTER_PORTTYPE_SAMPLE)
            || filter_add(f) == -1)
                return -1;

	return 0;
}
