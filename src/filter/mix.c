/*
 * mix.c
 * $Id: mix.c,v 1.1 2000/01/20 14:54:19 richi Exp $
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
#include "filter.h"
#include "util.h"


/* this is slightly more complex, it does work with
 * any number of input channels and one output channel. */
int mix(filter_node_t *n)
{
	filter_buffer_t **in, *out, *lastout;
	int i, eofs, *pos, opos;
	SAMPLE s;

	/* init */
	eofs = 0;
	if (!(in = ALLOCN(n->nr_inputs, filter_buffer_t *))
	    || !(pos = ALLOCN(n->nr_inputs, int))) {
		free(in);
		return -1;
	}

	/* get first input buffers from all channels */
	for (i=0; i<n->nr_inputs; i++) {
		if (!(in[i] = fbuf_get(n->inputs[i])))
			eofs++;
		pos[i] = 0;
	}

	/* get first output buffer - FIXME, check for EOF of in[0] */
	out = fbuf_alloc(fbuf_size(in[0]));
	opos = 0;

	while (pthread_testcancel(), (eofs != n->nr_inputs)) {
		s = 0;
		for (i=0; i<n->nr_inputs; i++) {
			/* check, if we need a new input buffer */
			if (in[i] && (fbuf_size(in[i]) == pos[i])) {
				/* unref the processed buffer */
			        fbuf_unref(in[i]);
				/* get new buffer */
				if (!(in[i] = fbuf_get(n->inputs[i])))
					eofs++;
				pos[i] = 0;
			}
			/* sum the channels - FIXME */
			s += fbuf_buf(in[i])[pos[i]];
			pos[i]++;
		}

		/* check, if we need a new output buffer */
		if (fbuf_size(out) == opos) {
			/* submit the (full) output buffer.
			 * we have already a reference to out (ours! - and
			 * we wont release it */
			fbuf_queue(n->outputs[0], out);
			/* alloc new buffer - FIXME, check for EOFs */
			out = fbuf_alloc(fbuf_size(in[0]));
			opos = 0;
		}

		/* write the sample - FIXME */
		fbuf_buf(out)[opos] = s/(n->nr_inputs-eofs);
		opos++;
	}

	/* submit the last pending output buffer - but truncate
	 * it to the actual necessary length */
	lastout = fbuf_alloc(opos);
	memcpy(fbuf_buf(lastout), fbuf_buf(out), sizeof(SAMPLE)*opos);
	fbuf_unref(out);
	fbuf_queue(n->outputs[0], lastout);

	/* cleanup - FIXME, we need to do this with a
	 * pthread_cleanup_pop() - and previous
	 * pthread_cleanup_push() - ugh! */
	free(in);
	free(pos);

	return 0;
}



