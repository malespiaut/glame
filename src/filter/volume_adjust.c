/*
 * volume_adjust.c
 * $Id: volume_adjust.c,v 1.6 2000/02/07 10:32:05 richi Exp $
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

#include "filter.h"


/* this is simple, it does work with one channel
 * only. */
int volume_adjust(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *b, *work;
	filter_param_t *scaleparam;
	float scale;
	SAMPLE *buf;
	int i;

	if (!(in = hash_find_input("in", n))
	    || !(out = hash_find_output("out", n)))
		return -1;
	if ((scaleparam = hash_find_param("factor", n)))
		scale = scaleparam->val.f;
	else
		scale = 1.0;

	FILTER_AFTER_INIT;

	/* get_buffer returns NULL, if there will be no more
	 * data - i.e. NULL is an EOF mark.
	 * the pthread_testcancel is important (do it first to
	 * avoid deadlocks)! */
	while (pthread_testcancel(),
	       (b = sbuf_get(in))) {
		/* we get the input buffer referenced for us by
		 * our source. */

		/* we want to be clever and do read-modify-write
		 * cycles with input/output buffer beeing the
		 * same. so we need to lock the buffer (if there
		 * are more references than our the buffer will
		 * be copied and we dont notice, neither care) */
		work = sbuf_make_private(b);

		/* ok, this is not clever - FIXME for clamping! */
		buf = sbuf_buf(work);
		for (i=0; i<sbuf_size(work); i++) {
			*buf = *buf*scale;
			buf++;
		}

		/* we need to get a reference for our
		 * destination and then queue the buffer
		 * in the destinations pipe. */
		sbuf_queue(out, work);
	}

	/* forward the EOF mark */
	sbuf_queue(out, b);

	FILTER_BEFORE_CLEANUP;

	return 0;
}

