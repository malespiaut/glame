/*
 * volume_adjust.c
 * $Id: volume_adjust.c,v 1.3 2000/01/27 10:30:30 richi Exp $
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

	/* get_buffer returns NULL, if there will be no more
	 * data - i.e. NULL is an EOF mark.
	 * the pthread_testcancel is important (do it first to
	 * avoid deadlocks)! */
	while (pthread_testcancel(),
	       (b = fbuf_get(in))) {
		/* we get the input buffer referenced for us by
		 * our source. */

		/* we want to be clever and do read-modify-write
		 * cycles with input/output buffer beeing the
		 * same. so we need to lock the buffer (if there
		 * are more references than our the buffer will
		 * be copied and we dont notice, neither care) */
		work = fbuf_lock(b);

		/* ok, this is not clever - FIXME for clamping! */
		buf = fbuf_buf(work);
		for (i=0; i<fbuf_size(work); i++) {
			*buf = *buf*scale;
			buf++;
		}

		/* we are ready with processing, unlock the buffer
		 * (we still hold a read-only reference!) */
		fbuf_unlock(work);

		/* we need to get a reference for our
		 * destination and then queue the buffer
		 * in the destinations pipe. */
		fbuf_ref(work);
		fbuf_queue(out, work);

		/* now we drop our own reference of the
		 * buffer as we are ready with it now. */
		fbuf_unref(work);
	}

	/* forward the EOF mark */
	fbuf_queue(out, b);

	return 0;
}

