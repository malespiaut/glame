/*
 * volume_adjust.c
 * $Id: volume_adjust.c,v 1.2 2000/01/24 10:22:52 richi Exp $
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
	filter_buffer_t *in, *work;
	SAMPLE *buf;
	int i;

	/* get_buffer returns NULL, if there will be no more
	 * data - i.e. NULL is an EOF mark.
	 * the pthread_testcancel is important (do it first to
	 * avoid deadlocks)! */
	while (pthread_testcancel(),
	       (in = fbuf_get(n->inputs[0]))) {
		/* we get the input buffer referenced for us by
		 * our source. */

		/* we want to be clever and do read-modify-write
		 * cycles with input/output buffer beeing the
		 * same. so we need to lock the buffer (if there
		 * are more references than our the buffer will
		 * be copied and we dont notice, neither care) */
		work = fbuf_lock(in);

		/* ok, this is not clever - FIXME for clamping! */
		buf = fbuf_buf(work);
		for (i=0; i<fbuf_size(work); i++) {
			*buf = *buf*n->params[0].f;
			buf++;
		}

		/* we are ready with processing, unlock the buffer
		 * (we still hold a read-only reference!) */
		fbuf_unlock(work);

		/* we need to get a reference for our
		 * destination and then queue the buffer
		 * in the destinations pipe. */
		fbuf_ref(work);
		fbuf_queue(n->outputs[0], work);

		/* now we drop our own reference of the
		 * buffer as we are ready with it now. */
		fbuf_unref(work);
	}

	/* forward the EOF mark */
	fbuf_queue(n->outputs[0], in);

	return 0;
}

