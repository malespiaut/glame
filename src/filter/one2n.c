/*
 * one2n.c
 * $Id: one2n.c,v 1.1 2000/01/20 14:54:19 richi Exp $
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


/* another channel routing filter:
 * n - times duplication of one input channel */
int one2n(filter_node_t *n)
{
	filter_buffer_t *in;
	int i;

	/* get_buffer returns NULL, if there will be no more
	 * data - i.e. NULL is an EOF mark.
	 * the pthread_testcancel is important (do it first to
	 * avoid deadlocks)! */
	while (pthread_testcancel(),
	       (in = fbuf_get(n->inputs[0]))) {
		/* we get the input buffer referenced for us by
		 * our source. */

		/* forward the input buffer n times */
		for (i=0; i<n->nr_outputs; i++) {
			/* we need to get a reference for our
			 * destination and then queue the buffer
			 * in the destinations pipe. */
			fbuf_ref(in);
			fbuf_queue(n->outputs[i], in);
		}

		/* now we drop our own reference of the
		 * buffer as we are ready with it now. */
		fbuf_unref(in);
	}

	return 0;
}
