/*
 * dup.c
 * $Id: dup.c,v 1.3 2000/01/27 10:30:30 richi Exp $
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


/* This is simple, it does work with one input and two output channels only.
 * As this functionality is also provided by the one2n filter, this filter
 * is for educational purposes only.
 */
int dup(filter_node_t *n)
{
	filter_buffer_t *buf;
	filter_pipe_t *in, *out1, *out2;

	if (!(in = hash_find_input("in", n))
	    || !(out1 = hash_find_output("out1", n))
	    || !(out2 = hash_find_output("out2", n)))
		return -1;

	/* get_buffer returns NULL, if there will be no more
	 * data - i.e. NULL is an EOF mark, so we check for
	 * buf == NULL at the end of the loop to correctly
	 * forward the EOF mark. */
	do {
		buf = fbuf_get(in);
		/* we get the input buffer referenced for us by
		 * our source. */

		/* we need to get a reference for our first
		 * destination and then queue the buffer
		 * in the destinations pipe. */
		fbuf_ref(buf);
		fbuf_queue(out1, buf);

		/* we dont need to get a reference for our second
		 * destination - ours is good enough, we just are
		 * not allowed to muck with it anymore. */
		fbuf_queue(out2, buf);
	} while (pthread_testcancel(), buf);

	return 0;
}

