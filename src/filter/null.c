/*
 * null.c
 * $Id: null.c,v 1.4 2000/01/27 14:28:53 richi Exp $
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


/* This does a "null operation" on one input channel.
 * This feature is also done by the one2n filter if
 * only one output is connected.
 * So this is a filter for educational purpose.
 */
int null(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;

	in = hash_find_input("in", n);
	out = hash_find_output("out", n);
	if (!in || !out)
		return -1;

	/* The loop condition is at the end to get and
	 * forward the EOF mark. */
	do {
		/* get an input buffer */
		buf = fbuf_get(in);

		/* just forward every buffer */
		fbuf_queue(out, buf);
	} while (pthread_testcancel(), buf);

	return 0;
}


