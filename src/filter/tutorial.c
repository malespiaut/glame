/*
 * tutorial.c
 * $Id: tutorial.c,v 1.3 2000/03/14 14:29:26 richi Exp $
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
 * This file collects the source of the filters (not yet) mentioned in the
 * filter tutorial.
 * Contained filters are
 * - dup
 * - null
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"


/* null does a "null operation" on one input channel.
 * This feature is also done by the one2n filter if
 * only one output is connected.
 * So this is a filter for educational purpose.
 */
static int null_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;

	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!in || !out)
		FILTER_ERROR_RETURN("no input or no output");

	FILTER_AFTER_INIT;

	/* The loop condition is at the end to get and
	 * forward the EOF mark. */
	do {
		FILTER_CHECK_STOP;
		/* get an input buffer */
		buf = fbuf_get(in);

		/* just forward every buffer */
		fbuf_queue(out, buf);
	} while (buf);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}


/* dup is simple, it does work with one input and two output channels only.
 * As this functionality is also provided by the one2n filter, this filter
 * is for educational purposes only.
 */
static int dup_f(filter_node_t *n)
{
	filter_buffer_t *buf;
	filter_pipe_t *in, *out1, *out2;

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out1 = filternode_get_output(n, "out1"))
	    || !(out2 = filternode_get_output(n, "out2")))
		FILTER_ERROR_RETURN("insufficient connections");

	FILTER_AFTER_INIT;

	/* get_buffer returns NULL, if there will be no more
	 * data - i.e. NULL is an EOF mark, so we check for
	 * buf == NULL at the end of the loop to correctly
	 * forward the EOF mark. */
	do {
		FILTER_CHECK_STOP;
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
	} while (buf);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}



/* Registry setup of all contained filters
 */
int tutorial_register()
{
	filter_t *f;

	if (!(f = filter_alloc("dup", "duplicates one input stream", dup_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_ANY)
	    || !filter_add_output(f, "out1", "output",
				  FILTER_PORTTYPE_ANY)
	    || !filter_add_output(f, "out2", "output",
				  FILTER_PORTTYPE_ANY)
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("null", "does nothing on one input stream", null_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_ANY)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				  FILTER_PORTTYPE_ANY)
	    || filter_add(f) == -1)
		return -1;

	return 0;
}
