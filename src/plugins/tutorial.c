/*
 * tutorial.c
 * $Id: tutorial.c,v 1.8 2001/04/11 08:37:28 richi Exp $
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

#define _NO_FILTER_COMPATIBILITY
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


PLUGIN_SET(tutorial, "null dup")


/* null does a "null operation" on one input channel.
 * This feature is also done by the one2n filter if
 * only one output is connected.
 * So this is a filter for educational purpose.
 */
static int null_f(filter_t *n)
{
	filter_port_t *in_port, *out_port;
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;

	in_port = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	out_port = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	in = filterport_get_pipe(in_port);
	out = filterport_get_pipe(out_port);
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

int null_register(plugin_t *p)
{
	filter_t *f;
	if (!(f = filter_creat(NULL)))
		return -1;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_ANY, FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_ANY, FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "output stream",
			      FILTERPORT_END);

	f->f = null_f;

	plugin_set(p, PLUGIN_DESCRIPTION, "does nothing on one input stream");
	plugin_set(p, PLUGIN_PIXMAP, "null.png");
	plugin_set(p, PLUGIN_CATEGORY, "Routing");
	filter_register(f, p);

	return 0;
}


/* dup is simple, it does work with one input and two output channels only.
 * As this functionality is also provided by the one2n filter, this filter
 * is for educational purposes only.
 */
static int dup_f(filter_t *n)
{
	filter_buffer_t *buf;
	filter_port_t *in_port, *out1_port, *out2_port;
	filter_pipe_t *in, *out1, *out2;

	in_port = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	out1_port = filterportdb_get_port(filter_portdb(n), "out1");
	out2_port = filterportdb_get_port(filter_portdb(n), "out2");
	if (!(in = filterport_get_pipe(in_port))
	    || !(out1 = filterport_get_pipe(out1_port))
	    || !(out2 = filterport_get_pipe(out1_port)))
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

int dup_register(plugin_t *p)
{
	filter_t *f;
	if (!(f = filter_creat(NULL)))
		return -1;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_ANY, FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), "out1",
			      FILTER_PORTTYPE_ANY, FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "original stream",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), "out2",
			      FILTER_PORTTYPE_ANY, FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "duplicate stream",
			      FILTERPORT_END);

	f->f = dup_f;

	plugin_set(p, PLUGIN_DESCRIPTION, "duplicates one input stream");
	plugin_set(p, PLUGIN_PIXMAP, "dup.png");
	plugin_set(p, PLUGIN_CATEGORY, "Routing");
	return filter_register(f, p);
}
