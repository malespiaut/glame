/*
 * tutorial.c
 * $Id: tutorial.c,v 1.14 2002/02/17 13:53:31 richi Exp $
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
 * - dup (filter)
 * - null (filter)
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <guile/gh.h>
#include "filter.h"
#include "util.h"
#include "gpsm.h"
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
	filter_param_t *pos, *level;
	filter_buffer_t *buf;

	in_port = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	out_port = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	in = filterport_get_pipe(in_port);
	out = filterport_get_pipe(out_port);
	if (!in || !out)
		FILTER_ERROR_RETURN("no input or no output");

	pos = filterparamdb_get_param(
		filter_paramdb(n), FILTERPARAM_LABEL_POS);
	filterparam_val_set_pos(pos, 0);
	level = filterparamdb_get_param(filter_paramdb(n), "level");
	filterparam_val_double(level) = 0.0;

	FILTER_AFTER_INIT;

	/* The loop condition is at the end to get and
	 * forward the EOF mark. */
	do {
		FILTER_CHECK_STOP;
		/* get an input buffer */
		buf = fbuf_get(in);
		if (!buf)
			goto skip;

		/* correct stream position for GUI poll */
		if (filterpipe_type(in) == FILTER_PIPETYPE_SAMPLE)
			filterparam_val_set_pos(
				pos, filterparam_val_long(pos) + sbuf_size(buf));
		else
			filterparam_val_set_pos(
				pos, filterparam_val_long(pos) + fbuf_size(buf));

		/* correct current level (if SAMPLE stream) for GUI poll */
		if (filterpipe_type(in) == FILTER_PIPETYPE_SAMPLE) {
			SAMPLE *s = sbuf_buf(buf);
			int cnt = sbuf_size(buf);
			float value = 0.0;
			while (cnt--) {
				value += *s * *s;
				s++;
			}
			filterparam_val_double(level) = sqrtf(value/sbuf_size(buf));
		}

skip:
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
	filterparamdb_add_param_pos(filter_paramdb(f));
	filterparamdb_add_param_double(filter_paramdb(f), "level",
				       FILTER_PARAMTYPE_DOUBLE, 0.0,
				       FILTERPARAM_END);

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

