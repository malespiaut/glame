/*
 * debug.c
 * $Id: debug.c,v 1.14 2002/02/17 13:53:31 richi Exp $
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <unistd.h>
#include <stdio.h>
#include "filter.h"
#include "glplugin.h"


PLUGIN_SET(debug, "ping");


/* this is a latency metering filter, pinging short packets
 * through the network, accepting them back and timing the
 * duration.
 * for now, frequency, packet size and packet count is
 * hardcoded.
 * all is very simple and _synchron_(!), so dont lower dt
 * too much. */
static int ping(filter_t *n)
{
	filter_buffer_t *in, *out;
	filter_pipe_t *i, *o;
	struct timeval start, end;
	int cnt, size;
	float dt;
	int time;

	cnt = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "cnt"));
	dt = filterparam_val_double(
		filterparamdb_get_param(filter_paramdb(n), "dt"));
	size = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "size"));

	i = filterport_get_pipe(
		filterportdb_get_port(filter_portdb(n), PORTNAME_IN));
	o = filterport_get_pipe(
		filterportdb_get_port(filter_portdb(n), PORTNAME_OUT));
	if (!i || !o)
		FILTER_ERROR_RETURN("no input or no output");

	FILTER_AFTER_INIT;

	while (cnt>0) {
		FILTER_CHECK_STOP;
		usleep(dt*1000);

		/* create new buffer */
		out = sbuf_alloc(size, n);

		gettimeofday(&start, NULL);

		/* queue buffer */
		sbuf_queue(o, out);

		/* get input buffer (blocks) */
		in = sbuf_get(i);

		gettimeofday(&end, NULL);

		/* free the buffer */
		sbuf_unref(in);
		
		time = ((end.tv_sec - start.tv_sec)*1000000
			+ (end.tv_usec - start.tv_usec));
		fprintf(stderr, "%i - ping time %i usec\n", cnt, time);

		cnt--;
	}

	/* send an EOF */
	sbuf_queue(o, NULL);

	/* wait for EOF passed through */
	in = sbuf_get(i);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}

int ping_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = ping;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_END);

	filterparamdb_add_param_long(filter_paramdb(f), "cnt",
				FILTER_PARAMTYPE_LONG, 10,
				FILTERPARAM_END);
	filterparamdb_add_param_double(filter_paramdb(f), "dt",
				  FILTER_PARAMTYPE_TIME_MS, 250,
				  FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "size",
				FILTER_PARAMTYPE_LONG, 128,
				FILTERPARAM_END);
	plugin_set(p, PLUGIN_CATEGORY, "Analyze");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Catching_Bugs");
	plugin_set(p, PLUGIN_PIXMAP, "ping.png");
	filter_register(f, p);

	return 0;
}
