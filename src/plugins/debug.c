/*
 * debug.c
 * $Id: debug.c,v 1.2 2000/03/20 09:51:53 richi Exp $
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
#include <unistd.h>
#include <stdio.h>
#include "filter.h"
#include "glplugin.h"


PLUGIN_DESCRIPTION(debug, "filters for debugging purposes (ping)");
PLUGIN_SET(debug, "ping");


/* this is a latency metering filter, pinging short packets
 * through the network, accepting them back and timing the
 * duration.
 * for now, frequency, packet size and packet count is
 * hardcoded.
 * all is very simple and _synchron_(!), so dont lower dt
 * too much. */
static int ping(filter_node_t *n)
{
	filter_buffer_t *in, *out;
	filter_pipe_t *i, *o;
	filter_param_t *param;
	struct timeval start, end;
	int cnt = 10;
	int dt = 250000;
	int size = 128;
	int time;

	if ((param = filternode_get_param(n, "cnt")))
		cnt = filterparam_val_int(param);
	if ((param = filternode_get_param(n, "dt")))
		dt = filterparam_val_int(param);
	if ((param = filternode_get_param(n, "size")))
		size = filterparam_val_int(param);

	i = filternode_get_input(n, PORTNAME_IN);
	o = filternode_get_output(n, PORTNAME_OUT);
	if (!i || !o)
		FILTER_ERROR_RETURN("no input or no output");

	FILTER_AFTER_INIT;

	while (cnt>0) {
		FILTER_CHECK_STOP;
		usleep(dt);

		/* create new buffer */
		out = fbuf_alloc(size, &n->net->nodes);

		gettimeofday(&start, NULL);

		/* queue buffer */
		fbuf_queue(o, out);

		/* get input buffer (blocks) */
		in = fbuf_get(i);

		gettimeofday(&end, NULL);

		/* free the buffer */
		fbuf_unref(in);
		
		time = ((end.tv_sec - start.tv_sec)*1000000
			+ (end.tv_usec - start.tv_usec));
		fprintf(stderr, "%i - ping time %i usec\n", cnt, time);

		cnt--;
	}

	/* send an EOF */
	fbuf_queue(o, NULL);

	/* wait for EOF passed through */
	in = fbuf_get(i);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}

PLUGIN_DESCRIPTION(ping, "ping filter to measure latencies");
PLUGIN_PIXMAP(ping, "debug.png");
int ping_register()
{
	filter_t *f;

	if (!(f = filter_alloc(ping))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_MISC)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				  FILTER_PORTTYPE_MISC)
	    || !filter_add_param(f, "cnt", "count", FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f, "dt", "delay time", FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f, "size", "buffer size", FILTER_PARAMTYPE_INT)
	    || filter_add(f, "ping", "ping") == -1)
		return -1;
	return 0;
}
