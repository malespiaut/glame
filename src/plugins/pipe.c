/*
 * pipe.c
 * $Id: pipe.c,v 1.1 2000/03/27 12:08:31 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther
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
 * This is a set of "pipe" filters.
 * - pipe-in feeds input from a pipe fed by a subprocess into the
 *   filternetwork
 * - pipe-out [not implemented]
 * - pipe-inout [not implemented]
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


PLUGIN_SET(pipe, "pipe_in")


static int pipe_f(filter_node_t *n)
{
	filter_param_t *param;
	filter_buffer_t *lbuf, *rbuf;
	filter_pipe_t *lout, *rout;
	SAMPLE *ls, *rs;
	short *b, *bb;
	FILE *p;
	char cmd[256];
	int res, q;
	
	if (!(lout = filternode_get_output(n, PORTNAME_OUT))
	    || !(param = filternode_get_param(n, "cmd")))
		FILTER_ERROR_RETURN("insufficient configuration");
	rout = filternode_next_output(lout);

	q = 2;
	if (rout)
		q = 4;

	strncpy(cmd, filterparam_val_string(param), 255);
	if ((param = filternode_get_param(n, "tail")))
		strncat(cmd, filterparam_val_string(param), 255);

	if (!(p = popen(cmd, "r")))
		FILTER_ERROR_RETURN("popen failed");
	b = malloc(q*4096);

	FILTER_AFTER_INIT;

	while ((res = fread(b, q, 4096, p)) > 0) {
		FILTER_CHECK_STOP;

		lbuf = sbuf_make_private(sbuf_alloc(res, n));
		ls = sbuf_buf(lbuf);
		if (rout) {
			rbuf = sbuf_make_private(sbuf_alloc(res, n));
			rs = sbuf_buf(rbuf);
		}
		bb = b;
		do {
			*(ls++) = SHORT2SAMPLE(*(bb++));
			if (rout)
				*(rs++) = SHORT2SAMPLE(*(bb++));
			res--;
		} while (res > 0);

		sbuf_queue(lout, lbuf);
		if (rout)
			sbuf_queue(rout, rbuf);
	}
	sbuf_queue(lout, NULL);
	if (rout)
		sbuf_queue(rout, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	pclose(p);
	free(b);

	FILTER_RETURN;
}

static int pipe_connect_out(filter_node_t *source, const char *port,
			    filter_pipe_t *p)
{
	filter_param_t *param;
	int rate;

	if (filternode_nroutputs(source) > 1)
		return -1;

	rate = 44100;
	if ((param = filternode_get_param(source, "rate")))
		rate = filterparam_val_int(param);

	if (filternode_nroutputs(source) == 0) {
		filterpipe_settype_sample(p, rate, FILTER_PIPEPOS_DEFAULT);
	} else {
		filterpipe_settype_sample(p, rate, FILTER_PIPEPOS_RIGHT);
		p = filternode_get_output(source, PORTNAME_OUT);
		filterpipe_settype_sample(p, rate, FILTER_PIPEPOS_LEFT);
	}

	return 0;
}

PLUGIN_DESCRIPTION(pipe_in, "pipe input")
PLUGIN_PIXMAP(pipe_in, "default.png")
int pipe_in_register()
{
	filter_t *f;
	filter_paramdesc_t *d;

	if (!(f = filter_alloc(pipe_f))
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				  FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC)
	    || !filter_add_param(f, "cmd", "command string",
				 FILTER_PARAMTYPE_STRING)
	    || !(d = filter_add_param(f, "tail", "command string tail",
				      FILTER_PARAMTYPE_STRING))
	    || !filter_add_param(f, "rate", "data rate",
				 FILTER_PARAMTYPE_INT))
		return -1;
	filterparamdesc_string_settype(d, FILTER_PARAM_STRINGTYPE_FILENAME);
	f->connect_out = pipe_connect_out;
	if (filter_add(f, "pipe-in", "pipe input") == -1)
		return -1;

	return 0;
}
