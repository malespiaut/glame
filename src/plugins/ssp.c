/*
 * ssp.c
 * $Id: ssp.c,v 1.4 2001/05/16 00:25:37 mag Exp $
 *
 * Copyright (C) 2001 Alexander Ehlert
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

#include <config.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "math.h"
#include "gpsm.h"

PLUGIN_SET(ssp, "ssp_streamer maxrms")

static int ssp_streamer_connect_out(filter_t *n, filter_port_t *port,
				   filter_pipe_t *p)
{
	filterpipe_settype_ssp(p, 44100, 64);
	return 0;
}

static int ssp_streamer_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf, *obuf;
	filter_param_t *param;
	
	SAMPLE  *ringbuf, *s, *ssp, accu, fak, ss;
	int	bsize, pos, cnt;
	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	
	param = filternode_get_param(n,"bsize");
	bsize = filterparam_val_int(param);
	if (bsize < 1)
		FILTER_ERROR_RETURN("bsize <= 0");
	
	DPRINTF("bsize = %d\n", bsize);
	fak = 1.0/(SAMPLE)bsize;
	accu = 0.0;
	ringbuf = ALLOCN(bsize, SAMPLE);
	if (ringbuf == NULL)
		FILTER_ERROR_RETURN("Error allocating ringbuf\n");
	pos = 0;

	FILTER_AFTER_INIT;
	
	goto entry;
	while (buf) {
		FILTER_CHECK_STOP;
		
		/* got an input buffer */
		s = sbuf_buf(buf);
		cnt = sbuf_size(buf);
		obuf = ssp_make_private(ssp_alloc(cnt, n));
		ssp = ssp_buf(obuf);
		
		while (cnt--) {
			ss = *s * *s;
			accu += fak * (ss - ringbuf[pos]);
			ringbuf[pos++] = ss;
			if (pos == bsize)
				pos = 0;
			s++;
			*(ssp++) = accu;
		}

		sbuf_unref(buf);
		ssp_queue(out, obuf);
entry:
		buf = sbuf_make_private(sbuf_get(in));
	};
	
	ssp_queue(out, NULL);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	free(ringbuf);

	FILTER_RETURN;
}

int ssp_streamer_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdb_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "audio stream in", FILTER_PORTTYPE_SAMPLE);
	filter_add_output(f, PORTNAME_OUT, "ssp stream out", FILTER_PORTTYPE_SSP);
	
	f->f = ssp_streamer_f;
	f->connect_out = ssp_streamer_connect_out;
	
	param = filter_paramdb(f);

	filterparamdb_add_param_int(param, "bsize", FILTER_PARAMTYPE_INT, 64 ,
				    FILTERPARAM_DESCRIPTION, "length of running average",
				    FILTERPARAM_END);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "ssp_streamer");
	plugin_set(p, PLUGIN_PIXMAP, "ssp.png"); 
	plugin_set(p, PLUGIN_CATEGORY, "Analyze");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Analyze");

	return filter_register(f, p);
}

static int maxrms_f(filter_t *n)
{
	filter_pipe_t	*in;
	filter_buffer_t	*buf;
	filter_param_t	*param;
	
	SAMPLE	maxrms = 0.0, *s;
	int	cnt;
	
	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	
	param = filternode_get_param(n, "maxrms");

	filterparam_set(param, &maxrms);

	DPRINTF("crash vorher?\n");
	
	FILTER_AFTER_INIT;
	
	goto entry;
	while (buf) {
		FILTER_CHECK_STOP;
		
		/* got an input buffer */
		s = sbuf_buf(buf);
		cnt = sbuf_size(buf);
		
		while (cnt--) {
			if (*s > maxrms)
				maxrms = *s;
			s++;
		}

		sbuf_unref(buf);
entry:
		buf = sbuf_make_private(sbuf_get(in));
	};

	maxrms = sqrtf(maxrms);

	filterparam_set(param, &maxrms);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int maxrms_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdb_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "ssp stream in", FILTER_PORTTYPE_SSP);
	
	f->f = maxrms_f;
	
	param = filter_paramdb(f);

	filterparamdb_add_param_float(param, "maxrms", FILTER_PARAMTYPE_FLOAT, 0.0 ,
				    FILTERPARAM_DESCRIPTION, "maximum rms",
				    FILTERPARAM_END);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "maxrms");
	plugin_set(p, PLUGIN_PIXMAP, "maxrms.png"); 
	plugin_set(p, PLUGIN_CATEGORY, "Analyze");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Analyze");

	return filter_register(f, p);
}

