/*
 * ssp.c
 * $Id: ssp.c,v 1.16 2004/10/23 13:09:29 richi Exp $
 *
 * Copyright (C) 2001, 2002 Alexander Ehlert
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

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "math.h"
#include "gpsm.h"

PLUGIN_SET(ssp, "ssp_streamer maxrms")


static void ssp_streamer_fixup_pipe(glsig_handler_t *h, long sig, va_list va)
{
	filter_pipe_t *in, *out;

	GLSIGH_GETARGS1(va, in);
	out = filterport_get_pipe(filterportdb_get_port(filter_portdb(filterport_filter(filterpipe_dest(in))), PORTNAME_OUT));
	if (!out)
		return;
	filterpipe_settype_ssp(out, filterpipe_sample_rate(in), filterpipe_ssp_bsize(out));
	glsig_emit(filterpipe_emitter(out), GLSIG_PIPE_CHANGED, out);
}

static int ssp_streamer_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	if (filterport_get_pipe(port))
		return -1;
	glsig_add_handler(filterpipe_emitter(p), GLSIG_PIPE_CHANGED,
			  ssp_streamer_fixup_pipe, NULL);
	return 0;
}

static int ssp_streamer_connect_out(filter_port_t *port, filter_pipe_t *p)
{
	filter_pipe_t *in;
	int rate = GLAME_DEFAULT_SAMPLERATE;
	int bsize;

	if (filterport_get_pipe(port))
		return -1;

	in = filterport_get_pipe(filterportdb_get_port(filter_portdb(filterport_filter(port)), PORTNAME_IN));
	if (in)
		rate = filterpipe_sample_rate(in);
	bsize = filterparam_val_long(filterparamdb_get_param(filter_paramdb(filterport_filter(port)), "bsize"));
	filterpipe_settype_ssp(p, rate, bsize);

	return 0;
}

static int ssp_streamer_bsize_set(filter_param_t *param, const void *val)
{
	filter_pipe_t *out;

	if (*(long *)val <= 0)
		return -1;

	if ((out = filterport_get_pipe(filterportdb_get_port(filter_portdb(filterparam_filter(param)), PORTNAME_OUT)))) {
		filterpipe_settype_ssp(out, filterpipe_ssp_rate(out),
				       *(long *)val);
		glsig_emit(filterpipe_emitter(out), GLSIG_PIPE_CHANGED, out);
	}
       	return 0;
}

static int ssp_streamer_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf, *obuf;
	filter_param_t *param;
	
	SAMPLE  *ringbuf, *s, *ssp, accu, fak, ss;
	int	bsize, pos, cnt;
	in = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN));
	out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT));
	
	param = filterparamdb_get_param(filter_paramdb(n), "bsize");
	bsize = filterparam_val_long(param);
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
			*ssp++ = accu;
		}

		sbuf_unref(buf);
		ssp_queue(out, obuf);
entry:
		buf = sbuf_get(in);
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
	filter_port_t *out, *in;
	filter_param_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = ssp_streamer_f;

	in = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				   FILTER_PORTTYPE_SAMPLE,
				   FILTER_PORTFLAG_INPUT,
				   FILTERPORT_DESCRIPTION, "audio stream in",
				   FILTERPORT_END);
	in->connect = ssp_streamer_connect_in;
	out = filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
				    FILTER_PORTTYPE_SSP,
				    FILTER_PORTFLAG_OUTPUT,
				    FILTERPORT_DESCRIPTION, "ssp stream out",
				    FILTERPORT_END);
	out->connect = ssp_streamer_connect_out;

	param = filterparamdb_add_param_long(filter_paramdb(f), "bsize",
					     FILTER_PARAMTYPE_LONG, 64,
					     FILTERPARAM_DESCRIPTION, "length of running average",
					     FILTERPARAM_END);
	param->set = ssp_streamer_bsize_set;

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
	
	if (!(in = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN))))
		FILTER_ERROR_RETURN("no input");
	
	param = filterparamdb_get_param(filter_paramdb(n), "maxrms");

	filterparam_val_double(param) = maxrms;
	
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
	filterparam_val_double(param) = maxrms;

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int maxrms_register(plugin_t *p)
{
	filter_t *f;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = maxrms_f;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SSP, FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "ssp stream in",
			      FILTERPORT_END);

	filterparamdb_add_param_double(filter_paramdb(f), "maxrms",
				      FILTER_PARAMTYPE_DOUBLE, 0.0 ,
				      FILTERPARAM_DESCRIPTION, "maximum rms",
				      FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "maxrms");
	plugin_set(p, PLUGIN_PIXMAP, "statistics.png"); 
	plugin_set(p, PLUGIN_CATEGORY, "Analyze");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Analyze");

	return filter_register(f, p);
}

