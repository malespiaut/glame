/*
 * waveform.c
 * $Id: waveform.c,v 1.6 2000/04/25 08:58:00 richi Exp $
 *
 * Copyright (C) 1999, 2000 Alexander Ehlert
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
 * This file contains a set of filters that generate various waveforms
 * Contained filters are
 * - sine
 * - const
 *
 * The waveform filters should only generate "one" buffer which can then
 * be repeated using the repeat filter. [richi]
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include <math.h>
#include "glplugin.h"


PLUGIN_SET(waveform, "sine const")


/* Standard waveform connect_out and fixup_param methods. These honour
 * optional parameters "rate" and "position".
 */
static int waveform_connect_out(filter_node_t *n, const char *port,
				filter_pipe_t *p)
{
	filter_param_t *param;
	int rate;
	float pos;

	rate = GLAME_DEFAULT_SAMPLERATE;
	if ((param = filternode_get_param(n, "rate")))
		rate = filterparam_val_int(param);
	pos = FILTER_PIPEPOS_DEFAULT;
	if ((param = filternode_get_param(n, "position")))
		pos = filterparam_val_float(param);
	filterpipe_settype_sample(p, rate, pos);
	return 0;
}
static int waveform_fixup_param(filter_node_t *n, filter_pipe_t *p,
				const char *name, filter_param_t *param)
{
	filter_pipe_t *out;

	if ((out = filternode_get_output(n, PORTNAME_OUT))) {
		waveform_connect_out(n, NULL, out);
		out->dest->filter->fixup_pipe(n, out);
	}
	return 0;
}

/* Standard waveform register. Does add the output port and the parameters "rate"
 * and "position". Also inits the methods.
 */
static filter_t *waveform_filter_alloc(int (*fm)(filter_node_t *))
{
	filter_t *f;
	filter_paramdesc_t *pos;

	if (!(f = filter_alloc(fm))
	    || !filter_add_output(f, PORTNAME_OUT, "waveform output stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f, "rate", 
	                         "samplerate of the generated output",
				 FILTER_PARAMTYPE_INT)
	    || !(pos = filter_add_param(f, "position", 
	                                "position of the output stream",
	                                FILTER_PARAMTYPE_FLOAT)))
		return NULL;
	filterparamdesc_float_settype(pos, FILTER_PARAM_FLOATTYPE_POSITION);
	f->connect_out = waveform_connect_out;
	f->fixup_param = waveform_fixup_param;

	return f;
}

/* Helpers. */
static int waveform_get_rate(filter_node_t *n)
{
	filter_param_t *param;

	if ((param = filternode_get_param(n, "rate")))
		return filterparam_val_int(param);
	else
		return GLAME_DEFAULT_SAMPLERATE;
}


/* This filter generates a sine test signal
 * defaults to 441 Hz and 0.5 max amplitude. 
 */
static int sine_f(filter_node_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	SAMPLE ampl;
	float freq;
	int rate, i, size;
	
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	/* globals */
	rate = waveform_get_rate(n);

	/* sane defaults */
	ampl = 0.5;
	freq = 441.0;

	/* user overrides with parameters */
	if ((param = filternode_get_param(n, "amplitude")))
		ampl = filterparam_val_sample(param);
	if ((param = filternode_get_param(n, "frequency")))
		freq = filterparam_val_float(param);

	/* FIXME: we should try to eliminate sampling frequency errors
	 * by finding optimal size. And we should at least fill a minimum
	 * sized buffer, too. */
	size = (((int)(GLAME_WBUFSIZE/(int)(rate/freq)))*rate)/freq;

	FILTER_AFTER_INIT;

	buf = sbuf_alloc(size, n);
	buf = sbuf_make_private(buf);
	/* FIXME: on linux (glibc) we want this to be sinf() - i.e.
	 * operate on floats w/o casting to double. On non glibc
	 * platforms like solaris sinf does not exist. ugh. */
        for (i=0; i<size; i++)
		sbuf_buf(buf)[i] = ampl*sin(i*2*M_PI*freq/rate);

	sbuf_queue(out, buf);
	sbuf_queue(out, NULL);

	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int sine_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = waveform_filter_alloc(sine_f))
	    || !filter_add_param(f, "amplitude", "sine peak amplitude(0.0-1.0)",
				 FILTER_PARAMTYPE_SAMPLE)
	    || !filter_add_param(f, "frequency", "sine frequency in Hz",
				 FILTER_PARAMTYPE_FLOAT))
		return -1;

	plugin_set(p, PLUGIN_DESCRIPTION, "generate sine signal");
	filter_attach(f, p);

	return 0;
}





/* This filter generates a constant signal,
 * signal value defaults to 0.0.
 */
static int const_f(filter_node_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	SAMPLE val;
	int rate, i, size;
	
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	/* globals */
	rate = waveform_get_rate(n);

	/* sane defaults */
	val = 0.0;

	/* user overrides with parameters */
	if ((param = filternode_get_param(n, "value")))
		val = filterparam_val_sample(param);

	/* we will generate one buffer with 0.1 sec samples. */
	size = rate/10;

	FILTER_AFTER_INIT;

	buf = sbuf_alloc(size, n);
	buf = sbuf_make_private(buf);
        for (i=0; i<size; i++)
		sbuf_buf(buf)[i] = val;

	sbuf_queue(out, buf);
	sbuf_queue(out, NULL);

	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int const_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = waveform_filter_alloc(const_f))
	    || !filter_add_param(f, "value", "signal value",
				 FILTER_PARAMTYPE_SAMPLE))
		return -1;

	plugin_set(p, PLUGIN_DESCRIPTION, "constant signal");
	filter_attach(f, p);

	return 0;
}
