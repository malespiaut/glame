/*
 * waveform.c
 * $Id: waveform.c,v 1.9 2000/05/02 07:46:36 richi Exp $
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
static void waveform_fixup_param(glsig_handler_t *h, long sig, va_list va)
{
	filter_param_t *param;
	filter_node_t *n;
	filter_pipe_t *out;

	GLSIGH_GETARGS1(va, param);
	n = filterparam_node(param);

	if ((out = filternode_get_output(n, PORTNAME_OUT))) {
		waveform_connect_out(n, NULL, out);
		glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	}
}

/* Standard waveform register. Does add the output port and the parameters "rate"
 * and "position". Also inits the methods.
 */
static filter_t *waveform_filter_alloc(int (*fm)(filter_node_t *))
{
	filter_t *f;

	if (!(f = filter_alloc(fm)))
		return NULL;
	filter_add_output(f, PORTNAME_OUT, "waveform output stream",
			  FILTER_PORTTYPE_SAMPLE);
	filterpdb_add_param_int(filter_pdb(f), "rate", 
				FILTER_PARAMTYPE_INT, GLAME_DEFAULT_SAMPLERATE,
				FILTERPARAM_END);
	filterpdb_add_param_float(filter_pdb(f), "position", 
				  FILTER_PARAMTYPE_POSITION, FILTER_PIPEPOS_DEFAULT,
				  FILTERPARAM_END);

	f->connect_out = waveform_connect_out;
	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED,
			  waveform_fixup_param, NULL);

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

	if (!(f = waveform_filter_alloc(sine_f)))
		return -1;

	filterpdb_add_param_float(filter_pdb(f), "amplitude",
				  FILTER_PARAMTYPE_SAMPLE, 1.0,
				  FILTERPARAM_END);
	filterpdb_add_param_float(filter_pdb(f), "frequency",
				  FILTER_PARAMTYPE_FLOAT, 441.0,
				  FILTERPARAM_END);

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

	if (!(f = waveform_filter_alloc(const_f)))
		return -1;

	filterpdb_add_param_float(filter_pdb(f), "value",
				  FILTER_PARAMTYPE_SAMPLE, 1.0,
				  FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "constant signal");
	filter_attach(f, p);

	return 0;
}
