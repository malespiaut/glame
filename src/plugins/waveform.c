/*
 * waveform.c
 * $Id: waveform.c,v 1.19 2001/05/03 11:31:13 richi Exp $
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
 * - rect
 * - pulse
 *
 * The waveform filters should only generate "one" buffer which can then
 * be repeated using the repeat filter. [richi]
 */

#define _NO_FILTER_COMPATIBILITY
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include <math.h>
#include "glplugin.h"


PLUGIN_SET(waveform, "sine const rect pulse")


/* Standard waveform connect_out and fixup_param methods. These honour
 * optional parameters "rate" and "position".
 */
static int waveform_connect_out(filter_t *n, filter_port_t *port,
				filter_pipe_t *p)
{
	int rate;
	float pos;

	rate = filterparam_val_int(filterparamdb_get_param(filter_paramdb(n), "rate"));
	pos = filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "position"));
	filterpipe_settype_sample(p, rate, pos);
	return 0;
}
static void waveform_fixup_param(glsig_handler_t *h, long sig, va_list va)
{
	filter_param_t *param;
	filter_t *n;
	filter_pipe_t *out;

	GLSIGH_GETARGS1(va, param);
	n = filterparam_filter(param);

	if ((out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT)))) {
		waveform_connect_out(n, NULL, out);
		glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	}
}

/* Standard waveform register. Does add the output port and the parameters "rate"
 * and "position". Also inits the methods.
 */
static filter_t *waveform_filter_alloc(int (*fm)(filter_t *))
{
	filter_t *f;

	if (!(f = filter_creat(NULL)))
		return NULL;
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "waveform output stream",
			      FILTERPORT_END);
	filterparamdb_add_param_int(filter_paramdb(f), "rate", 
				    FILTER_PARAMTYPE_INT, GLAME_DEFAULT_SAMPLERATE,
				    FILTERPARAM_END);
	filterparamdb_add_param_float(filter_paramdb(f), "position", 
				      FILTER_PARAMTYPE_POSITION, FILTER_PIPEPOS_DEFAULT,
				      FILTERPARAM_END);

	f->f = fm;
	f->connect_out = waveform_connect_out;
	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED,
			  waveform_fixup_param, NULL);

	return f;
}

/* Helpers. */
static int waveform_get_rate(filter_t *n)
{
	return filterparam_val_int(filterparamdb_get_param(filter_paramdb(n), "rate"));
}


/* This filter generates a sine test signal
 * defaults to 441 Hz and 0.5 max amplitude. 
 */
static int sine_f(filter_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	SAMPLE ampl;
	float freq;
	int rate, i, size;
	
	if (!(out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");

	/* globals */
	rate = waveform_get_rate(n);

	/* parameters for sine */
	ampl = filterparam_val_sample(filterparamdb_get_param(filter_paramdb(n), "amplitude"));
	freq = filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "frequency"));

	/* FIXME: we should try to eliminate sampling frequency errors
	 * by finding optimal size. And we should at least fill a minimum
	 * sized buffer, too. */
	size = (((int)(MAX(1,GLAME_WBUFSIZE/(int)(rate/freq))))*rate)/freq;

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

	filterparamdb_add_param_float(filter_paramdb(f), "amplitude",
				  FILTER_PARAMTYPE_SAMPLE, 1.0,
				  FILTERPARAM_END);
	filterparamdb_add_param_float(filter_paramdb(f), "frequency",
				  FILTER_PARAMTYPE_FLOAT, 441.0,
				  FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "generate sine signal");
	plugin_set(p, PLUGIN_CATEGORY, "Synthesis");
	plugin_set(p, PLUGIN_PIXMAP, "sine.png");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Generating_Waves");
	return filter_register(f, p);
}





/* This filter generates a constant signal,
 * signal value defaults to 0.0.
 */
static int const_f(filter_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	SAMPLE val;
	int rate, i, size;
	
	if (!(out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");

	/* globals */
	rate = waveform_get_rate(n);

	/* parameters for const */
	val = filterparam_val_sample(filterparamdb_get_param(filter_paramdb(n), "value"));

	/* we will generate one buffer with 0.1 sec samples. */
	size = GLAME_WBUFSIZE;

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

	filterparamdb_add_param_float(filter_paramdb(f), "value",
				  FILTER_PARAMTYPE_SAMPLE, 1.0,
				  FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "constant signal");
	plugin_set(p, PLUGIN_CATEGORY, "Synthesis");
	plugin_set(p, PLUGIN_PIXMAP, "const.png");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Generating_Waves");
	return filter_register(f, p);
}

/* This filter generates a rectangular signal,
 */

static int rect_f(filter_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	SAMPLE ampl;
	float freq, bs;
	int rate, i, size, blocks, nb;
	
	if (!(out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");

	/* globals */
	rate = waveform_get_rate(n);

	/* parameters for rect */
	ampl = filterparam_val_sample(filterparamdb_get_param(filter_paramdb(n), "amplitude"));
	freq = filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "frequency"));

	bs = (rate/freq)/2.0;
	size = (((int)(MAX(1,GLAME_WBUFSIZE/(int)(rate/freq))))*rate)/freq;
	
	FILTER_AFTER_INIT;

	buf = sbuf_alloc(size, n);
	buf = sbuf_make_private(buf);

	/* You might think the following code is a bit weird/slow for creating a rectangular wave,
	 * but sampling rate is integer and the blocksize real. This way the code generates a
	 * rectangle of variable length and thereby getting closer to the desired frequency.
	 */
	
	blocks = 0;
        for (i=0; i<size; i++) {
		nb = (int)((float)i/bs);
		if (nb>blocks) { 
			ampl = -ampl; 
			blocks = nb;
		}
		sbuf_buf(buf)[i]=ampl;
	}
		
	sbuf_queue(out, buf);
	sbuf_queue(out, NULL);

	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int rect_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = waveform_filter_alloc(rect_f)))
		return -1;

	filterparamdb_add_param_float(filter_paramdb(f), "amplitude",
				  FILTER_PARAMTYPE_SAMPLE, 1.0,
				  FILTERPARAM_END);
	
	filterparamdb_add_param_float(filter_paramdb(f), "frequency",
				  FILTER_PARAMTYPE_SAMPLE, 440.0,
				  FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "rectangular signal");
	plugin_set(p, PLUGIN_CATEGORY, "Synthesis");
	plugin_set(p, PLUGIN_PIXMAP, "rect.png");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Generating_Waves");
	return filter_register(f, p);
}


typedef enum {
	PULSE_MODE_NONE=0,
	PULSE_MODE_ONESHOT=1,
	PULSE_MODE_INVERT=2
} pulse_mode_t;

typedef enum {
	PULSE_STATE_LEVEL,
	PULSE_STATE_ATTACK,
	PULSE_STATE_DECAY,
	PULSE_STATE_DONE
} pulse_state_t;

static int pulse_f(filter_t *n)
{
	filter_pipe_t *out;
	float rate, dt;
	float t_on = 0.0, t_off = 0.0;
	float duration = 0.0, sno = 0.0;
	float attack = 1.0, release = 1.0;
	SAMPLE gain;
	pulse_state_t state = PULSE_STATE_LEVEL;
	pulse_mode_t mode = PULSE_MODE_NONE;
	
	out = filterport_get_pipe(
		filterportdb_get_port(filter_portdb(n), PORTNAME_OUT));
	if (!out)
		FILTER_ERROR_RETURN("no output port");

	rate = filterpipe_sample_rate(out);
	dt = 1000.0/rate;

	t_on = filterparam_val_float(
		filterparamdb_get_param(filter_paramdb(n), "pulse_on"));
	t_off = filterparam_val_float(
		filterparamdb_get_param(filter_paramdb(n), "t_off"));
	duration = filterparam_val_float(
		filterparamdb_get_param(filter_paramdb(n), "duration"));

	attack = filterparam_val_float(
		filterparamdb_get_param(filter_paramdb(n), "attack"));
	if (!(attack > 0.0))
		attack = 1.0;
	else
		attack = dt/attack;

	release = filterparam_val_float(
		filterparamdb_get_param(filter_paramdb(n), "release"));
	if (!(release > 0.0))
		release = 1.0;
	else
		release = dt/release;

	if (!(t_on > 0.0 && t_off > 0.0))
		mode |= PULSE_MODE_ONESHOT;

	if (t_off < t_on)
		mode |= PULSE_MODE_INVERT;

	if (!!(mode & PULSE_MODE_ONESHOT) == !!(~mode & PULSE_MODE_INVERT)) 
		gain = 1.0;
	else 
		gain = 0.0;

	DPRINTF("mode %s%s, gain %f\n", 
	        mode & PULSE_MODE_ONESHOT ? "oneshot" : "pulse",
		mode & PULSE_MODE_INVERT ? " invert" : "", 
		gain);

	FILTER_AFTER_INIT;
	
	while (state != PULSE_STATE_DONE) {
		int i;
		filter_buffer_t *buf;
		SAMPLE *s;

		FILTER_CHECK_STOP;

		buf = sbuf_alloc(GLAME_WBUFSIZE, n);
		if (!buf)
			break;
		
		buf = sbuf_make_private(buf);
		s = sbuf_buf(buf);
		
		for (i=0; i<sbuf_size(buf); sno+=dt, i++) {
			
			if (sno > duration) {
				sbuf_realloc(buf, i);
				state = PULSE_STATE_DONE;
				break;
			}
			
			if (sno > t_on)
				state = PULSE_STATE_ATTACK;
			
			if (sno > t_off)
				state = PULSE_STATE_DECAY;
			
			if (state == PULSE_STATE_ATTACK) {
				gain += attack;
				if (gain > 1.0) {
					gain = 1.0;
					state = PULSE_STATE_LEVEL;
				}
			} else if (state == PULSE_STATE_DECAY) {
				gain -= release;
				if (gain < 0.0) {
					gain = 0.0;
					state = PULSE_STATE_LEVEL;
				}
			}
			*s++ = gain;
		}
		sbuf_queue(out, buf);
	}

	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

static int pulse_connect_out(filter_t *src, filter_port_t *out,
			     filter_pipe_t *pipe)
{
	int rate;

	rate = filterparam_val_int(
		filterparamdb_get_param(filter_paramdb(src), "rate"));
	filterpipe_settype_sample(pipe, rate, 0.0);

	return 0;
}


static void pulse_fixup_param(glsig_handler_t *h, long sig, va_list va)
{
	filter_t *n;
	filter_param_t *param;
	filter_pipe_t *out;
	int rate;	
	
	GLSIGH_GETARGS1(va, param);
	n = filterparam_filter(param);
	out = filterport_get_pipe(filterportdb_get_port(
		filter_portdb(n), "out"));
	
	if (!out)
		return;

	if (strcmp("rate", filterparam_label(param)))
		return;

	rate = filterparam_val_int(param);
	
	if (rate == filterpipe_sample_rate(out))
		return;
	
	filterpipe_settype_sample(out, rate, 0.0);
	glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
}

int pulse_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL)))
		return -1;
	
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "output",
			      FILTERPORT_END);

	filterparamdb_add_param_float(filter_paramdb(f), "time_on",
			FILTER_PARAMTYPE_TIME_MS, 0.0,
			FILTERPARAM_DESCRIPTION, "switch on time[ms]",
			FILTERPARAM_END);

	filterparamdb_add_param_float(filter_paramdb(f), "time_off",
			FILTER_PARAMTYPE_TIME_MS, 0.0,
			FILTERPARAM_DESCRIPTION, "switch off time[ms]",
			FILTERPARAM_END);
		
	filterparamdb_add_param_float(filter_paramdb(f), "duration",
			FILTER_PARAMTYPE_TIME_MS, 0.0,
			FILTERPARAM_DESCRIPTION, "end output after[ms]",
			FILTERPARAM_END);

	filterparamdb_add_param_float(filter_paramdb(f), "attack",
			FILTER_PARAMTYPE_TIME_MS, 0.0,
			FILTERPARAM_DESCRIPTION, "Attack Time[ms]",
			FILTERPARAM_END);

	filterparamdb_add_param_float(filter_paramdb(f), "release",
			FILTER_PARAMTYPE_TIME_MS, 0.0,
			FILTERPARAM_DESCRIPTION, "Release Time[ms]",
			FILTERPARAM_END);
	
	filterparamdb_add_param_int(filter_paramdb(f), "rate",
			FILTER_PARAMTYPE_INT, GLAME_DEFAULT_SAMPLERATE,
			FILTERPARAM_END);

	f->f = pulse_f;
	f->connect_out = pulse_connect_out;
	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED, pulse_fixup_param,
	                  NULL);

	plugin_set(p, PLUGIN_DESCRIPTION, "generates a single ramp or pulse signal");
	plugin_set(p, PLUGIN_PIXMAP, "pulse.png");
	plugin_set(p, PLUGIN_CATEGORY, "Synthesis");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Generating_Waves");

	return filter_register(f, p);
}
