/*
 * $Id: time.c,v 1.8 2001/04/26 12:17:03 richi Exp $
 * time.c
 *
 * A simple time gate.  It will switch on at a specified time, or switch
 * off at a specified time, or skip a block, or only pass a block.
 * Exact behavior depends on the settings of the start and stop values.
 *
 * Copyright (C) 2000, 2001 Stuart Purdie, Daniel Kobras
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
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"

typedef enum {
	TIME_MODE_NONE=0,
	TIME_MODE_ONESHOT=1,
	TIME_MODE_INVERT=2
} time_mode_t;

typedef enum {
	TIME_STATE_LEVEL,
	TIME_STATE_ATTACK,
	TIME_STATE_DECAY,
	TIME_STATE_DONE
} time_state_t;

static int time_f(filter_t *n)
{
	filter_pipe_t *out;
	filter_param_t *param;
	float rate, dt;
	float t_on = 0.0, t_off = 0.0;
	float duration = 0.0, sno = 0.0;
	float attack = 1.0, release = 1.0;
	SAMPLE gain;
	time_state_t state = TIME_STATE_LEVEL;
	time_mode_t mode = TIME_MODE_NONE;
	
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!out)
		FILTER_ERROR_RETURN("no output port");

	rate = filterpipe_sample_rate(out);
	dt = 1000.0/rate;
	
	if ((param=filternode_get_param(n, "time_on")))
			t_on = filterparam_val_float(param);
	if ((param=filternode_get_param(n, "time_off")))
			t_off = filterparam_val_float(param);
	if ((param=filternode_get_param(n, "duration")))
			duration = filterparam_val_float(param);
	
	if ((param=filternode_get_param(n, "attack"))) {
		attack = filterparam_val_float(param);
		if (!(attack > 0.0))
			attack = 1.0;
		else
			attack = dt/attack;
	}
	if ((param=filternode_get_param(n, "release"))) {
		release = filterparam_val_float(param);
		if (!(release > 0.0))
			release = 1.0;
		else
			release = dt/release;
	}

	if (!(t_on > 0.0 && t_off > 0.0))
		mode |= TIME_MODE_ONESHOT;

	if (t_off < t_on)
		mode |= TIME_MODE_INVERT;

	if (!!(mode & TIME_MODE_ONESHOT) == !!(~mode & TIME_MODE_INVERT)) 
		gain = 1.0;
	else 
		gain = 0.0;

	DPRINTF("mode %s%s, gain %f\n", 
	        mode & TIME_MODE_ONESHOT ? "oneshot" : "pulse",
		mode & TIME_MODE_INVERT ? " invert" : "", 
		gain);

	FILTER_AFTER_INIT;
	
	while (state != TIME_STATE_DONE) {
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
				state = TIME_STATE_DONE;
				break;
			}
			
			if (sno > t_on)
				state = TIME_STATE_ATTACK;
			
			if (sno > t_off)
				state = TIME_STATE_DECAY;
			
			if (state == TIME_STATE_ATTACK) {
				gain += attack;
				if (gain > 1.0) {
					gain = 1.0;
					state = TIME_STATE_LEVEL;
				}
			} else if (state == TIME_STATE_DECAY) {
				gain -= release;
				if (gain < 0.0) {
					gain = 0.0;
					state = TIME_STATE_LEVEL;
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

static int time_connect_out(filter_t *src, filter_port_t *out,
                            filter_pipe_t *pipe)
{
	filter_param_t *param;
	int rate = GLAME_DEFAULT_SAMPLERATE;
	
	if ((param = filternode_get_param(src, "rate")))
		rate = filterparam_val_int(param);

	filterpipe_settype_sample(pipe, rate, 0.0);

	return 0;
}


static void time_fixup_param(glsig_handler_t *h, long sig, va_list va)
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

int time_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL)))
		return -1;
	
	filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE);

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

	f->f = time_f;
	f->connect_out = time_connect_out;
	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED, time_fixup_param,
	                  NULL);

	plugin_set(p, PLUGIN_DESCRIPTION, "generates a single ramp or pulse signal");
	plugin_set(p, PLUGIN_PIXMAP, "time.png");
	plugin_set(p, PLUGIN_CATEGORY, "Synthesis");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Generating_Waves");

	return filter_register(f, p);
}
