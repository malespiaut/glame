/*
 * flanger.c
 * $Id: 
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

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "math.h"

PLUGIN_SET(reverbs,"flanger")

static int flanger_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	
	float	ef_gain, ct_gain, dry_gain, rampl, sweep_rate, pdepth, swdepth, fampl,
		rfampl1, rfampl2;
	int	rate, rbsize, pos, cpos, fpos, swpdepth;
	SAMPLE  *ringbuf, *s;
	int	*lfo, lfosize, i, lfopos, lfotype;

	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!in || !out)
		FILTER_ERROR_RETURN("no in/output connected");
	
	if ((param=filternode_get_param(n,"depth")))
			pdepth=filterparam_val_float(param);

	if (pdepth <= 0.0)
		FILTER_ERROR_RETURN("negative depth is not allowed.");
	
	if ((param=filternode_get_param(n,"sweep depth")))
	                swdepth=filterparam_val_float(param);
	
	if (swdepth <= 0.0)
		FILTER_ERROR_RETURN("negative sweep depth is not allowed.");
	
	if ((param=filternode_get_param(n,"sweep rate")))
	                sweep_rate=filterparam_val_float(param);

	if (sweep_rate <= 0.0)
		FILTER_ERROR_RETURN("negative sweep rate is not allowed.");
	
	if ((param=filternode_get_param(n,"dry gain")))
	                dry_gain=filterparam_val_float(param);
	
	if ((param=filternode_get_param(n,"effect gain")))
	                ef_gain=filterparam_val_float(param);
	
	if ((param=filternode_get_param(n,"feedback gain")))
	                ct_gain=filterparam_val_float(param);
	
	if ((param=filternode_get_param(n,"lfo type")))
			lfotype=filterparam_val_int(param);
	
	rampl = 1.0/(dry_gain + ef_gain);
	fampl = 1.0 + ct_gain;
	rfampl1 = 1.0/fampl;
	rfampl2 = ct_gain/fampl;
	rate = filterpipe_sample_rate(in);
	rbsize = TIME2CNT(int, pdepth, rate);
	DPRINTF("ringbuffer size = %d\n", rbsize);
	ringbuf = ALLOCN(rbsize, SAMPLE);
	pos = 0;
	cpos = rbsize >> 1;
	swpdepth = TIME2CNT(int, swdepth, rate);
	if (swpdepth > cpos)
		swpdepth = cpos;

	lfosize = rate/sweep_rate;
	lfo = ALLOCN(lfosize, int);
	DPRINTF("lfosize = %d cpos=%d swpdepth=%d\n", lfosize, cpos, swpdepth);

	switch(lfotype)
	{
		case 0:
			DPRINTF("LFO: ramp up");
			for (i=0; i < lfosize;i++) 
				lfo[i] = swpdepth*i/lfosize-swpdepth;
			break;
		case 1:
			DPRINTF("LFO: ramp down");
			for (i=0; i < lfosize;i++) 
				lfo[i] = swpdepth-swpdepth*i/lfosize;
			break;
		case 2: 
			DPRINTF("LFO: sinus");
			for (i=0; i < lfosize; i++)
				lfo[i] = (int)(swpdepth * sin(i*2*M_PI/lfosize));
			break;
		default:
			FILTER_ERROR_RETURN("unknown lfo type");
	}

	lfopos = 0;
	
	FILTER_AFTER_INIT;
	
	goto entry;
	while (buf) {
		FILTER_CHECK_STOP;
		
		/* got an input buffer */
		s = &sbuf_buf(buf)[0];
		for(i=0; i < sbuf_size(buf); i++) {
			fpos = cpos + lfo[lfopos];
			if (fpos >= rbsize)
				fpos -= rbsize;
			 else if (fpos < 0)
				fpos += rbsize;
			ringbuf[pos] = *s * rfampl1 + ringbuf[cpos] * rfampl2;
			*s *= dry_gain;
			*s += ringbuf[fpos]*ef_gain;
			*s *= rampl;
			s++;
			lfopos++;
			if (lfopos == lfosize)
				lfopos = 0;
			pos++;
			if (pos == rbsize)
				pos = 0;
			cpos++;
			if (cpos == rbsize)
				cpos = 0;
		}
		sbuf_queue(out, buf);
entry:
		buf = sbuf_make_private(sbuf_get(in));
	};
	
	sbuf_queue(out, buf);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	free(lfo);
	free(ringbuf);

	FILTER_RETURN;
}

int flanger_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdb_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "audio stream in", FILTER_PORTTYPE_SAMPLE);
	filter_add_output(f, PORTNAME_OUT, "audio stream out", FILTER_PORTTYPE_SAMPLE);
	
	f->f = flanger_f;
	
	param = filter_paramdb(f);

	filterparamdb_add_param_float(param, "depth", FILTER_PARAMTYPE_FLOAT, 5,
				    FILTERPARAM_DESCRIPTION, "flanger depth in ms",
				    FILTERPARAM_END);
	
	filterparamdb_add_param_float(param, "sweep depth", FILTER_PARAMTYPE_FLOAT, 2.5,
				    FILTERPARAM_DESCRIPTION, "sweep depth in ms",
				    FILTERPARAM_END);
	
	filterparamdb_add_param_float(param, "sweep rate", FILTER_PARAMTYPE_FLOAT, 0.5,
				    FILTERPARAM_DESCRIPTION, "oscillator frequency",
				    FILTERPARAM_END);
	
	filterparamdb_add_param_float(param, "dry gain", FILTER_PARAMTYPE_FLOAT, 1.0,
				      FILTERPARAM_DESCRIPTION, "dry gain",
				      FILTERPARAM_END);

	filterparamdb_add_param_float(param, "effect gain", FILTER_PARAMTYPE_FLOAT, 0.9,
				      FILTERPARAM_DESCRIPTION, "flanger effect feedback gain",
				      FILTERPARAM_END);

	filterparamdb_add_param_float(param, "feedback gain", FILTER_PARAMTYPE_FLOAT, 0.4,
				      FILTERPARAM_DESCRIPTION, "center tap feedback gain",
				      FILTERPARAM_END);

	/* FIXME 
	 * should change PARAMTYPE_INT to PARAMTYPE_MUTUAL to enable togglebutton in GUI
	*/
	filterparamdb_add_param_int(param, "lfo type", FILTER_PARAMTYPE_INT, 0 ,
				    FILTERPARAM_DESCRIPTION, "lfotype: (0) ramp up, (1) ramp down, (2) sinus",
				    FILTERPARAM_END);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "flanger effect");
	/* plugin_set(p, PLUGIN_PIXMAP, "flanger.xpm"); */
	plugin_set(p, PLUGIN_CATEGORY, "Effects");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Flanger");

	return filter_register(f, p);
}
