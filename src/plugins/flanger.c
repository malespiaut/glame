/*
 * flanger.c
 * $Id: flanger.c,v 1.21 2002/10/02 13:11:21 richi Exp $
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

PLUGIN(flanger)


void rek_fbm(int *lfo, int l, int r, int swpd, int iter)
{
	int d, p, ra;
	d = (lfo[r] + lfo[l])/2;
	p = (r + l)/2;
	if ((p==r)||(p==l))
		return;
	ra = swpd / iter * ( 2.0 * (float)(rand()-(RAND_MAX>>1)) / (float)RAND_MAX);
	d += ra;
	lfo[p] = d;
	rek_fbm(lfo, l, p, swpd, iter*2);
	rek_fbm(lfo, p, r, swpd, iter*2);
}	

void fbm_lfo(int *lfo, int size, int swpd) 
{
	int i;
	
	for(i=0;i<size;i++)
		lfo[i] = 0;
	
	rek_fbm(lfo, 0, size-1, swpd, 1);
}

static int flanger_set_param(filter_param_t *param, const void *val)
{
	double x,y;
	filter_param_t *p;
	filter_t *n = filterparam_filter(param);

	if (n->priv!=NULL)
		return 0;

	x = *((double*)val);

	if (strcmp("depth", filterparam_label(param))==0) {
		if(x<=0.0)
			return -1;
		p = filterparamdb_get_param(filter_paramdb(n), "sweep depth");
		y = filterparam_val_double(p);
		if(y>(x*0.5)) {
			y = x*0.5;
			n->priv = p;
			filterparam_set(p, &y);
			n->priv = NULL;
		}
	}

	if (strcmp("sweep depth", filterparam_label(param))==0) {
		p = filterparamdb_get_param(filter_paramdb(n), "depth");
		y = filterparam_val_double(p);
		if ((x > (y*0.5))||(x<0.0))
			return -1;
	}
	if (strcmp("sweep rate", filterparam_label(param))==0) {
		if (x<0.0)
			return -1;
	}
	if (strcmp("drywet", filterparam_label(param))==0) {
		if ((x<0.0) || (x>1.0))
			return -1;
	}
	if (strcmp("feedback gain", filterparam_label(param))==0) {
		if (x<0.0)
			return -1;
	}

	return 0;
}

static int flanger_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *dparam, *swdparam, *swrparam, *drwparam, *fbgparam, *lfoparam, *dbgparam;
	
	float	efgain = 1.0, fbgain = 0.0, drywet = 1.0, 
		sweep_rate = 0.5, pdepth = 10.0, swdepth = 5.0, fampl, dbgain;
	int	rate, rbsize, pos, cpos, fpos, swpdepth;
	SAMPLE  *ringbuf, *efringbuf, *s;
	int	*lfo, lfosize, i, lfopos, lfotype = 0;

	in = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN));
	out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT));
	if (!in || !out)
		FILTER_ERROR_RETURN("no in/output connected");
	
	dparam=filterparamdb_get_param(filter_paramdb(n), "depth");
	pdepth=filterparam_val_double(dparam);

	swdparam=filterparamdb_get_param(filter_paramdb(n), "sweep depth");
	swdepth=filterparam_val_double(swdparam);
	
	swrparam=filterparamdb_get_param(filter_paramdb(n), "sweep rate");
	sweep_rate=filterparam_val_double(swrparam);

	drwparam=filterparamdb_get_param(filter_paramdb(n), "drywet");
	drywet=filterparam_val_double(drwparam);
	
	fbgparam=filterparamdb_get_param(filter_paramdb(n), "feedback gain");
	fbgain=filterparam_val_double(fbgparam);
	
	dbgparam=filterparamdb_get_param(filter_paramdb(n), "dbgain");
	dbgain=DB2GAIN(filterparam_val_double(dbgparam));
	
	lfoparam=filterparamdb_get_param(filter_paramdb(n), "lfo type");
	lfotype=filterparam_val_long(lfoparam);
	
	
	efgain = 1.0 - drywet;
	fampl = 1.0 + fbgain;
	fampl/=dbgain;
	drywet /= fampl;
	efgain /= fampl;
	fbgain /= fampl;
	
	rate = filterpipe_sample_rate(in);
	rbsize = TIME2CNT(int, pdepth, rate);
	DPRINTF("ringbuffer size = %d\n", rbsize);
	ringbuf = ALLOCN(rbsize, SAMPLE);
	efringbuf = ALLOCN(rbsize, SAMPLE);
	pos = 0;
	cpos = rbsize >> 1;
	swpdepth = TIME2CNT(int, swdepth, rate);
	if (swpdepth > cpos)
		swpdepth = cpos;

	if (sweep_rate > 0.0) {
		lfosize = rate/sweep_rate;
		lfo = ALLOCN(lfosize, int);
		DPRINTF("lfosize = %d cpos=%d swpdepth=%d\n", lfosize, cpos, swpdepth);

		if (lfo==NULL)
			FILTER_ERROR_RETURN("failed to alloc lfo buffer\n");

		switch(lfotype)
			{
			case 0: 
				DPRINTF("LFO: sinus");
				for (i=0; i < lfosize; i++)
					lfo[i] = (int)(swpdepth * sin(i*2*M_PI/lfosize));
				break;
			case 1:
				DPRINTF("LFO: ramp up");
				for (i=0; i < lfosize;i++) 
					lfo[i] = swpdepth*i/lfosize-swpdepth;
				break;
			case 2:
				DPRINTF("LFO: ramp down");
				for (i=0; i < lfosize;i++) 
					lfo[i] = swpdepth-swpdepth*i/lfosize;
				break;
			case 3: 
				DPRINTF("LFO: fractal");
				fbm_lfo(lfo, lfosize, swpdepth);
				break;
			default:
				FILTER_ERROR_RETURN("unknown lfo type");
			}
	} else {
		lfosize = 32;
		lfo = ALLOCN(lfosize, int);
		if (lfo==NULL)
			FILTER_ERROR_RETURN("failed to alloc lfo buffer\n");
	}

	lfopos = 0;
	
	FILTER_AFTER_INIT;
	
	goto entry;
	while (buf) {
		FILTER_CHECK_STOP;
	
		drywet=filterparam_val_double(drwparam);
		fbgain=filterparam_val_double(fbgparam);
		dbgain=DB2GAIN(filterparam_val_double(dbgparam));
	
		efgain = 1.0 - drywet;
		fampl = 1.0 + fbgain;
		fampl/=dbgain;
		drywet /= fampl;
		efgain /= fampl;
		fbgain /= fampl;
		
		/* got an input buffer */
		s = &sbuf_buf(buf)[0];
		for(i=0; i < sbuf_size(buf); i++) {
			fpos = cpos + lfo[lfopos];
			if (fpos >= rbsize)
				fpos -= rbsize;
			else if (fpos < 0)
				fpos += rbsize;

			efringbuf[pos] = *s;
			*s = *s * drywet + efringbuf[fpos] * efgain + ringbuf[cpos] * fbgain;
			ringbuf[pos] = *s = sinf(*s); /* Saturate signal */
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
	free(efringbuf);

	FILTER_RETURN;
}

int flanger_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdb_t *pdb;
	filter_param_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = flanger_f;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "audio stream in",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "audio stream out",
			      FILTERPORT_END);
	
	pdb = filter_paramdb(f);
	param = filterparamdb_add_param_double(pdb, "depth", FILTER_PARAMTYPE_TIME_MS, 10,
				    FILTERPARAM_DESCRIPTION, "flanger depth in ms",
				    FILTERPARAM_LABEL,   "Effect Depth [ms]",
				    FILTERPARAM_END);
	param->set = flanger_set_param;
	
	param = filterparamdb_add_param_double(pdb, "sweep depth", FILTER_PARAMTYPE_TIME_MS, 5,
				    FILTERPARAM_DESCRIPTION, "sweep depth in ms",
				    FILTERPARAM_LABEL,   "Detune Range [ms]",
				    FILTERPARAM_END);
	param->set = flanger_set_param;
	
	param = filterparamdb_add_param_double(pdb, "sweep rate", FILTER_PARAMTYPE_FREQ, 1,
				    FILTERPARAM_DESCRIPTION, "oscillator frequency",
				    FILTERPARAM_LABEL,   "LFO Speed [Hz]",
				    FILTERPARAM_END);
	param->set = flanger_set_param;
	
	param = filterparamdb_add_param_double(pdb, "drywet", FILTER_PARAMTYPE_DOUBLE, 0.5,
				      FILTERPARAM_DESCRIPTION, "drywet",
				      FILTERPARAM_LABEL, "Dry/Wet Balance",
				      FILTERPARAM_END);
	param->set = flanger_set_param;

	param = filterparamdb_add_param_double(pdb, "feedback gain", FILTER_PARAMTYPE_DOUBLE, 0.5,
				      FILTERPARAM_DESCRIPTION, "feedback gain",
				      FILTERPARAM_LABEL, "Feedback Gain",
				      FILTERPARAM_END);
	param->set = flanger_set_param;
	
	param = filterparamdb_add_param_double(pdb, "dbgain", FILTER_PARAMTYPE_DOUBLE, 6.0,
				      FILTERPARAM_DESCRIPTION, "Attenuate/Amplify output signal",
				      FILTERPARAM_LABEL, "Gain [dB]",
				      FILTERPARAM_END);
	param->set = flanger_set_param;

	param = filterparamdb_add_param_long(pdb, "lfo type", FILTER_PARAMTYPE_LONG, 0 ,
				    FILTERPARAM_DESCRIPTION, 
				    "(0) sine\n"
				    "(1) ramp up\n" 
				    "(2) ramp down\n"
				    "(3) fractal",
				    FILTERPARAM_GLADEXML,
"<?xml version=\"1.0\"?><GTK-Interface><widget>\n" 
"	<class>GtkOptionMenu</class>\n"
"	<name>widget</name>\n"
"	<can_focus>True</can_focus>\n"
"	<items>Sine\n"
"Ramp up\n"
"Ramp down\n"
"Fractal</items>\n" 
"	<initial_choice>0</initial_choice>\n"
"</widget></GTK-Interface>",
				    FILTERPARAM_LABEL, "LFO Type",
				    FILTERPARAM_END);
	param->set = flanger_set_param;
	
	plugin_set(p, PLUGIN_DESCRIPTION, "flanger effect");
	/* plugin_set(p, PLUGIN_PIXMAP, "flanger.xpm"); */
	plugin_set(p, PLUGIN_CATEGORY, "Effects");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Flanger");

	return filter_register(f, p);
}
