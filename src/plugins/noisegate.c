/*
 * noisegate.c
 * $Id: noisegate.c,v 1.13 2001/06/05 09:25:13 xwolf Exp $
 *
 * Copyright (C) 2000 Alexander Ehlert
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

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"

/* Implementation of a noise gate using ideas found
 * in Toby Kuriens page about Audio effect algorithms:
 * http://users.iafrica.com/k/ku/kurient/dsp/algorithms.html
 * voila we got a full featured noise gate:
 *
 * Hold Time: If the signal falls into the gating region for long
 *            as the hold time the noise gate is turned on
 * 	      on  : abs(input) < threshold_on
 * 	      off : abs(input) > threshold_off
 * 	      
 *            For effective noisegating threshold_on < threshold_off !!
 * 
 * Attacktime: After the noisegate is turned off the signal is faded
 *             to full amplitude within attacktime
 *             
 * Releasetime: After the noisegate is turned off the signal is faded
 *              to zero amplitude within releasetime
 *
 * WARNING: Setting Releasetime/Attacktime to zero leads to distorted sound !
 */

static int noisegate_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	float t_on=0.0,t_off=0.0;
	int i;
	int hold=0;
	int holdtime=0;
	int is_releasing=0;
	float gain=1.0,attack=1.0,release=1.0;
	
	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!in || !out)
		FILTER_ERROR_RETURN("no in-/output port(s)");

	if ((param=filternode_get_param(n,"threshold_on")))
			t_on=filterparam_val_float(param);
	if ((param=filternode_get_param(n,"threshold_off")))
			t_off=filterparam_val_float(param);
	if ((param=filternode_get_param(n,"hold")))
			holdtime=TIME2CNT(int,filterparam_val_float(param),filterpipe_sample_rate(in));
	if ((param=filternode_get_param(n,"attack")))
		if (filterparam_val_float(param)>0.0)
			attack=1.0/TIME2CNT(float,filterparam_val_float(param),filterpipe_sample_rate(in));
	if ((param=filternode_get_param(n,"release")))
		if (filterparam_val_int(param)>0.0)
			release=1.0/TIME2CNT(float,filterparam_val_float(param),filterpipe_sample_rate(in));

	FILTER_AFTER_INIT;
	
	do {
		FILTER_CHECK_STOP;
		buf = sbuf_get(in);
		if (buf){
			buf=sbuf_make_private(buf);
			for(i=0;i<sbuf_size(buf);i++){
				/* This code is probably not very fast...
				 * Some more flags and less floating point operations
				 * would probably speed things up.
				 * But it's at least readable, I hope :*)
				 */
				if ((fabs(sbuf_buf(buf)[i])<t_on) && !is_releasing) {
					hold++;
					is_releasing=(hold>holdtime);
				} else if (fabs(sbuf_buf(buf)[i])>t_off)
				{ 
					hold=0;
					is_releasing=0;
					gain+=attack;
				}
				if (is_releasing) gain-=release;
				if (gain<0.0)
					gain=0.0;
				else if (gain>1.0)
					gain=1.0;
				
				sbuf_buf(buf)[i]=gain*sbuf_buf(buf)[i];
			}
		}
		sbuf_queue(out, buf);
	} while (pthread_testcancel(), buf);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}


int noisegate_register(plugin_t *p)
{
	filter_t *f;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = noisegate_f;

	filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_SAMPLE);
	filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE);

	filterparamdb_add_param_float(filter_paramdb(f),"threshold_on",
				FILTER_PARAMTYPE_FLOAT,0.0,
				FILTERPARAM_DESCRIPTION,"if input < threshold_on noisegate is turned on",
				FILTERPARAM_END);

	filterparamdb_add_param_float(filter_paramdb(f),"threshold_off",
			FILTER_PARAMTYPE_FLOAT,0.0,
			FILTERPARAM_DESCRIPTION,"if input > threshold_off noisegate is turned off",
			FILTERPARAM_END);
	
	filterparamdb_add_param_float(filter_paramdb(f),"hold",
			FILTER_PARAMTYPE_TIME_MS, 0.0,
			FILTERPARAM_DESCRIPTION,"Hold Time[ms]",
			FILTERPARAM_END);
	
	filterparamdb_add_param_float(filter_paramdb(f),"attack",
			FILTER_PARAMTYPE_TIME_MS, 0.0,
			FILTERPARAM_DESCRIPTION,"Attack Time[ms]",
			FILTERPARAM_END);
	
	filterparamdb_add_param_float(filter_paramdb(f),"release",
			FILTER_PARAMTYPE_TIME_MS, 0.0,
			FILTERPARAM_DESCRIPTION,"Release Time[ms]",
			FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "noisegate filters all signals that are below the threshold");
	plugin_set(p, PLUGIN_PIXMAP, "noisegate.png");
	plugin_set(p, PLUGIN_CATEGORY, "Filter");
	plugin_set(p, PLUGIN_GUI_HELP_PATH,"Noisegate");
		
	filter_register(f, p);
	
	return 0;
}
