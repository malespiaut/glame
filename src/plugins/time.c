/*
 * $Id:
 * time.c
 *
 * A simple time gate.  It will switch on at a specified time, or switch
 * off at a specified time, or skip a block, or only pass a block.
 * Exact behavior depends on the settings of the start and stop values.
 *
 * Copyright (C) 2000 Stuart Purdie
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

#define TIME_FLAG_CLEAR 	0
#define TIME_FLAG_ATTACK 	1
#define TIME_FLAG_DECAY 	2
#define TIME_FLAG_ALWAYS_ON 	4
#define TIME_FLAG_ALWAYS_OFF 	8


/* Implementation of a programable single action chopper.  Attack / decay
 * system taken from noisegate.c
 *
 * t_on and t_off are held as a fixed sample number
 *
 * Attacktime: After the chopper is turned on the signal is faded
 *             to full amplitude within attacktime
 *             
 * Releasetime: After the chopper is turned off the signal is faded
 *              to zero amplitude within releasetime
 *
 * WARNING: Setting Releasetime/Attacktime to zero leads to distorted sound !
 */

static int time_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
  	// Hmm, not a very portable code - really
	long long t_on = 0, t_off = 0;
	long long sno = 0;	/* need infinite precision here.  Needs to
				 * be an integer (mathematical style), so
				 * float is out.
				 */
	float gain=1.0, attack=1.0, release=1.0;

	int i;
	int flag = TIME_FLAG_CLEAR;
	
	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!in || !out)
		FILTER_ERROR_RETURN("no in/output port(s)");

	if ((param=filternode_get_param(n,"time_on")))
			t_on=TIME2CNT(long long,filterparam_val_float(param),filterpipe_sample_rate(in));
	if ((param=filternode_get_param(n,"time_off")))
			t_off=TIME2CNT(long long,filterparam_val_float(param),filterpipe_sample_rate(in));
	if ((param=filternode_get_param(n,"attack")))
		if (filterparam_val_float(param)>0.0)
			attack=1.0/TIME2CNT(float,filterparam_val_float(param),filterpipe_sample_rate(in));
	if ((param=filternode_get_param(n,"release")))
		if (filterparam_val_float(param)>0.0)
			release=1.0/TIME2CNT(float,filterparam_val_float(param),filterpipe_sample_rate(in));

/* Define the inital conditions for the filter */

	if(t_on == t_off) { /* avoids a not well defined case - makes it always on*/
		t_on = 0;
		t_off = 0;
		flag = TIME_FLAG_ALWAYS_ON;
		}

	if(t_on < t_off) {
		if(t_on == 0) 
			gain = 1.0; /* Single switch off */
		else
			gain = 0.0; /* Pass block */
		}
	else {
		if(t_off == 0)
			gain = 0.0; /* Single switch on */
		else
			gain = 1.0; /* Reject block */
		}


	FILTER_AFTER_INIT;
	
	do {
		FILTER_CHECK_STOP;
		buf = sbuf_get(in);

		if (buf){
			if(flag == TIME_FLAG_ALWAYS_ON) {
				; /* do nothing to the buffer */
				}
			else {
				buf=sbuf_make_private(buf);
				for(i=0;i<sbuf_size(buf);i++) {
					if(flag == TIME_FLAG_ALWAYS_OFF) {
						sbuf_buf(buf)[i] = 0.0;
						}
					else {
						if(sno == t_on)
							flag = TIME_FLAG_ATTACK;
						if(sno == t_off)
							flag = TIME_FLAG_DECAY;
						sno++;

						if(flag == TIME_FLAG_ATTACK) {
							gain += attack;
							if(gain >= 1.0) {
								gain = 1.0;
								if(t_off < t_on)
									flag = TIME_FLAG_ALWAYS_ON;
								else
									flag = TIME_FLAG_CLEAR;
								}
							}

						if(flag == TIME_FLAG_DECAY) {
							gain-=release;
							if(gain <= 0.0) {
								gain = 0.0;
								if(t_on < t_off)
									flag = TIME_FLAG_ALWAYS_OFF;
								else
									flag = TIME_FLAG_CLEAR;
								}
							}

						sbuf_buf(buf)[i]=gain*sbuf_buf(buf)[i];
						} /* else FLAG_ALWAYS_OFF */
					} /* for() */
				} /* else FLAG_ALWAYS_ON */
			} /* if(buf) */ 
			sbuf_queue(out, buf);
		} while (pthread_testcancel(), buf);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}


int time_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = time_f;
	
	filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_SAMPLE);
	filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE);

	filterparamdb_add_param_float(filter_paramdb(f),"time_on",
			FILTER_PARAMTYPE_TIME_MS,0.0,
			FILTERPARAM_DESCRIPTION,"switch on time[ms]",
			FILTERPARAM_END);

	filterparamdb_add_param_float(filter_paramdb(f),"time_off",
			FILTER_PARAMTYPE_TIME_MS,0.0,
			FILTERPARAM_DESCRIPTION,"switch off time[ms]",
			FILTERPARAM_END);
		
	filterparamdb_add_param_float(filter_paramdb(f),"attack",
			FILTER_PARAMTYPE_TIME_MS,0.0,
			FILTERPARAM_DESCRIPTION,"Attack Time[ms]",
			FILTERPARAM_END);

	filterparamdb_add_param_float(filter_paramdb(f),"release",
			FILTER_PARAMTYPE_TIME_MS,0.0,
			FILTERPARAM_DESCRIPTION,"Release Time[ms]",
			FILTERPARAM_END);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "chopper selects the apropriate region, as defined by the t_on and t_off values");
	plugin_set(p, PLUGIN_PIXMAP, "bitfence.xpm");
	plugin_set(p, PLUGIN_CATEGORY, "Time");
	filter_register(f, p);
	
	return 0;
}
