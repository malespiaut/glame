
/*
 * maggy.c
 * $Id: maggy.c,v 1.1 2000/03/15 13:07:10 richi Exp $
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
 *
 * This is maggy's filter collection!
 * Please refrain from commiting any changes to cvs, just send me a patch!
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"

/* Implementation of a noise gate using ideas found
 * in Tobi Kuriens page about Audio effect algorithms
 * FIXME forgot url...
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
 *              to zero ampkitude within releasetime
 *
 * WARNING: Setting Releasetime/Attacktime to zero leads to distorted sound !
 */

static int noisegate_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	float t_on=0.0,t_off=0.0;
	int i;
	int hold=0;
	int holdtime=0;
	int is_releasing;
	float gain=1.0,attack=1.0,release=1.0;
	
	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!in || !out)
		return -1;

	if ((param=filternode_get_param(n,"threshold_on")))
			t_on=filterparam_val_float(param);
	if ((param=filternode_get_param(n,"threshold_off")))
			t_off=filterparam_val_float(param);
	if ((param=filternode_get_param(n,"hold")))
			holdtime=filterpipe_sample_rate(in)*filterparam_val_int(param)/1000;
	if ((param=filternode_get_param(n,"attack")))
		if (filterparam_val_int(param)>0.0)
			attack=1000.0/(filterpipe_sample_rate(in)*filterparam_val_int(param));
	if ((param=filternode_get_param(n,"release")))
		if (filterparam_val_int(param)>0.0)
			release=1000.0/(filterpipe_sample_rate(in)*filterparam_val_int(param));
	
	FILTER_AFTER_INIT;
	
	do {
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

	FILTER_BEFORE_CLEANUP;

	return 0;
}

int maggy_register()
{
	filter_t *f;

	if (!(f = filter_alloc("noisegate", "The noisegate filters all signals that are below the threshold", noisegate_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f,"threshold_on","if input < threshold_on noisegate is turned on",FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f,"threshold_off","if input > threshold_off noisegate is turned off",FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f,"hold","Hold Time[ms]",FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f,"attack","Attack Time[ms]",FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f,"release","Release Time[ms]",FILTER_PARAMTYPE_INT)
	    || filter_add(f) == -1)
		return -1;

	return 0;
}
