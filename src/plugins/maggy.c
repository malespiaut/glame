/*
 * maggy.c
 * $Id: maggy.c,v 1.3 2000/03/20 09:51:53 richi Exp $
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
			holdtime=filterpipe_sample_rate(in)*filterparam_val_int(param)/1000;
	if ((param=filternode_get_param(n,"attack")))
		if (filterparam_val_int(param)>0.0)
			attack=1000.0/(filterpipe_sample_rate(in)*filterparam_val_int(param));
	if ((param=filternode_get_param(n,"release")))
		if (filterparam_val_int(param)>0.0)
			release=1000.0/(filterpipe_sample_rate(in)*filterparam_val_int(param));
	
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

/* Statistic filter detects
 *   - dc offset
 *   - overall rms
 *   - peak rms in a fixed size window
 *     NOTE: I don't use sliding windows, but joined ones
 *
 *   The filter sends a stream of peak rms buffers and sents
 *   an overall statistics buffer last before sending eof
 *   FIXME we have to find a way that the gui can actually access these
 *   values, we should probably write the rms buffer into a swapfile meta data file
 *   FIXME any speed optimizations welcome :)
 */

static int statistic_f(filter_node_t *n){
	filter_pipe_t *in,*out;
	filter_buffer_t *sbuf,*rbuf;
	filter_param_t *param;
	ulong pos=0,peak_pos;
	ulong wsize;
	float rms,peak_rms;
	double total_rms,offset;
	SAMPLE min,max;
	int cnt=0,i;
	SAMPLE s;
	
	if (!(in=filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	if (!(out=filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	if ((param=filternode_get_param(n,"windowsize")))
		wsize=TIME2CNT(ulong, filterparam_val_float(param), filterpipe_sample_rate(in));
	else
		wsize=TIME2CNT(ulong, 500.0, filterpipe_sample_rate(in));

	FILTER_AFTER_INIT;
	sbuf=sbuf_get(in);

	peak_rms=rms=total_rms=0.0;
	min=max=0.0;
	
	while(sbuf){
		FILTER_CHECK_STOP;
		sbuf=sbuf_make_private(sbuf);
		i=0;
		while(i<sbuf_size(sbuf)){
			s=sbuf_buf(sbuf)[i++];
			min=MIN(s,min);
			max=MAX(s,max);
			rms+=s*s;
			offset+=s;
			cnt++;
			if (cnt==wsize){
				rms/=wsize;
				total_rms+=(double)wsize/(double)(pos+wsize)*(rms-total_rms);
				rms=sqrt(rms);
				rbuf=rms_alloc(n);
				rbuf=rms_make_private(rbuf);
				rms_set_mode_window(rbuf);
				rms_set_window(rbuf,rms,pos);
				rms_queue(out,rbuf);
				if(rms>peak_rms){
					peak_rms=rms;
					peak_pos=pos;
				}
				pos+=wsize;
				cnt=0;
				rms=0.0;
			}
		}
		sbuf_unref(sbuf);
		sbuf=sbuf_get(in);
	}

	if(cnt!=0){
		rms/=cnt;
		total_rms+=(double)cnt/(double)(pos+cnt)*(rms-total_rms);
		rms=sqrt(rms);
		rbuf=rms_alloc(n);
		rbuf=rms_make_private(rbuf);
		rms_set_mode_window(rbuf);
		rms_set_window(rbuf,rms,pos);
		rms_queue(out,rbuf);
		pos+=cnt;
	}
	
	offset/=pos;	
	total_rms=sqrt(total_rms);
	rbuf=rms_alloc(n);
	rbuf=rms_make_private(rbuf);
	rms_set_mode_total(rbuf);
	rms_set_total_rms(rbuf,(float)total_rms);
	rms_set_total_offset(rbuf,(float)offset);
	rms_set_peak(rbuf,peak_rms,peak_pos);
	rms_min(rbuf)=min;
	rms_max(rbuf)=max;
	rms_queue(out,rbuf);
	rms_queue(out,NULL);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

static int debugrms_f(filter_node_t *n){
	filter_pipe_t *in;
	filter_buffer_t *r;
	float peakrms;
	ulong pos;
	
	if (!(in=filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	
	FILTER_AFTER_INIT;
	
	r=rms_get(in);
	while(r){
		FILTER_CHECK_STOP;
		r=rms_make_private(r);
		if (rms_get_mode(r)==RMS_WINDOW) 
			rms_unref(r);
		else if (rms_get_mode(r)==RMS_TOTAL){
			DPRINTF("RMS       = %f\n",rms_get_total_rms(r));
			DPRINTF("DC Offset = %f\n",rms_get_total_offset(r));
			rms_get_peak(r,peakrms,pos);
			DPRINTF("Peak RMS  = %f\n",peakrms);
			DPRINTF("Peak pos  = %ld\n",pos);
			DPRINTF("Peak max  = %f\n",rms_max(r));
			DPRINTF("Peak min  = %f\n",rms_min(r));
		} else DPRINTF("oops!\n");
		r=rms_get(in);
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}
			
int maggy_register()
{
	filter_t *f;
	filter_paramdesc_t *p;

	if (!(f = filter_alloc(noisegate_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f,"threshold_on","if input < threshold_on noisegate is turned on",FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f,"threshold_off","if input > threshold_off noisegate is turned off",FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f,"hold","Hold Time[ms]",FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f,"attack","Attack Time[ms]",FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f,"release","Release Time[ms]",FILTER_PARAMTYPE_INT)
	    || filter_add(f, "noisegate", "The noisegate filters all signals that are below the threshold") == -1)
		return -1;
	
	if (!(f = filter_alloc("statistic","Calculates RMS, RMS in window & DC-Offset",statistic_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
		    		 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
		    		 FILTER_PORTTYPE_RMS)
	    || !(p=filter_add_param(f,"windowsize","timeslice in ms for which peak rms is calculated",FILTER_PARAMTYPE_FLOAT))
	    || filter_add(f)==-1)
		return -1;
	filterparamdesc_float_settype(p, FILTER_PARAM_FLOATTYPE_TIME);

	if (!(f = filter_alloc("debugrms","eats rms buffers and shows debug output",debugrms_f))
	    || !filter_add_input(f,PORTNAME_IN, "input",
		    		 FILTER_PORTTYPE_RMS)
	    || filter_add(f)==-1)
		return -1;
	
	return 0;
}
