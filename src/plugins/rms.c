
/*
 * rms.c
 * $Id: rms.c,v 1.7 2000/05/25 12:57:08 mainzelm Exp $
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

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"

PLUGIN_SET(rms,"statistic debugrms")

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
 
static int statistic_connect_out(filter_node_t *n, const char *port, filter_pipe_t *p){
	filterpipe_type(p)=FILTER_PIPETYPE_RMS;
	return 0;
}

static int statistic_f(filter_node_t *n){
	filter_pipe_t *in,*out;
	filter_buffer_t *sbuf,*rbuf;
	unsigned long pos=0,peak_pos;
	int wsize;
	float rms,peak_rms;
	double total_rms,offset;
	SAMPLE min,max;
	int cnt=0,i;
	SAMPLE s;
	
	if (!(in=filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	if (!(out=filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	wsize=TIME2CNT(int, filterparam_val_float(filternode_get_param(n,"windowsize")), filterpipe_sample_rate(in));

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

int statistic_register(plugin_t *p)
{
	filter_t *f;
	
	if (!(f = filter_alloc(statistic_f))) 
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "audio input", FILTER_PORTTYPE_SAMPLE);
	filter_add_output(f, PORTNAME_OUT, "rms output", FILTER_PORTTYPE_RMS);
	
	filterpdb_add_param_float(filter_pdb(f),"windowsize",
			FILTER_PARAMTYPE_TIME_MS, 500.0,
			FILTERPARAM_DESCRIPTION,"timeslice in ms for which peak rms is calculated",
			FILTERPARAM_END);
	
	f->connect_out = statistic_connect_out;

	plugin_set(p, PLUGIN_DESCRIPTION, "Calculates RMS, RMS in window & DC-Offset");
	plugin_set(p, PLUGIN_PIXMAP, "statistic.xpm");

	filter_attach(f,p);

	return 0;
}

static int debugrms_f(filter_node_t *n){
	filter_pipe_t *in;
	filter_buffer_t *r;
	float peakrms;
	unsigned long pos;
	
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
			/* Don't use DPRINTF here - we want this output
			 * even when compiled without debugging support!
			 */
			printf("RMS       = %f\n",rms_get_total_rms(r));
			printf("DC Offset = %f\n",rms_get_total_offset(r));
			rms_get_peak(r,peakrms,pos);
			printf("Peak RMS  = %f\n",peakrms);
			printf("Peak pos  = %ld\n",pos);
			printf("Peak max  = %f\n",rms_max(r));
			printf("Peak min  = %f\n",rms_min(r));
			rms_unref(r);
		} else DPRINTF("oops!\n");
		r=rms_get(in);
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int debugrms_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_alloc(debugrms_f)))
		return -1;
	
	filter_add_input(f,PORTNAME_IN, "rms input", FILTER_PORTTYPE_RMS);

	plugin_set(p, PLUGIN_DESCRIPTION, "Eats rms buffers and shows debug output");
	plugin_set(p, PLUGIN_PIXMAP, "debug.xpm");

	filter_attach(f, p);
	
	return 0;
}
