/*
 * maggy.c
 * $Id: maggy.c,v 1.6 2000/03/21 05:56:44 mag Exp $
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
 
static int statistic_connect_out(filter_node_t *n, const char *port, filter_pipe_t *p){
	filterpipe_type(p)=FILTER_PIPETYPE_RMS;
	return 0;
}
	
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
			
/* chebyshev calculates coefficients for a chebyshev filter
 * a,b coefficients
 * n   number of poles(2,4,6,...)
 * m   0..lowpass, 1..highpass
 * fc  cutoff frequency in percent of samplerate
 * pr  percent ripple in passband (0.5 is optimal)
 *
 * Code from DSPGUIDE Chapter 20, pp341
 */

#define GLAME_IIR_LOWPASS  0
#define GLAME_IIR_HIGHPASS 1

int chebyshev(float *a, float *b, int n, int mode, float fc, float pr){

	int i,j,res=-1;
	float *ta,*tb;
	float *aa,*bb;
	float sa,sb;
	float gain;
	float rp,ip,es,vx,kx,t,w,m,d,k;
	float *x,*y;
	float h;
	
	if (mode)
	  DPRINTF("\nHighpass mode\n");
	else
	  DPRINTF("\nLowpass mode\n");
	
	if ((n%2)!=0)
		goto _error;
	if (!(ta=ALLOCN(n+2,float)))
		goto _error;
	if (!(tb=ALLOCN(n+2,float)))
		goto _error;
	aa=ALLOCN(3,float);
	bb=ALLOCN(2,float);
	x=ALLOCN(3,float);
	y=ALLOCN(2,float);

	if ((fc<0) || (fc>0.5)) 
		goto _error;
	a[2]=1.0;
	b[2]=1.0;

	for(i=0;i<n/2;i++){
		h=M_PI/(n*2.0)+i*M_PI/n;
		rp=-cos(h);
		ip=sin(h);
/*	
		DPRINTF("\nrp=%f\nip=%f\n",rp,ip);
*/
		if(pr>0.0) {
			h=100.0/(100.0-pr);
			es=sqrt(h*h-1.0);
			h=1.0/es;
			vx=1.0/n*log(h+sqrt(h*h+1));
			kx=1.0/n*log(h+sqrt(h*h-1));
			kx=(exp(kx)+exp(-kx))/2.0;
			h=exp(vx);
			rp*=(h-1.0/h)*0.5/kx;
			ip*=(h+1.0/h)*0.5/kx;
		}

		t=2.0*tan(0.5);
		w=2.0*M_PI*fc;
		m=rp*rp+ip*ip;
		d=4.0-4.0*rp*t+m*t*t;
		x[0]=t*t/d;
		x[1]=2*x[0];
		x[2]=x[0];
		y[0]=(8.0-2.0*m*t*t)/d;
		y[1]=(-4.0-4.0*rp*t-m*t*t)/d;
/*
		DPRINTF("\nt=%f\nw=%f\nm=%f\nd=%f\n",t,w,m,d);
		DPRINTF("\nx0=%f\nx1=%f\nx2=%f\ny1=%f\ny2=%f\n",x[0],x[1],x[2],y[0],y[1]);
*/
		if (mode)
			k=-cos(w*0.5+0.5)/cos(w*0.5-0.5);
		else
			k=sin(0.5-w*0.5)/sin(0.5+w*0.5);
		d=1+y[0]*k-y[1]*k*k;
		aa[0]=(x[0]-x[1]*k+x[2]*k*k)/d;
		aa[1]=(-2.0*x[0]*k+x[1]+x[1]*k*k-2.0*x[2]*k)/d;
		aa[2]=(x[0]*k*k-x[1]*k+x[2])/d;
		bb[0]=(2.0*k+y[0]+y[0]*k*k-2.0*y[1]*k)/d;
		bb[1]=(-k*k-y[0]*k+y[1])/d;
		if(mode){
			aa[1]=-aa[1];
			bb[1]=-bb[1];
		}
/*			
		DPRINTF("\na0=%f\na1=%f\na2=%f\nb0=%f\nb1=%f\n",aa[0],aa[1],aa[2],bb[0],bb[1]);
*/
		memcpy(ta,a,sizeof(float)*n);
		memcpy(tb,b,sizeof(float)*n);
		for(j=2;j<n+2;j++){
			a[j]=aa[0]*ta[j]+aa[1]*ta[j-1]+aa[2]*ta[j-2];
			b[j]=tb[j]-bb[0]*tb[j-1]-bb[1]*tb[j-2];
		}
	}
	
	b[2]=0.0;
	for(i=0;i<n+1;i++){
		a[i]=a[i+2];
		b[i]=b[i+2];
	}
	sa=sb=0.0;
	for(i=0;i<n+1;i++){
		if (!mode){
			sa+=a[i];
			sb+=b[i];
		}else{
			sa+=a[i]*pow(-1.0,i);
			sb+=b[i]*pow(-1.0,i);
		}
	}
	
	gain=sa/(1.0-sb);
	for(i=0;i<n+1;i++)
		a[i]/=gain;
	
	res=0;
_error:
	free(aa);
	free(bb);
	free(ta);
	free(tb);
	free(x);
	free(y);
	return res;
}
	
void debugcheb(){
	float *a,*b;

	a=ALLOCN(8,float);
	b=ALLOCN(8,float);
	chebyshev(a,b,4,GLAME_IIR_LOWPASS,0.1,0);
	chebyshev(a,b,4,GLAME_IIR_HIGHPASS,0.1,0);
	free(a);
	free(b);
}

int maggy_register()
{
	filter_t *f;
	filter_paramdesc_t *p;
	
	debugcheb();
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
	
	if (!(f = filter_alloc(statistic_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
		    		 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
		    		 FILTER_PORTTYPE_RMS)
	    || !(p=filter_add_param(f,"windowsize","timeslice in ms for which peak rms is calculated",
			            FILTER_PARAMTYPE_FLOAT)))
		return -1;
	f->connect_out = statistic_connect_out;
	
	if (filter_add(f,"statistic","Calculates RMS, RMS in window & DC-Offset")==-1)
		return -1;
	filterparamdesc_float_settype(p, FILTER_PARAM_FLOATTYPE_TIME);

	if (!(f = filter_alloc(debugrms_f))
	    || !filter_add_input(f,PORTNAME_IN, "input",
		    		 FILTER_PORTTYPE_RMS)
	    || filter_add(f, "debugrms","eats rms buffers and shows debug output")==-1)
		return -1;
	
	return 0;
}
