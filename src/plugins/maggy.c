/*
 * maggy.c
 * $Id: maggy.c,v 1.10 2000/03/30 12:00:10 mag Exp $
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
#include "glplugin.h"

PLUGIN_SET(maggy,"iir")

#define GLAME_IIR_LOWPASS  0
#define GLAME_IIR_HIGHPASS 1
#define GLAME_IIR_BANDPASS 2

/* To get better filter accuracy I decided to compute the single
 * stages of the filter seperatly and apply them one by one 
 * to the sample data. According to the DSPGUIDE chapter 20 pp339
 * filters are more stable when applied in stages.
 * Who doesn't like that can still combine
 * all stages to one stage by just convoluting the single stages. 
 * But in the moment it's up to the user that he knows what he's doing. 
 * float accuracy can't be enough for certain parameters.
 */

/* This type should cover every possible IIR filter */

#define gliirt	float

typedef struct glame_iir glame_iir_t;
struct glame_iir {
	int np;		/* Number of poles */
	int mode;	/* Filter mode low/high/bandpass... */
	int nstages;	/* Number of filterstages */
	int na;		/* number of a coefficients per stage */
	int nb;		/* number of b coefficients per stage */
	gliirt fc;	/* cutoff frequency */
	gliirt ppr;	/* percent of ripple in passband */
	gliirt spr;	/* percent of ripple in stopand */
	gliirt **coeff;	/* Actual filter coefficients */
};

glame_iir_t *init_glame_iir(int mode, int nstages, int na, int nb){
	glame_iir_t *dum=NULL;
	int i;
	DPRINTF("Trying to allocate glame_iir_t\n");
	if ((dum=ALLOCN(1,glame_iir_t))){
		DPRINTF("glame_iir_t success\n");
		dum->mode=mode;
		dum->nstages=nstages;
		dum->na=na;
		dum->nb=nb;
		dum->coeff=(gliirt **)malloc(nstages*sizeof(gliirt *));
		for(i=0;i<nstages;i++)
			dum->coeff[i]=(gliirt *)malloc((na+nb)*sizeof(gliirt));
	}
	return dum;
}

void free_glame_iir(glame_iir_t *gt){
	int i;
	DPRINTF("free coeff");
	for(i=0;i<gt->nstages;i++)
		free(gt->coeff[i]);
	free(gt->coeff);
	DPRINTF("free gt");
	free(gt);
}


/* chebyshev calculates coefficients for a chebyshev filter
 * a,b coefficients
 * n   number of poles(2,4,6,...)
 * m   0..lowpass, 1..highpass
 * fc  cutoff frequency in percent of samplerate
 * pr  percent ripple in passband (0.5 is optimal)
 *
 * Code from DSPGUIDE Chapter 20, pp341 
 * online version http://www.dspguide.com
 */

#define chebtype float

int chebyshev_stage(glame_iir_t *gt, int n){
	chebtype h,rp,ip,es,kx,vx,t,w,m,d,k,sum;
	chebtype *x,*y,*a,*b;
	int res=-1,i;
	
	if (n>gt->nstages)
		goto _error;
	if (gt->na+gt->nb!=5)
		goto _error;

	if (!(x=ALLOCN(3,chebtype)))
		goto _error;
	if (!(y=ALLOCN(2,chebtype)))
		goto _error;
	if (!(a=ALLOCN(3,chebtype)))
		goto _error;
	if (!(b=ALLOCN(2,chebtype)))
		goto _error;
	
	h=M_PI/((chebtype)gt->np*2.0)+n*M_PI/(chebtype)gt->np;
	rp=-cos(h);
	ip=sin(h);
	/*
	DPRINTF("rp=%f ip=%f np=%d h=%f ppr=%f\n",rp,ip,gt->np,h,gt->ppr);
	*/
	if(gt->ppr>0.0) {
		h=100.0/(100.0-gt->ppr);
		es=sqrt(h*h-1.0);
		h=1.0/es;
		vx=1.0/(chebtype)gt->np*log(h+sqrt(h*h+1.0));
		kx=1.0/(chebtype)gt->np*log(h+sqrt(h*h-1.0));
		kx=(exp(kx)+exp(-kx))/2.0;
		h=exp(vx);
		rp*=(h-1.0/h)*0.5/kx;
		ip*=(h+1.0/h)*0.5/kx;
	}
/*
	DPRINTF("rp=%f ip=%f es=%f kx=%f vx=%f h=%f\n",rp,ip,es,kx,vx,h);
*/
	t=2.0*tan(0.5);
	w=2.0*M_PI*gt->fc;
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
	if (gt->mode)
		k=-cos(w*0.5+0.5)/cos(w*0.5-0.5);
	else
		k=sin(0.5-w*0.5)/sin(0.5+w*0.5);
	d=1+y[0]*k-y[1]*k*k;
	a[0]=(x[0]-x[1]*k+x[2]*k*k)/d;
	a[1]=(-2.0*x[0]*k+x[1]+x[1]*k*k-2.0*x[2]*k)/d;
	a[2]=(x[0]*k*k-x[1]*k+x[2])/d;
	b[0]=(2.0*k+y[0]+y[0]*k*k-2.0*y[1]*k)/d;
	b[1]=(-k*k-y[0]*k+y[1])/d;
	if(gt->mode){
		a[1]=-a[1];
		b[1]=-b[1];
	}
	/* Adjust gain for single stages
	 * FIXME Don't know if that works
	 */
	/*
	sum=a[0]+abs(a[1])+a[2];
	DPRINTF("a0=%f a1=%f a2=%f\n",a[0],a[1],a[2]);
	for(i=0;i<3;i++) a[i]/=sum;
*/
/*
	DPRINTF("n=%d a0=%f a1=%f a2=%f b1=%f b2=%f sum=%f\n",n,a[0],a[1],a[2],b[0],b[1],sum);
*/	
	gt->coeff[n][0]=(gliirt)(a[0]);
	gt->coeff[n][1]=(gliirt)(a[1]);
	gt->coeff[n][2]=(gliirt)(a[2]);
	gt->coeff[n][3]=(gliirt)(b[0]);
	gt->coeff[n][4]=(gliirt)(b[1]);
	
	res=0;
_error:
	free(x);
	free(y);
	free(a);
	free(b);
	return res;
}

glame_iir_t *chebyshev(int n, int mode, float fc, float pr){
	glame_iir_t *gt;
	int i;
	
	if (n%2!=0){
		DPRINTF("number of poles must be even.\n");
		return NULL;
	}
	
	if ((mode!=0) && (mode!=1)){
		DPRINTF("Mode %d not supported.\n",mode);
		return NULL;
	}
	
	if ((fc<0.0) || (fc>0.5)){
		DPRINTF("Invalid range for cutoff frequency(0-0.5)\n");
		return NULL;
	}

	if ((gt=init_glame_iir(mode,n/2,3,2))){
		DPRINTF("init_glame_iir done!\n");
		gt->ppr=pr;
		gt->fc=fc;
		gt->np=n;
		for(i=0;i<n/2;i++)
			chebyshev_stage(gt,i);
		DPRINTF("Filter setup done!\n");
	}
	return gt;	
}
	
static int iir_f(filter_node_t *n)
{
	typedef struct {
		gliirt *iring;
		gliirt *oring;
		int	ipos;
		int	opos;
	} iirf_t;
	
	filter_param_t *param;
	filter_pipe_t *in, *out;
	filter_buffer_t *inb;
	glame_iir_t *gt;
	iirf_t *iirf;
	SAMPLE *s;
	int i,pos,j,nb,nt,z;

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out = filternode_get_output(n, PORTNAME_OUT)))
	        FILTER_ERROR_RETURN("no in- or output");


	/* Just setup a 4 pole lowpass filter with 0.1*samplefrequency cutoff and 0.5 ripple in passband 
	 * later on I want to call iir_f from different filters providing gt
	 * having something like static int iir_f(filter_node_t *n, glame_iir_t *gt)
	 * Then you can register all kind of filters that are using iir_f as kernel
	 */
	
	if (!(gt=chebyshev(4,GLAME_IIR_LOWPASS,0.2,0.5)))
		FILTER_ERROR_RETURN("chebyshev failed");
	
	/* here follow generic code */
	
	if (gt->nstages==0)
		FILTER_ERROR_RETURN("iir filter needs at least one stage");
			
	if (!(iirf=ALLOCN(gt->nstages,iirf_t)))
		FILTER_ERROR_RETURN("memory allocation error");

	for(i=0;i<gt->nstages;i++){
		DPRINTF("Stage %d\n",i);
		for(j=0;j<gt->na;j++)
			DPRINTF("a[%d]=%f ",j,gt->coeff[i][j]);
		DPRINTF("\n");
		for(;j<gt->na+gt->nb;j++)
			DPRINTF("b[%d]=%f ",j-gt->nb,gt->coeff[i][j]);
		DPRINTF("\n");
		if (!(iirf[i].iring=(gliirt*)malloc(gt->na*sizeof(gliirt))))
			FILTER_ERROR_RETURN("memory allocation error");
		if (!(iirf[i].oring=(gliirt*)malloc((gt->nb+1)*sizeof(gliirt))))
			FILTER_ERROR_RETURN("memory allocation error");
		iirf[i].ipos=0;
		iirf[i].opos=0;
	}
	
	nb=gt->nb+1;
	nt=gt->na+gt->nb;
	
	FILTER_AFTER_INIT;
	
	/* Yes I know that this code is very ugly and possibly quiet slow, but it's my first try :) */
	goto entry;
        do{
		FILTER_CHECK_STOP;
		while(pos<sbuf_size(inb)){
			/* Feed sample into ringbuffer of the first stage */
			iirf[0].iring[iirf[0].ipos]=sbuf_buf(inb)[pos];
			for(i=0;i<gt->nstages;i++){
				iirf[i].oring[iirf[i].opos]=0.0;
				/* y[n]=a0*x[n]+a1*x[n-1]+... */
				for(j=0;j<gt->na;j++){
					z=iirf[i].ipos-j;
					if (z<0)
						z=gt->na-z;
					else if (z>gt->na)
						z=z-gt->na;
					iirf[i].oring[iirf[i].opos]+=gt->coeff[i][j]*iirf[i].iring[z];
				}
				/* y[n]=y[n]+b1*y[n-1]+b2*y[n-2]+... */
				for(j=gt->na;j<nt;j++){
					z=iirf[i].opos-j-1;
					if (z<0)
						z=gt->nb-z;
					else if (z>gt->nb)
						z=z-gt->nb;
				  	iirf[i].oring[iirf[i].opos]+=gt->coeff[i][j]*iirf[i].oring[z];
				}
				/* Feed output of last stage to next stage */
				if (i+1<gt->nstages)
					iirf[i+1].iring[iirf[i+1].ipos]=iirf[i].oring[iirf[i].opos];
			}
			/* At least I process it in place :) */
			sbuf_buf(inb)[pos++]=iirf[gt->nstages-1].oring[iirf[gt->nstages-1].opos];
			/* Adjust ringbuffers */
			for(i=0;i<gt->nstages;i++){
				iirf[i].ipos++;
				if (iirf[i].ipos>gt->na)
					iirf[i].ipos=0;
				iirf[i].opos++;
				if (iirf[i].opos>nb)
					iirf[i].opos=0;
			}
		}
		sbuf_queue(out,inb);
entry:
		inb=sbuf_get(in);
		inb=sbuf_make_private(inb);
		pos=0;
	}
	while(inb);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	for(i=0;i<gt->nstages;i++){
		free(iirf[i].iring);
		free(iirf[i].oring);
	}
	free(iirf);

	/* Later on this should be done by the calling filter */

	free_glame_iir(gt);

	FILTER_RETURN;
}


PLUGIN_DESCRIPTION(iir, "iir effect")
PLUGIN_PIXMAP(iir, "iir.xpm")

int iir_register()
{
	filter_t *f;

	if (!(f = filter_alloc(iir_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
		    		FILTER_PORTTYPE_SAMPLE))
		return -1;
	if (filter_add(f, "iir", "iir effect") == -1)
		return -1;

	return 0;
}
