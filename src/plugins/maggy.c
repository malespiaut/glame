/*
 * maggy.c
 * $Id: maggy.c,v 1.7 2000/03/22 08:45:47 mag Exp $
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

#define GLAME_IIR_LOWPASS  0
#define GLAME_IIR_HIGHPASS 1
#define GLCTYPE double
int chebyshev(GLCTYPE *a, GLCTYPE *b, int n, int mode, GLCTYPE fc, GLCTYPE pr){

	int i,j,res=-1;
	GLCTYPE *ta,*tb;
	GLCTYPE *aa,*bb;
	GLCTYPE sa,sb;
	GLCTYPE gain;
	GLCTYPE rp,ip,es,vx,kx,t,w,m,d,k;
	GLCTYPE *x,*y;
	GLCTYPE h;
	
	if ((n%2)!=0)
		goto _error;
	if (!(ta=ALLOCN(n+3,GLCTYPE)))
		goto _error;
	if (!(tb=ALLOCN(n+3,GLCTYPE)))
		goto _error;
	aa=ALLOCN(3,GLCTYPE);
	bb=ALLOCN(2,GLCTYPE);
	x=ALLOCN(3,GLCTYPE);
	y=ALLOCN(2,GLCTYPE);

	if ((fc<0) || (fc>0.5)) 
		goto _error;
	a[2]=1.0;
	b[2]=1.0;

	for(i=0;i<n/2;i++){
		h=M_PI/(n*2.0)+i*M_PI/n;
		rp=-cos(h);
		ip=sin(h);
		if(pr>0.0) {
			h=100.0/(100.0-pr);
			es=sqrt(h*h-1.0);
			h=1.0/es;
			vx=1.0/(GLCTYPE)n*logf(h+sqrt(h*h+1.0));
			kx=1.0/(GLCTYPE)n*logf(h+sqrt(h*h-1.0));
			kx=(exp(kx)+exp(-kx))/2.0;
			h=exp(vx);
			rp*=(h-1.0/h)*0.5/kx;
			ip*=(h+1.0/h)*0.5/kx;
		}
/*
		DPRINTF("\ni=%d rp=%f ip=%f es=%f kx=%f vx=%f\n",i,rp,ip,es,kx,vx);
*/
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
		memcpy(ta,a,sizeof(GLCTYPE)*(n+3));
		memcpy(tb,b,sizeof(GLCTYPE)*(n+3));
		for(j=2;j<n+3;j++){
			a[j]=aa[0]*ta[j]+aa[1]*ta[j-1]+aa[2]*ta[j-2];
			b[j]=tb[j]-bb[0]*tb[j-1]-bb[1]*tb[j-2];
		}
	}
	
	b[2]=0.0;
	for(i=0;i<n+1;i++){
		a[i]=a[i+2];
		b[i]=-b[i+2];
	}
	sa=sb=0.0;
	for(i=0;i<n+1;i++){
		if (mode==GLAME_IIR_LOWPASS){
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
	GLCTYPE *a,*b;
	int i,n=6;
	
	a=ALLOCN(n+3,GLCTYPE);
	b=ALLOCN(n+3,GLCTYPE);
	chebyshev(a,b,n,GLAME_IIR_LOWPASS,0.01,0.5);
	for(i=0;i<n+1;i++) 
		DPRINTF("a[%d]=%.6E b[%d]=%.6E\n",i,a[i],i,b[i]);
	free(a);
	free(b);
}

int maggy_register()
{
	filter_t *f;
	filter_paramdesc_t *p;
	
	debugcheb();
	
	return 0;
}
