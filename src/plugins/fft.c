/*
 * fft.c
 * $Id: 
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
#include "srfftw.h"	/* single precision real to complex fft */
#include "pthread.h"

PLUGIN_SET(basicfft,"fft ifft fft_resample")

pthread_mutex_t planlock = PTHREAD_MUTEX_INITIALIZER;

static int fft_connect_out(filter_node_t *n, const char *port, filter_pipe_t *p){
	int rate, bsize, osamp;
	float hangle;
	filter_pipe_t *in;
	filter_param_t *param;

	if ((in = filternode_get_input(n, PORTNAME_IN))) {
		if ((param=filternode_get_param(n,"blocksize")))
			bsize=filterparam_val_int(param);
		
		if ((param=filternode_get_param(n,"oversamp")))
			osamp=filterparam_val_int(param);

		rate=filterpipe_sample_rate(in);
		hangle=filterpipe_sample_hangle(in);
		filterpipe_settype_fft(p,rate,hangle,bsize,osamp);
	}
	
	return 0;
}

static void fft_fixup_param(glsig_handler_t *h, long sig, va_list va) {
	filter_param_t *param;
	filter_node_t *n;
	filter_pipe_t *out;
	
	GLSIGH_GETARGS1(va, param);
	n=filterparam_node(param);
	
	if ((out = filternode_get_output(n, PORTNAME_OUT))) {
		fft_connect_out(n,NULL,out);
		glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	}
}

static int fft_f(filter_node_t *n){
	filter_pipe_t *in,*out;
	filter_buffer_t *inb,*outb;
	filter_param_t *param;
	rfftw_plan p;
	SAMPLE *overlap,*iptr,*optr;
	int osamp,bsize;
	int ioff,i,ooff,todo;
	
	if (!(in=filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	if (!(out=filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	if ((param=filternode_get_param(n,"blocksize")))
		bsize=filterparam_val_int(param);
	
	if ((param=filternode_get_param(n,"oversamp")))
		osamp=filterparam_val_int(param);
	else osamp=1;

	/* plans are not threadsafe! */
	
	pthread_mutex_lock(&planlock);
	p = rfftw_create_plan(bsize, FFTW_REAL_TO_COMPLEX, FFTW_ESTIMATE);
	pthread_mutex_unlock(&planlock);
	
	overlap=ALLOCN(bsize,SAMPLE);
	
	FILTER_AFTER_INIT;
	
	inb=sbuf_get(in);
	inb=sbuf_make_private(inb);
	iptr=sbuf_buf(inb);
	outb=sbuf_alloc(GLAME_WBUFSIZE,n);
	outb=sbuf_make_private(outb);
	optr=sbuf_buf(outb);
	ioff=0;
	ooff=0;
	
	while(inb){
		FILTER_CHECK_STOP;
		
		if (osamp==1) {
			todo=MIN(sbuf_size(inb)-ioff,GLAME_WBUFSIZE-ooff);
			i=todo/bsize;
			if (i!=0) {
				rfftw(p,i,(fftw_real *)iptr,1,bsize,(fftw_real *)optr,1,bsize);
				iptr+=i*bsize;
			  	optr+=i*bsize;
				ioff+=i*bsize;
				ooff+=i*bsize;
			}
			else
			{
				memcpy(overlap,iptr,todo*SAMPLE_SIZE); 
				sbuf_unref(inb);
				inb=sbuf_get(in);
				if (inb) {
					inb=sbuf_make_private(inb);
					ioff=MIN(bsize-todo,sbuf_size(inb));
					memcpy(overlap+todo,sbuf_buf(inb),ioff*SAMPLE_SIZE); 
					iptr=sbuf_buf(inb)+ioff;
				}
				else 
				{
					memset(overlap+todo,0,(bsize-todo)*SAMPLE_SIZE); 
				}
				rfftw_one(p,(fftw_real *)overlap,optr);
				optr+=bsize;
				ooff+=bsize;
			}
			if (ooff==GLAME_WBUFSIZE) {
				sbuf_queue(out,outb);
				outb=sbuf_alloc(GLAME_WBUFSIZE,n);
				outb=sbuf_make_private(outb);
				ooff=0;
				optr=sbuf_buf(outb);
			}
			if (ioff==sbuf_size(inb)) {
				sbuf_unref(inb);
				inb=sbuf_get(in);
				if (!(inb)) goto out;
				inb=sbuf_make_private(inb);
				ioff=0;
				iptr=sbuf_buf(inb);
			}
		}
	}
out:
	if ((ooff!=GLAME_WBUFSIZE) && (ooff!=0))
	{
		inb=sbuf_alloc(ooff,n);
		inb=sbuf_make_private(inb);
		memcpy(sbuf_buf(inb),sbuf_buf(outb),ooff*SAMPLE_SIZE);
		sbuf_unref(outb);
		outb=inb;
		sbuf_queue(out,outb);
	}
	else if (ooff==0) sbuf_unref(outb);
	else sbuf_queue(out,outb);
	sbuf_queue(out,NULL);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(overlap);
	rfftw_destroy_plan(p);
	
	FILTER_RETURN;
}

int fft_register(plugin_t *p)
{
	filter_t *f;
	
	if (!(f = filter_alloc(fft_f))) 
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "audio input", FILTER_PORTTYPE_SAMPLE);
	filter_add_output(f, PORTNAME_OUT, "fft output", FILTER_PORTTYPE_FFT);
	
	filterpdb_add_param_int(filter_pdb(f),"blocksize",
			FILTER_PARAMTYPE_INT, 64,
			FILTERPARAM_DESCRIPTION,"fft-block size",
			FILTERPARAM_END);
	
	filterpdb_add_param_int(filter_pdb(f),"oversamp",
			FILTER_PARAMTYPE_INT, 1,
			FILTERPARAM_DESCRIPTION,"oversampling factor",
			FILTERPARAM_END);
	
	f->connect_out = fft_connect_out;
	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED, fft_fixup_param, NULL);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "Transform audio-stream to fft-stream");
	plugin_set(p, PLUGIN_PIXMAP, "fft.xpm");

	filter_attach(f,p);

	return 0;
}

static int ifft_connect_out(filter_node_t *n, const char *port, filter_pipe_t *p){
	filter_pipe_t *in;

	if ((in = filternode_get_input(n, PORTNAME_IN)))
		filterpipe_settype_sample(p,filterpipe_fft_rate(in),filterpipe_fft_hangle(in));
	return 0;
}

static int ifft_f(filter_node_t *n){
	filter_pipe_t *in,*out;
	filter_buffer_t *inb;
	rfftw_plan p;
	SAMPLE *overlap;
	int osamp,bsize,i;
	float gain;
	
	if (!(in=filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	if (!(out=filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	bsize=filterpipe_fft_bsize(in);
	osamp=filterpipe_fft_osamp(in);

	DPRINTF("registered blocksize %d, %d-times oversampling\n",bsize,osamp);
	DPRINTF("sample rate %d, pipeposition %f\n",filterpipe_fft_rate(in),filterpipe_fft_hangle(in));

	pthread_mutex_lock(&planlock);
	p = rfftw_create_plan(bsize, FFTW_COMPLEX_TO_REAL, FFTW_ESTIMATE | FFTW_IN_PLACE);
	pthread_mutex_unlock(&planlock);

	overlap=ALLOCN(bsize,SAMPLE);
	
	FILTER_AFTER_INIT;

	gain=1.0/(float)bsize;	/* fft is not normalized */
	goto entry;

	while(inb){
		FILTER_CHECK_STOP;
		
		/* In case we have no oversampling, we can process in place */
		if (osamp==1) {
			rfftw(p,sbuf_size(inb)/bsize,(fftw_real *)sbuf_buf(inb),1,bsize,NULL,1,1);
			for(i=0;i<sbuf_size(inb);i++)
				sbuf_buf(inb)[i]*=gain;
		}
		sbuf_queue(out,inb);
entry:
		inb=sbuf_get(in);
		inb=sbuf_make_private(inb);
	}
	sbuf_queue(out,NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	
	free(overlap);
	rfftw_destroy_plan(p);
	
	FILTER_RETURN;
}

int ifft_register(plugin_t *p)
{
	filter_t *f;
	
	if (!(f = filter_alloc(ifft_f))) 
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "fft input", FILTER_PORTTYPE_FFT);
	filter_add_output(f, PORTNAME_OUT, "audio output", FILTER_PORTTYPE_SAMPLE);
	
	f->connect_out = ifft_connect_out;

	plugin_set(p, PLUGIN_DESCRIPTION, "Transform fft-stream to audio-stream");
	plugin_set(p, PLUGIN_PIXMAP, "fft.xpm");

	filter_attach(f,p);

	return 0;
}


static int fft_resample_connect_out(filter_node_t *n, const char *port, filter_pipe_t *p){
	int rate, bsize;
	filter_pipe_t *in;
	filter_param_t *param;

	if ((in = filternode_get_input(n, PORTNAME_IN))) {
		if ((param=filternode_get_param(n,"frequency")))
			rate=filterparam_val_int(param);
	
		bsize=((rate*filterpipe_fft_bsize(in)/filterpipe_fft_rate(in))>>2)<<2;
		rate=bsize*filterpipe_fft_rate(in)/filterpipe_fft_bsize(in);
		DPRINTF("resampling to bsize %d and frequency %d\n",bsize,rate);
		filterpipe_settype_fft(p,rate,filterpipe_fft_hangle(in),bsize,filterpipe_fft_osamp(in));
	}
	
	return 0;
}


static void fft_resample_fixup_param(glsig_handler_t *h, long sig, va_list va) {
	filter_param_t *param;
	filter_node_t *n;
	filter_pipe_t *out;
	
	GLSIGH_GETARGS1(va, param);
	n=filterparam_node(param);
	
	if ((out = filternode_get_output(n, PORTNAME_OUT))) {
		fft_resample_connect_out(n,NULL,out);
		glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	}
}

static int fft_resample_f(filter_node_t *n){
	filter_pipe_t *in,*out;
	filter_buffer_t *inb,*outb;
	filter_param_t *param;
	int bsize,blocks,nbsize;
	int i,len,rate;
	float gain;
	
	if (!(in=filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	if (!(out=filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	bsize=filterpipe_fft_bsize(in);
	
	if ((param=filternode_get_param(n,"frequency")))
		rate=filterparam_val_int(param);

	nbsize=((rate*filterpipe_fft_bsize(in)/filterpipe_fft_rate(in))>>2)<<2;
	DPRINTF("Transforming blocksize %d to blocksize %d, new frequency %d\n",bsize,nbsize,rate);
	len=MIN(nbsize,bsize)/2;
	gain=(float)nbsize/(float)bsize;
	
	FILTER_AFTER_INIT;
	
	goto entry;
	
	while (inb) {
		FILTER_CHECK_STOP;
		
		inb=sbuf_make_private(inb);
		blocks=sbuf_size(inb)/bsize;
		outb=sbuf_make_private(sbuf_alloc(nbsize*blocks,n));
		for(i=0;i<blocks;i++) {
			memcpy(sbuf_buf(outb)+i*nbsize,sbuf_buf(inb)+i*bsize,len*SAMPLE_SIZE);
			memcpy(sbuf_buf(outb)+(i+1)*nbsize-len,sbuf_buf(inb)+(i+1)*bsize-len,len*SAMPLE_SIZE);
		}
		for(i=0;i<sbuf_size(outb);i++) 
			sbuf_buf(outb)[i]*=gain;
		sbuf_queue(out,outb);
		sbuf_unref(inb);
entry:
		inb=sbuf_get(in);
	}
	sbuf_queue(out,NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int fft_resample_register(plugin_t *p)
{
	filter_t *f;
	
	if (!(f = filter_alloc(fft_resample_f))) 
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_FFT);
	filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_FFT);
	
	filterpdb_add_param_int(filter_pdb(f),"frequency",
			FILTER_PARAMTYPE_INT, 44100,
			FILTERPARAM_DESCRIPTION,"resample frequency",
			FILTERPARAM_END);
	
	f->connect_out = fft_resample_connect_out;

	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED, fft_resample_fixup_param, NULL);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "Resample fft-stream");
	plugin_set(p, PLUGIN_PIXMAP, "fft.xpm");

	filter_attach(f,p);

	return 0;
}
