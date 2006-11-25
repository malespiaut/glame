/*
 * fft.c
 * $Id: fft_plugins.c,v 1.24 2006/11/25 19:07:32 richi Exp $
 *
 * Copyright (C) 2000, 2001, 2002 Alexander Ehlert
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
#include "srfftw.h"	/* real to complex fft */
#include "pthread.h"
#include "math.h"

PLUGIN_SET(fft_plugins,"fft ifft fft_resample fft_equalizer fft_bandpass")

pthread_mutex_t planlock = PTHREAD_MUTEX_INITIALIZER;

static SAMPLE *hanning(int n)
{
	SAMPLE *win;
	int i;
	
	if (!(win=ALLOCN(n,SAMPLE)))
		return NULL;
	for(i=0;i<n;i++)
		win[i]=0.5-0.5*cos((SAMPLE)i/(SAMPLE)(n-1)*2.0*M_PI);
	return win;
}

static SAMPLE window_gain(SAMPLE *win, int n, int osamp)
{
	SAMPLE *s;
	double max;
	int i, j, off;
	
	off = n / osamp;
	s = ALLOCN(n, SAMPLE);

	memcpy(s, win, n*SAMPLE_SIZE);
	
	for(i=1; i<osamp; i++)
		for(j=0; j<n; j++)
			s[j] += win[(j+i*off)%n];

	max = 0;
	for(i=0; i<n; i++)
		max += s[i]*s[i];

	max /= n;
	max = sqrt(max);

	free(s);
	return (SAMPLE)max;
}

static void fft_update_pipes(filter_t *n, filter_pipe_t *opipe)
{
	int rate = 44100, bsize = 2048, osamp = 8;
	float hangle = 0.0;
	filter_pipe_t *in;
	filter_param_t *param;
	filter_port_t *in_port;
	
	in_port = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);

	in = filterport_get_pipe(in_port);
	
	if ((param=filterparamdb_get_param(filter_paramdb(n), "blocksize")))
		bsize=filterparam_val_long(param);
	
	if ((param=filterparamdb_get_param(filter_paramdb(n), "oversamp")))
		osamp=filterparam_val_long(param);

	if (in) {	
		rate=filterpipe_sample_rate(in);
		hangle=filterpipe_sample_hangle(in);
	}
	
	filterpipe_settype_fft(opipe,rate,hangle,bsize,osamp);
}

static int fft_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	if (filterport_get_pipe(port))
		return -1;
	return 0;
}

static int fft_connect_out(filter_port_t *port, filter_pipe_t *p)
{
	filter_t *n = filterport_filter(port);
	
	if(filterport_get_pipe(port))
		return -1;

	fft_update_pipes(n, p);

	return 0;
}

static void fft_fixup_pipe(glsig_handler_t *h, long sig, va_list va)
{
	filter_t	*n;
	filter_port_t   *oport;
	filter_pipe_t   *opipe, *pipe;

	GLSIGH_GETARGS1(va, pipe);
	n = filterport_filter(filterpipe_dest(pipe));
	oport = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	opipe = filterport_get_pipe(oport);
	if(!opipe)
		return;
	
	fft_update_pipes(n, opipe);

	glsig_emit(filterpipe_emitter(opipe), GLSIG_PIPE_CHANGED, opipe);
}

static int fft_blocksize_set(filter_param_t *param, const void *val)
{
	filter_pipe_t *out;

	if (*((long *)val) < 2)
		return -1;

	out = filterport_get_pipe(filterportdb_get_port(
		filter_portdb(filterparam_filter(param)), PORTNAME_OUT));
	if (out) {
		filterpipe_settype_fft(out,
				       filterpipe_fft_rate(out),
				       filterpipe_fft_hangle(out),
				       *((long *)val),
				       filterpipe_fft_osamp(out));
		glsig_emit(filterpipe_emitter(out), GLSIG_PIPE_CHANGED, out);
	}
	return 0;
}

static int fft_oversamp_set(filter_param_t *param, const void *val)
{
	filter_pipe_t *out;

	if (*((long *)val) < 1)
		return -1;

	out = filterport_get_pipe(filterportdb_get_port(
		filter_portdb(filterparam_filter(param)), PORTNAME_OUT));
	if (out) {
		filterpipe_settype_fft(out,
				       filterpipe_fft_rate(out),
				       filterpipe_fft_hangle(out),
				       filterpipe_fft_bsize(out),
				       *((long *)val));
		glsig_emit(filterpipe_emitter(out), GLSIG_PIPE_CHANGED, out);
	}
	return 0;
}

static int fft_f(filter_t *n)
{
	in_queue_t    queue;
	filter_pipe_t *in,*out;
	filter_buffer_t *outb,*outb2;
	rfftw_plan p;
	SAMPLE *overlap, *s, *win;
	int osamp, bsize = 2048;
	int ooff, obufsize, obufcnt, cnt, i, j;
	
	if (!(in=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN))))
		FILTER_ERROR_RETURN("no input");
	if (!(out=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");

	bsize = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "blocksize"));
	osamp = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "oversamp"));

	/* plans are not threadsafe! */
	pthread_mutex_lock(&planlock);
	p = rfftw_create_plan(bsize, FFTW_REAL_TO_COMPLEX, FFTW_ESTIMATE | FFTW_IN_PLACE);
	pthread_mutex_unlock(&planlock);

	if (!(overlap=ALLOCN(bsize,SAMPLE)))
		FILTER_ERROR_RETURN("couldn't allocate overlap buffer");
	if (!(win=hanning(bsize)))
		FILTER_ERROR_RETURN("couldn't allocate window buffer");

	in_queue_init(&queue, in, n);
	
	FILTER_AFTER_INIT;
	
	ooff = bsize / osamp;
	obufsize = MAX(bsize, (GLAME_WBUFSIZE / bsize) * bsize);
	obufcnt = obufsize / bsize;

	DPRINTF("ooff = %d, obufsize=%d, obufcnt=%d\n", ooff, obufsize, obufcnt);
	
	do {
		FILTER_CHECK_STOP;
		outb = sbuf_make_private(sbuf_alloc(obufsize, n));
		s = sbuf_buf(outb);
		
		for(cnt=0; cnt<obufcnt; cnt++) {
			if (in_queue_copy_pad(&queue, s, bsize)) {
				cnt++;
				break;
			}
			in_queue_shift(&queue, ooff);
			s += bsize;
		}
		
		s = sbuf_buf(outb);
		if (osamp>1) 
			for(i=0; i<cnt; i++)
				for(j=0;j<bsize;j++)
					*s++ *= win[j];
		
		rfftw(p, cnt, (fftw_real *)sbuf_buf(outb), 1, bsize, NULL, 1, 1);
	
		if (cnt == obufcnt)
			sbuf_queue(out, outb);
	} while ( (cnt == obufcnt) && (!queue.done));

	if (cnt == 0) {
		sbuf_unref(outb);
	} else if (cnt!=obufcnt) {
		DPRINTF("realloc\n");
		outb2 = sbuf_make_private(sbuf_alloc(cnt*bsize, n));
		memcpy(sbuf_buf(outb2), sbuf_buf(outb), cnt*bsize*SAMPLE_SIZE);
		sbuf_unref(outb);
		sbuf_queue(out, outb2);
	}
	sbuf_queue(out, NULL);
	in_queue_drain(&queue);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(win);
	rfftw_destroy_plan(p);

	FILTER_RETURN;
}

int fft_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *out, *in;
	filter_param_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = fft_f;

	in = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				   FILTER_PORTTYPE_SAMPLE,
				   FILTER_PORTFLAG_INPUT,
				   FILTERPORT_DESCRIPTION, "audio stream",
				   FILTERPORT_END);
	in->connect = fft_connect_in;
	out = filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
				    FILTER_PORTTYPE_FFT,
				    FILTER_PORTFLAG_OUTPUT,
				    FILTERPORT_DESCRIPTION, "fft stream",
				    FILTERPORT_END);
	out->connect = fft_connect_out;
	
	param = filterparamdb_add_param_long(filter_paramdb(f),"blocksize",
			FILTER_PARAMTYPE_LONG, 2048,
			FILTERPARAM_DESCRIPTION,"fft-block size",
			FILTERPARAM_END);
	param->set = fft_blocksize_set;
	
	param = filterparamdb_add_param_long(filter_paramdb(f),"oversamp",
			FILTER_PARAMTYPE_LONG, 8,
			FILTERPARAM_DESCRIPTION,"oversampling factor",
			FILTERPARAM_END);
	param->set = fft_oversamp_set;

	glsig_add_handler(&f->emitter, GLSIG_PIPE_CHANGED, fft_fixup_pipe, NULL);	

	plugin_set(p, PLUGIN_DESCRIPTION, "Transform audio-stream to fft-stream");
	plugin_set(p, PLUGIN_PIXMAP, "fft.png");
	plugin_set(p, PLUGIN_CATEGORY, "FFT");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "FFT");

	filter_register(f,p);

	return 0;
}

static int ifft_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	if (filterport_get_pipe(port))
		return -1;
	return 0;
}

static int ifft_connect_out(filter_port_t *port, filter_pipe_t *p)
{
	filter_t *n = filterport_filter(port);
	filter_pipe_t *in;
	int rate = GLAME_DEFAULT_SAMPLERATE;
	float hangle = 0.0;

	if(filterport_get_pipe(port))
		return -1;

	if ((in = filterport_get_pipe(filterportdb_get_port(
		filter_portdb(n), PORTNAME_IN)))) {
		rate = filterpipe_fft_rate(in);
		hangle = filterpipe_fft_hangle(in);
	}
	filterpipe_settype_sample(p, rate, hangle);

	return 0;
}

static void ifft_fixup_pipe(glsig_handler_t *h, long sig, va_list va)
{
	filter_t *n = NULL;
	filter_pipe_t *opipe, *pipe;
	filter_port_t *oport;	

	GLSIGH_GETARGS1(va, pipe);
	n = filterport_filter(filterpipe_dest(pipe));
	oport = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	opipe = filterport_get_pipe(oport);
	if(!opipe)
		return;

	filterpipe_settype_sample(opipe,
				  filterpipe_fft_rate(pipe),
				  filterpipe_fft_hangle(pipe));

	glsig_emit(filterpipe_emitter(opipe), GLSIG_PIPE_CHANGED, opipe);
}

static int ifft_f(filter_t *n)
{
	out_queue_t	queue;
	filter_pipe_t	*in, *out;
	filter_buffer_t *inb;
	rfftw_plan p;
	SAMPLE *win, *s;
	float fak;
	int osamp, bsize, i, j, ibufcnt, ooff;
	float gain;
	double delta = 0.0;
	int idelta = 0;
	double drift = 0.0;
	
	if (!(in=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN))))
		FILTER_ERROR_RETURN("no input");
	if (!(out=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");

	drift = filterparam_val_double(filterparamdb_get_param(
				       filter_paramdb(n), "drift"));
	DPRINTF("compensating a drift of %.3e\n", drift);

	bsize=filterpipe_fft_bsize(in);
	osamp=filterpipe_fft_osamp(in);

	DPRINTF("registered blocksize %d, %d-times oversampling\n",bsize,osamp);
	DPRINTF("sample rate %d, pipeposition %f\n",filterpipe_fft_rate(in),filterpipe_fft_hangle(in));

	pthread_mutex_lock(&planlock);
	p = rfftw_create_plan(bsize, FFTW_COMPLEX_TO_REAL, FFTW_ESTIMATE | FFTW_IN_PLACE);
	pthread_mutex_unlock(&planlock);

	if (!(win=hanning(bsize)))
		FILTER_ERROR_RETURN("couldn't allocate window buffer");
	

	gain = 1.0/(float)bsize;	/* fft is not normalized */
	ooff = bsize / osamp;
	
	if (osamp>1) {
		fak = window_gain(win, bsize, osamp);
		DPRINTF("fak = %f\n",fak);
		gain /= fak;
	}

	for (j=0; j < bsize; j++)
		win[j] *= gain;
	
	out_queue_init(&queue, out, n);

	FILTER_AFTER_INIT;

	goto entry;
	
	while (inb) {
		FILTER_CHECK_STOP;
	
		ibufcnt = sbuf_size(inb)/bsize;
		s = sbuf_buf(inb);
		
		/* In case we have no oversampling, we can process in place */
		rfftw(p, ibufcnt, (fftw_real *)s, 1, bsize, NULL, 1, 1);
		
		s = sbuf_buf(inb);
		if (osamp>1)
			for (i=0; i<ibufcnt; i++)
				for(j=0;j<bsize;j++)
					*s++ *= win[j];
		else
			glsimd.scalar_product_1dI(s, sbuf_size(inb), gain);

		s = sbuf_buf(inb);
		
		for (i=0; i<ibufcnt; i++) {
			delta += ooff * (1.0 - drift);
			idelta = (int)delta;
			delta -= idelta;
			out_queue_add(&queue, s, bsize);
			out_queue_shift(&queue, ooff+idelta);
			s += bsize;
		}
		sbuf_unref(inb);
entry:		
		inb = sbuf_make_private(sbuf_get(in));
	}
	out_queue_drain(&queue);
	sbuf_queue(out,NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	
	free(win);
	rfftw_destroy_plan(p);
	
	FILTER_RETURN;
}

int ifft_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *out, *in;
	
	if (!(f = filter_creat(NULL)))
		return -1;

	in = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				   FILTER_PORTTYPE_FFT,
				   FILTER_PORTFLAG_INPUT,
				   FILTERPORT_DESCRIPTION, "fft stream",
				   FILTERPORT_END);
	in->connect = ifft_connect_in;
	out = filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
				    FILTER_PORTTYPE_SAMPLE,
				    FILTER_PORTFLAG_OUTPUT,
				    FILTERPORT_DESCRIPTION, "audio stream",
				    FILTERPORT_END);
	out->connect = ifft_connect_out;

	filterparamdb_add_param_double(filter_paramdb(f),"drift",
			FILTER_PARAMTYPE_DOUBLE, 1.0,
			FILTERPARAM_DESCRIPTION,"drift to compensate",
			FILTERPARAM_END);

	f->f = ifft_f;
	glsig_add_handler(&f->emitter, GLSIG_PIPE_CHANGED, ifft_fixup_pipe, NULL);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "Transform fft-stream to audio-stream");
	plugin_set(p, PLUGIN_PIXMAP, "fft.png");
	plugin_set(p, PLUGIN_CATEGORY, "FFT");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "IFFT");
	
	filter_register(f,p);

	return 0;
}

/* Calculate best output blocksize from the target rate and the input
   blocksizes and sample rates.  Sets orate to the effective output
   sample rate.  */
static void fft_resample_orate_and_obsize (long rate,
					   long irate, long ibsize,
					   long *orate, long *obsize)
{
	/* Even output blocksize matching the target rate best.  */
	*obsize = 2 * (long)(0.5 * (double)rate * ibsize / irate + 0.5);
	*orate = *obsize * irate / ibsize;
	DPRINTF("rate %li: %li, %li -> %li, %li\n", rate, irate, ibsize,
		*orate, *obsize);
}

static int fft_resample_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	if (filterport_get_pipe(port))
                return -1;
	return 0;
}

static int fft_resample_connect_out(filter_port_t *port, filter_pipe_t *p)
{
	filter_t *n = filterport_filter(port);
	filter_pipe_t *in;
	long rate = GLAME_DEFAULT_SAMPLERATE, bsize = 2048, osamp = 1;
	float hangle = 0.0;

	if(filterport_get_pipe(port))
		return -1;

	if ((in = filterport_get_pipe(filterportdb_get_port(
		filter_portdb(n), PORTNAME_IN)))) {
		rate = filterparam_val_long(filterparamdb_get_param(
			filter_paramdb(n), "frequency"));

		fft_resample_orate_and_obsize (rate, filterpipe_fft_rate(in),
					       filterpipe_fft_bsize(in),
					       &rate, &bsize);
		osamp = filterpipe_fft_osamp(in);
		hangle = filterpipe_fft_hangle(in);
	}
	filterpipe_settype_fft(p, rate, hangle, bsize, osamp);
	
	return 0;
}


static int fft_resample_frequency_set(filter_param_t *param, const void *val)
{
	filter_t *n = filterparam_filter(param);
	filter_pipe_t *out, *in;
	long rate = *((long *)val);
	long in_rate = GLAME_DEFAULT_SAMPLERATE, in_bsize = 2048;
	long out_rate, out_bsize;

	if (rate <= 0)
		return -1;

	if ((in = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN)))) {
		in_rate = filterpipe_fft_rate(in);
		in_bsize = filterpipe_fft_bsize(in);
	}
	if ((out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT)))) {
		fft_resample_orate_and_obsize (rate, in_rate, in_bsize,
					       &out_rate, &out_bsize);
		filterpipe_settype_fft(out, out_rate,
				       filterpipe_fft_hangle(out), out_bsize,
				       filterpipe_fft_osamp(out));

		glsig_emit(filterpipe_emitter(out), GLSIG_PIPE_CHANGED, out);
	}
	return 0;
}

static void fft_resample_fixup_pipe(glsig_handler_t *h, long sig, va_list va)
{
	filter_t	*n;
	filter_port_t   *oport;
	filter_pipe_t   *opipe, *in;
	long rate = GLAME_DEFAULT_SAMPLERATE, bsize = 2048;

	GLSIGH_GETARGS1(va, in);
	n = filterport_filter(filterpipe_dest(in));
	oport = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	opipe = filterport_get_pipe(oport);
	if(!opipe)
		return;

	rate = filterparam_val_long(filterparamdb_get_param(
		filter_paramdb(n), "frequency"));
	fft_resample_orate_and_obsize (rate, filterpipe_fft_rate(in),
				       filterpipe_fft_bsize(in),
				       &rate, &bsize);
	filterpipe_settype_fft(opipe, rate, filterpipe_fft_hangle(in),
			       bsize, filterpipe_fft_osamp(in));

	glsig_emit(filterpipe_emitter(opipe), GLSIG_PIPE_CHANGED, opipe);
}

static int fft_resample_f(filter_t *n)
{
	filter_pipe_t *in,*out;
	filter_buffer_t *inb,*outb;
	int bsize,blocks,nbsize;
	int i,len,rate,orate;
	float gain,drift;
	
	if (!(in=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN))))
		FILTER_ERROR_RETURN("no input");
	if (!(out=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");

	bsize=filterpipe_fft_bsize(in);

	nbsize = filterpipe_fft_bsize (out);
	rate = filterpipe_fft_rate (out);

	len=MIN(nbsize,bsize)/2;
	gain=(float)nbsize/(float)bsize;

	orate = filterparam_val_long(filterparamdb_get_param(
		filter_paramdb(n), "frequency"));
	drift = (float)rate/orate;
	DPRINTF("Difference in frequencies cause a drift of %.3e\n", drift);

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
	filter_port_t *out, *in;
	filter_param_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;

	in = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_FFT,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "fft stream",
			      FILTERPORT_END);
	
	in->connect = fft_resample_connect_in;

	out = filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
				    FILTER_PORTTYPE_FFT,
				    FILTER_PORTFLAG_OUTPUT,
				    FILTERPORT_DESCRIPTION, "resamppled fft stream",
				    FILTERPORT_END);
	out->connect = fft_resample_connect_out;
	
	param = filterparamdb_add_param_long(filter_paramdb(f),"frequency",
			FILTER_PARAMTYPE_LONG, 44100,
			FILTERPARAM_DESCRIPTION,"resample frequency",
			FILTERPARAM_END);
	param->set = fft_resample_frequency_set;
	
	glsig_add_handler(&f->emitter, GLSIG_PIPE_CHANGED, fft_resample_fixup_pipe, NULL);
	f->f = fft_resample_f;
	
	plugin_set(p, PLUGIN_DESCRIPTION, "Resample fft-stream");
	plugin_set(p, PLUGIN_PIXMAP, "resample.png");
	plugin_set(p, PLUGIN_CATEGORY, "FFT");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "FFT_Resample");
	
	filter_register(f,p);

	return 0;
}

static int fft_equalizer_f(filter_t *n)
{
	filter_pipe_t *in,*out;
	filter_buffer_t *inb;
	filter_param_t *param;
	int bsize, blocks;
	int i, j, step, off;
	float f[5];
	SAMPLE *s, *c;


	if (!(in=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN))))
		FILTER_ERROR_RETURN("no input");
	
	if (!(out=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");

	bsize = filterpipe_fft_bsize(in);
	DPRINTF("bsize = %d\n", bsize);
	
	if (!(c=ALLOCN(bsize, SAMPLE)))
		FILTER_ERROR_RETURN("allocation error");
	
	off = bsize/2;
	step = off/5;

	if ((param = filterparamdb_get_param(filter_paramdb(n), "low")))
		f[0] = filterparam_val_double(param);

	if ((param = filterparamdb_get_param(filter_paramdb(n), "midlow")))
		f[1] = filterparam_val_double(param);

	if ((param = filterparamdb_get_param(filter_paramdb(n), "mid")))
		f[2] = filterparam_val_double(param);
	
	if ((param = filterparamdb_get_param(filter_paramdb(n), "midhigh")))
		f[3] = filterparam_val_double(param);
	
	if ((param = filterparamdb_get_param(filter_paramdb(n), "high")))
		f[4] = filterparam_val_double(param);

	DPRINTF("f[0-4] = %f %f %f %f %f\n",f[0], f[1], f[2], f[3], f[4]);
	
	for(i=0; i<bsize/2;i++)
		c[i] = c[bsize-i] = f[MIN(i/step, 4)];
	c[bsize/2] = f[4];

	FILTER_AFTER_INIT;
	
	goto entry;
	
	while (inb) {
		FILTER_CHECK_STOP;
		
		inb=sbuf_make_private(inb);
		i = 0;
		s = sbuf_buf(inb);
		blocks = sbuf_size(inb)/bsize;
		for(i=0; i < blocks; i++) {
			for(j=0; j < bsize; j++)
				*s++ *= c[j];
		}
		sbuf_queue(out,inb);
entry:
		inb=sbuf_get(in);
	}
	sbuf_queue(out,NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(c);

	FILTER_RETURN;
}

int fft_equalizer_register(plugin_t *p)
{
	filter_t *f;
	
	if (!(f = filter_creat(NULL)))
		return -1;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_FFT,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "fft stream",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_FFT,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "equalized fft stream",
			      FILTERPORT_END);
	
	filterparamdb_add_param_double(filter_paramdb(f),"low",
			FILTER_PARAMTYPE_DOUBLE, 1.0,
			FILTERPARAM_DESCRIPTION,"low frequency gain",
			FILTERPARAM_END);

	filterparamdb_add_param_double(filter_paramdb(f),"midlow",
			FILTER_PARAMTYPE_DOUBLE, 1.0,
			FILTERPARAM_DESCRIPTION,"middle low frequency gain",
			FILTERPARAM_END);
	
	filterparamdb_add_param_double(filter_paramdb(f),"mid",
			FILTER_PARAMTYPE_DOUBLE, 1.0,
			FILTERPARAM_DESCRIPTION,"low frequency gain",
			FILTERPARAM_END);
	
	filterparamdb_add_param_double(filter_paramdb(f),"midhigh",
			FILTER_PARAMTYPE_DOUBLE, 1.0,
			FILTERPARAM_DESCRIPTION,"middle high frequency gain",
			FILTERPARAM_END);
	
	filterparamdb_add_param_double(filter_paramdb(f),"high",
			FILTER_PARAMTYPE_DOUBLE, 1.0,
			FILTERPARAM_DESCRIPTION,"high frequency gain",
			FILTERPARAM_END);

	
	f->f = fft_equalizer_f;

	plugin_set(p, PLUGIN_DESCRIPTION, "FFT 5-Band Equalizer");
	plugin_set(p, PLUGIN_PIXMAP, "equalizer.png");
	plugin_set(p, PLUGIN_CATEGORY, "FFT");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "FFT_Bandpass");
	
	filter_register(f,p);

	return 0;
}

static int fft_bandpass_f(filter_t *n){
	filter_pipe_t *in,*out;
	filter_buffer_t *inb;
	filter_param_t *param;
	int bsize, blocks;
	int i, j, off, fmin = 0, fmax = 0, freq;
	float gain = 1.0;
	SAMPLE *s, *c;


	if (!(in=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN))))
		FILTER_ERROR_RETURN("no input");
	
	if (!(out=filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");

	bsize = filterpipe_fft_bsize(in);
	freq  = filterpipe_fft_rate(in);
	DPRINTF("bsize = %d, frequency resolution = %d\n", bsize, freq/(bsize/2));
	
	if (!(c=ALLOCN(bsize, SAMPLE)))
		FILTER_ERROR_RETURN("allocation error");
	
	off = bsize/2;

	if ((param = filterparamdb_get_param(filter_paramdb(n), "band minimum")))
		fmin = filterparam_val_long(param);

	if ((param = filterparamdb_get_param(filter_paramdb(n), "band maximum")))
		fmax = filterparam_val_long(param);

	if ((param = filterparamdb_get_param(filter_paramdb(n), "gain")))
		gain = pow(10.0,filterparam_val_double(param)/20.0);
	
	fmin = fmin/(freq/off);
	fmax = MIN(fmax/(freq/off), bsize/2);
	
	DPRINTF("fft bandfilter range: [%d-%d], gain = %f\n", fmin*(freq/off), fmax*(freq/off), gain);
	if (fmin >= fmax) 
		FILTER_ERROR_RETURN("FFT Band too narrow: Increase FFT blocksize or enlarge window!\n");
	
	for(i=fmin; i<fmax; i++)
		c[i] = c[bsize-i] = gain;

	FILTER_AFTER_INIT;
	
	goto entry;
	
	while (inb) {
		FILTER_CHECK_STOP;
		
		inb=sbuf_make_private(inb);
		i = 0;
		s = sbuf_buf(inb);
		blocks = sbuf_size(inb)/bsize;
		for(i=0; i < blocks; i++) {
			for(j=0; j < bsize; j++)
				*s++ *= c[j];
		}
		sbuf_queue(out,inb);
entry:
		inb=sbuf_get(in);
	}
	sbuf_queue(out,NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(c);

	FILTER_RETURN;
}

int fft_bandpass_register(plugin_t *p)
{
	filter_t *f;
	
	if (!(f = filter_creat(NULL)))
		return -1;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_FFT,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "fft stream",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_FFT,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "fft stream band",
			      FILTERPORT_END);
	
	filterparamdb_add_param_long(filter_paramdb(f),"band minimum",
			FILTER_PARAMTYPE_LONG, 0,
			FILTERPARAM_DESCRIPTION,"Lower band frequency limit",
			FILTERPARAM_END);

	filterparamdb_add_param_long(filter_paramdb(f),"band maximum",
			FILTER_PARAMTYPE_LONG, 44100,
			FILTERPARAM_DESCRIPTION,"Upper band frequency limit",
			FILTERPARAM_END);
	
	filterparamdb_add_param_double(filter_paramdb(f),"gain",
			FILTER_PARAMTYPE_DOUBLE, 0.0,
			FILTERPARAM_DESCRIPTION,"band gain [dB]",
			FILTERPARAM_END);
	
	
	f->f = fft_bandpass_f;

	plugin_set(p, PLUGIN_DESCRIPTION, "FFT Bandpass");
	plugin_set(p, PLUGIN_PIXMAP, "bandpass.png");
	plugin_set(p, PLUGIN_CATEGORY, "FFT");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "FFT_Equalizer");
	
	filter_register(f,p);

	return 0;
}
