/*
 * waveform.c
 * $Id: waveform.c,v 1.14 2000/02/17 17:58:36 nold Exp $
 *
 * Copyright (C) 1999, 2000 Alexander Ehlert
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
 * This file contains a set of filters that generate various waveforms
 * Contained filters are
 * - sinus
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include <math.h>

/* This filter generates a sinus test signal
 * defaults to 400z signal of 10000ms duration
 */
static int sinus_f(filter_node_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	SAMPLE 	ampl;
	float	freq;
	int duration,i,size,cnt;
	
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!out) 
		return -1;

	/* sane defaults */
	ampl = 0.5;
	freq = 441.0;
	duration = 10000;

	/* user overrides with parameters */
	if ((param = filternode_get_param(n, "amplitude")))
		ampl = filterparam_val_sample(param);
	if ((param = filternode_get_param(n, "frequency")))
		freq = filterparam_val_float(param);
	if ((param = filternode_get_param(n, "duration")))
		duration = filterparam_val_int(param);
	

	size = (int)(44100.0/freq);
	cnt = (int)(freq*duration/1000.0);
	if (!(buf = sbuf_alloc(size, n)))
		return -1;

	DPRINTF("cnt=%d\n",cnt);
	DPRINTF("Allocated Buffer with size %d(%i)! Generating Sinus!\n",
		size, sbuf_size(buf));
	buf = sbuf_make_private(buf);
        for (i=0; i<size; i++)
		sbuf_buf(buf)[i] = ampl*sinf(i*2*M_PI/size);

	FILTER_AFTER_INIT;

	while(pthread_testcancel(), cnt--){
		sbuf_ref(buf);
		sbuf_queue(out, buf);
	}
	sbuf_queue(out, NULL);
	DPRINTF("All buffers sent!\n");

	FILTER_BEFORE_CLEANUP;
	sbuf_unref(buf);

	return 0;
}

static int sinus_connect_out(filter_node_t *n, const char *port,
			     filter_pipe_t *p)
{
	filterpipe_settype_sample(p, 44100, FILTER_PIPEPOS_DEFAULT);
	return 0;
}


/* Registry setup of all contained filters
 */
int waveform_register()
{
	filter_t *f;

	if (!(f = filter_alloc("sinus", "generate sinus test signal", sinus_f))
	    || !filter_add_output(f, PORTNAME_OUT, "sinus output stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f,"amplitude","sinus peak amplitude(0.0-1.0)",
				 FILTER_PARAMTYPE_SAMPLE)
	    || !filter_add_param(f,"frequency","sinus frequency in Hz",
				 FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f,"duration","length of signal in ms",
				 FILTER_PARAMTYPE_INT))
		return -1;
	f->connect_out = sinus_connect_out;
	if (filter_add(f) == -1)
		return -1;

	return 0;
}







