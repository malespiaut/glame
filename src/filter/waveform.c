/*
 * waveform.c
 * $Id: 
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
	SAMPLE ampl,freq;
	int duration,i,size,cnt;
	
	out = hash_find_output("output", n);
	if (!out) return -1;
	
	if ((param = hash_find_param("amplitude",n)))
		ampl = param->val.sample;
	else
		ampl = 0.5;
	if ((param = hash_find_param("frequency",n)))
		freq = param->val.sample;
	else
		freq = 441.0;
	if ((param = hash_find_param("duration",n)))
		duration = param->val.i;
	else
		duration = 10000;
	
	size=(int)(44100.0/freq);
	if ((buf = fbuf_alloc(size, SAMPLE_SIZE, n))==NULL) return -1;

	cnt=(int)(44100.0/size*duration/1000.0);

	DPRINTF("cnt=%d\n",cnt);
	DPRINTF("Allocated Buffer with size %d! Generating Sinus!\n",size);
        for(i=0;i<size;i++) fbuf_buf(buf)[i]=ampl*sin(i*2*M_PI/size);

	FILTER_AFTER_INIT;
	DPRINTF("Semaphores done!\n");

	while(pthread_testcancel(),cnt--){
		fbuf_ref(buf);
		DPRINTF("Sent Buffer %d\n",cnt);
		fbuf_queue(out,buf);
	}
	fbuf_queue(out,NULL);			

	FILTER_BEFORE_CLEANUP;
	DPRINTF("Exiting.\n");
	fbuf_unref(buf);

	return 0;
}



/* Registry setup of all contained filters
 */
int waveform_register()
{
	filter_t *f;

	if (!(f = filter_alloc("sinus", "generate sinus test signal", sinus_f))
	    || !filter_add_output(f, "output", "sinus output stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f,"amplitude","sinus peak amplitude(0.0-1.0)",
				 FILTER_PARAMTYPE_SAMPLE)
	    || !filter_add_param(f,"frequency","sinus frequency in Hz",
				 FILTER_PARAMTYPE_SAMPLE)
	    || !filter_add_param(f,"duration","length of signal in ms",
				 FILTER_PARAMTYPE_INT)
	    || filter_add(f) == -1)
		return -1;

	return 0;
}







