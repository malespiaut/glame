/*
 * echo.c
 * $Id: echo.c,v 1.8 2000/02/17 17:58:36 nold Exp $
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
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"


static int echo_f(filter_node_t *n)
{
	filter_buffer_t *bin,*bout;
	filter_pipe_t *in,*out;
	filter_param_t *param;
	
	int echotime;
	int bufsiz;
	SAMPLE *ring=NULL;
	int ringp;
	float mix;
	int binpos,boutpos;
        int sent=0,received=0;
	
	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);

	if(!in || !out){
		DPRINTF("Couldn't find ports!\n");
		return -1;
	}

	if((param = filternode_get_param(n, "time")))
		echotime=filterparam_val_int(param);
	else
		echotime=100;

	if((param = filternode_get_param(n, "mix")))
		mix=filterparam_val_float(param);
	else
		mix=0.7;
	
	bufsiz=44100*echotime/1000;
	if(bufsiz==0) bufsiz=1;
	
	if ((ring=(SAMPLE *)malloc(bufsiz*sizeof(SAMPLE)))==NULL){
		DPRINTF("Couldn't alloc ringbuffer!\n");
		return -1;
	}

	FILTER_AFTER_INIT;

	memset(ring,0,bufsiz*sizeof(SAMPLE));
	ringp=0;
	bin=sbuf_get(in);
	binpos=0;
	
	while(pthread_testcancel(),bin){
		received++;
		bout=sbuf_alloc(sbuf_size(bin), n);
		boutpos=0;
		while(binpos<sbuf_size(bin)){
			ring[ringp++]=sbuf_buf(bin)[binpos];
			if (ringp==bufsiz)
				ringp=0;
			sbuf_buf(bout)[boutpos++]=(sbuf_buf(bin)[binpos]+mix*ring[ringp])/(1.0+mix);
			binpos++;
		}
		sbuf_queue(out,bout);
		sent++;
		sbuf_unref(bin);
		bin=sbuf_get(in);
		binpos=0;
	}

	/* Empty ring buffer */
	
	bout=sbuf_alloc(bufsiz,n);
	for(boutpos=0;boutpos<bufsiz;boutpos++){
		sbuf_buf(bout)[boutpos]=(ring[ringp++]*mix)/(1.0+mix);
		if (ringp==bufsiz)
			ringp=0;
	}
	sbuf_queue(out,bout);
	sbuf_queue(out,NULL);
	sent++;

	DPRINTF("sent=%d received=%d\n",sent,received);
	
	FILTER_BEFORE_CLEANUP;
	free(ring);
	return 0;
}

/* Registry setup of all contained filters
 */
void echo_fixup_break_in(filter_node_t *n, filter_pipe_t *in)
{
	filter_pipe_t *out;
	/* FIXME Hmm if input pipe breaks, just disconnect output pipe.
	 * [richi] As we can cope with weird in/out in f(), this automatic
         *         breakage can lead to a collapse of the entire network
	 *         which is not "nice" for the user.
	 *         So please break connections only actively, not passively
	 *         (i.e. a parameter which forbids an output/input may break
	 *         it, but a missing input/output should not break the other
	 *         one - chicken and egg problem.) */
	out=filternode_get_output(n, PORTNAME_OUT);
	if (out) filternetwork_break_connection(out);
}

int echo_register()
{
	filter_t *f;

	if (!(f = filter_alloc("echo", "echo effect", echo_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT,"output",
		    		FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f,"time","echo time in ms",
		    		FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f,"mix","mixer ratio",
		    		FILTER_PARAMTYPE_FLOAT))
		return -1;

	f->fixup_break_in = echo_fixup_break_in;

	if (filter_add(f) == -1)
		return -1;
	return 0;
}
