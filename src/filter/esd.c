/*
 * mix.c
 * $Id: esd.c,v 1.3 2000/01/27 13:32:19 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther, Alexander Ehlert
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
#ifdef HAVE_ESD
#include <esd.h>
#endif
#include <limits.h>

/* I don't know what I'm doing, but I just try to 
 * write a simple esound output filter... */
int esd(filter_node_t *n)
{
#ifndef HAVE_ESD
	return -1;
#else
	filter_buffer_t **in;
	int i, eofs, *pos, wbpos;
	SAMPLE s;
	
	esd_format_t format = ESD_BITS16 | ESD_STREAM | ESD_PLAY;
	int rate=44100;
	int esound_socket;
	char *host = NULL;
	char *name = NULL;
        short int *wbuf;
	
	if(n->nr_inputs==1){
		format|=ESD_MONO;
	}else if(n->nr_inputs==2){
		format|=ESD_STEREO;
	}else{
		printf("FIXME: esd-output only supports mono or stereo playback!\n");
		return -1;
	}

	esound_socket = esd_play_stream_fallback(format, rate, host, name);
	if (esound_socket<0){
	        printf("couldn't open esd-socket connection!\n");
	        return -1;
        }

	wbuf=(short int*)malloc(GLAME_WBUFSIZ*sizeof(short int));
	wbpos=0;
	if (wbuf==NULL){
		printf("esd-filter: couldn't alloc wbuf!\n");
		return -1;
	}
	
	/* init */
	eofs = 0;
	if (!(in = ALLOCN(n->nr_inputs, filter_buffer_t *))
	    || !(pos = ALLOCN(n->nr_inputs, int))) {
		free(in);
		return -1;
	}

	/* get first input buffers from all channels */
	for (i=0; i<n->nr_inputs; i++) {
		if (!(in[i] = fbuf_get(n->inputs[i])))
			eofs++;
		pos[i] = 0;
	}

	while (pthread_testcancel(), (eofs != n->nr_inputs)) {
		s = 0;
		for (i=0; i<n->nr_inputs; i++) {
			/* check, if we need a new input buffer */
			if (in[i] && (fbuf_size(in[i]) == pos[i])) {
				/* unref the processed buffer */
			        fbuf_unref(in[i]);
				/* get new buffer */
				if (!(in[i] = fbuf_get(n->inputs[i])))
					eofs++;
				pos[i] = 0;
			}
			/* send audio data to esd */
			wbuf[wbpos]=(short int)s*SHRT_MAX;
			wbpos++;
			if(wbpos==GLAME_WBUFSIZ*sizeof(short int)){
				write(esound_socket,wbuf,GLAME_WBUFSIZ*sizeof(short int));
				wbpos=0;
			}
		}
	}

	if(wbpos>0) write(esound_socket,wbuf,wbpos);
	
	/* cleanup - FIXME, we need to do this with a
	 * pthread_cleanup_pop() - and previous
	 * pthread_cleanup_push() - ugh! */
	free(wbuf);
	free(in);
	free(pos);

	return 0;
#endif
}



