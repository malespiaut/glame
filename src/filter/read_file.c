/*
 * read_file.c
 * $Id: read_file.c,v 1.12 2000/02/09 15:37:37 richi Exp $ 
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef HAVE_AUDIOFILE
#include "filter.h"

int read_file_register()
{
	return 0;
}

#else

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include <audiofile.h>
#include <string.h>
#include <math.h>

static int read_file_f(filter_node_t *n)
{
	filter_pipe_t *left,*right;
	filter_buffer_t	*lbuf,*rbuf;
	filter_param_t	*param;
	char	*filename;
	AFfilehandle    file;
        AFframecount    frameCount;
	int sampleFormat,sampleWidth,channelCount,frameSize;
	short int *buffer;
	char *cbuffer;
	int rpos,lpos,olpos,orpos,frames,sent=0;
	int i,j;
	double sampleRate;
	int sclfak;

	DPRINTF("read-file started!\n");

	left = hash_find_output(PORTNAME_LEFT_OUT, n);
	right = hash_find_output(PORTNAME_RIGHT_OUT, n);

	if (!left || !right){
		DPRINTF("Couldn't find channels!\n");
		return -1;
	}

	if ((param = hash_find_param("filename",n))) {
		filename=strdup(param->val.string);
	} else {
		DPRINTF("Missing parameter filename!\n");
		return -1;
	}
	
	if ((file=afOpenFile(filename,"r",NULL))==NULL){
		DPRINTF("File not found!\n");
		return -1;
	}

	frameCount=afGetFrameCount(file, AF_DEFAULT_TRACK);
	channelCount = afGetChannels(file, AF_DEFAULT_TRACK);
	afGetSampleFormat(file, AF_DEFAULT_TRACK, &sampleFormat, &sampleWidth);
	frameSize = afGetFrameSize(file, AF_DEFAULT_TRACK, 1);
	sampleRate = afGetRate(file, AF_DEFAULT_TRACK);
	
	sclfak=(int)(44100.0/sampleRate);	/* FIXME Quickhack, should be done by resample filter and better...*/
	
	DPRINTF("framesize=%d channelCount=%d frameCount=%d sampleRate=%.2f sclfak=%d\n",frameSize,channelCount,(int)frameCount,sampleRate,sclfak);

	if ((sampleFormat != AF_SAMPFMT_TWOSCOMP) && (sampleFormat != AF_SAMPFMT_UNSIGNED)){
		DPRINTF("Format not yet supported!\n");
		goto _bailout;
	}
	
        if ((channelCount > 2)){
		DPRINTF("Max. 2 channels allowed!\n");
		goto _bailout;
	}

	if ((buffer=(short int*)malloc(GLAME_WBUFSIZE*frameSize))==NULL){
		DPRINTF("Couldn't allocate buffer!\n");
		goto _bailout;
	}

	FILTER_AFTER_INIT;

	cbuffer=(char *)buffer;

	while(pthread_testcancel(),frameCount){
		if (!(frames=afReadFrames(file, AF_DEFAULT_TRACK, buffer,MIN(GLAME_WBUFSIZE,frameCount))))
				break;
		frameCount-=frames;
		if (channelCount==1){
			lbuf=sbuf_alloc(frames*sclfak, n);
			i=0;
			lpos=0;
			while(i<frames){
				olpos=lpos;
				switch(sampleWidth) {
				case 16 : 
					sbuf_buf(lbuf)[lpos++]=SHORT2SAMPLE(buffer[i++]);
					break;
				case  8 : 
					sbuf_buf(lbuf)[lpos++]=CHAR2SAMPLE(cbuffer[i++]);
					break;
				}
				for(j=1;j<sclfak;j++) sbuf_buf(lbuf)[lpos++]=sbuf_buf(lbuf)[olpos];
			}
			sbuf_ref(lbuf);
			sbuf_queue(left,lbuf);
			sbuf_queue(right,lbuf);
			sent+=2;
		}else{
			lbuf=sbuf_alloc(frames*sclfak, n);
			rbuf=sbuf_alloc(frames*sclfak, n);
			i=0;
			rpos=0;
			lpos=0;
			while(i<frames*2){
				olpos=lpos;
				orpos=rpos;
				switch(sampleWidth){
				case 16 :
					sbuf_buf(lbuf)[lpos++]=SHORT2SAMPLE(buffer[i++]);
					sbuf_buf(rbuf)[rpos++]=SHORT2SAMPLE(buffer[i++]);
					break;
				case  8 :
					sbuf_buf(lbuf)[lpos++]=CHAR2SAMPLE(cbuffer[i++]);
					sbuf_buf(rbuf)[rpos++]=CHAR2SAMPLE(cbuffer[i++]);
					break;
				}
				for(j=1;j<sclfak;j++){
					sbuf_buf(lbuf)[lpos++]=sbuf_buf(lbuf)[olpos];
					sbuf_buf(rbuf)[rpos++]=sbuf_buf(rbuf)[orpos];
				}
			}
			sbuf_queue(left,lbuf);
			sbuf_queue(right,rbuf);
			sent+=2;
		}
	}

	sbuf_queue(left,NULL);
	sbuf_queue(right,NULL);

	FILTER_BEFORE_CLEANUP;

	free(buffer);
	afCloseFile(file);
	
	DPRINTF("sent %d buffers\n",sent);
	return 0;
_bailout:
	DPRINTF("read-file bailout!\n");
	afCloseFile(file);
	return -1;
}


/* Registry setup of all contained filters
 */
int read_file_register()
{
	filter_t *f;

	if (!(f = filter_alloc("read_file", "reads audiofile", read_file_f)))
		return -1;
	if (!filter_add_output(f, PORTNAME_LEFT_OUT, "left channel",
			       FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_RIGHT_OUT, "right channel",
				  FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f,"filename","filename",FILTER_PARAMTYPE_STRING))
		return -1;
	 if (filter_add(f) == -1)
		return -1;

	return 0;
}

#endif

