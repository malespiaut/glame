/*
 * read_file.c
 * $Id: file_io.c,v 1.7 2000/02/20 15:26:29 richi Exp $ 
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

int file_io_register()
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

static int write_file_f(filter_node_t *n)
{
	filter_pipe_t *left,*right;
	filter_buffer_t *lbuf,*rbuf;
	filter_param_t *param;
	int sampleFormat,sampleWidth,channelCount,frameSize;
	AFfilehandle    file=AF_NULL_FILEHANDLE;
	char *filename=NULL;
	int format;
	AFfilesetup	fsetup;
	short	*wbuf=NULL;
	int res=-1;
	int wbpos,framesWritten,lpos,rpos;
	
	DPRINTF("write-file started!\n");

	left = filternode_get_input(n, PORTNAME_LEFT_IN);
	right = filternode_get_input(n, PORTNAME_RIGHT_IN);

	channelCount=0;
	
	if(left)  channelCount++;
	if(right){
		if(!left){ 
			left=right;
			right=NULL;
		}
		channelCount++;
	}
	if(channelCount==0) {
		DPRINTF("No input channels\n");
		goto _bailout;
	} else DPRINTF("channelCount=%d\n",channelCount);

	if ((param = filternode_get_param(n, "ext"))) {
		if(strcmp(param->val.string,"wav")==0)
			format=AF_FILE_WAVE;
		else {
			DPRINTF("unknown fileformat!\n");
			goto _bailout;
		}
	} else format=AF_FILE_WAVE;

	if ((param = filternode_get_param(n, "filename")))
			filename=strdup(param->val.string);
	else {
		DPRINTF("missing filename!\n");
		goto _bailout;
	}
	fsetup=afNewFileSetup();
	afInitFileFormat(fsetup,format);
	afInitChannels(fsetup, AF_DEFAULT_TRACK, channelCount);
	afInitSampleFormat(fsetup, AF_DEFAULT_TRACK, AF_SAMPFMT_TWOSCOMP, 16);
	file=afOpenFile(filename, "w", fsetup);
	if (file==AF_NULL_FILEHANDLE){
		DPRINTF("couldn't open %s\n",filename);
		goto _bailout;
	}

	if ((wbuf=(short*)malloc(GLAME_WBUFSIZE*sizeof(short)))==NULL){
		DPRINTF("couldn't allocate writebuffer!\n");
		goto _bailout;
	}
	
	FILTER_AFTER_INIT;

	lbuf=sbuf_get(left);
	if(right) rbuf=sbuf_get(right);
	else rbuf=NULL;

	do {
		while (wbpos < GLAME_WBUFSIZE && (lbuf || rbuf)) {
	               if (lbuf) 
			       wbuf[wbpos++] = SAMPLE2SHORT(sbuf_buf(lbuf)[lpos++]);
	               else wbuf[wbpos++] = 0;
		       if (right) { 
			       if (rbuf) 
				       wbuf[wbpos++] = SAMPLE2SHORT(sbuf_buf(rbuf)[rpos++]); 
			       else wbuf[wbpos++] = 0; 
		       }
		       if(lpos==sbuf_size(lbuf)){
			       sbuf_unref(lbuf);
			       lbuf=sbuf_get(left);
			       lpos=0;
		       }
		       if ( (right) && (rpos==sbuf_size(rbuf))){
			       sbuf_unref(rbuf);
			       rbuf=sbuf_get(right);
			       rpos=0;
		       }
		}
		if((wbpos%channelCount)==1) DPRINTF("Oergs\n");
		framesWritten = afWriteFrames(file, AF_DEFAULT_TRACK, wbuf, wbpos/channelCount);
		wbpos=0;
	} while (lbuf || (right && rbuf));

	afCloseFile(file);
	afFreeFileSetup(fsetup);

	res=0;
_bailout:
	if(filename) free(filename);
	if(wbuf) free(wbuf);
	if (res==-1) {
		if (file!=AF_NULL_FILEHANDLE) afCloseFile(file);
	        FILTER_AFTER_INIT;
	}
	FILTER_BEFORE_CLEANUP;
	return res;
}
	

static int read_file_f(filter_node_t *n)
{
	typedef struct {
		filter_pipe_t 	*p;
		filter_buffer_t *buf;
		int		pos;
	} track_t;
	filter_pipe_t	*p_out;
	filter_param_t	*param;
	int		activecnt;
	track_t		*track=NULL;
	
	AFfilehandle    file;
        AFframecount    frameCount;
	int 		sampleFormat,sampleWidth,channelCount,frameSize;
	short int 	*buffer;
	char 		*cbuffer;
	int 		frames;
	int 		i,j;
	double 		sampleRate;

	DPRINTF("read-file started!\n");

	p_out = filternode_get_output(n, PORTNAME_OUT);

	if (!p_out){
		DPRINTF("Couldn't find channels!\n");
		return -1;
	}

	if (!(param = filternode_get_param(n, "filename"))) {
		DPRINTF("Missing parameter filename!\n");
		return -1;
	}

	DPRINTF("Opening %s\n",filterparam_val_string(param));

	if ((file=afOpenFile(filterparam_val_string(param),"r",NULL))==NULL){
		DPRINTF("File not found!\n");
		return -1;
	}

	frameCount=afGetFrameCount(file, AF_DEFAULT_TRACK);
	channelCount = afGetChannels(file, AF_DEFAULT_TRACK);
	afGetSampleFormat(file, AF_DEFAULT_TRACK, &sampleFormat, &sampleWidth);
	frameSize = afGetFrameSize(file, AF_DEFAULT_TRACK, 1);
	sampleRate = afGetRate(file, AF_DEFAULT_TRACK);
	
	if ((sampleFormat != AF_SAMPFMT_TWOSCOMP) && (sampleFormat != AF_SAMPFMT_UNSIGNED)){
		DPRINTF("Format not yet supported!\n");
		goto _bailout;
	}
	
	if ((buffer=(short int*)malloc(GLAME_WBUFSIZE*frameSize))==NULL){
		DPRINTF("Couldn't allocate buffer!\n");
		goto _bailout;
	}

	FILTER_AFTER_INIT;

	activecnt=0;
	filternode_foreach_output(n,p_out) activecnt++;

	DPRINTF("%d pipes active\n",activecnt);

	if (activecnt<channelCount){
		DPRINTF("Not enough output pipes connected!\n");
		goto _cleanup;
	} else {
		if (!(track=(track_t *)malloc(activecnt*sizeof(track_t)))){
			DPRINTF("alloc track failed!\n");
			goto _cleanup;
		}
		
		/* store active pipes */
		p_out=filternode_get_output(n, PORTNAME_OUT);
		i=0;
		do {
			track[i].p=p_out;
			/* FIXME is it ok to set this stuff in here ? 
			 * where else...
			 * It's not ok here, but IMHO sampleRate should go into sbuf -> more flexible!
			 */
			p_out->type = FILTER_PIPETYPE_SAMPLE;
			p_out->u.sample.rate = (int)sampleRate;
			i++;
			p_out=filternode_next_output(p_out);
		} while(i!=channelCount);
	
		/* send EOF's to all useless output pipes */
		while(p_out){
			sbuf_queue(p_out,NULL);
			p_out=filternode_next_output(p_out);
		}
	}
	cbuffer=(char *)buffer;

	while(pthread_testcancel(),frameCount){
		if (!(frames=afReadFrames(file, AF_DEFAULT_TRACK, buffer,MIN(GLAME_WBUFSIZE,frameCount))))
				break;
		frameCount-=frames;

		for(i=0;i<channelCount;i++){
			track[i].buf=sbuf_alloc(frames,n);
			track[i].buf=sbuf_make_private(track[i].buf);
			track[i].pos=0;
			if(i<=SBUF_POS_RIGHT)
				sbuf_pos(track[i].buf)=i;
			else
				sbuf_pos(track[i].buf)=SBUF_POS_ANY;
		}
		i=0;
		while(i<frames*channelCount){
			for(j=0;j<channelCount;j++){
				switch(sampleWidth) {
				case 16 : 
					sbuf_buf(track[j].buf)[track[j].pos++]=SHORT2SAMPLE(buffer[i++]);
					break;
				case  8 : 
					sbuf_buf(track[j].buf)[track[j].pos++]=CHAR2SAMPLE(cbuffer[i++]);
					break;
				}
			}
		}	
		for(i=0;i<channelCount;i++) sbuf_queue(track[i].p,track[i].buf);
	}

_cleanup:
	/* clean exit, send EOF to each pipe
	 * FIXME does it matter if NULL is sent twice to a port?
	 */
	filternode_foreach_output(n,p_out)
		sbuf_queue(p_out,NULL);
	FILTER_BEFORE_CLEANUP;

	free(track);
	free(buffer);
	afCloseFile(file);
	
	return 0;
_bailout:
	DPRINTF("read-file bailout!\n");
	afCloseFile(file);
	return -1;
}

#if 0

int read_file_connect_out(filter_node_t *n, const char *port,
			  filter_pipe_t *p)
{
	/* FIXME */
	filterpipe_settype_sample(p, 44100, FILTER_PIPEPOS_DEFAULT);

	return 0;
}

int read_file_fixup_param(filter_node_t *n, filter_pipe_t *p,
			  const char *name, filter_param_t *prm)
{
	filter_pipe_t *left,*right;
	filter_param_t *param;
	char *filename;
	AFfilehandle    file;
	int channelCount;
	
	if ((param = filternode_get_param(n, "filename"))) {
                 filename=strdup(param->val.string);
        } else {
                 DPRINTF("Missing parameter filename!\n");
                 return -1;
        }
	
	if ((file=afOpenFile(filename,"r",NULL))==NULL){
	         DPRINTF("File not found!\n");
	         return -1;
	}

	channelCount = afGetChannels(file, AF_DEFAULT_TRACK);
	afCloseFile(file);
	
	left = filternode_get_output(n, PORTNAME_LEFT_OUT);
	right = filternode_get_output(n, PORTNAME_RIGHT_OUT);
	
	if (left && right && (channelCount==1)){
		DPRINTF("Running mono mode!\n");
		filternetwork_break_connection(right);
	}else{
		DPRINTF("Running stereo mode!\n");
	}
	return 0;
}

#endif

/* Registry setup of all contained filters
 */
int file_io_register()
{
	filter_t *f;

	if (!(f = filter_alloc("read_file", "This filter reads various audiofile formats\n"
					    "supported by libaudiofile including:       \n"
					    " - Microsoft Wav                 (.wav)    \n"
					    " - Sun audio                     (.au)     \n"
					    " - Audio Interchange File Format (.aiff)   \n"
					    " etc.                                      \n"
					    " For more information consult libaudiofile \n"
					    " documentation.                            \n\n"
					    "(c) Alexander Ehlert 2000                  \n", read_file_f)))
		return -1;
	if (!filter_add_output(f, PORTNAME_OUT, "output channels",
			       FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC)
	    || !filter_add_param(f,"filename","filename",FILTER_PARAMTYPE_STRING))
		return -1;
#if 0
	f->connect_out = read_file_connect_out;
	f->fixup_param = read_file_fixup_param;
#endif	
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("write_file", "writes audiofile", write_file_f)))
		return -1;
	if (!filter_add_input(f, PORTNAME_LEFT_IN, "left channel",
				FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_input(f, PORTNAME_RIGHT_IN, "right channel",
		    		 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f,"filename","filename",FILTER_PARAMTYPE_STRING))
		return -1;

	if (filter_add(f) == -1)
		return -1;
	
	return 0;
}

#endif

