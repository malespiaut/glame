/*
 * read_file.c
 * $Id: read_file.c,v 1.3 2000/02/02 15:13:32 nold Exp $ 
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
	int rpos,lpos,frames,sent=0;
	int i=0;
	
	left = hash_find_output("left",n);
	right = hash_find_output("right",n);

	if (!left || !right)
		return -1;

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

	printf("framesize %d\n",frameSize);
	printf("channelCount %d\n",channelCount);

	if ((sampleFormat != AF_SAMPFMT_TWOSCOMP) && (sampleFormat != AF_SAMPFMT_UNSIGNED)){
		DPRINTF("Format not yet supported!\n");
		goto _bailout;
	}
	
        if ((sampleWidth != 16) || (channelCount > 2)){
		DPRINTF("Only 16bit samples and max. 2 channels allowed!\n");
		goto _bailout;
	}

	buffer=(short int*)malloc(GLAME_WBUFSIZE);

	while(pthread_testcancel(),frameCount){
		frames=afReadFrames(file, AF_DEFAULT_TRACK, buffer,GLAME_WBUFSIZE/frameSize);
		frameCount-=frames;
		printf("Read %d frames!\n",i);
		if (channelCount==1){
			lbuf=fbuf_alloc(frames);
			for(i=0;i<frames;i++) fbuf_buf(lbuf)[i]=SHORT2SAMPLE(buffer[i]);
			fbuf_ref(lbuf);
			fbuf_queue(left,lbuf);
			fbuf_queue(right,lbuf);
			sent+=2;
		}else{
			lbuf=fbuf_alloc(GLAME_WBUFSIZE*4/sampleWidth);
			rbuf=fbuf_alloc(GLAME_WBUFSIZE*4/sampleWidth);
			i=0;
			rpos=0;
			lpos=0;
			while(i<GLAME_WBUFSIZE*8/sampleWidth){
				fbuf_buf(lbuf)[lpos++]=SHORT2SAMPLE(buffer[i++]);
				fbuf_buf(rbuf)[rpos++]=SHORT2SAMPLE(buffer[i++]);
			}
			fbuf_queue(left,lbuf);
			fbuf_queue(right,rbuf);
			sent+=2;
		}
	}

	fbuf_queue(left,NULL);
	fbuf_queue(right,NULL);

	free(buffer);
	afCloseFile(file);
	
	printf("sent %d buffers\n",sent);
	return 0;
_bailout:
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
	if (filter_add_output(f, "left", "left channel",FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add_output(f, "right", "right channel",FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add_param(f,"filename","filename",FILTER_PARAMTYPE_STRING) == -1)
		return -1;
	 if (filter_add(f) == -1)
		return -1;

	return 0;
}
