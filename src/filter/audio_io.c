/*
 * audio_io.c
 * $Id: audio_io.c,v 1.6 2000/02/07 00:09:07 mag Exp $
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
#include <limits.h>

#ifdef HAVE_ESD
#include <esd.h>

/* Now I better know what to do ;) 
 * and we have an input filter
 */

static int esd_in_f(filter_node_t *n)
{
	filter_pipe_t *left, *right;
	filter_buffer_t *lbuf,*rbuf;
	
	esd_format_t	format;
	int rate=44100;
	int bits = ESD_BITS16, channels = ESD_STEREO;
	int mode = ESD_STREAM, func = ESD_RECORD ;
	short int *buf;
	int sock;
	int length;
	int i,lpos,rpos;
        char *host=NULL;
	char *name=NULL;

	/* FIXME some parameter handling to be added */

	printf("esd_in_f started!\n");

	format = bits | channels | mode | func;
	sock = esd_record_stream_fallback( format, rate, host, name );
	if (sock <= 0){
		printf("Couldn't open esd socket!\n");
		return -1;
	}

	left=hash_find_output("left_out",n);
	right=hash_find_output("right_out",n);
	
	if (!left || !right){
		DPRINTF("Couldn't find output pipes!\n");
		return -1;
	}
	
	if ((buf=(short int*)malloc(ESD_BUF_SIZE))==NULL){
		DPRINTF("Couldn't alloc input buffer!\n");
		return -1;
	}

	printf("Start sampling!\n");

	while(pthread_testcancel(),1){
		length=read(sock,buf,ESD_BUF_SIZE);
		if (!length){
			DPRINTF("Read failed!\n");
			return -1;
		}
		printf("sampled %d bytes!\n",length);
		lpos=rpos=i=0;
		lbuf=fbuf_alloc(length/4);	/* FIXME 16bit stereo only */
		rbuf=fbuf_alloc(length/4);
		while(i<length/2){
			fbuf_buf(lbuf)[lpos++]=SHORT2SAMPLE(buf[i++]);
			fbuf_buf(rbuf)[rpos++]=SHORT2SAMPLE(buf[i++]);
			fbuf_queue(left,lbuf);
			fbuf_queue(right,rbuf);
		}	
	}
	fbuf_queue(left,NULL);
	fbuf_queue(right,NULL);
	free(buf);
	return 0;
}

/* I don't know what I'm doing, but I just try to 
 * write a simple esound output filter... */
static int esd_out_f(filter_node_t *n)
{
	filter_pipe_t *left, *right;
	filter_buffer_t *lbuf, *rbuf;
	int lpos, rpos;
	int wbpos;
	
	esd_format_t format;
	int rate=44100;
	int esound_socket;
	char *host = NULL;
	char *name = NULL;
        short int *wbuf;
	int written,size,cnt=0;
	
	DPRINTF("esd_out_f started!\n");

	format = ESD_BITS16 | ESD_STREAM | ESD_PLAY;

	/* query both input channels, if left only -> MONO,
	 * else STEREO output. */
	if (!(right = hash_find_input("right_in", n)))
		format |= ESD_MONO;
	else{
		format |= ESD_STEREO;
		printf("Stereo!\n");
	}
	if (!(left = hash_find_input("left_in", n)))
		return -1;

	DPRINTF("Trying to open esd-socket!\n");
	
	esound_socket = esd_play_stream_fallback(format, rate, host, name);
	if (esound_socket<0){
	        DPRINTF("couldn't open esd-socket connection!\n");
	        return -1;
        }

	wbuf=(short int*)malloc(GLAME_WBUFSIZE*sizeof(short int));
	wbpos=0;
	if (wbuf==NULL){
		DPRINTF("esd-filter: couldn't alloc wbuf!\n");
		return -1;
	}


	/* get the first buffers to start with something */
	lbuf = fbuf_get(left);
	DPRINTF("Got left fbuf with size %d\n",fbuf_size(lbuf));
	if (right){
		rbuf = fbuf_get(right);
		printf("Got right fbuf with size %d\n",fbuf_size(rbuf));
	}
	else
		rbuf = NULL;
	lpos = rpos = 0;

	do {
		/* write into wbuf until wbuf is full or
		 * either left or right is empty.
		 * if either channel gets empty during play
		 * we fill in zeros for it. */
		while (wbpos < GLAME_WBUFSIZE
		       && lpos < fbuf_size(lbuf)
		       && (!right || rpos < fbuf_size(rbuf))) {
			if (lbuf)
				wbuf[wbpos++] = SAMPLE2SHORT(fbuf_buf(lbuf)[lpos++]);
			else
				wbuf[wbpos++] = 0;
			if (right) {
				if (rbuf)
					wbuf[wbpos++] = SAMPLE2SHORT(fbuf_buf(rbuf)[rpos++]);
				else
					wbuf[wbpos++] = 0;
			}
		}
		/* send audio data to esd */
		written=0;
		while(written<wbpos*sizeof(short int)){
			size=write(esound_socket, wbuf+written, wbpos*sizeof(short int));
			if(size<0){
				DPRINTF("Error %d in write!\n",size);
			}else {
				written+=size;
				if (size!=wbpos*sizeof(short int)) DPRINTF("%d bytes written!\n",size);
			}
		}
			
		wbpos = 0;

		/* check, if we need new data */
		if (lpos >= fbuf_size(lbuf)) {
			fbuf_unref(lbuf);
			lbuf = fbuf_get(left);
			lpos = 0;
			cnt++;
		}
		if (right && rpos >= fbuf_size(rbuf)) {
			fbuf_unref(rbuf);
			rbuf = fbuf_get(right);
			rpos = 0;
			cnt++;
		}
	} while (lbuf || (right && rbuf));

	printf("Received %d buffers.\n",cnt);
	return 0;
}
#endif



int audio_io_register()
{
	filter_t *f;

#if defined HAVE_ESD
	if (!(f = filter_alloc("audio_out", "play stream", esd_out_f))
	    || filter_add_input(f, "left_in", "left or mono channel",
				FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add_input(f, "right_in", "right channel",
				FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("audio_in","record stream",esd_in_f))
	    || filter_add_output(f,"left_out","left channel",
		    		FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add_output(f,"right_out","right channel",
		    		FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;

#elif defined HAVE_OSS
#elif defined HAVE_SUNAUDIO
#endif

	return 0;
}




