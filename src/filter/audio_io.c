/*
 * audio_io.c
 * $Id: audio_io.c,v 1.16 2000/02/09 12:33:24 richi Exp $
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

	DPRINTF("esd_in_f started!\n");

	format = bits | channels | mode | func;
	sock = esd_record_stream_fallback( format, rate, host, name );
	if (sock <= 0){
		DPRINTF("Couldn't open esd socket!\n");
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

	DPRINTF("Start sampling!\n");

	FILTER_AFTER_INIT;
	
	while(pthread_testcancel(),1){
		length=read(sock,buf,ESD_BUF_SIZE);
		if (!length){
			DPRINTF("Read failed!\n");
			return -1;
		}
		DPRINTF("sampled %d bytes!\n",length);
		lpos=rpos=i=0;
		lbuf=sbuf_alloc(length/4,n);	/* FIXME 16bit stereo only */
		rbuf=sbuf_alloc(length/4,n);
		while(i<length/2){
			sbuf_buf(lbuf)[lpos++]=SHORT2SAMPLE(buf[i++]);
			sbuf_buf(rbuf)[rpos++]=SHORT2SAMPLE(buf[i++]);
			sbuf_queue(left,lbuf);
			sbuf_queue(right,rbuf);
		}	
	}
	sbuf_queue(left,NULL);
	sbuf_queue(right,NULL);

	FILTER_BEFORE_CLEANUP;

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
	
	int rate;
	esd_format_t	format = ESD_BITS16|ESD_STREAM|ESD_PLAY;
	int esound_socket;
	char *host = NULL;
	char *name = NULL;
        short int *wbuf = NULL;
	int written,size,cnt=0;
	
	/* query both input channels, one channel only -> MONO
	 * (always left), else STEREO output (but with the same
	 * samplerate, please!). */
	left = hash_find_input("left_in", n);
	right = hash_find_input("right_in", n);
	/* right only? */
	if (!left) {
		left = right;
		right = NULL;
	}
	/* no channel? */
	if (!left)
		return -1;
	rate = left->u.sample.rate;
	/* right channel different sample rate? */
	if (right && right->u.sample.rate != rate)
		return -1;
	/* finally decide mono/stereo */
	if (!right)
		format |= ESD_MONO;
	else
		format |= ESD_STEREO;

	wbpos = 0;
	wbuf = (short int*)malloc(GLAME_WBUFSIZE*sizeof(short int));
	if (wbuf==NULL){
		DPRINTF("couldn't alloc wbuf!\n");
		return -1;
	}

	if(!rate){
		DPRINTF("Input filter didn't supply rate info, assuming 44,1kHz\n");
		rate=44100;
	}
	
	if(format&ESD_MONO) 
		DPRINTF("Playing mono!\n");
	else
		DPRINTF("Playing stereo!\n");
	
	DPRINTF("Trying to open esd-socket with rate=%d!\n",rate);
	esound_socket = esd_play_stream_fallback(format, rate, host, name);
	if (esound_socket<=0) {
	        DPRINTF("couldn't open esd-socket connection!\n");
	        return -1;
        }
	DPRINTF("esound_socket %d\n",esound_socket);
	
	FILTER_AFTER_INIT;


	/* get the first buffers to start with something */
	DPRINTF("Waiting for buffers to come...\n");
	lbuf = sbuf_get(left);
	DPRINTF("Got left sbuf with size %i\n", sbuf_size(lbuf));
	rbuf = sbuf_get(right);
	DPRINTF("Got right sbuf with size %i\n", sbuf_size(rbuf));
	lpos = rpos = 0;

	do {
		/* write into wbuf until wbuf is full or
		 * either left or right is empty.
		 * if either channel gets empty during play
		 * we fill in zeros for it. */
		while (wbpos < GLAME_WBUFSIZE
		       && (!lbuf || lpos < sbuf_size(lbuf))
		       && (!right || !rbuf || rpos < sbuf_size(rbuf))) {
			if (lbuf)
				wbuf[wbpos++] = SAMPLE2SHORT(sbuf_buf(lbuf)[lpos++]);
			else
				wbuf[wbpos++] = 0;
			if (right) {
				if (rbuf)
					wbuf[wbpos++] = SAMPLE2SHORT(sbuf_buf(rbuf)[rpos++]);
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
		if (lbuf && lpos >= sbuf_size(lbuf)) {
			sbuf_unref(lbuf);
			lbuf = sbuf_get(left);
			lpos = 0;
			cnt++;
		}
		if (rbuf && rpos >= sbuf_size(rbuf)) {
			sbuf_unref(rbuf);
			rbuf = sbuf_get(right);
			rpos = 0;
			cnt++;
		}
	} while (lbuf || (right && rbuf));

	FILTER_BEFORE_CLEANUP;

	DPRINTF("Received %d buffers.\n",cnt);
	close(esound_socket);
	free(wbuf);

	return 0;
}
#endif



int audio_io_register()
{
	filter_t *f;

#if defined HAVE_ESD
	if (!(f = filter_alloc("audio_out", "play stream", esd_out_f))
	    || !filter_add_input(f, "left_in", "left or mono channel",
				FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_input(f, "right_in", "right channel",
				FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("audio_in","record stream",esd_in_f))
	    || !filter_add_output(f,"left_out","left channel",
		    		FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f,"right_out","right channel",
		    		FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f) == -1)
		return -1;

#elif defined HAVE_OSS
#elif defined HAVE_SUNAUDIO
#endif

	return 0;
}




