/*
 * audio_io.c
 * $Id: audio_io.c,v 1.2 2000/02/01 10:23:13 mag Exp $
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

	fprintf(stderr,"esd_out_f started!\n");

	format = ESD_BITS16 | ESD_STREAM | ESD_PLAY;

	/* query both input channels, if left only -> MONO,
	 * else STEREO output. */
	if (!(right = hash_find_input("right", n)))
		format |= ESD_MONO;
	else
		format |= ESD_STEREO;
	if (!(left = hash_find_input("left", n)))
		return -1;

	fprintf(stderr,"Trying to open esd-socket!\n");
	
	esound_socket = esd_play_stream_fallback(format, rate, host, name);
	if (esound_socket<0){
	        printf("couldn't open esd-socket connection!\n");
	        return -1;
        }

	wbuf=(short int*)malloc(GLAME_WBUFSIZE*sizeof(short int));
	wbpos=0;
	if (wbuf==NULL){
		printf("esd-filter: couldn't alloc wbuf!\n");
		return -1;
	}


	/* get the first buffers to start with something */
	lbuf = fbuf_get(left);
	fprintf(stderr,"Got fbuf with size %d\n",fbuf_size(lbuf));
	if (right)
		rbuf = fbuf_get(right);
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
		write(esound_socket, wbuf, wbpos*sizeof(short));
		wbpos = 0;

		/* check, if we need new data */
		if (lpos >= fbuf_size(lbuf)) {
			fbuf_unref(lbuf);
			lbuf = fbuf_get(left);
			lpos = 0;
		}
		if (right && rpos >= fbuf_size(rbuf)) {
			fbuf_unref(rbuf);
			rbuf = fbuf_get(right);
			rpos = 0;
		}
	} while (lbuf || (right && rbuf));

	return 0;
}
#endif



int audio_io_register()
{
	filter_t *f;

#if defined HAVE_ESD
	if (!(f = filter_alloc("audio_out", "play stream", esd_out_f))
	    || filter_add_input(f, "left", "left or mono channel",
				FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add_input(f, "right", "right channel",
				FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;
#elif defined HAVE_OSS
#elif defined HAVE_SUNAUDIO
#endif

	return 0;
}




