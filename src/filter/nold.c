/*
 * nold.c
 * $Id: nold.c,v 1.7 2000/02/20 15:31:42 richi Exp $
 *
 * Copyright (C) 2000 Daniel Kobras
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

/*
 * This is nold's personal playground. Stuff in here is strictly 
 * confidential, under permantent development and probably highly
 * broken. You have been warned.
 */
 
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glame_types.h"
#include <limits.h>

/* esd is broken on IRIX. Let's have a go at native audio output 
 * instead.
 */

#ifdef HAVE_SGIAUDIO
#include <dmedia/audio.h>


static int sgi_audio_out_f(filter_node_t *n)
{
	typedef struct {
		filter_pipe_t 	*pipe;
		filter_buffer_t	*buf;
		ssize_t		pos;
		ssize_t		to_go;
	} sgi_audioparam_t;

	sgi_audioparam_t *in = NULL;
	filter_pipe_t	*p_in;
	SAMPLE		**bufs = NULL;
	SAMPLE		*zero_buf;
	
	int		rate;
	int		chunk_size, last_chunk;
	
	int		ch_active;
	int		max_ch=0;
	int		ch=0;

	ALconfig	c = NULL;
	ALport		p = NULL;
	ALpv		v[1];
	int		resource = AL_DEFAULT_OUTPUT;
	int		qsize;

	int		ret = -1;

	/* The first run through our input pipes is basically to 
	 * determine the number of inputs. Since we only have a
	 * single input pipe type, we could also abuse the number of
	 * inputs resource of our filter node, but let's keep it the
	 * generic way for educational purpose.
	 */
	p_in = filternode_get_input(n, "in");
	if(!p_in) {
		DPRINTF("No input channels given.\n");
		goto _cleanup;
	}
	rate = filterpipe_sample_rate(p_in);
	do {
		max_ch++;
		if(rate != filterpipe_sample_rate(p_in)) {
			DPRINTF("Sample rate mismatch!\n");
			goto _cleanup;
		}
	} while((p_in = filternode_next_input(p_in)));

	if(rate <= 0) {
		DPRINTF("No valid sample rate given.\n");
		goto _cleanup;
	}

	/* 'in' is our internal array of pipe goodies. 'bufs' was added
	 * to make use of libaudio's spiffy feature to interleave 
	 * multiple output buffers to an audio port.
	 */
	in = (sgi_audioparam_t *)malloc(max_ch * sizeof(sgi_audioparam_t));
	bufs = (SAMPLE **)malloc(max_ch * sizeof(SAMPLE *));
	if(!in || !bufs) {
		DPRINTF("Failed to alloc input structs.\n");
		goto _cleanup;
	}

	/* Second pass through the input pipes to set up our internal
	 * struct.
	 */
	p_in = filternode_get_input(n, "in");
	do {
		sgi_audioparam_t *ap = &in[ch++];
		ap->pipe = p_in;
		ap->buf = NULL;
		ap->pos = 0;
		ap->to_go = 0;
	} while((p_in = filternode_next_input(p_in)));
	
	/* Paranoia. */
	if(ch != max_ch) 
		PANIC("Huh!? Input pipes changed under us!?\n");
			
	c = alNewConfig();
	if(!c) {
		DPRINTF("Failed to create audio configuration: %s\n",
			alGetErrorString(oserror()));
			goto _cleanup;
	}

	/* SGI AL has this nice feature of directly supporting our
	 * native floating point format. How charming... :-)
	 */
	alSetSampFmt(c, AL_SAMPFMT_FLOAT);
	alSetFloatMax(c, 1.0);
	alSetChannels(c, max_ch);
	/* Using the default queuesize... */

	DPRINTF("rate %d\n", rate);
	if(alSetDevice(c, resource) < 0) {
		DPRINTF("Resource invalid: %s\n",
			alGetErrorString(oserror()));
		goto _cleanup;
	}
	v[0].param = AL_RATE;
	v[0].value.ll = alDoubleToFixed(rate);
	if(alSetParams(resource, v, 1) < 0) {
		DPRINTF("Failed to set audio output parameters: %s\n",
			alGetErrorString(oserror()));
		goto _cleanup;
	}
	if(v[0].sizeOut < 0) {
		DPRINTF("Invalid sample rate.\n");
		goto _cleanup;
	}
	/* The QueueSized is used as an initializer to our output chunk
	 * size later on.
	 */
	qsize = alGetQueueSize(c);
	if(qsize <= 0) {
		DPRINTF("Invalid QueueSize.\n");
		goto _cleanup;
	}
	p = alOpenPort("GLAME audio output", "w", c);
	if(!p) {
		DPRINTF("Failed to open audio output port: %s\n",
			alGetErrorString(oserror()));
			goto _cleanup;
	}
	alFreeConfig(c);
	c = NULL;

	/* Uh-uh! Playing dirty games here! Wish me luck... */
	zero_buf = (SAMPLE *)calloc(qsize, sizeof(SAMPLE));
	
	FILTER_AFTER_INIT;
	/* May not fail from now on... */
	ret = 0;
	
	ch = 0;
	ch_active = max_ch;
	chunk_size = 0;

	/* Not really necessary as chunk_size is 0 so alWriteBuffers()
	 * would return immediately. But hey, evil gotos are fun!
	 */
	goto _entry;
	
	while(pthread_testcancel(), ch_active) {

#if 0
		DPRINTF("Writing %d sample chunk.\n", chunk_size);
#endif
		/* Queue audio chunk. All-mono buffers. Subsequent
		 * buffers get directed to different channels. 
		 */
		alWriteBuffers(p, (void **)bufs, NULL, chunk_size);
	_entry:
		last_chunk = chunk_size;
		chunk_size = qsize;
		do {
			sgi_audioparam_t *ap = &in[ch];
			ap->to_go -= last_chunk;
			ap->pos += last_chunk;
			/* Note that sbuf_*() will handle NULL values
			 * gracefully.
			 */
			if(!ap->to_go) {
				/* Fetch next buffer */
				sbuf_unref(ap->buf);
				ap->buf = sbuf_get(ap->pipe);
				ap->to_go = sbuf_size(ap->buf);
				ap->pos = 0;
			}
			if(!ap->buf) {
				if(bufs[ch] != zero_buf) {
					ch_active--;
					/* Ugly, but... */
					bufs[ch] = zero_buf;
				}
				ap->to_go = qsize;
			} else {
				bufs[ch] = &sbuf_buf(ap->buf)[ap->pos];
			}
			chunk_size = MIN(chunk_size, ap->to_go);
		} while((ch = ++ch % max_ch));
	}

	FILTER_BEFORE_CLEANUP;

_cleanup:
	if(p)
		alClosePort(p);
	if(c)
		alFreeConfig(c);
	/* free() is supposed to handle NULLs gracefully. */
	free(bufs);
	free(in);
	return ret;
}

#endif

int nold_register()
{

#ifdef HAVE_SGIAUDIO
	filter_t *f;
	
	DPRINTF("Trying to register\n");
	if (!(f = filter_alloc("audio_out2", "play stream", sgi_audio_out_f))) {
		DPRINTF("filter_alloc failed.\n");
		return -1;
	}
	if(!filter_add_input(f, "in", "input channel",
				FILTER_PORTTYPE_SAMPLE | 
				FILTER_PORTTYPE_AUTOMATIC)) {
		DPRINTF("filter_add_input failed.\n");
		return -1;
	}
	if(!filter_add(f) == -1) {
		DPRINTF("filter_add failed.\n");
		return -1;
	}
#endif
	return 0;
}




