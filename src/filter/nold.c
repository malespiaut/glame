/*
 * <nold.c>
 *
 * $Id: nold.c,v 1.3 2000/02/10 14:54:21 nold Exp $
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

#include <sys/time.h>
#include <sys/types.h>
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

	sgi_audioparam_t *in;
	filter_pipe_t	*p_in;
	SAMPLE		**bufs;
	SAMPLE		*zero_buf;
	
	int		rate;
	int		chunk_size, last_chunk;
	
	int		ch_active;
	int		max_ch=0;
	int		ch=0;

	ALconfig	c;
	ALport		p;
	ALpv		v[1];
	int		resource = AL_DEFAULT_OUTPUT;
	int		qsize;

	DPRINTF("ENTER\n");

	p_in = hash_find_input("in", n);
	if(!p_in) {
		DPRINTF("No input channels given.\n");
		return -1;
	}
	rate = p_in->u.sample.rate;
	do {
		max_ch++;
		if(rate != p_in->u.sample.rate) {
			DPRINTF("Sample rate mismatch!\n");
			return -1;
		}
	} while((p_in = hash_next_input(p_in)));

	if(rate <= 0) {
		DPRINTF("No valid sample rate given.\n");
		return -1;
	}

	in = (sgi_audioparam_t *)malloc(max_ch * sizeof(sgi_audioparam_t));
	bufs = (SAMPLE **)malloc(max_ch * sizeof(SAMPLE *));
	if(!in || !bufs) {
		free(in);
		DPRINTF("Failed to alloc input structs.\n");
		return -1;
	}

	p_in = hash_find_input("in", n);
	do {
		sgi_audioparam_t *ap = &in[ch++];
		ap->pipe = p_in;
		ap->buf = NULL;
		ap->pos = 0;
		ap->to_go = 0;
	} while((p_in = hash_next_input(p_in)));
	
	if(ch != max_ch) {
		DPRINTF("Huh!? Input pipes changed under us!?\n");
		return -1;
	}
			
	c = alNewConfig();
	if(!c) {
		DPRINTF("Failed to create audio configuration: %s\n",
			alGetErrorString(oserror()));
			return -1;
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
		return -1;
	}
	v[0].param = AL_RATE;
	v[0].value.ll = alDoubleToFixed(rate);
	if(alSetParams(resource, v, 1) < 0) {
		DPRINTF("Failed to set audio output parameters: %s\n",
			alGetErrorString(oserror()));
		return -1;
	}
	if(v[1].sizeOut < 0) {
		DPRINTF("Invalid sample rate.\n");
		return -1;
	}
	qsize = alGetQueueSize(c);
	if(qsize <= 0) {
		DPRINTF("Invalid QueueSize.\n");
		return -1;
	}
	p = alOpenPort("GLAME audio output", "w", c);
	if(!p) {
		DPRINTF("Failed to open audio output port: %s\n",
			alGetErrorString(oserror()));
			return -1;
	}
	alFreeConfig(c);

	/* Uh-uh! Playing dirty games here! Wish me luck... */
	zero_buf = (SAMPLE *)calloc(qsize, sizeof(SAMPLE));
	
	FILTER_AFTER_INIT;

	ch = 0;
	ch_active = max_ch;
	chunk_size = 0;

	/* Not really necessary as chunk_size is 0 so alWriteBuffers()
	 * would return immediately. But hey, evil gotos are fun!
	 */
	goto _entry;
	
	while(ch_active) {

#if 0
		DPRINTF("Writing %d sample chunk.\n", chunk_size);
#endif
		/* Queue audio chunk. All-mono buffers. */
		alWriteBuffers(p, (void **)bufs, NULL, chunk_size);
	_entry:
		last_chunk = chunk_size;
		chunk_size = qsize;
		do {
			sgi_audioparam_t *ap = &in[ch];
			ap->to_go -= last_chunk;
			ap->pos += last_chunk;
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

	/* Drain audio output */
	/* FIXME */
	alClosePort(p);
	free(bufs);
	free(in);
	DPRINTF("LEAVING\n");
	return 0;
}

#endif

int nold_register()
{
	filter_t *f;
	
#ifdef HAVE_SGIAUDIO
	DPRINTF("Trying to register\n");
	if (!(f = filter_alloc("audio_out", "play stream", sgi_audio_out_f))) {
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




