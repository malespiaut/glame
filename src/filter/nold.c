/*
 * <nold.c>
 *
 * $Id: nold.c,v 1.1 2000/02/09 21:40:35 nold Exp $
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
#include <limits.h>

/* esd is broken on IRIX. Let's have a go at native audio output 
 * instead.
 */

#ifdef HAVE_SGIAUDIO
#include <dmedia/audio.h>

static int sgi_audio_out_f(filter_node_t *n)
{
	enum { LEFT=0, RIGHT=1 };
	filter_pipe_t	*pipe[2];
	filter_buffer_t	*buf[2] = { NULL, NULL };
	ssize_t		ssize[2] = { 0, 0 };
	SAMPLE		out[2]
	/* char		*device; */
	ssize_t		pos[2] = { 0, 0 };
	int		is_stereo;
	int		rate;

	int		ch=0, run=0;

	ALconfig	c;
	ALport		p;
	ALpv		v[1];

	pipe[LEFT] = hash_find_input("left_in", n);
	pipe[RIGHT] = hash_find_input("right_in", n);
	/* device = hash_find_input("device", n); */

	if(!pipe[LEFT]) {
		pipe[LEFT]=pipe[RIGHT];
		pipe[RIGHT]=NULL;
	}
	if(!pipe[LEFT]) {
		DPRINTF("No valid input channel!\n");
		return -1;
	}
	is_stereo=((pipe[RIGHT]) ? 1 : 0);
	
	rate = pipe[LEFT]->u.sample.rate;
	if(pipe[RIGHT] && (rate != pipe[RIGHT]->u.sample.rate)) {
		DPRINTF("Sample rate mismatch!\n");
		return -1;
	}
	if(rate <= 0) {
		DPRINTF("No valid sample rate given.\n");
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
	alSetChannels(c, ((is_stereo) ? AL_STEREO : AL_MONO));
	/* Using the default queuesize... */

	/* Setup to our internal format [-1.0;1.0] */
	alSetFloatMax(c, 1.0);

	v[0].param = AL_RATE;
	v[1].value.ll = alDoubleToFixed((float)(rate));
	if(alSetParams(c, v, 1) < 0) {
		DPRINTF("Failed to set audio output parameters: %s\n",
			alGetErrorString(oserror()));
		return -1;
	}
	if(v[1].sizeOut < 0) {
		DPRINTF("Invalid sample rate.\n");
		return -1;
	}

	p = alOpenPort("GLAME audio output", "w", c);
	if(!p) {
		DPRINTF("Failed to open audio output port: %s\n",
			alGetErrorString(oserror()));
			return -1;
	}
	alFreeConfig(c);

	FILTER_AFTER_INIT;

_next_sample:
	do {
		if(!buf[ch] || pos[ch] >= ssize[ch]) {
			/* Fetch next buffer */
			if(buf[ch])
				sbuf_unref(buf[ch]);
			buf[ch] = sbuf_get(pipe[ch]);
			ssize[ch] = sbuf_size(buf[ch]);
		}
		if(!buf[ch])
			goto _end;

		out[ch] = sbuf_buf(buf[ch])[pos[ch]++];	/* Ugh! ;-) */
	} while(ch = ++run & is_stereo);

	/* This is a hack! We're writing out one sample at a time (two if
	 * stereo). That's to get the interleaving right. alWriteBuffers()
	 * turns out to be much better suited to what we have in mind, but
	 * unfortunately this function was not available prior to IRIX 6.3.
	 * So let's play it safe for a start and optimize later. Actually
	 * it isn't as bad as it looks because alWriteFrames is extremely
	 * fast - it just queues the samples into an internal buffer.
	 */
	alWriteFrames(p, out, is_stereo+1);
	goto _next_sample;

_end:	
	FILTER_BEFORE_CLEANUP;

	/* Drain audio output */
	/* FIXME */
	alClosePort(p);
	return 0;
}

#endif

int nold_register()
{
	filter_t *f;

#ifdef HAVE_SGIAUDIO
	if (!(f = filter_alloc("audio_out", "play stream", sgi_audio_out_f))
	    || !filter_add_input(f, "left_in", "left or mono channel",
				FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_input(f, "right_in", "right channel",
				FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f) == -1)
		return -1;
#endif
	return 0;
}




