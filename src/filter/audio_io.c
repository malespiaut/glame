/*
 * audio_io.c
 * $Id: audio_io.c,v 1.30 2000/02/22 17:19:20 nold Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther, Alexander Ehlert, Daniel Kobras
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
#include "glame_types.h"
#include "filter.h"
#include "util.h"
#include <limits.h>


/* Generic versions of filter methods. Called from various low-level
 * implementations
 *
 * TODO: Handling of default 'rate' and 'hangle' parameters. 
 *       Currently we just assume sane defaults.
 */

/* Generic connect_in method for audio output filters. We assume a
 * single (automatic) input port.
 */

static int aio_generic_connect_in(filter_node_t *dest, const char *port,
                                  filter_pipe_t *pipe)
{
	filter_pipe_t *portishead;
	
	/* Limit to stereo output */
	if (filternode_nrinputs(dest) > 1)
		return -1;
	
	/* Assert a common sample rate between all input pipes */
	if ((portishead = filternode_get_input(dest, port)))
			if (filterpipe_sample_rate(portishead) !=
			    filterpipe_sample_rate(pipe))
				return -1;
	return 0;
}

/* Generic connect_out method for audio input filters. We assume there
 * are no input ports and a single (automatic) output port.
 */

static int aio_generic_connect_out(filter_node_t *src, const char *port,
                                   filter_pipe_t *pipe)
{
	filter_param_t *ratep;
	filter_pipe_t *prev;
	float phi = FILTER_PIPEPOS_CENTRE, rate = 44100;

	/* Limit to stereo capture */
	if (filternode_nroutputs(src) > 1)
		return -1;
	
	/* Check for default rate parameter */
	if ((ratep=filternode_get_param(src, "rate")))
		rate = filterparam_val_int(ratep);

	/* That's a bit messy. If there are two pipes and the user
	 * didn't specify explicit directional information, we set 
	 * this port to stereo right and the other port to left. We
	 * default to centre, if we are the only port.
	 */
	if (filternode_nroutputs(src) == 1) {
		prev = filternode_get_output(src, port);
		if (!prev)
			PANIC("nroutputs and get_output disagree!\n");
		if (!filterpipe_get_sourceparam(prev, "hangle"))
			filterpipe_sample_hangle(prev) = FILTER_PIPEPOS_LEFT;
		phi = FILTER_PIPEPOS_RIGHT;
	}
	
	filterpipe_settype_sample(pipe, rate, phi);

	return 0;
}						

/* Clumsy try to clean up the register mess a bit. */

static int aio_generic_register_input(char *name, int (*f)(filter_node_t *))
{
	filter_t *filter;

	if (!f)
		return -1;

	if (!(filter=filter_alloc(name, "record stream", f)) ||
		!filter_add_output(filter, PORTNAME_OUT, "output port",
				FILTER_PORTTYPE_SAMPLE |
				FILTER_PORTTYPE_AUTOMATIC))
		return -1;

	filter->connect_out = aio_generic_connect_out;

	if(filter_add(filter) == -1)
		return -1;

	return 0;
}
			
static int aio_generic_register_output(char *name, int (*f)(filter_node_t *)) 
{
	filter_t *filter;
	
	if (!f)
		return -1;
	
	if (!(filter=filter_alloc(name, "playback stream", f)) ||
		!filter_add_input(filter, PORTNAME_IN, "input port",
				FILTER_PORTTYPE_SAMPLE | 
				FILTER_PORTTYPE_AUTOMATIC))
		return -1;
	
	filter->connect_in = aio_generic_connect_in;

	if(filter_add(filter) == -1)
		return -1;

	return 0;
}


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

	max_ch = filternode_nrinputs(n);
	if(!max_ch) {
		DPRINTF("No input channels given.\n");
		FILTER_DO_CLEANUP;
	}

	/* The connect and fixup methods already make sure we have a 
	 * common sample rate among all pipes. */
	p_in = filternode_get_input(n, PORTNAME_IN);
	rate = filterpipe_sample_rate(p_in);
	if(rate <= 0) {
		DPRINTF("No valid sample rate given.\n");
		FILTER_DO_CLEANUP;
	}

	/* 'in' is our internal array of pipe goodies. 'bufs' was added
	 * to make use of libaudio's spiffy feature to interleave 
	 * multiple output buffers to an audio port.
	 */
	in = (sgi_audioparam_t *)malloc(max_ch * sizeof(sgi_audioparam_t));
	bufs = (SAMPLE **)malloc(max_ch * sizeof(SAMPLE *));
	if(!in || !bufs) {
		DPRINTF("Failed to alloc input structs.\n");
		FILTER_DO_CLEANUP;
	}

	/* Cycle through the input pipes to set up our internal
	 * struct.
	 */
	do {
		sgi_audioparam_t *ap = &in[ch++];
		ap->pipe = p_in;
		ap->buf = NULL;
		ap->pos = 0;
		ap->to_go = 0;
	} while((p_in = filternode_next_input(p_in)));
	
	/* Fix left/right mapping. Audiolib shoves first buffer to the
	 * right.
	 */	
	if(max_ch > 1)
		if(filterpipe_sample_hangle(in[0].pipe) <
		   filterpipe_sample_hangle(in[1].pipe)) {
			filter_pipe_t *t = in[0].pipe;
			in[0].pipe = in[1].pipe;
			in[1].pipe = t;
		}
	
	c = alNewConfig();
	if(!c) {
		DPRINTF("Failed to create audio configuration: %s\n",
			alGetErrorString(oserror()));
		FILTER_DO_CLEANUP;
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
		FILTER_DO_CLEANUP;
	}
	v[0].param = AL_RATE;
	v[0].value.ll = alDoubleToFixed(rate);
	if(alSetParams(resource, v, 1) < 0) {
		DPRINTF("Failed to set audio output parameters: %s\n",
			alGetErrorString(oserror()));
		FILTER_DO_CLEANUP;
	}
	if(v[0].sizeOut < 0) {
		DPRINTF("Invalid sample rate.\n");
		FILTER_DO_CLEANUP;
	}
	/* The QueueSized is used as an initializer to our output chunk
	 * size later on.
	 */
	qsize = alGetQueueSize(c);
	if(qsize <= 0) {
		DPRINTF("Invalid QueueSize.\n");
		FILTER_DO_CLEANUP;
	}
	p = alOpenPort("GLAME audio output", "w", c);
	if(!p) {
		DPRINTF("Failed to open audio output port: %s\n",
			alGetErrorString(oserror()));
		FILTER_DO_CLEANUP;
	}
	alFreeConfig(c);
	c = NULL;

	/* May not fail from now on... */
	ret = 0;
	FILTER_AFTER_INIT;
	
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
				if(bufs[ch]) {
					ch_active--;
					/* Supress output on this channel */
					bufs[ch] = NULL;
				}
				ap->to_go = qsize;
			} else {
				bufs[ch] = &sbuf_buf(ap->buf)[ap->pos];
			}
			chunk_size = MIN(chunk_size, ap->to_go);
		} while((ch = ++ch % max_ch));
	}

	FILTER_BEFORE_CLEANUP;

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
	int length,time,maxtime;
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

	left = filternode_get_output(n, PORTNAME_OUT);
	right = filternode_get_output(n, PORTNAME_OUT);
	
	if (!left || !right){
		DPRINTF("Couldn't find output pipes!\n");
		return -1;
	}
	
	if (filterpipe_sample_hangle(left) > filterpipe_sample_hangle(right)) {
		filter_pipe_t *t = left;
		left = right;
		right = t;
	}
	
	if ((buf=(short int*)malloc(ESD_BUF_SIZE))==NULL){
		DPRINTF("Couldn't alloc input buffer!\n");
		return -1;
	}

        maxtime=10*rate*2;
	time=0;
	
	DPRINTF("Start sampling!\n");

	FILTER_AFTER_INIT;
	
	while(pthread_testcancel(),time<maxtime){
		length=0;
		while(length<ESD_BUF_SIZE) length+=read(sock,buf+length,ESD_BUF_SIZE-length);
		if (!length){
			DPRINTF("Read failed!\n");
			return -1;
		}
		DPRINTF("sampled %d bytes!\n",length);
		lpos=rpos=i=0;
		lbuf=sbuf_alloc(length/4,n);	/* FIXME 16bit stereo only */
		lbuf=sbuf_make_private(lbuf);
		rbuf=sbuf_alloc(length/4,n);
		rbuf=sbuf_make_private(rbuf);
		if(!lbuf || !rbuf){
			DPRINTF("alloc error!\n");
			return -1;
		}
		while(i<length/2){
			sbuf_buf(lbuf)[lpos++]=SHORT2SAMPLE(buf[i++]);
			sbuf_buf(rbuf)[rpos++]=SHORT2SAMPLE(buf[i++]);
		}
		DPRINTF("lpos=%d, rpos=%d, i=%d\n",lpos,rpos,i);
		sbuf_queue(left,lbuf);
		sbuf_queue(right,rbuf);
		time+=length;
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
	filter_pipe_t *left, *right,*swap;
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
	filternode_foreach_input(n,swap)
		DPRINTF("angle=%f\n",filterpipe_sample_hangle(swap));

	left = filternode_get_input(n, PORTNAME_IN);
	right = filternode_next_input(left);
	
	if (!FILTER_SAMPLEPIPE_IS_LEFT(left)){
		swap=left;
		left=right;
		right=swap;
	}

	/* right only? */
	if (!left) {
		left = right;
		right = NULL;
	}
	/* no channel? */
	if (!left)
		return -1;
	rate = filterpipe_sample_rate(left);
	/* right channel different sample rate? */
	if (right && filterpipe_sample_rate(right) != rate)
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
		DPRINTF("Input filter didn't supply rate info! Fix it!\n");
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
	if(right){
		rbuf = sbuf_get(right);
		DPRINTF("Got right sbuf with size %i\n", sbuf_size(rbuf));
	}
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


/* Try to register all working audio output filters. The default 'audio_out'
 * filter will be the one the most specific to our hardware.
 */
int audio_io_register()
{
	int (*audio_out)(filter_node_t *);
	int (*audio_in)(filter_node_t *);	
	
	audio_out = audio_in = NULL;
	
#if defined HAVE_ESD
	if(!aio_generic_register_output("esd_audio_out", esd_out_f)) 
		audio_out = esd_out_f;
	if(!aio_generic_register_input("esd_audio_in", esd_in_f))
		audio_in = esd_in_f;
#endif
#if defined HAVE_SGIAUDIO
	if(!aio_generic_register_output("sgi_audio_out", sgi_audio_out_f)) 
		audio_out = sgi_audio_out_f;
#endif
#if defined HAVE_OSS
	/* TODO */
#endif
#if defined HAVE_ALSA
	/* TODO */
#endif 
#if defined HAVE_SUNAUDIO
	/* TODO */
#endif

	/* It's okay if there is no input filter but we require at least
	 * one working output filter.
	 */
	aio_generic_register_input("audio_in", audio_in);
	return aio_generic_register_output("audio_out", audio_out);
}




