/*
 * audio_io.c
 * $Id: audio_io.c,v 1.10 2000/03/27 09:20:10 richi Exp $
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
#include <errno.h>
#include <limits.h>
#include "glame_types.h"
#include "filter.h"
#include "util.h"
#include "glplugin.h"

/* TODO: (before 0.2)
 *       * oss and sgi audio in.
 *       (post 0.2)
 *       * The whole file needs a file_io'ish re-write to further
 *         centralize common code. This is post-0.2 work however. Let's
 *         stick with a little code duplication for now.
 */

PLUGIN_DESCRIPTION(audio_io, "filter set for audio support")
PLUGIN_SET(audio_io, "audio_out audio_in")

/* Generic versions of filter methods. Called from various low-level
 * implementations.
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
	float phi = FILTER_PIPEPOS_CENTRE;
	int rate = GLAME_DEFAULT_SAMPLERATE;

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
	if ((prev = filternode_get_output(src, port))) {
		if (!filterpipe_get_sourceparam(prev, "hangle"))
			filterpipe_sample_hangle(prev) = FILTER_PIPEPOS_LEFT;
		phi = FILTER_PIPEPOS_RIGHT;
	}
	
	filterpipe_settype_sample(pipe, rate, phi);

	return 0;
}						

/* Generic fixup_pipe method for audio output filters. Assert a common
 * sample rate between all input pipes.
 */

static void aio_generic_fixup_pipe(filter_node_t *src, filter_pipe_t *pipe)
{
	int rate = filterpipe_sample_rate(pipe);

	filternode_foreach_input(src, pipe) {
		if (rate != filterpipe_sample_rate(pipe)) {
		        filternode_set_error(src, "mismatching input samplerates");
			return;
		}
	}
	filternode_clear_error(src);
	/* No output pipes. */
}

/* Generic fixup_param method for audio input filters. */

static int aio_generic_fixup_param(filter_node_t *src, filter_pipe_t *pipe,
                                   const char *name, filter_param_t *param)
{
	int rate;

	if (pipe) {
		/* Must've been a hangle change. */
		float hangle = filterparam_val_float(param);
		if (hangle <= -M_PI || hangle > M_PI)
			return -1;
		filterpipe_sample_hangle(pipe) = hangle;
		src->filter->fixup_pipe(src, pipe);
		return 0;
	}
	if (strcmp(name, "rate"))
		return 0;	/* Device changes are always okay. */

	rate = filterparam_val_int(param);
	
	/* Make sure to update rate param on all pipes
	 */
	filternode_foreach_output(src, pipe) {
		if (filterpipe_sample_rate(pipe) == rate)
			continue;
		filterpipe_sample_rate(pipe) = rate; /* WTF?? */
		src->filter->fixup_pipe(src, pipe);
	}
	return 0;
}
/* Clumsy try to clean up the register mess a bit. */

static int aio_generic_register_input(char *name, int (*f)(filter_node_t *))
{
	filter_t *filter;
	filter_portdesc_t *p;

	if (!f)
		return -1;

	if (!(filter = filter_alloc(f)) ||
		!(p = filter_add_output(filter, PORTNAME_OUT, "output port",
				FILTER_PORTTYPE_SAMPLE |
				FILTER_PORTTYPE_AUTOMATIC)) ||
		!filter_add_param(filter, "rate", "sample rate", 
				FILTER_PARAMTYPE_INT) ||
		!filter_add_param(filter, "duration", "seconds to record",
				FILTER_PARAMTYPE_FLOAT) ||
		!filter_add_param(filter, "device", "input device",
				FILTER_PARAMTYPE_STRING) ||
		!filterport_add_param(p, "position", "position of the stream",
				FILTER_PARAMTYPE_FLOAT))
		return -1;
	filter->connect_out = aio_generic_connect_out;
	filter->fixup_param = aio_generic_fixup_param;

	if (filter_add(filter, name, "record stream") == -1)
		return -1;

	return 0;
}
			
static int aio_generic_register_output(char *name, int (*f)(filter_node_t *)) 
{
	filter_t *filter;
	
	if (!f)
		return -1;
	
	if (!(filter=filter_alloc(f)) ||
		!filter_add_input(filter, PORTNAME_IN, "input port",
				FILTER_PORTTYPE_SAMPLE | 
				FILTER_PORTTYPE_AUTOMATIC) ||
		!filter_add_param(filter, "device", "output device",
				FILTER_PARAMTYPE_STRING))
		return -1;
	filter->connect_in = aio_generic_connect_in;
	filter->fixup_pipe = aio_generic_fixup_pipe;

	if (filter_add(filter, name, "playback stream") == -1)
		return -1;

	return 0;
}


#ifdef HAVE_ALSA
#include <sys/asoundlib.h>
#include <linux/types.h>
#include <string.h>

static int alsa_audio_out_f(filter_node_t *n)
{
	typedef struct {
		filter_pipe_t	*pipe;
		filter_buffer_t	*buf;
		int		pos;
		int		to_go;
	} alsa_audioparam_t;
	
	alsa_audioparam_t	*in = NULL;
	__s16			neutral, *wbuf, *out = NULL;
	filter_pipe_t		*p_in;
	
	int card=0, dev=0;
	filter_param_t *dev_param;
	char *dev_paramstr;
	
	int rate, blksz, ssize;

	int ch, max_ch, ch_active;
	int chunk_size;
	int to_go;
	
	int i;
	
	snd_pcm_t			*pcm = NULL;
	snd_pcm_channel_params_t	ch_param;
	snd_pcm_channel_setup_t 	setup;


	/* Boilerplate init section - will go into a generic function one day.
	 */
	
	max_ch = filternode_nrinputs(n);
	if (!max_ch)
		FILTER_ERROR_RETURN("No input channels given.");
	
	p_in = filternode_get_input(n, PORTNAME_IN);
	rate = filterpipe_sample_rate(p_in);
	if (rate <= 0)
		FILTER_ERROR_RETURN("No valid sample rate given.");

	in = (alsa_audioparam_t *)malloc(max_ch * sizeof(alsa_audioparam_t));
	if (!in)
		FILTER_ERROR_RETURN("Failed to alloc input structs.");

	ch = 0;
	do {
		alsa_audioparam_t *ap = &in[ch++];
		ap->pipe = p_in;
		ap->buf = NULL;
		ap->pos = ap->to_go = 0;
	} while ((p_in = filternode_next_input(p_in)));

	/* Fixup hangle mapping. */
	if (ch > 1)
		if (filterpipe_sample_hangle(in[0].pipe) >
		    filterpipe_sample_hangle(in[1].pipe)) {
			filter_pipe_t *t = in[0].pipe;
			in[0].pipe = in[1].pipe;
			in[1].pipe = t;
		}

	/* ALSA specific initialisation.
	 */
	
	dev_param = filternode_get_param(n, "device");
	if (dev_param) {
		char *cpos;
		dev_paramstr = filterparam_val_string(dev_param);
		cpos = strchr(dev_paramstr, (int)':');
		if (cpos) {
			*cpos++ = '\0';
			dev = atoi(cpos);
		}
		card = atoi(dev_paramstr);
	}

	if (snd_pcm_open(&pcm, card, dev, SND_PCM_OPEN_PLAYBACK))
		FILTER_ERROR_CLEANUP("Could not open audio device.");
	
	/* FIXME: Can we assume s16 native endian is always supported?
	 *        (The plugin stuff performs a limited set of conversions.)
	 */
	ssize = 2;
	neutral = SAMPLE2SHORT(0.0);

	memset(&ch_param, 0, sizeof(snd_pcm_channel_params_t));
	ch_param.format.voices = max_ch;
	ch_param.format.rate = rate;
#ifdef SND_LITTLE_ENDIAN
	ch_param.format.format = SND_PCM_SFMT_S16_LE;
#else
# if SND_BIG_ENDIAN
	ch_param.format.format = SND_PCM_SFMT_S16_BE;
# else
#  error Unsupported endianness.
# endif
#endif
	ch_param.format.interleave = 1;
	ch_param.channel = SND_PCM_CHANNEL_PLAYBACK;
	ch_param.mode = SND_PCM_MODE_BLOCK;
	ch_param.start_mode = SND_PCM_START_FULL;
	ch_param.stop_mode = SND_PCM_STOP_ROLLOVER;
	ch_param.buf.block.frag_size = GLAME_WBUFSIZE * ssize * max_ch;
	ch_param.buf.block.frags_min = 1;
	ch_param.buf.block.frags_max = -1;
	if (snd_pcm_plugin_params(pcm, &ch_param))
		FILTER_ERROR_CLEANUP("Unsupported audio format.");
	
	if (snd_pcm_plugin_prepare(pcm, SND_PCM_CHANNEL_PLAYBACK))
		FILTER_ERROR_CLEANUP("Channel won't prepare.");

	setup.mode = SND_PCM_MODE_BLOCK;
	setup.channel = SND_PCM_CHANNEL_PLAYBACK;
	if (snd_pcm_plugin_setup(pcm, &setup))
		FILTER_ERROR_CLEANUP("Could not get channel setup.");
	
	blksz = setup.buf.block.frag_size;
	if (blksz <= 0)
		FILTER_ERROR_CLEANUP("Illegal size of audio buffer");
	DPRINTF("Using %d byte buffers.\n", blksz);
	
	out = (__s16 *)malloc(blksz);
	if (!out)
		FILTER_ERROR_CLEANUP("Not enough memory for conversion buffer.");

	blksz /= max_ch * ssize;
	wbuf = out;

	FILTER_AFTER_INIT;
			
	ch_active = ch;
	to_go = blksz;

	goto _entry;

	do {
		ssize_t ret;

		/* Convert to signed 16 bit. */
		for (i = 0; i < max_ch; i++) {
			int done;

			if (!in[i].buf) {
				for (done = 0; done < chunk_size; done++)
					out[max_ch*done + i] = neutral;
				continue;
			}
			for (done = 0; done < chunk_size; done++)
				out[max_ch*done + i] = SAMPLE2SHORT(
					sbuf_buf(in[i].buf)[in[i].pos++]);
			in[i].to_go -= done;
		}

		to_go -= chunk_size;

		if (!to_go) {
			out = wbuf;
			to_go = blksz;
			if ((ret=snd_pcm_plugin_write(pcm, out, 
				max_ch * ssize * blksz)) != 
				max_ch * ssize * blksz) {
				DPRINTF("Audio device had failure. "
				        "Samples might be dropped.\n");
			}
		} else {
			out = &out[chunk_size * max_ch];
		}

_entry:
		chunk_size = to_go;
		ch = 0;

		FILTER_CHECK_STOP;

		do {
			alsa_audioparam_t *ap = &in[ch];
			if (!ap->to_go) {
				sbuf_unref(ap->buf);
				ap->buf = sbuf_get(ap->pipe);
				ap->to_go = sbuf_size(ap->buf);
				ap->pos = 0;
			}
			if (!ap->buf) {
				if (ap->pipe) {
					ch_active--;
					ap->pipe = NULL;
					DPRINTF("Channel %d/%d stopped, "
					        "%d active.\n",
						ch, max_ch, ch_active);
				}
				ap->to_go = to_go;
			}
			chunk_size = MIN(chunk_size, ap->to_go);
		} while (++ch < max_ch);
	} while (ch_active || (to_go != blksz));

	FILTER_BEFORE_CLEANUP;
	FILTER_BEFORE_STOPCLEANUP;

	if (pcm) {
		snd_pcm_plugin_flush(pcm, SND_PCM_CHANNEL_PLAYBACK);
		snd_pcm_close(pcm);
	}
	free(out);
	free(in);
	FILTER_RETURN;
					
}
#endif

#ifdef HAVE_OSS
#include <linux/soundcard.h>
#include <linux/types.h>
#include <fcntl.h>
#include <sys/ioctl.h>

/* Use native endianness for audio output */
#ifdef WORD_BIGENDIAN
#define AFMT_U16_NE	AFMT_U16_BE
#else
#define AFMT_U16_NE	AFMT_U16_LE
#endif

typedef struct {
	filter_pipe_t	*pipe;
	filter_buffer_t *buf;
	int		pos;
	int		to_go;
} oss_audioparam_t;

/* Conversion from our internal float to the output format (8 or 16 bit)
 * in done in this helper routine.
 */
void oss_convert_bufs(oss_audioparam_t *in, __u8 *out, int max_ch,
                      int ssize, int chunk_size, int interleave)
{
	int i, done;

	for (i = 0; i < max_ch; i++) {
		if (!in[i].buf) {
			/* No more input - add silence. */
			if (ssize == 1) {
				__u8 neutral = SAMPLE2UCHAR(0.0);
				for (done = 0; done < chunk_size; done++)
					*(out + done*interleave + i) = neutral;
			} else {
				/* ssize == 2 */
				__u16 neutral = SAMPLE2USHORT(0.0);
				for (done = 0; done < chunk_size; done++)
					*(__u16 *)(out + done*interleave 
							+ 2*i) = neutral;
			}
			continue;
		}
		if (ssize == 1) 
			for (done = 0; done < chunk_size; done++)
				*(out + done*interleave + i) =
					SAMPLE2UCHAR(sbuf_buf(in[i].buf)[in[i].pos++]); 
		else /* ssize == 2 */
			for (done = 0; done < chunk_size; done++)
				*(__u16 *)(out + done*interleave +  2*i) =
					SAMPLE2USHORT(sbuf_buf(in[i].buf)[in[i].pos++]);

		in[i].to_go -= done;
	}		
}


/* OSS returns the max number of hardware channels supported,
 * must take care!
 */
static int oss_audio_out_f(filter_node_t *n)
{

	oss_audioparam_t	*in = NULL;
	__u8			*out = NULL;
	filter_pipe_t		*p_in;
	int			rate;
	int			formats;
	int			blksz, ssize = 0;

	int	ch, max_ch, interleave, ch_active;
	int	chunk_size;

	filter_param_t *dev_param;
	int	dev = -1;

	max_ch = filternode_nrinputs(n);
	if (!max_ch)
		FILTER_ERROR_CLEANUP("No input channels given!");

	/* The connect and fixup methods already make sure we have a 
	 * common sample rate among all pipes. */
	p_in = filternode_get_input(n, PORTNAME_IN);
	rate = filterpipe_sample_rate(p_in);
	if (rate <= 0)
		FILTER_ERROR_CLEANUP("No valid sample rate given.");

	in = (oss_audioparam_t *)malloc(max_ch * sizeof(oss_audioparam_t));
	if (!in)
		FILTER_ERROR_CLEANUP("Failed to alloc input structs.");

	ch = 0;
	do {
		oss_audioparam_t *ap = &in[ch++];
		ap->pipe = p_in;
		ap->buf = NULL;
		ap->pos = ap->to_go = 0;
	} while ((p_in = filternode_next_input(p_in)));

	/* Fix left/right mapping. Channel 0 goes left. */
	if (ch > 1)
		if (filterpipe_sample_hangle(in[0].pipe) >
		    filterpipe_sample_hangle(in[1].pipe)) {
			filter_pipe_t *t = in[0].pipe;
			in[0].pipe = in[1].pipe;
			in[1].pipe = t;
		}
	
	/* Ugly OSS ioctl() mess. Keep your eyes closed and proceed. */

	dev_param = filternode_get_param(n, "device");
	dev = open(dev_param ? filterparam_val_string(dev_param) : "/dev/dsp", 
	           O_WRONLY);
	if (dev == -1)
		FILTER_ERROR_CLEANUP("Could not open audio device.");

	if (ioctl(dev, SNDCTL_DSP_SPEED, &rate) == -1)
		FILTER_ERROR_CLEANUP("Unsupported sample rate.");
	DPRINTF("Hardware sample rate: %d.\n", rate);

	if (ioctl(dev, SOUND_PCM_WRITE_CHANNELS, &ch) == -1)
		FILTER_ERROR_CLEANUP("Unable to set number of channels.");
	if (max_ch != ch) {
		DPRINTF("Warning! %d channels connected but hardware supports only %d.\n", max_ch, ch);
		max_ch = ch;
	}

	if (ioctl(dev, SNDCTL_DSP_GETFMTS, &formats) == -1)	
		FILTER_ERROR_CLEANUP("Error querying available audio formats.\n");
	if (formats & AFMT_U16_NE) {
		formats = AFMT_U16_NE;
		ssize = 2;
	} else if (formats & AFMT_U8) {
		formats = AFMT_U8;	/* Must be supported */
		ssize = 1;
	} else 
		PANIC("OSS implementation not to specs!");

	if (ioctl(dev, SNDCTL_DSP_SETFMT, &formats) == -1)
		FILTER_ERROR_CLEANUP("Unable to set sample format");

	if (ioctl(dev, SNDCTL_DSP_GETBLKSIZE, &blksz) == -1)
		FILTER_ERROR_CLEANUP("Couldn't query size of audio buffer.");
	if (blksz <= 0)
		PANIC("Illegal size of audio buffer!");

	/* Allocate conversion buffer */
	out = (__u8 *)malloc(blksz);
	if (!out)
		FILTER_ERROR_CLEANUP("Not enough memory for conversion buffer.");
	
	DPRINTF("Allocated %d byte conversion buffer at %p.\n", blksz, out);
	interleave = max_ch * ssize; 

	FILTER_AFTER_INIT;

	ch_active = ch;

	goto _entry;

	do {
		ssize_t todo = chunk_size * interleave;
		void *wpos = out;
		
		oss_convert_bufs(in, out, max_ch, ssize, chunk_size, 
		                 interleave);
		do {
			ssize_t done;
			if ((done = write(dev, wpos, todo)) == -1 &&
			     errno != EINTR) {
				DERROR("Audio device had failure. "
			       	       "Samples might be dropped\n");
				break;
			}
			todo -= done;
			wpos += ssize * done;
		} while (todo);
	_entry:
		chunk_size = blksz/interleave;
		ch = 0;

		FILTER_CHECK_STOP;

		do {
			oss_audioparam_t *ap = &in[ch];
			if (!ap->to_go) {
				/* Fetch next buffer */
				sbuf_unref(ap->buf);
				ap->buf = sbuf_get(ap->pipe);
				ap->to_go = sbuf_size(ap->buf);
				ap->pos = 0;
			}
			if (!ap->buf) {
				if (ap->pipe) {
					ch_active--;
					ap->pipe = NULL;
					DPRINTF("Channel %d/%d stopped, "
					        "%d active.\n", 
						ch, max_ch, ch_active);
				}
				ap->to_go = blksz/interleave;
			}
			chunk_size = MIN(chunk_size, ap->to_go);
		} while (++ch < max_ch);
	} while (ch_active);

	FILTER_BEFORE_CLEANUP;
	FILTER_BEFORE_STOPCLEANUP;

	if (dev != -1)
		close(dev);
	free(out);
	free(in);
	FILTER_RETURN;
}
#endif


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
	filter_param_t  *dev_param;
	SAMPLE		**bufs = NULL;
	
	int		rate;
	int		chunk_size, last_chunk;
	
	int		ch_active;
	int		max_ch=0;
	int		ch=0;

	ALconfig	c = NULL;
	ALport		p = NULL;
	ALpv		v[1];
	/* FIXME: Should be set via default "device" param */
	int		resource = AL_DEFAULT_OUTPUT;
	int		qsize;

	int		ret = -1;

	max_ch = filternode_nrinputs(n);
	if (!max_ch)
	        FILTER_ERROR_CLEANUP("No input channels given.");

	/* The connect and fixup methods already make sure we have a 
	 * common sample rate among all pipes. */
	p_in = filternode_get_input(n, PORTNAME_IN);
	rate = filterpipe_sample_rate(p_in);
	if (rate <= 0)
		FILTER_ERROR_CLEANUP("No valid sample rate given.");

	/* 'in' is our internal array of pipe goodies. 'bufs' was added
	 * to make use of libaudio's spiffy feature to interleave 
	 * multiple output buffers to an audio port.
	 */
	in = (sgi_audioparam_t *)malloc(max_ch * sizeof(sgi_audioparam_t));
	bufs = (SAMPLE **)malloc(max_ch * sizeof(SAMPLE *));
	if(!in || !bufs)
		FILTER_ERROR_CLEANUP("Failed to alloc input structs.");

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

	if (dev_param = filternode_get_param(n, "device"))
		resource = alGetResourceByName(AL_SYSTEM, 
		             filterparam_val_string(dev_param),
			     AL_DEVICE_TYPE);
	if (alSetDevice(c, resource) < 0) {
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
	if(v[0].sizeOut < 0)
		FILTER_ERROR_CLEANUP("Invalid sample rate.");

	/* The QueueSize is used as an initializer to our output chunk
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

	FILTER_AFTER_INIT;

	ch_active = max_ch;
	chunk_size = 0;

	/* Not really necessary as chunk_size is 0 so alWriteBuffers()
	 * would return immediately. But hey, evil gotos are fun!
	 */
	goto _entry;
	
	while(ch_active) {
		
		FILTER_CHECK_STOP;
		
		/* Queue audio chunk. All-mono buffers. Subsequent
		 * buffers get directed to different channels. 
		 */
		alWriteBuffers(p, (void **)bufs, NULL, chunk_size);
	_entry:
		last_chunk = chunk_size;
		chunk_size = qsize;
		ch = 0;
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
		} while(++ch < max_ch);
	}

	FILTER_BEFORE_CLEANUP;
	FILTER_BEFORE_STOPCLEANUP;

	if(p)
		alClosePort(p);
	if(c)
		alFreeConfig(c);
	/* free() is supposed to handle NULLs gracefully. */
	free(bufs);
	free(in);
	FILTER_RETURN;
}

#endif


#ifdef HAVE_ESD
#include <esd.h>

static int esd_in_f(filter_node_t *n)
{
	filter_pipe_t	*pipe[2];
	filter_buffer_t	*fbuf;
	filter_param_t	*dev_param, *duration, *rate_param;
	char	*in;
	short	*buf;
	ssize_t	buf_size = ESD_BUF_SIZE, fbuf_size;
	
	esd_format_t	format;
        char	*host = NULL;
	int	channels, ch, rate = GLAME_DEFAULT_SAMPLERATE;
	int	sock, length, endless = 0;
	float	time = 0.0, maxtime = 0.0;

	if (!(channels = filternode_nroutputs(n)))
		FILTER_ERROR_RETURN("No outputs.");

	dev_param = filternode_get_param(n, "device");
	if (dev_param)
		host = filterparam_val_string(dev_param);

	rate_param = filternode_get_param(n, "rate");
	if (rate_param)
		rate = filterparam_val_int(rate_param);

	/* XXX: Don't like this part at all. Proper fix is a 'control' port
	 * the UI can use to send start/stop signals to individual nodes
	 * (not to the network as a whole like now). This is post-0.2 stuff.
	 */ 
	duration = filternode_get_param(n, "duration");
	if (duration)
		maxtime = filterparam_val_float(duration) * rate * 2;
	if (maxtime <= 0.0)
		endless = 1;
	
	pipe[0] = filternode_get_output(n, PORTNAME_OUT);
	pipe[1] = filternode_get_output(n, PORTNAME_OUT); /* Okay if NULL */
	
	if (pipe[1] && filterpipe_sample_hangle(pipe[0]) > 
	               filterpipe_sample_hangle(pipe[1])) {
		filter_pipe_t *t = pipe[0];
		pipe[0] = pipe[1];
		pipe[1] = t;
	}
	
	format = ESD_BITS16 | ESD_STREAM | ESD_RECORD | 
	         (channels == 1) ? ESD_MONO : ESD_STEREO;
	sock = esd_record_stream_fallback(format, rate, host, NULL);
	if (sock <= 0)
		FILTER_ERROR_RETURN("Couldn't open esd socket!");

	if ((buf = (short *)malloc(buf_size)) == NULL)
		FILTER_ERROR_CLEANUP("Couldn't alloc input buffer!");
	fbuf_size = buf_size/channels;
	
	FILTER_AFTER_INIT;
	
	while (endless || time < maxtime) {
		int i, pos;
		
		FILTER_CHECK_STOP;
		
		length = buf_size;
		in = (char *)buf;
		while (length) {
			ssize_t ret;
		        ret = read(sock, in, length);
			if (ret == -1) {
				DPRINTF("Read failed!\n");
				goto _out;
			}
			in += ret;
			length -= ret;
		}
		for (ch = 0; ch < channels; ch++) {
			fbuf = sbuf_make_private(sbuf_alloc(fbuf_size, n));
			if (!fbuf) {
				DPRINTF("alloc error!\n");
				goto _out;
			}
			pos = 0;
			i = ch;
			while (pos < fbuf_size) {
				sbuf_buf(fbuf)[pos++] = SHORT2SAMPLE(buf[i]);
				i += channels;
			}
			sbuf_queue(pipe[ch], fbuf);
		}
		time += fbuf_size;
	}
_out:
	for (ch = 0; ch < channels; ch++)
		sbuf_queue(pipe[ch], NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	close(sock);
	free(buf);
	FILTER_RETURN;
}

/* I don't know what I'm doing, but I just try to 
 * write a simple esound output filter... */
static int esd_out_f(filter_node_t *n)
{
	typedef struct {
		filter_pipe_t	*pipe;
		filter_buffer_t	*buf;
		int 		pos;
		int 		to_go;
	} esdout_param_t;

	esdout_param_t		*in = NULL;
	short			neutral, *wbuf, *out = NULL;
	filter_pipe_t		*p_in;

	
	filter_param_t *dev_param;
	char *host = NULL;
	
	int rate, ssize;
	int max_ch, ch, ch_active, to_go;
	ssize_t blksz, chunk_size;
	int i;
	
	esd_format_t format = ESD_BITS16 | ESD_STREAM | ESD_PLAY;
	int esound_socket;

	/* Boilerplate init section - will go into a generic function one day.
	 */

	if (!(max_ch = filternode_nrinputs(n)))
		FILTER_ERROR_RETURN("no inputs");

	p_in = filternode_get_input(n, PORTNAME_IN);
	rate = filterpipe_sample_rate(p_in);
	if (rate <= 0)
		FILTER_ERROR_RETURN("No valid sample rate given.");
	
	if (!(in = ALLOCN(max_ch, esdout_param_t)))
		FILTER_ERROR_RETURN("Failed to alloc input structs.");

	ch = 0;
	do {
		esdout_param_t *ap = &in[ch++];
		ap->pipe = p_in;
		ap->buf = NULL;
		ap->pos = ap->to_go = 0;
	} while ((p_in = filternode_next_input(p_in)));

	/* Fixup hangle mapping. */
	if (ch > 1)
		if (filterpipe_sample_hangle(in[0].pipe) >
		    filterpipe_sample_hangle(in[1].pipe)) {
			filter_pipe_t *t = in[0].pipe;
			in[0].pipe = in[1].pipe;
			in[1].pipe = t;
		}

	/* ESD specific initialisation
	 */

	dev_param = filternode_get_param(n, "device");
	if (dev_param)
		host = filterparam_val_string(dev_param);
	format |= (max_ch == 1) ? ESD_MONO : ESD_STEREO;
	esound_socket = esd_play_stream_fallback(format, rate, host, NULL);
	if (esound_socket <= 0)
	        FILTER_ERROR_RETURN("couldn't open esd-socket connection!");

	ssize = sizeof(short);
	neutral = SAMPLE2SHORT(0.0);
	blksz = ESD_BUF_SIZE;
	out = (short *)malloc(blksz * max_ch * ssize);
	if (!out)
		FILTER_ERROR_RETURN("couldn't alloc wbuf!");
	wbuf = out;

	FILTER_AFTER_INIT;

	ch_active = ch;
	to_go = blksz;

	goto _entry;

	do {
		ssize_t ret;

		/* Convert to signed 16 bit. */
		for (i = 0; i < max_ch; i++) {
			int done;

			if (!in[i].buf) {
				for (done = 0; done < chunk_size; done++)
					out[max_ch*done + i] = neutral;
				continue;
			}
			for (done = 0; done < chunk_size; done++)
				out[max_ch*done + i] = SAMPLE2SHORT(
					sbuf_buf(in[i].buf)[in[i].pos++]);
			in[i].to_go -= done;
		}

		to_go -= chunk_size;

		if (!to_go) {
			ssize_t cnt = max_ch * ssize * blksz;
			char *s = (char *)wbuf;

			do {
				if ((ret = write(esound_socket, s, cnt)) 
				         == -1) {
					perror("error in write to esd");
					break;
				}
				s += ret;
				cnt -= ret;
			} while (cnt > 0);
			
			out = wbuf;
			to_go = blksz;

		} else {
			out = &out[chunk_size * max_ch];
		}

_entry:
		chunk_size = to_go;
		ch = 0;
		
		FILTER_CHECK_STOP;

		do {
			esdout_param_t *ap = &in[ch];
			if (!ap->to_go) {
				sbuf_unref(ap->buf);
				ap->buf = sbuf_get(ap->pipe);
				ap->to_go = sbuf_size(ap->buf);
				ap->pos = 0;
			}
			if (!ap->buf) {
				if (ap->pipe) {
					ch_active--;
					ap->pipe = NULL;
					DPRINTF("Channel %d/%d stopped, "
					        "%d active.\n",
						ch, max_ch, ch_active);
				}
				ap->to_go = to_go;
			}
			chunk_size = MIN(chunk_size, ap->to_go);
		} while (++ch < max_ch);
	} while (ch_active || (to_go != blksz));

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	close(esound_socket);
	free(wbuf);
	free(in);
	FILTER_RETURN;
}
#endif



/* The registration functions.
 * Only two "real" plugins will appear - the generic audio_out
 * and the audio_in plugin/filter. Others are only accessible
 * directly.
 */

PLUGIN_DESCRIPTION(audio_out, "audio output");
PLUGIN_PIXMAP(audio_out, "bla.png");
int audio_out_register()
{
	int (*audio_out)(filter_node_t *);

	audio_out = NULL;

#if defined HAVE_OSS
	if (!aio_generic_register_output("oss-audio-out", oss_audio_out_f))
		audio_out = oss_audio_out_f;
#endif
#if defined HAVE_ALSA
	if (!aio_generic_register_output("alsa-audio-out", alsa_audio_out_f))
		audio_out = alsa_audio_out_f;
#endif 
#if defined HAVE_ESD
	if (!aio_generic_register_output("esd-audio-out", esd_out_f)) 
		audio_out = esd_out_f;
#endif
#if defined HAVE_SGIAUDIO
	if (!aio_generic_register_output("sgi-audio-out", sgi_audio_out_f)) 
		audio_out = sgi_audio_out_f;
#endif
#if defined HAVE_SUNAUDIO
	/* TODO */
#endif

	return aio_generic_register_output("audio-out", audio_out);
}

PLUGIN_DESCRIPTION(audio_in, "audio input");
PLUGIN_PIXMAP(audio_in, "bla.png");
int audio_in_register()
{
	int (*audio_in)(filter_node_t *);

	audio_in = NULL;

#if defined HAVE_OSS
	/* TODO */
#endif
#if defined HAVE_ALSA
	/* TODO */
#endif 
#if defined HAVE_ESD
	if (!aio_generic_register_input("esd-audio-in", esd_in_f))
		audio_in = esd_in_f;
#endif
#if defined HAVE_SGIAUDIO
	/* TODO */
#endif
#if defined HAVE_SUNAUDIO
	/* TODO */
#endif

	return aio_generic_register_input("audio-in", audio_in);
}
