/*
 * audio_io_alsa_v050.c
 * $Id: audio_io_alsa_v050.c,v 1.1 2001/06/06 08:50:28 nold Exp $
 *
 * Copyright (C) 2001 Richard Guenther, Alexander Ehlert, Daniel Kobras
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

#include <string.h>
#include "audio_io.h"

PLUGIN_SET(audio_io_alsa, "alsa_audio_out")


static int alsa_audio_out_f(filter_t *n)
{
	typedef struct {
		filter_pipe_t	*pipe;
		filter_buffer_t	*buf;
		int		pos;
		int		to_go;
	} alsa_audioparam_t;
	
	alsa_audioparam_t	*in = NULL;
	gl_s16			neutral, *wbuf, *out = NULL;
	filter_port_t           *inport;
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
	
	inport = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	max_ch = filterport_nrpipes(inport);
	if (!max_ch)
		FILTER_ERROR_RETURN("No input channels given.");
	
	p_in = filterport_get_pipe(inport);
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
	} while ((p_in = filterport_next_pipe(inport, p_in)));

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
	
	dev_param = filterparamdb_get_param(filter_paramdb(n), "device");
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
# ifdef SND_BIG_ENDIAN
	ch_param.format.format = SND_PCM_SFMT_S16_BE;
# else
#error Unsupported endianness.
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
	
	out = (gl_s16 *)malloc(blksz);
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


int alsa_audio_out_register(plugin_t *p)
{
	return aio_generic_register_output(p, "alsa-audio-out",
					   alsa_audio_out_f, "0:0");
}

