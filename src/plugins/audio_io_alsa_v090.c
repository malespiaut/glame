/*
 * audio_io_alsa.c
 * $Id: audio_io_alsa_v090.c,v 1.1 2001/04/28 23:17:54 mag Exp $
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

#include <sys/asoundlib.h>
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
	filter_pipe_t		*p_in;
	
	filter_param_t *dev_param, *pos_param;
	
	int rate, blksz, ssize;

	int ch, max_ch, ch_active;
	int chunk_size;
	int to_go, pos;
	
	int i, err, buffer_time, period_time;
	
	snd_pcm_t		*handle;
	snd_pcm_format_t	format;
	snd_pcm_hw_params_t	*params;
        snd_pcm_sw_params_t	*swparams;
	snd_pcm_status_t 	*status;
	static snd_output_t 	*log;
	char*			pcm_name="plug:0,0";	

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
	if (filterparam_val_string(dev_param) != NULL)
		pcm_name = filterparam_val_string(dev_param);

	if (snd_pcm_open(&handle, pcm_name, SND_PCM_STREAM_PLAYBACK, 0))
		FILTER_ERROR_CLEANUP("Could not open audio device.");
	
	/* FIXME: Can we assume s16 native endian is always supported?
	 *        (The plugin stuff performs a limited set of conversions.)
	 */
	ssize = 2;
	neutral = SAMPLE2SHORT(0.0);

#ifdef SND_LITTLE_ENDIAN
	format = SND_PCM_FORMAT_S16_LE;
#elif SND_BIG_ENDIAN
	format = SND_PCM_FORMAT_S16_BE;
#else
#error Unsupported endianness.
#endif

	err = snd_output_stdio_attach(&log, stderr, 0);

	snd_pcm_hw_params_alloca(&params);
	snd_pcm_sw_params_alloca(&swparams);
	err = snd_pcm_hw_params_any(handle, params);
	
	if (err < 0)
		FILTER_ERROR_CLEANUP("Broken configuration for this pcm");

	err = snd_pcm_hw_params_set_access(handle, params,
					   SND_PCM_ACCESS_RW_INTERLEAVED);

	if (err < 0)
		FILTER_ERROR_CLEANUP("Access type interleaved not available");

	err = snd_pcm_hw_params_set_format(handle, params, format);

	if (err < 0)
		FILTER_ERROR_CLEANUP("Unsupported format");

	err = snd_pcm_hw_params_set_channels(handle, params, max_ch);

	if (err < 0)
		FILTER_ERROR_CLEANUP("unsupported number of channels");

	err = snd_pcm_hw_params_set_rate_near(handle, params, rate, 0);

	if (err < 0)
		FILTER_ERROR_CLEANUP("unsupported sample rate");
	
	/* doesn't do anything, maybe we can tweak latency with it? */
	err = snd_pcm_hw_params_set_tick_time_near(handle, params, 1000, 0);

	if (err < 0)
		FILTER_ERROR_CLEANUP("unsupported tick time");
	
	buffer_time = snd_pcm_hw_params_set_buffer_time_near(handle, params, 
							40000, 0);

	period_time = buffer_time >> 2;
	/* if your computer is fast enough you can leave 5ms chunksize,
	 * my Maestro doesn't like it */
	snd_pcm_hw_params_set_period_time_near(handle, params, 10000, 0);

	err = snd_pcm_hw_params(handle, params);

	if (err < 0) {
		snd_pcm_hw_params_dump(params, log);
		FILTER_ERROR_CLEANUP("unable to install hardware params");
	}

	snd_pcm_hw_params_dump(params, log);
	snd_pcm_sw_params_current(handle, swparams);
	snd_pcm_sw_params_dump(swparams, log);

	blksz = snd_pcm_hw_params_get_period_size(params, 0);
	
	DPRINTF("Got period size%d\n", blksz);

	out = (gl_s16 *)malloc(blksz*ssize*max_ch);
	if (!out)
		FILTER_ERROR_CLEANUP("Not enough memory for conversion buffer.");

	wbuf = out;

	pos=0;
	pos_param = filterparamdb_get_param(filter_paramdb(n), FILTERPARAM_LABEL_POS);
	filterparam_val_set_pos(pos_param, 0);

	snd_pcm_status_alloca(&status);

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
			if ((ret=snd_pcm_writei(handle, out, blksz)) != blksz) 
			{
				if ((snd_pcm_status(handle, status))<0) {
					FILTER_ERROR_STOP("couldn't get device status");
				}
				if (snd_pcm_status_get_state(status) == SND_PCM_STATE_XRUN) {
					if ((snd_pcm_prepare(handle))<0) {
						FILTER_ERROR_STOP("write error on alsa device");
					}
				}
			}
			filterparam_val_set_pos(pos_param, pos);
			pos+=blksz;
			
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

	snd_pcm_close(handle);
	
	free(out);
	free(in);
	FILTER_RETURN;
					
}


int alsa_audio_out_register(plugin_t *p)
{
	return aio_generic_register_output(p, "alsa-audio-out",
					   alsa_audio_out_f, "plug:0,0");
}

