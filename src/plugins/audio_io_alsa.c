/*
 * audio_io_alsa_v090.c
 *
 * $Id: audio_io_alsa.c,v 1.17 2003/04/15 20:27:57 richi Exp $
 *
 * Copyright (C) 2001 Richard Guenther, Alexander Ehlert, Daniel Kobras
 * thanks to Josh Green(http://smurf.sourceforge.net) for various fixes
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
#include <endian.h>
#include <pthread.h>

#define ALSA_PCM_NEW_HW_PARAMS_API

#ifdef ALSA_H_IN_SYS
#include <sys/asoundlib.h>
#else
#include <alsa/asoundlib.h>
#endif

PLUGIN_SET(audio_io_alsa, "alsa_audio_in alsa_audio_out midi_in_alsa")

/*
configure code shamelessly stolen from Paul Barton Davis       
*/
pthread_mutex_t audio_io_mutex = PTHREAD_MUTEX_INITIALIZER;

int configure_pcm(snd_pcm_t * handle,
		  snd_pcm_hw_params_t * hw_params,
		  snd_pcm_sw_params_t * sw_params,
		  int chancnt,
		  int rate, snd_pcm_uframes_t periodsize, int nperiods)
{
	int err, format, dir = 0;
	snd_pcm_uframes_t buffer_size;

	if ((err = snd_pcm_hw_params_any(handle, hw_params)) < 0) {
		DPRINTF("ALSA: no configurations available\n");
		return -1;
	}

	if ((err = snd_pcm_hw_params_set_access(handle, hw_params,
						SND_PCM_ACCESS_RW_INTERLEAVED))
	    < 0) {
		DPRINTF("ALSA: can't set interleaved rw access\n");
		return -1;
	}
#if __BYTE_ORDER == __LITTLE_ENDIAN
	format = SND_PCM_FORMAT_S16_LE;
#elif __BYTE_ORDER == __BIG_ENDIAN
	format = SND_PCM_FORMAT_S16_BE;
#else
#error Unsupported endianness.
#endif

	if ((err =
	     snd_pcm_hw_params_set_format(handle, hw_params,
					  format)) < 0) {
		DPRINTF
		    ("Sorry. The audio interface doesn't support signed 16 format");
		return -1;
	}

	if ((err =
	     snd_pcm_hw_params_set_rate_near(handle, hw_params, &rate,
					     0)) < 0) {
		DPRINTF("ALSA: cannot set sample/frame rate to %d\n",
			rate);
		return -1;
	}

	/*      nchns = snd_pcm_hw_params_get_channels_max (hw_params); */

	DPRINTF("got %d channels to allocate\n", chancnt);

	if ((err =
	     snd_pcm_hw_params_set_channels(handle, hw_params,
					    chancnt)) < 0) {
		DPRINTF("ALSA: cannot set channel count to %d\n", chancnt);
		return -1;
	}

	/* ALSA doesn't suck :) (that's Josh's opinion) */
	/*
	   DPRINTF("trying to set periods to %d\n", nperiods);

	   if ((err = snd_pcm_hw_params_set_periods_near(handle, hw_params,
	   nperiods, 0)) < 0) {
	   DPRINTF("ALSA: failed to set period count near %d\n", nperiods);
	   DPRINTF("%s\n", snd_strerror(err));
	   return -1;
	   }
	 */

	if ((err =
	     snd_pcm_hw_params_set_period_size_near(handle, hw_params,
						    &periodsize, 0)) < 0) {
		DPRINTF("ALSA: failed to set period size near %ld\n",
			periodsize);
		snd_pcm_hw_params_get_period_size(hw_params, &periodsize,
						  &dir);
		DPRINTF("ALSA: set to %ld\n", periodsize);
		return -1;
	}

	buffer_size = periodsize * nperiods;

	DPRINTF("periodsize = %ld\n", periodsize);

	DPRINTF("trying to set buffer_size to %ld\n", buffer_size);

	if ((err =
	     snd_pcm_hw_params_set_buffer_size_near(handle, hw_params,
						    &buffer_size)) < 0) {
		DPRINTF("ALSA: snd_pcm_hw_set_buffer_size failed\n");
		return -1;
	}

	if ((err =
	     snd_pcm_hw_params_get_periods(hw_params, &nperiods,
					   &dir)) < 0) {
		DPRINTF("ALSA: snd_pcm_hw_params_get_periods failed\n");
		return -1;
	}

	if ((err =
	     snd_pcm_hw_params_get_buffer_size(hw_params,
					       &buffer_size)) < 0) {
		DPRINTF
		    ("ALSA: snd_pcm_hw_params_get_buffer_size failed\n");
		return -1;
	}

	DPRINTF("periodsize = %ld, periods=%d\n", periodsize, nperiods);
	DPRINTF("buffer_size = %ld\n", buffer_size);

	if ((err = snd_pcm_hw_params(handle, hw_params)) < 0) {
		DPRINTF("ALSA: cannot set hardware parameters\n");
		return -1;
	}

	snd_pcm_sw_params_current(handle, sw_params);

	if ((err =
	     snd_pcm_sw_params_set_start_threshold(handle, sw_params,
						   0)) < 0) {
		DPRINTF("ALSA: cannot set start threshold\n");
		return -1;
	}

	if ((err =
	     snd_pcm_sw_params_set_stop_threshold(handle, sw_params,
						  buffer_size)) < 0) {
		DPRINTF("ALSA: cannot set stop threshold\n");
		return -1;
	}

	if ((err =
	     snd_pcm_sw_params_set_avail_min(handle, sw_params,
					     periodsize)) < 0) {
		DPRINTF("ALSA: cannot set avail min\n");
		return -1;
	}

	if ((err = snd_pcm_sw_params(handle, sw_params)) < 0) {
		DPRINTF("ALSA: cannot set software parameters\n");
		return -1;
	}

	return 0;
}

static int xrun_handle(snd_pcm_t * handle, int ret)
{
	int err = -EPIPE;

	if (ret == -EPIPE) {
		err = snd_pcm_prepare(handle);
		if (err < 0)
			return err;
	} else if (ret == -ESTRPIPE) {
		while ((err = snd_pcm_resume(handle)) == -EAGAIN)
			sleep(1);	/* wait until the suspend flag is released */

		if (err < 0) {
			err = snd_pcm_prepare(handle);
			if (err < 0)
				return err;
		}
	}
	return err;
}

static int alsa_audio_in_f(filter_t * n)
{
	typedef struct {
		filter_pipe_t *pipe;
		filter_buffer_t *buf;
		int pos;
		int to_go;
	} alsa_audioparam_t;

	alsa_audioparam_t *in = NULL;
	gl_s16 *buf = NULL, *is;
	filter_port_t *outport;
	filter_pipe_t **pipe, *p;
	filter_buffer_t *obuf;
	filter_param_t *param, *xrun_param;

	/* Alsa Stuff */

	int rate = GLAME_DEFAULT_SAMPLERATE;
	snd_pcm_uframes_t blksz;
	long maxsamples = 0, nsamples;
	int dropouts;
	int endless = 0;
	int chancnt;
	int dir, i, err, j, ret;
	int handleisok = 0;

	SAMPLE *s;

	snd_pcm_t *handle;
	snd_pcm_hw_params_t *params;
	snd_pcm_sw_params_t *swparams;
	static snd_output_t *log;
	char *pcm_name = "plughw:0,0";

	outport = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	chancnt = filterport_nrpipes(outport);
	if (chancnt == 0)
		FILTER_ERROR_RETURN("No output channels!");

	/* For <=2 channels this makes us record left/right to
	 * the right pipes. For >2 channels, we cant do better, either. */
	i = 0;
	pipe = alloca(chancnt * sizeof(filter_pipe_t *));
	filterport_foreach_pipe(outport, p)
	    pipe[i++] = p;

	if (chancnt > 1
	    && filterpipe_sample_hangle(pipe[0]) >
	    filterpipe_sample_hangle(pipe[1])) {
		filter_pipe_t *t = pipe[0];
		pipe[0] = pipe[1];
		pipe[1] = t;
	}

	param = filterparamdb_get_param(filter_paramdb(n), "device");
	if (param)
		pcm_name = filterparam_val_string(param);

	param = filterparamdb_get_param(filter_paramdb(n), "rate");
	if (param)
		rate = filterparam_val_long(param);

	param = filterparamdb_get_param(filter_paramdb(n), "duration");
	if (param)
		maxsamples = (int) (filterparam_val_double(param) * rate);
	if (maxsamples <= 0.0)
		endless = 1;

	xrun_param = filterparamdb_get_param(filter_paramdb(n), "XRUNs");
	filterparam_val_long(xrun_param) = 0;

	/* ALSA specific initialisation.
	 */

	/* I'm not sure whether snd_pcm_open is threadsafe */

	pthread_mutex_lock(&audio_io_mutex);
	err =
	    snd_pcm_open(&handle, pcm_name, SND_PCM_STREAM_CAPTURE,
			 SND_PCM_NONBLOCK);
	pthread_mutex_unlock(&audio_io_mutex);

	if (err < 0)
		FILTER_ERROR_CLEANUP("Could not open audio device.");
	handleisok = 1;
	/* set back to blocking io */
	snd_pcm_nonblock(handle, 0);

	/* FIXME: Can we assume s16 native endian is always supported?
	 *        (The plugin stuff performs a limited set of conversions.)
	 */

	pthread_mutex_lock(&audio_io_mutex);
	err = snd_output_stdio_attach(&log, stderr, 0);

	snd_pcm_hw_params_alloca(&params);
	snd_pcm_sw_params_alloca(&swparams);

	err =
	    configure_pcm(handle, params, swparams, chancnt, rate,
			  GLAME_WBUFSIZE, 4);
	pthread_mutex_unlock(&audio_io_mutex);
	if (err == -1)
		FILTER_ERROR_CLEANUP
		    ("configuration of audio device failed");

	snd_pcm_sw_params_dump(swparams, log);
	snd_pcm_dump(handle, log);

	snd_pcm_hw_params_get_period_size(params, &blksz, &dir);

	if (blksz < 0)
		DPRINTF("%s\n", snd_strerror(blksz));

	DPRINTF("blksz=%ld, chancnt=%d\n", blksz, chancnt);

	buf = ALLOCN(blksz * chancnt, gl_s16);

	if (!buf)
		FILTER_ERROR_CLEANUP
		    ("Not enough memory for conversion buffer.");

	nsamples = 0;
	dropouts = 0;

	FILTER_AFTER_INIT;

	while (endless || nsamples < maxsamples) {
		FILTER_CHECK_STOP;

		if ((ret = snd_pcm_readi(handle, buf, blksz)) != blksz) {
			if (ret == -EAGAIN)
				continue;

			if (ret < 0) {
				if (xrun_handle(handle, ret) < 0)
					FILTER_ERROR_RETURN
					    ("error handling buffer underrun\n");
				DPRINTF("recording underrun(%d)\n",
					++dropouts);
			}
			continue;
		}

		// We dont mind putting out some extra samples
		// (up to blksz-1).
		for (i = 0; i < chancnt; i++) {
			obuf = sbuf_make_private(sbuf_alloc(blksz, n));
			s = sbuf_buf(obuf);
			is = buf + i;
			for (j = 0; j < blksz; j++) {
				*s++ = SHORT2SAMPLE(*is);
				is += chancnt;
			}
			sbuf_queue(pipe[i], obuf);
		}
		nsamples += blksz;
	}

	FILTER_BEFORE_STOPCLEANUP;

	for (i = 0; i < chancnt; i++)
		sbuf_queue(pipe[i], NULL);
	filterparam_val_long(xrun_param) = dropouts;
	DPRINTF("had %i dropouts while capturing\n", dropouts);

	FILTER_BEFORE_CLEANUP;

	if (handleisok == 1)
		snd_pcm_close(handle);

	free(buf);
	free(in);
	FILTER_RETURN;
}

static int alsa_audio_out_f(filter_t * n)
{
	typedef struct {
		filter_pipe_t *pipe;
		filter_buffer_t *buf;
		int pos;
		int to_go;
	} alsa_audioparam_t;

	alsa_audioparam_t *in = NULL;
	gl_s16 neutral, *wbuf, *out = NULL;
	filter_port_t *inport;
	filter_pipe_t *p_in;

	filter_param_t *dev_param, *pos_param;

	int rate, ssize;
	snd_pcm_uframes_t blksz;

	int ch, max_ch, ch_active;
	int chunk_size;
	int to_go, pos;

	int dir, i, err, dropouts;
	int fragments;
	int handleisok = 0;

	snd_pcm_t *handle;
	snd_pcm_format_t format;
	snd_pcm_hw_params_t *params;
	snd_pcm_sw_params_t *swparams;
	static snd_output_t *log;
	char *pcm_name = "plughw:0,0";
	ssize_t ret;

	/* Boilerplate init section - will go into a generic function one day.
	 */

	inport = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	max_ch = filterport_nrpipes(inport);
	if (max_ch == 0)
		FILTER_ERROR_RETURN("No input channels given.");

	p_in = filterport_get_pipe(inport);
	rate = filterpipe_sample_rate(p_in);
	if (rate <= 0)
		FILTER_ERROR_RETURN("No valid sample rate given.");

	in = (alsa_audioparam_t *) malloc(max_ch *
					  sizeof(alsa_audioparam_t));
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
	if (filterparam_val_string(dev_param) != NULL)
		pcm_name = filterparam_val_string(dev_param);

	/* I'm not sure whether snd_pcm_open is threadsafe */

	pthread_mutex_lock(&audio_io_mutex);
	err =
	    snd_pcm_open(&handle, pcm_name, SND_PCM_STREAM_PLAYBACK,
			 SND_PCM_NONBLOCK);
	pthread_mutex_unlock(&audio_io_mutex);

	if (err < 0) {
		DPRINTF("Failed to open ALSA audio device: %s\n",
			snd_strerror(err));
		FILTER_ERROR_CLEANUP("Could not open audio device.");
	}

	handleisok = 1;
	/* set back to blocking io */
	snd_pcm_nonblock(handle, 0);

	/* FIXME: Can we assume s16 native endian is always supported?
	 *        (The plugin stuff performs a limited set of conversions.)
	 */
	ssize = 2;
	neutral = SAMPLE2SHORT(0.0);

#if __BYTE_ORDER == __LITTLE_ENDIAN
	format = SND_PCM_FORMAT_S16_LE;
#elif __BYTE_ORDER == __BIG_ENDIAN
	format = SND_PCM_FORMAT_S16_BE;
#else
#error Unsupported endianness.
#endif

	pthread_mutex_lock(&audio_io_mutex);
	err = snd_output_stdio_attach(&log, stderr, 0);

	snd_pcm_hw_params_alloca(&params);
	snd_pcm_sw_params_alloca(&swparams);

	err =
	    configure_pcm(handle, params, swparams, max_ch, rate,
			  GLAME_WBUFSIZE, 4);
	pthread_mutex_unlock(&audio_io_mutex);

	if (err == -1)
		FILTER_ERROR_CLEANUP
		    ("configuration of audio device failed");

	snd_pcm_sw_params_dump(swparams, log);
	snd_pcm_dump(handle, log);

	snd_pcm_hw_params_get_period_size(params, &blksz, &dir);

	DPRINTF("Got period size %ld\n", blksz);

	snd_pcm_hw_params_get_periods(params, &fragments, 0);

	out = (gl_s16 *) malloc(blksz * ssize * max_ch);
	if (!out)
		FILTER_ERROR_CLEANUP
		    ("Not enough memory for conversion buffer.");

	wbuf = out;

	memset(out, 0, blksz * ssize * max_ch);

	pos = 0;
	pos_param =
	    filterparamdb_get_param(filter_paramdb(n),
				    FILTERPARAM_LABEL_POS);
	filterparam_val_set_pos(pos_param, 0);

	FILTER_AFTER_INIT;

	DPRINTF("init with %d fragments.\n", fragments);


	for (i = 0; i < fragments; i++) {
		if ((ret = snd_pcm_writei(handle, out, blksz)) != blksz) {
			if (ret == -EAGAIN)
				continue;

			if (ret < 0) {
				if (xrun_handle(handle, ret) < 0)
					FILTER_ERROR_RETURN
					    ("error handling buffer underrun\n");
				DPRINTF("playback underrun(%d)\n",
					++dropouts);
			}
		}
	}

	DPRINTF("init done\n");

	ch_active = ch;
	to_go = blksz;
	dropouts = 0;

	goto _entry;

	do {

		/* Convert to signed 16 bit. */
		for (i = 0; i < max_ch; i++) {
			int done;

			if (!in[i].buf) {
				for (done = 0; done < chunk_size; done++)
					out[max_ch * done + i] = neutral;
				continue;
			}
			for (done = 0; done < chunk_size; done++)
				out[max_ch * done + i] =
				    SAMPLE2SHORT(sbuf_buf(in[i].buf)
						 [in[i].pos++]);
			in[i].to_go -= done;
		}

		to_go -= chunk_size;

		if (!to_go) {
			out = wbuf;
			to_go = blksz;
			if ((ret =
			     snd_pcm_writei(handle, out,
					    blksz)) != blksz) {
				if (ret == -EAGAIN)
					continue;
				if (ret < 0) {
					if (xrun_handle(handle, ret) < 0)
						FILTER_ERROR_RETURN
						    ("error handling buffer underrun\n");

					DPRINTF("playback underrun(%d)\n",
						++dropouts);
				}
			}

			filterparam_val_set_pos(pos_param, pos);
			pos += blksz;

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
						"%d active.\n", ch, max_ch,
						ch_active);
				}
				ap->to_go = to_go;
			}
			chunk_size = MIN(chunk_size, ap->to_go);
		} while (++ch < max_ch);
	} while (ch_active || (to_go != blksz));
	
	FILTER_BEFORE_STOPCLEANUP;
	
	snd_pcm_drain(handle);

	DPRINTF("had playback %d dropouts\n", dropouts);

	FILTER_BEFORE_CLEANUP;
	
	if (handleisok == 1)
		snd_pcm_close(handle);

	free(out);
	free(in);
	FILTER_RETURN;

}

int alsa_audio_out_register(plugin_t * p)
{
	return aio_generic_register_output(p, "alsa-audio-out",
					   alsa_audio_out_f, "plughw:0,0");
}

int alsa_audio_in_register(plugin_t * p)
{
	return aio_generic_register_input(p, "alsa-audio-in",
					  alsa_audio_in_f, "plughw:0,0");
}
