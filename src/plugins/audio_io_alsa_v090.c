/*
 * audio_io_alsa_v090.c
 * $Id: audio_io_alsa_v090.c,v 1.9 2001/07/09 08:02:38 mag Exp $
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

PLUGIN_SET(audio_io_alsa, "alsa_audio_in alsa_audio_out")

/*
configure code shamelessly stolen from Paul Barton Davis       
*/

int configure_pcm(snd_pcm_t *handle, 
		  snd_pcm_hw_params_t *hw_params, 
		  snd_pcm_sw_params_t *sw_params, 
		  int chancnt,
		  int rate,
		  int periodsize)
{
  /* just hardcoded fragments = 4 here, because it works with my hoontech card 
   * FIXME? should we make fragments user configurable
   */

	int err, format, fragments = 2 , psize = 0, dir = 0;
	int minp, maxp;
	
	if ((err = snd_pcm_hw_params_any (handle, hw_params)) < 0)  {
		DPRINTF("ALSA: no playback configurations available\n");
		return -1;
	}

	if ((err = snd_pcm_hw_params_set_periods_integer (handle, hw_params)) < 0) {
		DPRINTF( "ALSA: cannot restrict period size to integral value.\n");
		return -1;
	}
	
	/*
	if ((err = snd_pcm_hw_params_set_access (handle, hw_params, 
						 SND_PCM_ACCESS_MMAP_NONINTERLEAVED)) < 0) {
		if ((err = snd_pcm_hw_params_set_access (handle, hw_params, 
							 SND_PCM_ACCESS_MMAP_INTERLEAVED)) < 0) {
			DPRINTF("ALSA: mmap-based access is not possible\n");
			return -1;
		}
	}
	*/

	if ((err = snd_pcm_hw_params_set_access(handle, hw_params,
						SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
		DPRINTF("ALSA: can't set interleaved rw access\n");
		return -1;
	}

#ifdef SND_LITTLE_ENDIAN
	format = SND_PCM_FORMAT_S16_LE;
#else
# ifdef SND_BIG_ENDIAN
	format = SND_PCM_FORMAT_S16_BE;
# else
#error Unsupported endianness.
# endif
#endif

	if ((err = snd_pcm_hw_params_set_format (handle, hw_params, format)) < 0) {
		DPRINTF("Sorry. The audio interface doesn't support signed 16 format");
		return -1;
	}

	if ((err = snd_pcm_hw_params_set_rate_near(handle, hw_params, rate, 0)) < 0) {
		DPRINTF( "ALSA: cannot set sample/frame rate to %d\n", rate);
		return -1;
	}

	/*	nchns = snd_pcm_hw_params_get_channels_max (hw_params);*/
	
	DPRINTF("got %d channels to allocate\n", chancnt);

	if ((err = snd_pcm_hw_params_set_channels (handle, hw_params, chancnt)) < 0) {
		DPRINTF("ALSA: cannot set channel count to %d\n", chancnt);
		return -1;
	}

	/* ALSA suxx */
	
	minp = 2;
	maxp = snd_pcm_hw_params_get_periods_max(hw_params, 0);

	DPRINTF("minp = %d, maxp =%d\n", minp, maxp);

	fragments = minp;
	
	while ((snd_pcm_hw_params_test_periods(handle, hw_params, fragments++, 0)<0) && (fragments<maxp));

	if ((err = snd_pcm_hw_params_set_periods(handle, hw_params, fragments-1, 0)) < 0) {
		DPRINTF("ALSA: cannot set fragment count minimum to %d\n", fragments);
		DPRINTF("%s\n", snd_strerror(err));
		return -1;
	}
   
	fragments = snd_pcm_hw_params_get_periods(hw_params, 0);

	DPRINTF("Now we've got %d periods. Amazing.\n", fragments);

	psize = periodsize;
	
	while ((snd_pcm_hw_params_test_period_size(handle, hw_params, psize++, 0)<0) && (psize<65535));
	
	if ((err = snd_pcm_hw_params_set_period_size (handle, hw_params, psize-1, 0)) < 0) {
		DPRINTF( "ALSA: cannot set fragment length to %d\n", psize);
		psize = snd_pcm_hw_params_get_period_size(hw_params, &dir);
		DPRINTF("ALSA: set to %d\n", psize);
		return -1;
	}

	psize = snd_pcm_hw_params_get_period_size(hw_params, &dir);

	if ((err = snd_pcm_hw_params_set_buffer_size (handle, hw_params, fragments * psize)) < 0) {
		DPRINTF( "ALSA: cannot set buffer length to %d\n", fragments * psize);
		/*return -1;*/
	}

	if ((err = snd_pcm_hw_params (handle, hw_params)) < 0) {
		DPRINTF( "ALSA: cannot set hardware parameters\n");
		return -1;
	}

	snd_pcm_sw_params_current (handle, sw_params);

	if ((err = snd_pcm_sw_params_set_start_threshold (handle, sw_params, 0)) < 0) {
		DPRINTF( "ALSA: cannot set start threshold\n");
		return -1;
	}

	if ((err = snd_pcm_sw_params_set_stop_threshold (handle, sw_params, fragments * psize)) < 0) {
		DPRINTF( "ALSA: cannot set stop threshold\n");
		return -1;
	}

	/* dunno what the silence stuff is about ? someone please enlighten me */
	if ((err = snd_pcm_sw_params_set_silence_threshold (handle, sw_params, 0)) < 0) {
		DPRINTF( "ALSA: cannot set silence threshold\n");
		return -1;
	}

	if ((err = snd_pcm_sw_params_set_silence_size (handle, sw_params, 
						       0)) < 0) { /* fragments = 0*/
		DPRINTF("ALSA: cannot set silence size\n");
		return -1;
	}

	if ((err = snd_pcm_sw_params_set_avail_min (handle, sw_params, psize)) < 0) {
		DPRINTF("ALSA: cannot set avail min\n");
		return -1;
	}

	if ((err = snd_pcm_sw_params (handle, sw_params)) < 0) {
		DPRINTF("ALSA: cannot set software parameters\n");
		return -1;
	}

	return 0;
}


static int alsa_audio_in_f(filter_t *n)
{
	typedef struct {
		filter_pipe_t	*pipe;
		filter_buffer_t	*buf;
		int		pos;
		int		to_go;
	} alsa_audioparam_t;
	
	alsa_audioparam_t	*in = NULL;
	gl_s16			*buf = NULL, *is;
	filter_port_t           *outport;
	filter_pipe_t		*pipe[2];
	filter_buffer_t         *obuf;
	filter_param_t		*param;
	
	/* Alsa Stuff */

	int rate = GLAME_DEFAULT_SAMPLERATE;
	int blksz;
	long maxsamples = 0, nsamples, dropouts;
	int endless = 0;
	int chancnt;	
	int dir, i, err, j, ret;
	SAMPLE *s;

	snd_pcm_t		*handle;
	snd_pcm_hw_params_t	*params;
        snd_pcm_sw_params_t	*swparams;
	snd_pcm_status_t 	*status;
	static snd_output_t 	*log;
	char*			pcm_name="plughw:0,0";	
	
	outport = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	chancnt = filterport_nrpipes(outport);
	if (chancnt==0)
		FILTER_ERROR_RETURN("No output channels!");
	if (chancnt>2)
		FILTER_ERROR_RETURN("Only 2 channels supported");
	

	pipe[0] = filterport_get_pipe(outport);
	pipe[1] = filterport_next_pipe(outport, pipe[0]);

	if (pipe[1] && filterpipe_sample_hangle(pipe[0]) > 
		       filterpipe_sample_hangle(pipe[1])) 
	{ 
		filter_pipe_t *t = pipe[0]; 
		pipe[0] = pipe[1]; 
		pipe[1] = t; 
	}

	param = filterparamdb_get_param(filter_paramdb(n), "device");
	if (param)
		pcm_name = filterparam_val_string(param);
	
	param = filterparamdb_get_param(filter_paramdb(n), "rate");
	if (param)
		rate = filterparam_val_int(param);

	param = filterparamdb_get_param(filter_paramdb(n), "duration");
	if (param)
		maxsamples = (int)(filterparam_val_float(param) * rate);
	if (maxsamples <= 0.0)
		endless = 1;
	
	/* ALSA specific initialisation.
	 */
	
	if (snd_pcm_open(&handle, pcm_name, SND_PCM_STREAM_CAPTURE, 0))
		FILTER_ERROR_CLEANUP("Could not open audio device.");
	
	/* FIXME: Can we assume s16 native endian is always supported?
	 *        (The plugin stuff performs a limited set of conversions.)
	 */

	err = snd_output_stdio_attach(&log, stderr, 0);

	snd_pcm_hw_params_alloca(&params);
	snd_pcm_sw_params_alloca(&swparams);
	err = snd_pcm_hw_params_any(handle, params);
	
	configure_pcm(handle, params, swparams, chancnt, rate, GLAME_WBUFSIZE);

	snd_pcm_sw_params_dump(swparams, log);
	snd_pcm_dump(handle, log);

	blksz = snd_pcm_hw_params_get_period_size(params, &dir);
	
	if (blksz<0)
		DPRINTF("%s\n", snd_strerror(blksz));
		
	DPRINTF("blksz=%d, chancnt=%d\n", blksz, chancnt);

	buf = ALLOCN(blksz*chancnt, gl_s16);

	if (!buf)
		FILTER_ERROR_CLEANUP("Not enough memory for conversion buffer.");

	snd_pcm_status_alloca(&status);
	nsamples = 0;
	dropouts = 0;

	FILTER_AFTER_INIT;
	
	while (endless || nsamples < maxsamples) {
		FILTER_CHECK_STOP;

		if ((ret=snd_pcm_readi(handle, buf, blksz)) != blksz)
			if ((snd_pcm_status(handle, status))<0) {
				FILTER_ERROR_STOP("couldn't get device status");
			}
		if (snd_pcm_status_get_state(status) == SND_PCM_STATE_XRUN) {
			dropouts ++;
			if ((snd_pcm_prepare(handle))<0) {
				FILTER_ERROR_STOP("read error on alsa device");
			}
		}
		for(i=0;i<chancnt;i++){
			obuf = sbuf_make_private(sbuf_alloc(blksz, n));
			s = sbuf_buf(obuf);
			is = buf + i;
			for(j=0;j<blksz;j++) {
				*s++ = SHORT2SAMPLE(*is);
				is += chancnt;
			}
			sbuf_queue(pipe[i], obuf);
		}
	}
	FILTER_BEFORE_CLEANUP;
	FILTER_BEFORE_STOPCLEANUP;

	for(i=0;i<chancnt;i++)
		sbuf_queue(pipe[i], NULL);

	DPRINTF("had %ld dropouts while capturing\n", dropouts);

	snd_pcm_close(handle);
	
	free(buf);
	free(in);
	FILTER_RETURN;
}

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
	
	filter_param_t *dev_param, *pos_param;
	
	int rate, blksz, ssize;

	int ch, max_ch, ch_active;
	int chunk_size;
	int to_go, pos;
	
	int dir, i, err, dropouts;
	int fragments;

	snd_pcm_t		*handle;
	snd_pcm_format_t	format;
	snd_pcm_hw_params_t	*params;
        snd_pcm_sw_params_t	*swparams;
	snd_pcm_status_t 	*status;
	static snd_output_t 	*log;
	char*			pcm_name="plughw:0,0";	

	
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
#else
# ifdef SND_BIG_ENDIAN
	format = SND_PCM_FORMAT_S16_BE;
# else
#error Unsupported endianness.
# endif
#endif

	err = snd_output_stdio_attach(&log, stderr, 0);

	snd_pcm_hw_params_alloca(&params);
	snd_pcm_sw_params_alloca(&swparams);
	
	err = configure_pcm(handle, params, swparams, max_ch, rate, GLAME_WBUFSIZE);

	if (err == -1)
		FILTER_ERROR_CLEANUP("configuration of audio device failed");

	snd_pcm_sw_params_dump(swparams, log);
	snd_pcm_dump(handle, log);
	
	blksz = snd_pcm_hw_params_get_period_size(params, &dir);

	DPRINTF("Got period size%d\n", blksz);
	
	fragments = snd_pcm_hw_params_get_periods(params, 0);

	out = (gl_s16 *)malloc(blksz*ssize*max_ch);
	if (!out)
		FILTER_ERROR_CLEANUP("Not enough memory for conversion buffer.");

	wbuf = out;

	memset(out, 0, blksz*ssize*max_ch);
	
	for(i=0;i<fragments;i++)
		snd_pcm_writei(handle, out, blksz);
		
	pos=0;
	pos_param = filterparamdb_get_param(filter_paramdb(n), FILTERPARAM_LABEL_POS);
	filterparam_val_set_pos(pos_param, 0);

	snd_pcm_status_alloca(&status);

	FILTER_AFTER_INIT;
			
	ch_active = ch;
	to_go = blksz;
	dropouts = 0;

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
					dropouts ++;
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
	
	DPRINTF("had %d dropouts\n", dropouts);

	free(out);
	free(in);
	FILTER_RETURN;
					
}


int alsa_audio_out_register(plugin_t *p)
{
	return aio_generic_register_output(p, "alsa-audio-out",
					   alsa_audio_out_f, "plughw:0,0");
}

int alsa_audio_in_register(plugin_t *p)
{
	return aio_generic_register_input(p, "alsa-audio-in",
					   alsa_audio_in_f, "plughw:0,0");
}
