/*
 * audio_io_esd.c
 * $Id: audio_io_esd.c,v 1.11 2002/04/29 18:20:40 richi Exp $
 *
 * Copyright (C) 2001,2002 Richard Guenther, Alexander Ehlert, Daniel Kobras
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

#include <esd.h>
#include "audio_io.h"

PLUGIN_SET(audio_io_esd, "esd_audio_out")


/* I don't know what I'm doing, but I just try to 
 * write a simple esound output filter... */
static int esd_out_f(filter_t *n)
{
	typedef struct {
		filter_pipe_t	*pipe;
		filter_buffer_t	*buf;
		int 		pos;
		int 		to_go;
	} esdout_param_t;
	esdout_param_t		*in = NULL;
	gl_s16			neutral, *wbuf, *out = NULL;
	filter_port_t *inport;
	filter_pipe_t		*p_in;
	filter_param_t *dev_param, *pos_param;
	char *host = NULL;
	
	int rate, ssize;
	int max_ch, ch, ch_active, to_go;
	ssize_t blksz, chunk_size;
	int i, pos = 0;
	
	esd_format_t format = ESD_BITS16 | ESD_STREAM | ESD_PLAY;
	int esound_socket;

	/* Boilerplate init section - will go into a generic function one day.
	 */

	inport = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	if (!(max_ch = filterport_nrpipes(inport)))
		FILTER_ERROR_RETURN("no inputs");

	p_in = filterport_get_pipe(inport);
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
	} while ((p_in = filterport_next_pipe(inport, p_in)));

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

	dev_param = filterparamdb_get_param(filter_paramdb(n), "device");
	if (dev_param)
		host = filterparam_val_string(dev_param);
	format |= (max_ch == 1) ? ESD_MONO : ESD_STEREO;
	/* Beware. If no esd server was running, it will be started now.
	 * Startup latency is really erratic. I've seen it jump from
	 * unmeasurable to 10 seconds if only one more thread was spawned in
	 * the filter chain! 100% reproducable. This sucks.
	 */
	DPRINTF("Starting up esd stream - occasionally takes a long time.\n");
	esound_socket = esd_play_stream_fallback(format, rate, host, NULL);
	if (esound_socket <= 0)
	        FILTER_ERROR_RETURN("couldn't open esd-socket connection!");

	ssize = sizeof(gl_s16);	/* Yes, I know... */
	neutral = SAMPLE2SHORT(0.0);
	blksz = ESD_BUF_SIZE;
	out = (gl_s16 *)malloc(blksz * max_ch * ssize);
	if (!out)
		FILTER_ERROR_RETURN("couldn't alloc wbuf!");
	wbuf = out;

	pos_param = filterparamdb_get_param(filter_paramdb(n),
					    FILTERPARAM_LABEL_POS);
	filterparam_val_set_pos(pos_param, 0);

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

			filterparam_val_set_pos(pos_param, pos);
			pos += blksz;
			
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


int esd_audio_out_register(plugin_t *p)
{
	return aio_generic_register_output(p, "esd-audio-out",
					   esd_out_f, NULL);
}
