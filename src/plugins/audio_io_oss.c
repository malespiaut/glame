/*
 * audio_io_oss.c
 * $Id: audio_io_oss.c,v 1.2 2001/04/11 13:12:46 richi Exp $
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

#ifdef HAVE_OSS_LINUX
#include <linux/soundcard.h>
#elif HAVE_OSS_SYS
#include <sys/soundcard.h>
#elif HAVE_OSS_MACHINE
#include <machine/soundcard.h>
#else
#error BUG in OSS configuration.
#endif
#include <fcntl.h>
#include <sys/ioctl.h>
#include "audio_io.h"

PLUGIN_SET(audio_io_oss, "oss-audio-out")


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
void oss_convert_bufs(oss_audioparam_t *in, gl_u8 *out, int max_ch,
                      int ssize, int chunk_size, int interleave)
{
	int i, done;

	for (i = 0; i < max_ch; i++) {
		if (!in[i].buf) {
			/* No more input - add silence. */
			if (ssize == 1) {
				gl_u8 neutral = SAMPLE2UCHAR(0.0);
				for (done = 0; done < chunk_size; done++)
					*(out + done*interleave + i) = neutral;
			} else {
				/* ssize == 2 */
				gl_u16 neutral = SAMPLE2USHORT(0.0);
				for (done = 0; done < chunk_size; done++)
					*(gl_u16 *)(out + done*interleave 
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
				*(gl_u16 *)(out + done*interleave +  2*i) =
					SAMPLE2USHORT(sbuf_buf(in[i].buf)[in[i].pos++]);

		in[i].to_go -= done;
	}		
}


/* OSS returns the max number of hardware channels supported,
 * must take care!
 */
static int oss_audio_out_f(filter_t *n)
{

	oss_audioparam_t	*in = NULL;
	gl_u8			*out = NULL;
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
	dev = open(filterparam_val_string(dev_param), O_WRONLY);
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
	out = (gl_u8 *)malloc(blksz);
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


int oss_audio_out_register(plugin_t *p)
{
	return aio_generic_register_output(p, "oss-audio-out",
					   oss_audio_out_f, "/dev/dsp");
}

