#ifndef _FILTER_PIPE_H
#define _FILTER_PIPE_H

/*
 * filter_pipe.h
 * $Id: filter_pipe.h,v 1.2 2000/12/18 09:51:55 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther
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

#include "filter_types.h"
#include "filter_param.h"


/* Filter pipes represent a connection between two
 * instances of a filter. This is per filternode port
 * and depends on both filternode ports filter_port.
 */
#define FILTER_PIPETYPE_UNDEFINED FILTER_PORTTYPE_ANY
#define FILTER_PIPETYPE_SAMPLE    FILTER_PORTTYPE_SAMPLE
#define FILTER_PIPETYPE_RMS       FILTER_PORTTYPE_RMS
#define FILTER_PIPETYPE_MIDI      FILTER_PORTTYPE_MIDI
#define FILTER_PIPETYPE_CONTROL   FILTER_PORTTYPE_CONTROL
#define FILTER_PIPETYPE_FFT       FILTER_PORTTYPE_FFT

/* Common values for hangle value of a filter pipe
 */
#define FILTER_PIPEPOS_LEFT		-M_PI_2
#define FILTER_PIPEPOS_RIGHT		M_PI_2
#define	FILTER_PIPEPOS_CENTRE		0.0	/* Umm, needed at all? [dk] */
#define FILTER_PIPEPOS_DEFAULT		FILTER_PIPEPOS_CENTRE
#define FILTER_PIPEPOS_IS_LEFT(pos)	((pos)<0.0)
#define FILTER_PIPEPOS_IS_RIGHT(pos)	((pos)>0.0)
#define FILTER_PIPEPOS_IS_CENTRE(pos)	((pos)==0.0)
#define FILTER_SAMPLEPIPE_IS_LEFT(fp)	((fp)->u.sample.phi<0.0)
#define FILTER_SAMPLEPIPE_IS_RIGHT(fp)	((fp)->u.sample.phi>0.0)
#define FILTER_SAMPLEPIPE_IS_CENTRE(fp)	((fp)->u.sample.phi==0.0)
#define FILTER_SAMPLEPIPE_MORE_LEFT(fp1,fp2) ((fp1)->u.sample.phi<(fp2)->u.sample.phi)
#define FILTER_SAMPLEPIPE_MORE_RIGHT(fp1,fp2) ((fp1)->u.sample.phi>(fp2)->u.sample.phi)

struct filter_pipe {
	/* lists - source_list is the list of the source port,
	 * dest_list is the list of the destination port.
	 * Note that the destination ports are inputs of the
	 * associated filters and vice versa. */
	struct list_head source_list, dest_list;

	/* pipe context - source and destination ports. */
	filter_port_t *source;
	filter_port_t *dest;

	/* pipe specific parameters on the source/destination side */
	filter_paramdb_t source_params;
	filter_paramdb_t dest_params;

	/* fds used for communication */
	int source_fd, dest_fd;

	/* Signal emitter. Know signals are
	 * GLSIG_PIPE_CHANGED
	 * GLSIG_PIPE_DELETED */
	glsig_emitter_t emitter;

	/* data type specification */
	int type;
	union {
		struct {
			int rate; 	/* sample rate, [Hz] */
			float phi; 	/* polar coordinate, [rad] */
		} sample;
		struct{
			int rate;	/* sample rate, [Hz] */
			float phi;	/* polar coordinate, [rad] */
			int bsize;	/* size of single fft-block in half-complex format (see fftw) */
			int osamp;	/* oversampling factor */
		} fft;
		struct {
			int blocksize;
		} rms;
	        struct {
	                int dummy;
	        } midi;
	        struct {
                        int dummy;
	        } control;
	} u;	
};

/* Public access macros for the filter_pipe_t structure.
 * int filterpipe_type(filter_pipe_t *);
 * glsig_emitter_t *filterpipe_emitter(filter_pipe_t *);
 * filter_port_t *filterpipe_source(filter_pipe_t *);
 * filter_port_t *filterpipe_dest(filter_pipe_t *);
 * filter_paramdb_t *filterpipe_sourceparamdb(filter_pipe_t *);
 * filter_paramdb_t *fitlerpipe_destparamdb(filter_pipe_t *);
 */
#define filterpipe_type(fp) ((fp)->type)
#define filterpipe_emitter(fp) (&(fp)->emitter)
#define filterpipe_source(p) ((p)->source)
#define filterpipe_dest(p) ((p)->dest)
#define filterpipe_sourceparamdb(fp) (&(fp)->source_params)
#define filterpipe_destparamdb(fp) (&(fp)->dest_params)

/* Setters/getters for the sample protocol specific fields.
 * void filterpipe_settype_sample(filter_pipe_t *, int, float);
 * int filterpipe_sample_rate(filter_pipe_t *);
 * float filterpipe_sample_hangle(filter_pipe_t *);
 */
#define filterpipe_settype_sample(fp, freq, hangle) do { \
	(fp)->type = FILTER_PIPETYPE_SAMPLE; \
	(fp)->u.sample.rate = (freq); \
	(fp)->u.sample.phi = (hangle); \
} while (0)
#define filterpipe_sample_rate(fp) ((fp)->u.sample.rate)
#define filterpipe_sample_hangle(fp) ((fp)->u.sample.phi)

/* Setters/getters for the fft protocol specific fields.
 * void filterpipe_settype_fft(filter_pipe_t *, int, float, int, int);
 * int filterpipe_fft_rate(filter_pipe_t *);
 * float filterpipe_fft_hangle(filter_pipe_t *);
 * int filterpipe_fft_bsize(filter_pipe_t *);
 * int filterpipe_fft_osamp(filter_pipe_t *);
 */
#define filterpipe_settype_fft(fp, freq, hangle, bs, os) do { \
	(fp)->type = FILTER_PIPETYPE_FFT; \
	(fp)->u.fft.rate = (freq); \
	(fp)->u.fft.phi = (hangle); \
	(fp)->u.fft.bsize = (bs); \
	(fp)->u.fft.osamp = (os); \
} while (0)
#define filterpipe_fft_rate(fp) ((fp)->u.fft.rate)
#define filterpipe_fft_hangle(fp) ((fp)->u.fft.phi)
#define filterpipe_fft_bsize(fp) ((fp)->u.fft.bsize)
#define filterpipe_fft_osamp(fp) ((fp)->u.fft.osamp)


/* Internal structure to track filterport_connect() commands,
 * needed for correct re-creation in filter_to_string. */
struct fconnection {
	struct list_head list;
	filter_pipe_t *pipe;
	const char *source_filter;
	const char *source_port;
	const char *dest_filter;
	const char *dest_port;
};


#ifdef __cplusplus
extern "C" {
#endif


/* Connection managing API.
 */

/* Connect the two ports source and dest with a pipe, returns the created
 * pipe on success, or NULL on error. */
filter_pipe_t *filterport_connect(filter_port_t *source, filter_port_t *dest);

/* Breaks a previously established connection and deletes the associated
 * pipe. */
void filterpipe_delete(filter_pipe_t *pipe);


#ifdef __cplusplus
}
#endif


#endif
