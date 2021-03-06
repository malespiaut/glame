#ifndef _FILTER_PIPE_H
#define _FILTER_PIPE_H

/*
 * filter_pipe.h
 * $Id: filter_pipe.h,v 1.10 2004/10/23 13:09:22 richi Exp $
 *
 * Copyright (C) 2000, 2001 Richard Guenther
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
#define FILTER_PIPETYPE_SSP       FILTER_PORTTYPE_SSP
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
	struct glame_list_head source_list, dest_list;

	/* pipe context - source and destination ports. */
	filter_port_t *source;
	filter_port_t *dest;

	/* "real" connection as requested from filterport_connect(). */
	struct glame_list_head list;
	filter_port_t *real_source;
	filter_port_t *real_dest;

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
			int bsize;	/* length of running average */
			int rate;	/* sample rate */
		} ssp;
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

#define filterpipe_settype_ssp(fp, freq, bs) do { \
	(fp)->type = FILTER_PIPETYPE_SSP; \
	(fp)->u.ssp.rate = (freq); \
	(fp)->u.ssp.bsize = (bs); \
} while (0)
#define filterpipe_ssp_rate(fp) ((fp)->u.ssp.rate)
#define filterpipe_ssp_bsize(fp) ((fp)->u.ssp.bsize)


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

/* Checks, if a pipe is inside a feedback loop. Returns 1 if this is
 * the case, 0 otherwise. */
int filterpipe_is_feedback(filter_pipe_t *pipe);

/* filter_port_t *filterpipe_connection_source(filter_pipe_t *fp);
 * Queries the source port of the pipe fp that was initially passed to
 * filterport_connect() as source. Returns NULL, if this port cannot
 * be found anymore (possible, if the connection was automatically
 * redirected and the source node was deleted). */
#define filterpipe_connection_source(p) ((p)->real_source)

/* filter_port_t *filterpipe_connection_dest(filter_pipe_t *fp);
 * Queries the destination port of the pipe fp that was initially passed to
 * filterport_connect() as destination. Returns NULL, if this port cannot
 * be found anymore (possible, if the connection was automatically
 * redirected and the source node was deleted). */
#define filterpipe_connection_dest(p) ((p)->real_dest)


#ifdef __cplusplus
}
#endif


#endif
