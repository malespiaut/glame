/*
 * filter_protocols.h
 * $Id: filter_protocols.h,v 1.28 2001/05/03 17:46:56 mag Exp $
 *
 * Copyright (C) 2000 Daniel Kobras, Richard Guenther, Alexander Ehlert
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
 * This file contains the standard protocols
 *   - FFT
 *   - SSP
 *   - Sample
 *   - Control input
 */

#ifndef _FILTER_PROTOCOLS_H
#define _FILTER_PROTOCOLS_H

#include <sys/time.h>
#include <unistd.h>


/* Standard portnames.
 */
#define PORTNAME_IN "in"
#define PORTNAME_OUT "out"

/* squared sample protocol (ssp)
 * we just send a running squared sample average through a pipe
 * everyone who wants the rms should just get the square himself
 */
typedef struct ssp_header ssp_header_t;
struct ssp_header {
	char buf[1];
};
#define ssp_alloc(nrsamples, filternode) \
        fbuf_alloc(SAMPLE_SIZE*(nrsamples) + sizeof(ssp_header_t), \
		   &(filternode)->buffers)
#define ssp_realloc(ssp, nrsamples) \
	fbuf_realloc(ssp, SAMPLE_SIZE*(nrsamples) + sizeof(ssp_header_t))
#define ssp_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(ssp_header_t))/SAMPLE_SIZE)
#define ssp_buf(fb) ((SAMPLE *)(&((ssp_header_t *)fbuf_buf(fb))->buf[0]))
#define ssp_ref(fb) fbuf_ref(fb)
#define ssp_unref(fb) fbuf_unref(fb)
#define ssp_make_private(fb) fbuf_make_private(fb)
#define ssp_get(p) fbuf_get(p)
#define ssp_queue(p, fb) fbuf_queue(p, fb)

/* Sample protocol
 * is either used for sample- AND fft-protocol
 */

typedef struct sbuf_header sbuf_header_t;
struct sbuf_header {
	char buf[1];
};
#define sbuf_alloc(nrsamples, filternode) \
        fbuf_alloc(SAMPLE_SIZE*(nrsamples) + sizeof(sbuf_header_t), \
		   &(filternode)->buffers)
#define sbuf_realloc(sbuf, nrsamples) \
	fbuf_realloc(sbuf, SAMPLE_SIZE*(nrsamples) + sizeof(sbuf_header_t))
#define sbuf_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(sbuf_header_t))/SAMPLE_SIZE)
#define sbuf_buf(fb) ((SAMPLE *)(&((sbuf_header_t *)fbuf_buf(fb))->buf[0]))
#define sbuf_ref(fb) fbuf_ref(fb)
#define sbuf_unref(fb) fbuf_unref(fb)
#define sbuf_make_private(fb) fbuf_make_private(fb)
#define sbuf_get(p) fbuf_get(p)
#define sbuf_queue(p, fb) fbuf_queue(p, fb)


/* Control input protocol. Command/value style, provides timestamp.
 * Any number of compound commands per buffer.
 */

typedef struct cbuf_header cbuf_header_t;
struct cbuf_header {
	struct timeval time;
	char buf[1];
};
typedef struct {
	int cmd;
	union {
		int dummy;
	} u;
} cbuf_command_t;
static inline filter_buffer_t *cbuf_alloc(int nr, filter_t *n)
{
	filter_buffer_t *buf;
	cbuf_header_t *h;

        if (!(buf = fbuf_alloc(sizeof(cbuf_header_t)
			       + sizeof(cbuf_command_t)*nr, &n->buffers)))
		return NULL;
	h = (cbuf_header_t *)fbuf_buf(buf);
	gettimeofday(&h->time, NULL);

	return buf;
}
#define cbuf_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(cbuf_header_t))/sizeof(cbuf_command_t))
#define cbuf_buf(fb) ((cbuf_command_t *)(&((cbuf_header_t *)fbuf_buf(fb))->buf[0]))
#define cbuf_ref(fb) fbuf_ref(fb)
#define cbuf_unref(fb) fbuf_unref(fb)
#define cbuf_make_private(fb) fbuf_make_private(fb)
#define cbuf_get(p) fbuf_get(p)
#define cbuf_queue(p, fb) fbuf_queue(p, fb)


#endif
