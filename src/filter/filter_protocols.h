/*
 * filter_protocols.h
 * $Id: filter_protocols.h,v 1.22 2000/08/04 09:28:19 richi Exp $
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
 *   - RMS
 *   - Sample
 *   - Midi
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

/* FFT protocol 
 * The FFT Buffer is initialized with fft_alloc(wsize,filternode) 
 * - wsize is the 
 */

typedef struct fft_header fft_header_t;
struct fft_header {
	int osamp;	/* oversampling factor */
	char buf[1];	/* */
};

#define fft_alloc(wsize, filternode) \
        fbuf_alloc(SAMPLE_SIZE*(wsize) + sizeof(fft_header_t), \
		   &(filternode)->buffers)
#define fft_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(fft_header_t))/SAMPLE_SIZE)
#define fft_buf(fb) ((SAMPLE *)(&((fft_header_t *)fbuf_buf(fb))->buf[0]))
#define fft_ref(fb) fbuf_ref(fb)
#define fft_unref(fb) fbuf_unref(fb)
#define fft_make_private(fb) fbuf_make_private(fb)
#define fft_get(p) fbuf_get(p)
#define fft_queue(p, fb) fbuf_queue(p,fb)

#define fft_set_osamp(fb,os) do { ((fft_header_t *)fbuf_buf(fb))->osamp=os; } while (0)
#define fft_get_osamp(fb) ((fft_header_t *)fbuf_buf(fb))->osamp

/* A try for an RMS protocol
 * We don't need an actual buffer, "a header should be enough for everyone"
 */

#define RMS_TOTAL  1
#define RMS_WINDOW 2
typedef struct rms_header rms_header_t;
struct rms_header {
	int mode;	/* total/window */
	union {
		/* total stats */
		struct {
			float offset;	/* dc-offset */
			float rms;	/* average rms */
			float peak_rms; /* peak rms in window */
			unsigned long peak_pos;	/* position of window that contains peak rms */
			SAMPLE min,max; /* minimum/maximum sample value */
		} t;
		/* window stats */
		struct {
			float rms;		/* window rms */
			unsigned long pos;	/* window position */
		} w;
	} u;
};

#define rms_alloc(filternode) fbuf_alloc(sizeof(rms_header_t),&(filternode)->buffers)
#define rms_size(fb) ((fb)==NULL?0:sizeof(rms_header_t))
#define rms_buf(fb) ((rms_header_t *)(fbuf_buf(fb)))
#define rms_ref(fb) fbuf_ref(fb)
#define rms_unref(fb) fbuf_unref(fb)
#define rms_make_private(fb) fbuf_make_private(fb)
#define rms_get(p) fbuf_get(p)
#define rms_queue(p, fb) fbuf_queue(p,fb)
		
/* just for your convenience */
		
#define rms_set_mode_total(fb) rms_buf(fb)->mode=RMS_TOTAL
#define rms_set_mode_window(fb) rms_buf(fb)->mode=RMS_WINDOW
#define rms_get_mode(fb) rms_buf(fb)->mode
#define rms_set_total_rms(fb, trms) rms_buf(fb)->u.t.rms=trms
#define rms_get_total_rms(fb) rms_buf(fb)->u.t.rms
#define rms_set_total_offset(fb, toffset) rms_buf(fb)->u.t.offset=toffset
#define rms_get_total_offset(fb) rms_buf(fb)->u.t.offset
#define rms_min(fb) rms_buf(fb)->u.t.min
#define rms_max(fb) rms_buf(fb)->u.t.max
#define rms_set_peak(fb, peakrms, peakpos) \
do { \
	rms_buf(fb)->u.t.peak_rms=peakrms; \
	rms_buf(fb)->u.t.peak_pos=peakpos; \
} while(0)

#define rms_get_peak(fb, prms, ppos) \
do { \
	prms=rms_buf(fb)->u.t.peak_rms; \
	ppos=rms_buf(fb)->u.t.peak_pos; \
} while(0)

#define rms_set_window(fb, wrms, wpos) \
do { \
	rms_buf(fb)->u.w.rms=wrms; \
	rms_buf(fb)->u.w.pos=wpos; \
} while(0)

#define rms_get_window(fb, wrms, wpos) \
do { \
        wrms=rms_buf(fb)->u.w.rms; \
	wpos=rms_buf(fb)->u.w.pos; \
} while(0)

/* Simply the SAMPLE protocol. What do you expect?
 */

typedef struct sbuf_header sbuf_header_t;
struct sbuf_header {
	char buf[1];
};
#define sbuf_alloc(nrsamples, filternode) \
        fbuf_alloc(SAMPLE_SIZE*(nrsamples) + sizeof(sbuf_header_t), \
		   &(filternode)->buffers)
#define sbuf_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(sbuf_header_t))/SAMPLE_SIZE)
#define sbuf_buf(fb) ((SAMPLE *)(&((sbuf_header_t *)fbuf_buf(fb))->buf[0]))
#define sbuf_ref(fb) fbuf_ref(fb)
#define sbuf_unref(fb) fbuf_unref(fb)
#define sbuf_make_private(fb) fbuf_make_private(fb)
#define sbuf_get(p) fbuf_get(p)
#define sbuf_queue(p, fb) fbuf_queue(p, fb)


/* How about some MIDI 
 */

typedef struct mbuf_header {
	char buf[1];
} mbuf_header_t;

#define mbuf_alloc(nrevents, filternode) \
        fbuf_alloc(sizeof(midi_event_t)*(nrevents) + sizeof(mbuf_header_t), \
		   &(filternode)->buffers)
#define mbuf_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(mbuf_header_t))/sizeof(midi_event_t))
#define mbuf_buf(fb) ((midi_event_t *)(&((mbuf_header_t *)fbuf_buf(fb))->buf[0]))
#define mbuf_ref(fb) fbuf_ref(fb)
#define mbuf_unref(fb) fbuf_unref(fb)
#define mbuf_make_private(fb) fbuf_make_private(fb)
#define mbuf_get(p) fbuf_get(p)
#define mbuf_queue(p, fb) fbuf_queue(p, fb)


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
static inline filter_buffer_t *cbuf_alloc(int nr, filter_node_t *n)
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
