/*
 * filter_tools.h
 * $Id: filter_tools.h,v 1.26 2001/01/25 09:16:38 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther, Alexander Ehlert, Daniel Kobras
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

#ifndef _FILTER_TOOLS_H
#define _FILTER_TOOLS_H

#include "simd.h"


/* convert time in ms to number of samples */
#define TIME2CNT(type, time, rate) (type)(((time)*(rate))/1000.0)


/* Add your favorite generic tools for filter programming here.
 */
typedef struct queue queue_t;
struct queue {
	struct list_head	list;
	filter_t		*n;
	filter_pipe_t		*p;
	int			off;
	int			done;
};
#define queue_get_next(q, qe) list_getnext(&(q->list), qe, struct queue_entry, list)
#define queue_get_head(q) list_gethead(&(q->list), struct queue_entry, list)

typedef struct queue_entry queue_entry_t;
struct queue_entry {
	struct list_head list;
	filter_buffer_t *fb;
};

static inline void init_queue(queue_t* q, filter_pipe_t *p, filter_t *n) 
{
	INIT_LIST_HEAD(&(q->list));
	q->p = p;
	q->off = 0;
	q->n = n;
	q->done = 0;
}

static inline void queue_delete(queue_entry_t *qe)
{
	list_del(&qe->list);
	sbuf_unref(qe->fb);
	free(qe);
}

static inline void queue_add_delete(queue_entry_t *qe)
{
	list_del(&qe->list);
	free(qe);
}

static inline queue_entry_t *queue_add_tail(queue_t *q, filter_buffer_t *fb)
{
	struct queue_entry *qe;

	if (!fb)
		return NULL;
	qe = (struct queue_entry *)malloc(sizeof(struct queue_entry));
	INIT_LIST_HEAD(&qe->list);
	qe->fb = fb;
	list_add_tail(&qe->list, &q->list);
	return qe;
}

static inline void queue_drain(queue_t *q)
{
	queue_entry_t *qe;
	while ((qe = queue_get_head(q)))
		queue_delete(qe);
	q->off = 0;
}

static inline int queue_shift(queue_t *q, int off) {
	struct queue_entry *qe, *oldqe;
	filter_buffer_t		*buf;

	if (q->done) {
		return off;
	}

	qe = queue_get_head(q);
	goto entry;
	do {
		if ((sbuf_size(qe->fb) - q->off) > off) {
			q->off += off;
			return 0;
		}
		off -= sbuf_size(qe->fb) - q->off;
		oldqe = qe;
		qe = queue_get_next(q, qe);
		queue_delete(oldqe);
		q->off = 0;
entry:
		if (!qe) {
			buf = sbuf_get(q->p);
			if (buf)
				qe = queue_add_tail(q, buf);
			else
				q->done = 1;
		}
	} while (qe);

	return off;
}

static inline int queue_copy(queue_t *q, SAMPLE *s, int cnt)
{
	queue_entry_t *qe;
	int off;
	filter_buffer_t *fb;

	if (q->done) {
		return cnt;
	}
	qe = queue_get_head(q);
	off = q->off;
	goto entry;
	do {
		if ((sbuf_size(qe->fb) - off) > cnt) {
			memcpy(s, sbuf_buf(qe->fb)+off, cnt*SAMPLE_SIZE);
			return 0;
		}
		memcpy(s, sbuf_buf(qe->fb)+off, (sbuf_size(qe->fb)-off)*SAMPLE_SIZE);
		cnt -= sbuf_size(qe->fb) - off;
		s += sbuf_size(qe->fb) - off;
		qe = queue_get_next(q, qe);
		off = 0;
entry:
		if (!qe) {
			fb = sbuf_get(q->p);
			if (fb) 
				qe = queue_add_tail(q, fb);
			else
				q->done = 1;
		}
	} while (qe);

	return cnt;
}

static inline int queue_copy_pad(queue_t *q, SAMPLE *s, int cnt)
{
	int remaining;

	remaining = queue_copy(q, s, cnt);
	if (remaining)
		memset(s+cnt-remaining, 0, remaining*SAMPLE_SIZE);
	return remaining;
}


static inline int queue_add_shift(queue_t *q, int off) {
	struct queue_entry *qe, *oldqe;
	
	qe = queue_get_head(q);
	goto entry;
	do {
		if ((sbuf_size(qe->fb) - q->off) > off) {
			q->off += off;
			return 0;
		}
		off -= sbuf_size(qe->fb) - q->off;
		sbuf_queue(q->p, qe->fb);
		oldqe = qe;
		qe = queue_get_next(q, qe);
		queue_add_delete(oldqe);
		q->off = 0;
entry:
		if (!qe) {
			qe = queue_add_tail(q, 
			sbuf_make_private(sbuf_alloc(GLAME_WBUFSIZE, q->n)));
			memset((SAMPLE*)sbuf_buf(qe->fb), 0, GLAME_WBUFSIZE*SAMPLE_SIZE);
		}
	} while (qe);

	return off;
}

static inline int queue_add(queue_t *q, SAMPLE *s, int cnt)
{
	queue_entry_t *qe;
	int off, i;
	SAMPLE *x;
	
	qe = queue_get_head(q);
	off = q->off;
	goto entry;
	do {
		if ((sbuf_size(qe->fb) - off) > cnt) {
			x = sbuf_buf(qe->fb)+off;
			for(i=0; i<cnt; i++)
				*x++ += *s++;
			return 0;
		}
		x = sbuf_buf(qe->fb)+off;
		for(i=0; i<sbuf_size(qe->fb)-off; i++)
			*x++ += *s++;
			
		cnt -= sbuf_size(qe->fb) - off;
		qe = queue_get_next(q, qe);
		off = 0;
entry:
		if (!qe) {
			qe = queue_add_tail(q, 
			sbuf_make_private(sbuf_alloc(GLAME_WBUFSIZE, q->n)));
			memset((SAMPLE*)sbuf_buf(qe->fb), 0, GLAME_WBUFSIZE*SAMPLE_SIZE);
		}
	} while (qe);

	return cnt;
}

static inline void queue_add_drain(queue_t *q)
{
	queue_entry_t *qe;
	while ((qe = queue_get_head(q))) {
		sbuf_queue(q->p, qe->fb);
		queue_add_delete(qe);
	}
	q->off = 0;
}

/* Support for feedback and generic fifo queues
 * inside a filter.
 */

typedef struct list_head feedback_fifo_t;
#define INIT_FEEDBACK_FIFO(fifo) INIT_LIST_HEAD(&(fifo))

struct fifo_entry {
	struct list_head list;
	filter_buffer_t *fb;
};

#define has_feedback(fifo) (!list_empty(fifo))

static inline void add_feedback(feedback_fifo_t *f, filter_buffer_t *fb)
{
	struct fifo_entry *e;

	e = (struct fifo_entry *)malloc(sizeof(struct fifo_entry));
	INIT_LIST_HEAD(&e->list);
	e->fb = fb;
	list_add_tail(&e->list, f);
}

static inline filter_buffer_t *get_feedback(feedback_fifo_t *f)
{
	struct fifo_entry *e;
	filter_buffer_t *fb;

	if (list_empty(f))
		return NULL;
	e = list_entry(f->next, struct fifo_entry, list);
	fb = e->fb;
	list_del(&e->list);
	free(e);

	return fb;
}


/* Generic structures and functions for n to one filters
 * without write buffering. Synchron operation.
 */
typedef struct {
	filter_pipe_t *in;
	filter_buffer_t *buf;
	SAMPLE *s;
	int pos;
} nto1_state_t;
static inline int nto1_init(nto1_state_t **I, filter_port_t *port)
{
    	int nr = filterport_nrpipes(port), i;
	filter_pipe_t *pipe;
	if (nr == 0)
	    	return -1;
	if (!(*I = ALLOCN(nr, nto1_state_t)))
		return -1;
	i = 0;
	filterport_foreach_pipe(port, pipe) {
	    	(*I)[i].in = pipe;
		(*I)[i].buf = NULL;
		(*I)[i].s = NULL;
		(*I)[i].pos = 0;
		i++;
	}
	return nr;
}
static inline void nto1_cleanup(nto1_state_t *I)
{
    	free(I);
}
static inline int nto1_head(nto1_state_t *I, int nr)
{
	int cnt, i;

	/* Find the maximum number of samples we can
	 * process in one run. But not more than
	 * GLAME_WBUFSIZE. */
	cnt = GLAME_WBUFSIZE;
	for (i=0; i<nr; i++) {
		if (!I[i].buf)
			continue;
		cnt = MIN(cnt, sbuf_size(I[i].buf) - I[i].pos);
	}
	/* Fix the resulting buffer positions. */
	for (i=0; i<nr; i++)
		if (I[i].buf)
			I[i].pos += cnt;
	return cnt;
}
static inline int nto1_tail(nto1_state_t *I, int nr)
{
	int nr_deactivated = 0, i;

	/* Check for missing buffers and EOFs.
	 * This does work as entry as well because
	 * sbuf_size(NULL) == 0 and I is prezeroed. */
	for (i=0; i<nr; i++) {
		if (!I[i].in)
			continue;
		if (sbuf_size(I[i].buf) == I[i].pos) {
			sbuf_unref(I[i].buf);
			I[i].buf = sbuf_get(I[i].in);
			I[i].s = sbuf_buf(I[i].buf);
			I[i].pos = 0;
			if (!I[i].buf) {
				I[i].in = NULL;
				nr_deactivated++;
			}
		}
	}
	return nr_deactivated;
}


#endif
