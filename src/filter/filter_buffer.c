/*
 * filter_buffer.c
 * $Id: filter_buffer.c,v 1.17 2000/03/23 16:31:04 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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

#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <pthread.h>
#include <limits.h>
#include "filter.h"
#include "util.h"
#include "atomic.h"
#include "list.h"


void _buffer_free(filter_buffer_t *fb)
{
	atomic_set(&fb->refcnt, 1);
	fbuf_unref(fb);
}

void fbuf_ref(filter_buffer_t *fb)
{
	if (!fb)
		return;

	atomic_inc(&fb->refcnt);
}
void fbuf_unref(filter_buffer_t *fb)
{
	if (!fb)
		return;

	atomic_dec(&fb->refcnt);

	if (ATOMIC_VAL(fb->refcnt) == 0) {
		list_del(&fb->list);
		ATOMIC_RELEASE(fb->refcnt);
		free(fb);
	}
}

filter_buffer_t *fbuf_alloc(int size, struct list_head *list)
{
	filter_buffer_t *fb;

	if (!(fb = (filter_buffer_t *)malloc(sizeof(filter_buffer_t) + size)))
		return NULL;

	fb->size = size;
	ATOMIC_INIT(fb->refcnt, 1);
	INIT_LIST_HEAD(&fb->list);
	if (list)
		list_add(&fb->list, list);

	return fb;
}

filter_buffer_t *fbuf_make_private(filter_buffer_t *fb)
{
	filter_buffer_t *fbcopy;

	if (!fb)
		return NULL;

	/* this is _not_ a race condition! (I believe)
	 * if there are other possible lockers, there must
	 * be references for them. */
	if (ATOMIC_VAL(fb->refcnt) == 1)
	        return fb;

	if (!(fbcopy = fbuf_alloc(fb->size, &fb->list)))
		return NULL;
	memcpy(fbuf_buf(fbcopy), fbuf_buf(fb), fb->size);

	/* release reference to old buffer */
	fbuf_unref(fb);

	return fbcopy;
}


/* FIXME! totally br0ken! */
#define FBPIPE_WCNT (PIPE_BUF/(sizeof(void *)*1))
#define FBPIPE_WSIZE (FBPIPE_WCNT*sizeof(void *))
/* fbuf_get reads the address of the next pending filter buffer
 * from the input pipe p.
 * As NULL is an EOF marker, it is suitable as error marker, too.
 * FIXME! Wrt. to write throttling there need to be changes at the
 *        read end, too.
 */
filter_buffer_t *fbuf_get(filter_pipe_t *p)
{
        void *buf[FBPIPE_WCNT];
	int res;

	if (!p)
		return NULL;

	/* now I want to know it... */
	while ((res = read(p->dest_fd, buf, FBPIPE_WSIZE)) == -1
	       && errno == EINTR)
	        ;
	if (res == -1) {
#ifdef DEBUG
	        perror("fbuf_get");
#endif
        } else if (res != FBPIPE_WSIZE)
                PANIC("pipe reads are not atomic!");

	return res == -1 ? NULL : (filter_buffer_t *)(buf[0]);
}

/* fbuf_queue writes the address of the supplied filter buffer
 * to the output pipe. In case of errors we become a fbuf_unref
 * operation.
 * FIXME? Pipe writes of certain quantities are atomic?
 * FIXME! Write throttling in the current form is not "nice"!
 */
void fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf)
{
        void *buf[FBPIPE_WCNT];
	int res;

	if (!p) {
		fbuf_unref(fbuf);
		return;
	}

	if (fbuf
	    && p->type == FILTER_PIPETYPE_SAMPLE
	    && sbuf_size(fbuf) < GLAME_MIN_BUFSIZE)
		DPRINTF("queued small buffer (%i samples)\n", sbuf_size(fbuf));

	buf[0] = fbuf;
	while ((res = write(p->source_fd, buf, FBPIPE_WSIZE)) == -1
	       && errno == EINTR)
	        ;
	if (res == -1) {
#ifdef DEBUG
	        perror("fbuf_queue");
#endif
		fbuf_unref(fbuf);
	} else if (res != FBPIPE_WSIZE)
                PANIC("pipe writes are not atomic!");
}

void fbuf_drain(filter_pipe_t *p)
{
	char buf[128];

	fcntl(p->dest_fd, F_SETFL, O_NONBLOCK);
	fcntl(p->source_fd, F_SETFL, O_NONBLOCK);
	while (read(p->dest_fd, buf, 128) != -1)
		;
}
