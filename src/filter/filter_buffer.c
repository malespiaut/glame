/*
 * filter_buffer.c
 * $Id: filter_buffer.c,v 1.23 2000/11/06 09:45:55 richi Exp $
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
#include "filter_pipe.h"
#include "filter_buffer.h"


void fbuf_ref(filter_buffer_t *fb)
{
	if (!fb)
		return;

	if (ATOMIC_VAL(fb->refcnt) == 0)
		DERROR("refing buffer without a reference");

	atomic_inc(&fb->refcnt);
}
void fbuf_unref(filter_buffer_t *fb)
{
	if (!fb)
		return;

	if (ATOMIC_VAL(fb->refcnt) == 0)
		DERROR("unrefing buffer without a reference");

	if (atomic_dec_and_test(&fb->refcnt)) {
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

filter_buffer_t *fbuf_try_make_private(filter_buffer_t *fb)
{
	if (!fb)
		return NULL;

	if (ATOMIC_VAL(fb->refcnt) == 0)
		DERROR("trying to make buffer private without a reference");

	if (ATOMIC_VAL(fb->refcnt) == 1)
		return fb;

	return NULL;
}

filter_buffer_t *fbuf_make_private(filter_buffer_t *fb)
{
	filter_buffer_t *fbcopy;

	if (!fb)
		return NULL;

	if (ATOMIC_VAL(fb->refcnt) == 0)
		DERROR("making buffer private without a reference");

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


#define FBPIPE_WCNT (PIPE_BUF/(sizeof(void *)*2))
#define FBPIPE_WSIZE (FBPIPE_WCNT*sizeof(void *))
/* fbuf_get reads the address of the next pending filter buffer
 * from the input pipe p.
 * As NULL is an EOF marker, it is suitable as error marker, too.
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
		if (errno != EAGAIN)
	        	perror("fbuf_get");
#endif
        } else if (res != FBPIPE_WSIZE)
                PANIC("pipe reads are not atomic!");

	if (res != -1 && buf[0]
	    && ATOMIC_VAL(((filter_buffer_t *)buf[0])->refcnt) == 0)
		DERROR("got buffer without a reference\n");

	return res == -1 ? NULL : (filter_buffer_t *)(buf[0]);
}

/* fbuf_queue writes the address of the supplied filter buffer
 * to the output pipe. In case of errors we become a fbuf_unref
 * operation.
 */
void fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf)
{
        void *buf[FBPIPE_WCNT];
	int res;

	if (!p) {
		fbuf_unref(fbuf);
		return;
	}

	if (fbuf && ATOMIC_VAL(fbuf->refcnt) == 0)
		DERROR("queued buffer without holding a reference\n");

	buf[0] = fbuf;
	while ((res = write(p->source_fd, buf, FBPIPE_WSIZE)) == -1
	       && errno == EINTR)
	        ;
	if (res == -1) {
#ifdef DEBUG
		if (errno != EAGAIN)
	        	perror("fbuf_queue");
#endif
		fbuf_unref(fbuf);
	} else if (res != FBPIPE_WSIZE)
                PANIC("pipe writes are not atomic!");
}



/* Internal API used for cleanup in filter_ops.c
 */

void fbuf_drain(filter_pipe_t *p)
{
	char buf[128];

	fcntl(p->dest_fd, F_SETFL, O_NONBLOCK);
	fcntl(p->source_fd, F_SETFL, O_NONBLOCK);
	while (read(p->dest_fd, buf, 128) != -1)
		;
}

void fbuf_free_buffers(struct list_head *list)
{
	filter_buffer_t *fb;

	while (!list_empty(list)) {
		fb = list_gethead(list, filter_buffer_t, list);
		if (ATOMIC_VAL(fb->refcnt) == 0)
			DERROR("buffer without reference still in buffer list");
		DPRINTF("freeing buffer\n");
		atomic_set(&fb->refcnt, 1);
		fbuf_unref(fb);
	}
}
