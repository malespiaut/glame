/*
 * filter_buffer.c
 * $Id: filter_buffer.c,v 1.9 2000/02/07 10:32:05 richi Exp $
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
#include <stdlib.h>
#include <pthread.h>
#include "filter.h"
#include "util.h"
#include "atomic.h"
#include "list.h"


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



/* fbuf_get reads the address of the next pending filter buffer
 * from the input pipe p.
 * As NULL is an EOF marker, it is suitable as error marker, too.
 * FIXME? Pipe writes of certain quantity are atomic, reads, too?
 * FIXME! Wrt. to write throttling there need to be changes at the
 *        read end, too.
 */
filter_buffer_t *fbuf_get(filter_pipe_t *p)
{
        void *buf[64];
	int res;

	res = read(p->dest_fd, buf, 64*sizeof(void *));

	return res == -1 ? NULL : (filter_buffer_t *)buf[0];
}

/* fbuf_queue writes the address of the supplied filter buffer
 * to the output pipe. In case of errors we become a fbuf_unref
 * operation.
 * FIXME? Pipe writes of certain quantities are atomic?
 * FIXME! Write throttling in the current form is not "nice"!
 */
void fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf)
{
        void *buf[64]; /* weeeh - hack for write-throttling */

	buf[0] = fbuf;
	if (write(p->source_fd, buf, 64*sizeof(void *)) == -1)
		fbuf_unref(fbuf);
}
