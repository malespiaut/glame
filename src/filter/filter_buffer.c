/*
 * filter_buffer.c
 * $Id: filter_buffer.c,v 1.8 2000/02/05 15:59:26 richi Exp $
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
	        free(fb->buf);
		ATOMIC_RELEASE(fb->refcnt);
		free(fb);
	}
}

filter_buffer_t *_fbuf_alloc(int size, int atom_size, struct list_head *list)
{
	filter_buffer_t *fb;

	if (!(fb = ALLOC(filter_buffer_t)))
		return NULL;

	fb->buf = NULL;
	if (size > 0
	    && !(fb->buf = malloc(atom_size*size))) {
		free(fb);
		return NULL;
	}
	fb->atom_size = atom_size;
	fb->size = size;
	fb->buf_pos = 0;
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

	if (!(fbcopy = _fbuf_alloc(fb->size, fb->atom_size, &fb->list)))
		return NULL;
	memcpy(fbcopy->buf, fb->buf, sizeof(SAMPLE)*fb->size);
	fbcopy->buf_pos = fb->buf_pos;

	/* release reference to old buffer */
	fbuf_unref(fb);

	return fbcopy;
}



/* ok, this is supposed to get a new buffer address from the
 * pipe. BLOCKING! - i.e. the caller can do a simple
 * for (i=0; i<cnt; i++)
 *        buf[i] = get_buffer(pipe[i]);
 * and is guaranteed to have all buffers updated.
 * a NULL return-value is special (EOF mark). remember, the EOF
 * has to be forwarded to all output-channels!
 * NULL is also a good error-condition marker - EOF lets the
 * filter quit anyway.
 * as pipe-writes are atomic, pipe-reads (of this low quantity)
 * are atomic, too (??).
 */
filter_buffer_t *fbuf_get(filter_pipe_t *p)
{
        void *buf[64];
	int res;

	res = read(p->dest_fd, buf, 64*sizeof(void *));

	return res == -1 ? NULL : (filter_buffer_t *)buf[0];
}

/* send one buffer (address) along the specified pipe.
 * as pipe-writes are atomic, this is rather simple
 */
void fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf)
{
        void *buf[64]; /* weeeh - hack for write-throttling */

	buf[0] = fbuf;
	if (write(p->source_fd, buf, 64*sizeof(void *)) == -1)
		fbuf_unref(fbuf);
}
