/*
 * filter_buffer.c
 * $Id: filter_buffer.c,v 1.6 2000/02/02 11:46:30 richi Exp $
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



static pthread_mutex_t fb_mutex = PTHREAD_MUTEX_INITIALIZER;


int fbuf_ref(filter_buffer_t *fb)
{
	if (!fb)
		return 0;

	/* until we get an ATOMIC_INC this is necessary */
	pthread_mutex_lock(&fb_mutex);
	fb->refcnt++;
	pthread_mutex_unlock(&fb_mutex);

	return 0;
}
int fbuf_unref(filter_buffer_t *fb)
{
	if (!fb)
		return 0;

	/* until we get an ATOMIC_INC this is necessary */
	pthread_mutex_lock(&fb_mutex);
	fb->refcnt--;
	pthread_mutex_unlock(&fb_mutex);

	if (fb->refcnt == 0) {
	        free(fb->buf);
		free(fb);
	}

	return 0;
}

static filter_buffer_t *fbuf_create(SAMPLE *buf, int size)
{
	filter_buffer_t *fb;

	if (!(fb = ALLOC(filter_buffer_t)))
		return NULL;
	fb->size = size;
	fb->buf = buf;
	fb->refcnt = 1;

	return fb;
}

filter_buffer_t *fbuf_alloc(int size)
{
	SAMPLE *buf;

	if (!(buf = (SAMPLE *)malloc(sizeof(SAMPLE)*size)))
		return NULL;
	return fbuf_create(buf, size);
}

filter_buffer_t *fbuf_lock(filter_buffer_t *fb)
{
	filter_buffer_t *fbcopy;

	/* this is _not_ a race condition! (I believe)
	 * if there are other possible lockers, there must
	 * be references for them. */
	if (fb->refcnt == 1)
	        return fb;

	if (!(fbcopy = fbuf_alloc(fb->size)))
		return NULL;
	memcpy(fbcopy->buf, fb->buf, sizeof(SAMPLE)*fb->size);

	/* release reference to old buffer */
	fbuf_unref(fb);

	return fbcopy;
}

int fbuf_unlock(filter_buffer_t *fb)
{
        /* nop - nothing special was done to fb at lock time */

	return 0;
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
int fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf)
{
        void *buf[64]; /* weeeh - hack for write-throttling */
	int res;

	buf[0] = fbuf;
	res = write(p->source_fd, buf, 64*sizeof(void *));

	return res;
}
