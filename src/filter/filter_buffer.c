/*
 * filter_buffer.c
 * $Id: filter_buffer.c,v 1.31 2001/05/23 12:33:58 richi Exp $
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
#include <sys/time.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <pthread.h>
#include <limits.h>
#include "filter.h"
#include "filter_ops.h"
#include "filter_pipe.h"
#include "filter_port.h"
#include "filter_buffer.h"

/* FIXME! Either we need all callers of fbuf_unref, fbuf_alloc and
 * fbuf_realloc lock against concurrent access to the list passed to
 * fbuf_alloc or we need to lock ourselves (probably using a global
 * mutex). Done.
 */
static pthread_mutex_t listmx = PTHREAD_MUTEX_INITIALIZER;

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
	    	if (!list_empty(&fb->list)) {
	    		pthread_mutex_lock(&listmx);
			list_del(&fb->list);
			pthread_mutex_unlock(&listmx);
		}
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
	if (list) {
	    	pthread_mutex_lock(&listmx);
		list_add(&fb->list, list);
	    	pthread_mutex_unlock(&listmx);
	}

	return fb;
}

filter_buffer_t *fbuf_realloc(filter_buffer_t *fb, int size)
{
    	filter_buffer_t *newfb;
	struct list_head *list = NULL;

	if (!fb)
	    	return NULL;

	if ((ATOMIC_VAL(fb->refcnt) == 1) && (size <= fb->size)) {
	    	fb->size = size;
		return fb;
	}

	if (ATOMIC_VAL(fb->refcnt) == 0)
	    	DERROR("trying to realloc a buffer without a reference");

	if (ATOMIC_VAL(fb->refcnt) == 1) {
		if (!list_empty(&fb->list)) {
	    		pthread_mutex_lock(&listmx);
	   		list = fb->list.prev;
	    		list_del(&fb->list);
		}
	    	ATOMIC_RELEASE(fb->refcnt);
	    	if (!(newfb = realloc(fb, sizeof(filter_buffer_t) + size))) {
		    	ATOMIC_INIT(fb->refcnt, 1);
			if (list) {
				list_add(&fb->list, list);
				pthread_mutex_unlock(&listmx);
			}
		    	return NULL;
		}
		ATOMIC_INIT(newfb->refcnt, 1);
		if (list) {
			list_add(&newfb->list, list);
			pthread_mutex_unlock(&listmx);
		}
	} else {
	    	if (!(newfb = fbuf_alloc(size, &fb->list)))
		    	return NULL;
		memcpy(fbuf_buf(newfb), fbuf_buf(fb), fbuf_size(fb));
		fbuf_unref(fb);
	}
	newfb->size = size;

	return newfb;
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
	struct timeval timeout;
	fd_set fds;
	int res;

	if (!p)
		return NULL;

	/* Wait for data with timeout. */
	do {
		FD_ZERO(&fds);
		FD_SET(p->dest_fd, &fds);
		timeout.tv_sec = 5;
		timeout.tv_usec = 0;
		res = select(p->dest_fd+1, &fds, NULL, NULL, &timeout);
	} while ((res == -1 && errno == EINTR)
		 || (res == 0 && filter_is_ready(filterport_filter(filterpipe_source(p)))));

	/* Timeout? -> Deadlock. Break it returning NULL. */
	if (res == 0) {
		filter_set_error(filterport_filter(filterpipe_dest(p)), "Deadlock");
		return NULL;
	}

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

/* Externally locked against concurrent access of list. */
void fbuf_free_buffers(struct list_head *list)
{
	filter_buffer_t *fb;
	int nr_freed = 0;

	while (!list_empty(list)) {
		fb = list_gethead(list, filter_buffer_t, list);
		if (ATOMIC_VAL(fb->refcnt) == 0)
			DERROR("buffer without reference still in buffer list");
		atomic_set(&fb->refcnt, 1);
		fbuf_unref(fb);
		nr_freed++;
	}
	if (nr_freed > 0)
		DPRINTF("freed %i buffers\n", nr_freed);
}
