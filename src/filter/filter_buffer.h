#ifndef _FILTER_BUFFER_H
#define _FILTER_BUFFER_H

/*
 * filter_buffer.h
 * $Id: filter_buffer.h,v 1.7 2001/01/03 09:28:38 richi Exp $
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

#include "list.h"
#include "util.h"
#include "atomic.h"


/* Filter buffer structure. Only to be accessed using the
 * below defined access macros. */
struct filter_buffer {
	struct list_head list;
        glame_atomic_t refcnt;
	int size;              /* size of buffer in bytes */
	char buf[1];
};

/* Public access macros to the filter buffer fields.
 * int fbuf_size(filter_buffer_t *);
 * char *fbuf_buf(filter_buffer_t *); */
#ifndef DEBUG
#define fbuf_size(fb) ((fb)==NULL ? 0 : (fb)->size)
#define fbuf_buf(fb) ((fb)==NULL ? NULL : &(fb)->buf[0])
#else
static inline int fbuf_size(filter_buffer_t *fb)
{
	if (!fb)
		return 0;
	if (ATOMIC_VAL(fb->refcnt) == 0)
		DERROR("no buffer reference for fbuf_size");
	return fb->size;
}
static inline char *fbuf_buf(filter_buffer_t *fb)
{
	if (!fb)
		return NULL;
	if (ATOMIC_VAL(fb->refcnt) == 0)
		DERROR("no buffer reference for fbuf_buf");
	return &fb->buf[0];
}
#endif

/* fbuf_alloc creates a filter buffer with backing storage for size
 * bytes. The buffer is initially one time referenced.
 * If supplied the buffer is linked into the list (and removed from it
 * at free time).
 * Until the first time you queue the buffer the buffer is writable
 * i.e. private to you - see fbuf_make_private for further advice. */
filter_buffer_t *fbuf_alloc(int size, struct list_head *list);

/* fbuf_realloc can be an optimization if you need to resize an
 * existing buffer. For shrinking an already private buffer it is
 * basically a nop, for shrinking a shared buffer it boils down to
 * fbuf_alloc && memcpy, for enlarging a buffer its just less to
 * type - you really need to rethink your algorithm. 
 * So - its useful for rare special cases to "fix" wrong assumptions
 * in a code-size efficient manner. */
filter_buffer_t *fbuf_realloc(filter_buffer_t *fb, int size);

/* Get one extra reference (read-only!) of the buffer.
 * If the number of references drops to zero at any point,
 * the buffer may be freed without notice! */
void fbuf_ref(filter_buffer_t *fb);

/* Release one reference of the buffer. */
void fbuf_unref(filter_buffer_t *fb);

/* Tries to make the buffer private so you can read _and_ write.
 * Does not do it if it would require copying the buffer. Returns
 * NULL on failure. See fbuf_make_private for additional advice. */
filter_buffer_t *fbuf_try_make_private(filter_buffer_t *fb);

/* Make the buffer private so you can read _and_ write.
 * This tries to get exclusive access to the buffer either by
 * copying it or by just doing nothing.
 * Note that if you queue the returned buffer the private
 * status is lost and you may no longer write to it even
 * if you own another reference to it. */
filter_buffer_t *fbuf_make_private(filter_buffer_t *fb);

/* Get (blocking) the next buffer from the input stream. */
filter_buffer_t *fbuf_get(filter_pipe_t *p);

/* Queue (blocking!) the buffer to the output stream. */
void fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf);


#endif
