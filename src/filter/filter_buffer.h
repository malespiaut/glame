#ifndef _FILTER_BUFFER_H
#define _FILTER_BUFFER_H

/* Not for direct #include - is included by filter.h */

/* The filter buffer handling API for use inside of the filters
 * ->f() function.
 */

/* A filter buffer is the internal representation of a data
 * stream piped through the filter network.
 * Access is only through the fbuf_* functions and macros.
 */
struct filter_buffer;
typedef struct filter_buffer filter_buffer_t;
struct filter_buffer {
	struct list_head list;
        glame_atomic_t refcnt;
	int size;              /* size of buffer in bytes */
	char buf[0];
};

#define fbuf_size(fb) ((fb)==NULL ? 0 : (fb)->size)
#define fbuf_buf(fb) (&(fb)->buf[0])

/* fbuf_alloc creates a filter buffer with backing storage for size
 * bytes. The buffer is initially one time referenced.
 * If supplied the buffer is linked into the list (and removed from it
 * at free time) */
filter_buffer_t *fbuf_alloc(int size, struct list_head *list);



/* Get one extra reference (read-only!) of the buffer.
 * If the number of references drops to zero at any point,
 * the buffer may be freed without notice! */
void fbuf_ref(filter_buffer_t *fb);

/* Release one reference of the buffer. */
void fbuf_unref(filter_buffer_t *fb);

/* Make the buffer private so you can read _and_ write.
 * This tries to get exclusive access to the buffer either by
 * copying it or by just doing nothing. */
filter_buffer_t *fbuf_make_private(filter_buffer_t *fb);



/* Get (blocking) the next buffer from the input stream. */
filter_buffer_t *fbuf_get(filter_pipe_t *p);

/* Queue (blocking!) the buffer to the output stream. */
void fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf);


#endif
