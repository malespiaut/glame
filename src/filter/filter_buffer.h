/* Not for direct #include - is included by filter.h */

/* The filter buffer handling API for use inside of the filters
 * ->f() function.
 */

/* A filter buffer is the internal representation of a data
 * stream piped through the filter network.
 * Access is only through the fbuf_* functions and macros.
 */
typedef struct {
	struct list_head list;
        glame_atomic_t refcnt;
        int atom_size;   /* size of one buffer entry */
	int size;        /* size of buffer in atoms */
	int buf_pos;     /* position (timestamp) of buffer start in atoms */
	void *buf;
} filter_buffer_t;

#define fbuf_size(fb) ((fb)->size)
#define fbuf_atomsize(fb) ((fb)->atom_size)
#define fbuf_bufpos(fb) ((fb)->buf_pos)
#define fbuf_buf(fb) ((SAMPLE *)((fb)->buf))


/* _fbuf_alloc creates a filter buffer with backing storage for size
 * number of atoms (uninitialized!).
 * The buffer is initially one time referenced.
 * The fbuf_alloc macro is for your convenience and takes a
 * filter_node_t *node as third argument. */
filter_buffer_t *_fbuf_alloc(int size, int atom_size, struct list_head *list);
#define fbuf_alloc(s, as, node) _fbuf_alloc((s), (as), &(node)->net->buffers)

#define fbuf_setpos(fb, pos) do { (fb)->buf_pos = (pos); } while (0)



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
