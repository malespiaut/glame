#ifndef _GLSIGNAL_H
#define _GLSIGNAL_H

/* Generic signals via callbacks (glsig_handler), issuable from a
 * glsig_emitter. Signal masks are supported, as is a hierarchy of the
 * signal emitters.
 * WARNING! No explicit threading support is included, i.e. do your own
 * locking (you have to protect the emitter against concurrent addition
 * and removal of handlers and emit of signals). Also a handler will be
 * invoked in the thread context of the emitter, not in the one which
 * added the handler. If you want to emit signals from real signal handlers
 * you have to ensure yourself that your handlers are signal safe - also
 * they should be reentrant as parallel invocation from different threads
 * is not protected against.
 *
 * Brief description of the available API follows: 
 *
 * typedef void (glsig_callb_t)(glsig_handler_t *, long, va_list va);
 *   The type of the used callback functions through which signals
 *   are supposed to be handled.
 *
 * INIT_GLSIG_EMITTER(glsig_emitter_t *emitter);
 *   Inits an emitter.
 *
 * glsig_handler_t *glsig_add_handler(glsig_emitter_t *emitter,
 *	                      long sigmask, glsig_callb_t *, void *private);
 *   Adds a signal handler to the emitter using the specified sigmask
 *   and the callback handler. The private data is stored in the handlers
 *   ->private field which you should access using glsig_handler_private().
 *
 * glsig_handler_t *glsig_add_redirector(glsig_emitter_t *emitter,
 *				         glsig_emitter_t *dest);
 *   Adds a redirector to the emitter. All signals raised from the emitter
 *   will be raised again from the dest emitter.
 *
 * void glsig_emit(glsig_emitter_t *emitter, int sig, void *data);
 *   Emits the signal sig from the emitter and provides the data datum
 *   to the callbacks. signals are emitted bottom to top in the hierarchy.
 *
 * int glsig_copy_handlers(glsig_emitter_t *dest, glsig_emitter_t *source);
 *   Copies all handlers from one emitter to another - same "private"
 *   data, of course. Can return -1 on memory shortage.
 *
 * void glsig_delete_handler(glsig_handler_t *handler);
 *   Removes and destroyes the specified handler from its emitter.
 *
 * void glsig_delete_all_handlers(glsig_emitter_t *emitter);
 *   Removes and destroyes all signal handlers from the specified emitter.
 *   Use with care.
 */

#include <stdarg.h>
#include "list.h"


struct glsig_emitter;
typedef struct glsig_emitter glsig_emitter_t;

struct glsig_handler;
typedef struct glsig_handler glsig_handler_t;

typedef void (glsig_callb_t)(glsig_handler_t *, long, va_list);

struct glsig_emitter {
	struct list_head handlers;
};

struct glsig_handler {
	struct list_head list;

	long sigmask;
	glsig_callb_t *handler;

	void *private;
};
#define glsig_handler_private(h) ((h)->private)


#define INIT_GLSIG_EMITTER(emitter) do { \
        INIT_LIST_HEAD(&(emitter)->handlers); \
} while (0)


glsig_handler_t *glsig_add_handler(glsig_emitter_t *emitter,
		  long sigmask, glsig_callb_t *handler, void *private);

glsig_handler_t *glsig_add_redirector(glsig_emitter_t *emitter,
				      glsig_emitter_t *dest);


int glsig_copy_handlers(glsig_emitter_t *dest, glsig_emitter_t *source);

void glsig_delete_handler(glsig_handler_t *h);

void glsig_delete_all_handlers(glsig_emitter_t *e);


static inline void glsig_emit(glsig_emitter_t *e, long sig, ...)
{
	glsig_handler_t *h;
	va_list va;

	va_start(va, sig);
	list_foreach(&e->handlers, glsig_handler_t, list, h) {
		if (!(h->sigmask & sig))
			continue;
		h->handler(h, sig, va);
	}
	va_end(va);
}


/* Some strange macros to ease expansion of the varargs in the
 * handler function. Unfortunately only gcc supports typeof, so...
 *
 * You may want to define wrapper inline functions for your
 * favorite signal handler types like
 *   static inline emit_fixup_pipe(glsig_emitter_t *e, filter_node_t *n,
 *                                 filter_pipe_t *p)
 *   {
 *           glsig_emit(e, GLSIG_PIPE_CHANGED, n, p);
 *   }
 *
 *   #define FIXUP_PIPE_ARGS(va, node, pipe) GLSIGH_GETARGS2(va, node, pipe)
 */

#define GLSIGH_GETARGS1(va, arg1) do { \
        arg1 = va_arg(va, typeof(arg1)); \
} while (0)
#define GLSIGH_GETARGS2(va, arg1, arg2) do { \
	arg1 = va_arg(va, typeof(arg1)); \
	arg2 = va_arg(va, typeof(arg2)); \
} while (0)
#define GLSIGH_GETARGS3(va, arg1, arg2, arg3) do { \
	arg1 = va_arg(va, typeof(arg1)); \
	arg2 = va_arg(va, typeof(arg2)); \
	arg3 = va_arg(va, typeof(arg3)); \
} while (0)
#define GLSIGH_GETARGS4(va, arg1, arg2, arg3, arg4) do { \
	arg1 = va_arg(va, typeof(arg1)); \
	arg2 = va_arg(va, typeof(arg2)); \
	arg3 = va_arg(va, typeof(arg3)); \
	arg4 = va_arg(va, typeof(arg4)); \
} while (0)


#endif
