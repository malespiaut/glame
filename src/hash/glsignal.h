#ifndef _GLSIGNAL_H
#define _GLSIGNAL_H

/*
 * glsignal.h
 * $Id: glsignal.h,v 1.15 2004/10/23 13:09:27 richi Exp $
 *
 * Copyright (C) 2000, 2001 Richard Guenther
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
 *	                      long sigmask, glsig_callb_t *, void *priv);
 *   Adds a signal handler to the emitter using the specified sigmask
 *   and the callback handler. The priv data is stored in the handlers
 *   ->priv field which you should access using glsig_handler_private().
 *
 * glsig_handler_t *glsig_add_redirector(glsig_emitter_t *emitter, long sigmask,
 *				         glsig_emitter_t *dest);
 *   Adds a redirector to the emitter. All signals raised from the emitter
 *   matching the specified signal mask will be raised again from the dest emitter.
 *
 * void glsig_dont_copy_handler(glsig_handler_t *h);
 *   Marks the handler as not to be copied.
 *
 * void glsig_emit(glsig_emitter_t *emitter, int sig, ...);
 *   Emits the signal sig from the emitter and provides the varargs
 *   to the callbacks. Signals are emitted bottom to top in the hierarchy.
 *   It is safe to remove your signal handler during execution.
 *
 * int glsig_copy_handlers(glsig_emitter_t *dest, glsig_emitter_t *source);
 *   Copies all handlers from one emitter to another - same "private"
 *   data, of course. Can return -1 on memory shortage. Redirectors are
 *   not copied.
 *
 * int glsig_copy_redirectors(glsig_emitter_t *dest, glsig_emitter_t *source);
 *   Copies all redirectors from one emitter to another - same "private"
 *   data, of course. Can return -1 on memory shortage. Normal handlers
 *   are not copied.
 *
 * void glsig_delete_handler(glsig_handler_t *handler);
 *   Removes and destroyes the specified handler from its emitter.
 *
 * void glsig_delete_all(glsig_emitter_t *emitter);
 *   Removes and destroyes all signal handlers from the specified emitter.
 *   Use with care.
 *
 * void glsig_handler_exec(glsig_handler_t *h, long sig, ...);
 *   Executes the specified signal handler. Usually you dont want to use
 *   this, instead use glsig_emit().
 */

#include <stdarg.h>
#include "list.h"


struct glsig_emitter;
typedef struct glsig_emitter glsig_emitter_t;

struct glsig_handler;
typedef struct glsig_handler glsig_handler_t;

typedef void (glsig_callb_t)(glsig_handler_t *, long, va_list);

struct glsig_emitter {
	struct glame_list_head handlers;
};

struct glsig_handler {
	struct glame_list_head list;

	int flags;
	long sigmask;
	glsig_callb_t *handler;

	void *priv;
};
#define glsig_handler_private(h) ((h)->priv)


#define INIT_GLSIG_EMITTER(emitter) do { \
        GLAME_INIT_LIST_HEAD(&(emitter)->handlers); \
} while (0)


#ifdef __cplusplus
extern "C" {
#endif

glsig_handler_t *glsig_add_handler(glsig_emitter_t *emitter,
		  long sigmask, glsig_callb_t *handler, void *priv);

glsig_handler_t *glsig_add_redirector(glsig_emitter_t *emitter, long sigmask,
				      glsig_emitter_t *dest);

void glsig_dont_copy_handler(glsig_handler_t *h);

int glsig_copy_handlers(glsig_emitter_t *dest, glsig_emitter_t *source);

int glsig_copy_redirectors(glsig_emitter_t *dest, glsig_emitter_t *source);

void glsig_delete_handler(glsig_handler_t *h);

void glsig_delete_all(glsig_emitter_t *e);

void glsig_handler_exec(glsig_handler_t *h, long sig, ...);

void glsig_emit(glsig_emitter_t *e, long sig, ...);

#ifdef __cplusplus
}
#endif


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

#ifndef HAVE_GCC
/* HACK! works only for passing pointers as signal data */
#define typeof(foo) void *
#endif

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
