/*
 * glsignal.c
 * $Id: glsignal.c,v 1.11 2001/04/09 13:34:55 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther
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

#include "glsignal.h"


#define GLSIG_HANDLER_RUNNING (1<<31)


/* Execute a signal handler doing preparation for lazy deletion. */
static inline void _glsig_handler_exec(glsig_handler_t *h,
				       long sig, va_list va)
{
	int was_running = 0;

	/* !h->handler means this handler is really deleted. */
	if (!h->handler || !(h->sigmask & sig))
		return;
	if (h->sigmask & GLSIG_HANDLER_RUNNING)
		was_running = 1;
	h->sigmask |= GLSIG_HANDLER_RUNNING;
	h->handler(h, sig, va);
	if (!was_running)
		h->sigmask &= ~GLSIG_HANDLER_RUNNING;
}

/* Second run through the signal handler list - delete all handlers
 * marked for delayed deletion. */
static inline void _glsig_after_emit(glsig_emitter_t *e)
{
	glsig_handler_t *h;
	struct list_head *dummy;

	list_safe_foreach(&e->handlers, glsig_handler_t, list, dummy, h)
		if (!h->handler && !(h->sigmask & GLSIG_HANDLER_RUNNING))
			glsig_delete_handler(h);
}


static void glsig_redirector(glsig_handler_t *h, long sig, va_list va)
{
	glsig_emitter_t *dest = (glsig_emitter_t *)glsig_handler_private(h);

	list_foreach(&dest->handlers, glsig_handler_t, list, h)
		_glsig_handler_exec(h, sig, va);
	_glsig_after_emit(dest);
}


glsig_handler_t *glsig_add_handler(glsig_emitter_t *emitter,
		       long sigmask, glsig_callb_t *handler, void *priv)
{
	glsig_handler_t *h;

	if (!emitter || !sigmask || !handler)
		return NULL;
	if (!(h = ALLOC(glsig_handler_t)))
		return NULL;
	INIT_LIST_HEAD(&h->list);
	h->sigmask = sigmask;
	h->handler = handler;
	h->priv = priv;
	list_add(&h->list, &emitter->handlers);

	return h;
}

glsig_handler_t *glsig_add_redirector(glsig_emitter_t *emitter, long sigmask,
				      glsig_emitter_t *dest)
{
	return glsig_add_handler(emitter, sigmask, glsig_redirector, dest);
}


int glsig_copy_handlers(glsig_emitter_t *dest, glsig_emitter_t *source)
{
	glsig_handler_t *h;

	list_foreach(&source->handlers, glsig_handler_t, list, h) {
		if (h->handler == glsig_redirector)
			continue;
		if (!glsig_add_handler(dest, h->sigmask & ~GLSIG_HANDLER_RUNNING,
				       h->handler, h->priv))
			return -1;
	}
	return 0;
}

int glsig_copy_redirectors(glsig_emitter_t *dest, glsig_emitter_t *source)
{
	glsig_handler_t *h;

	list_foreach(&source->handlers, glsig_handler_t, list, h) {
		if (h->handler != glsig_redirector)
			continue;
		if (!glsig_add_handler(dest, h->sigmask & ~GLSIG_HANDLER_RUNNING,
				       h->handler, h->priv))
			return -1;
	}
	return 0;
}

void glsig_delete_handler(glsig_handler_t *h)
{
	/* If we are running mark us for delayed deletion. */
	if (h->sigmask & GLSIG_HANDLER_RUNNING) {
		h->handler = NULL;
		return;
	}
	list_del(&h->list);
	free(h);
}

void glsig_delete_all(glsig_emitter_t *e)
{
	glsig_handler_t *h;
	struct list_head *dummy;

	list_safe_foreach(&e->handlers, glsig_handler_t, list, dummy, h)
		glsig_delete_handler(h);
}


void glsig_handler_exec(glsig_handler_t *h, long sig, ...)
{
	va_list va;
	va_start(va, sig);
	_glsig_handler_exec(h, sig, va);
	va_end(va);
}

/* We use signal sign bit in the mask to signal "in execution".
 * This way we implement delayed removal/destruction of signal
 * handlers. Delayed deletion is signalled via NULL handler->handler. */
void glsig_emit(glsig_emitter_t *e, long sig, ...)
{
	glsig_handler_t *h;
	va_list va;

	va_start(va, sig);
	list_foreach(&e->handlers, glsig_handler_t, list, h)
		_glsig_handler_exec(h, sig, va);
	va_end(va);

	_glsig_after_emit(e);
}
