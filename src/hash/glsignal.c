/*
 * glsignal.c
 * $Id: glsignal.c,v 1.17 2004/03/26 18:38:03 richi Exp $
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


#define GLSIG_HANDLER_RUNNING (1<<0)
#define GLSIG_HANDLER_NOCOPY (1<<1)


/* Execute a signal handler doing preparation for lazy deletion. */
static inline void _glsig_handler_exec(glsig_handler_t *h,
				       long sig, va_list va)
{
	int was_running = 0;

	/* !h->handler means this handler is really deleted. */
	if (!h->handler || !(h->sigmask & sig))
		return;
	if (h->flags & GLSIG_HANDLER_RUNNING)
		was_running = 1;
	h->flags |= GLSIG_HANDLER_RUNNING;
	h->handler(h, sig, va);
	if (!was_running)
		h->flags &= ~GLSIG_HANDLER_RUNNING;
}

/* Second run through the signal handler list - delete all handlers
 * marked for delayed deletion. */
static inline void _glsig_after_emit(glsig_emitter_t *e)
{
	glsig_handler_t *h;
	struct glame_list_head *dummy;

	glame_list_safe_foreach(&e->handlers, glsig_handler_t, list, dummy, h)
		if (!h->handler && !(h->flags & GLSIG_HANDLER_RUNNING))
			glsig_delete_handler(h);
}


static void glsig_redirector(glsig_handler_t *h, long sig, va_list va)
{
	glsig_emitter_t *dest = (glsig_emitter_t *)glsig_handler_private(h);

	glame_list_foreach(&dest->handlers, glsig_handler_t, list, h) {
		va_list vac;
		va_copy(vac, va);
		_glsig_handler_exec(h, sig, vac);
		va_end(vac);
	}
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
	GLAME_INIT_LIST_HEAD(&h->list);
	h->flags = 0;
	h->sigmask = sigmask;
	h->handler = handler;
	h->priv = priv;
	glame_list_add(&h->list, &emitter->handlers);

	return h;
}

glsig_handler_t *glsig_add_redirector(glsig_emitter_t *emitter, long sigmask,
				      glsig_emitter_t *dest)
{
	return glsig_add_handler(emitter, sigmask, glsig_redirector, dest);
}

void glsig_dont_copy_handler(glsig_handler_t *h)
{
	if (!h)
		return;
	h->flags |= GLSIG_HANDLER_NOCOPY;
}


int glsig_copy_handlers(glsig_emitter_t *dest, glsig_emitter_t *source)
{
	glsig_handler_t *h;

	glame_list_foreach(&source->handlers, glsig_handler_t, list, h) {
		if (h->flags & GLSIG_HANDLER_NOCOPY)
			continue;
		if (h->handler == glsig_redirector)
			continue;
		if (!glsig_add_handler(dest, h->sigmask,
				       h->handler, h->priv))
			return -1;
	}
	return 0;
}

int glsig_copy_redirectors(glsig_emitter_t *dest, glsig_emitter_t *source)
{
	glsig_handler_t *h;

	glame_list_foreach(&source->handlers, glsig_handler_t, list, h) {
		if (h->flags & GLSIG_HANDLER_NOCOPY)
			continue;
		if (h->handler != glsig_redirector)
			continue;
		if (!glsig_add_handler(dest, h->sigmask,
				       h->handler, h->priv))
			return -1;
	}
	return 0;
}

void glsig_delete_handler(glsig_handler_t *h)
{
	/* If we are running mark us for delayed deletion. */
	if (h->flags & GLSIG_HANDLER_RUNNING) {
		h->handler = NULL;
		return;
	}
	glame_list_del(&h->list);
	free(h);
}

void glsig_delete_all(glsig_emitter_t *e)
{
	glsig_handler_t *h;
	struct glame_list_head *dummy;

	glame_list_safe_foreach(&e->handlers, glsig_handler_t, list, dummy, h)
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

	glame_list_foreach(&e->handlers, glsig_handler_t, list, h) {
		va_start(va, sig);
		_glsig_handler_exec(h, sig, va);
		va_end(va);
	}

	_glsig_after_emit(e);
}
