/*
 * glsignal.c
 * $Id: glsignal.c,v 1.6 2000/05/09 11:32:31 richi Exp $
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


static void glsig_redirector(glsig_handler_t *h, long sig, va_list va)
{
	glsig_emitter_t *dest = (glsig_emitter_t *)glsig_handler_private(h);

	list_foreach(&dest->handlers, glsig_handler_t, list, h) {
		if (!(h->sigmask & sig))
			continue;
		h->handler(h, sig, va);
	}
}


glsig_handler_t *glsig_add_handler(glsig_emitter_t *emitter,
		       long sigmask, glsig_callb_t *handler, void *private)
{
	glsig_handler_t *h;

	if (!emitter || !sigmask || !handler)
		return NULL;
	if (!(h = (glsig_handler_t *)malloc(sizeof(glsig_handler_t))))
		return NULL;
	INIT_LIST_HEAD(&h->list);
	h->sigmask = sigmask;
	h->handler = handler;
	h->private = private;
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
		if (!glsig_add_handler(dest, h->sigmask, h->handler,
				       h->private))
			return -1;
	}
	return 0;
}

void glsig_delete_handler(glsig_handler_t *h)
{
	list_del(&h->list);
	free(h);
}

void glsig_delete_all_handlers(glsig_emitter_t *e)
{
	while (!list_empty(&e->handlers))
		glsig_delete_handler(list_entry(e->handlers.next,
						glsig_handler_t, list));
}

