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
			 int sigmask, glsig_callb_t *handler, void *private)
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

glsig_handler_t *glsig_add_redirector(glsig_emitter_t *emitter,
				      glsig_emitter_t *dest)
{
	return glsig_add_handler(emitter, ~0, glsig_redirector, dest);
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

