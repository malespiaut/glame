#include <string.h>
#include "gldb_string.h"


static sitem_t *sitem_alloc(const char *str)
{
	sitem_t *s;

	if (!(s = (sitem_t *)malloc(sizeof(sitem_t))))
		return NULL;
	gldb_init_item(&s->entry);
	if (!(s->str = strdup(str))) {
		free(s);
		return NULL;
	}
	return s;
}

void sop_delete(gldb_item_t *item)
{
	sitem_t *s = (sitem_t *)item;

	free(s->str);
}

gldb_item_t *sop_copy(gldb_item_t *dest, gldb_item_t *source)
{
	sitem_t *s = (sitem_t *)source;
	sitem_t *d = (sitem_t *)dest;

	if (!d) {
		d = sitem_alloc(s->str);
	} else {
		free(d->str);
		d->str = strdup(s->str);
	}

	return &d->entry;
}

static int sop_add(gldb_t *db, gldb_item_t *i)
{
	return 0;
}

struct gldb_ops sops = { sop_delete, sop_copy, sop_add };


void glsdb_init(gldb_t *db)
{
	gldb_init(db, &sops);
}


void glsdb_set(gldb_t *db, const char *str, const char *label)
{
	sitem_t *s;

	if (!(s = (sitem_t *)gldb_query_item(db, label))) {
		s = sitem_alloc(str);
		gldb_add_item(db, &s->entry, label);
	} else {
		free(s->str);
		s->str = strdup(str);
	}
}

void glsdb_remove(gldb_t *db, const char *label)
{
	gldb_item_t *s;

	if ((s = gldb_query_item(db, label)))
		gldb_delete_item(s);
}

char *glsdb_query(gldb_t *db, const char *label)
{
	sitem_t *s;

	if (!(s = (sitem_t *)gldb_query_item(db, label)))
		return NULL;
	return s->str;
}
