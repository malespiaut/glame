/*
 * gldb_string.c
 * $Id: gldb_string.c,v 1.3 2000/05/01 12:22:16 richi Exp $
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
