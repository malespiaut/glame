/*
 * gldb_worm.c
 * $Id: gldb_worm.c,v 1.6 2004/10/23 13:09:27 richi Exp $
 *
 * Copyright (C) 2000, 2004 Richard Guenther
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

#include "gldb_worm.h"


/* empty destructor - no destruction ever happens by
 * definition. */
static void worm_delete(gldb_item_t *i)
{
}

/* copy may safely copy the data by reference. */
static gldb_item_t *worm_copy(gldb_item_t *source)
{
	glworm_t *d;
	glworm_t *s = (glworm_t *)source;

	if (!(d = glworm_alloc()))
		return NULL;
	d->u.ptr = s->u.ptr;
	return &d->item;
}

static int worm_add(gldb_t *db, gldb_item_t *i, gldb_item_t *source)
{
	return 0;
}

static struct gldb_ops worm_ops = { worm_delete, worm_copy, worm_add };

void glwdb_init(gldb_t *db)
{
	gldb_init(db, &worm_ops);
}

glworm_t *glworm_alloc()
{
	glworm_t *w;

	if (!(w = (glworm_t *)malloc(sizeof(glworm_t))))
		return NULL;
	gldb_init_item(&w->item);

	return w;
}

int glwdb_add_item(gldb_t *db, glworm_t *w, const char *label)
{
	return gldb_add_item(db, &w->item, label);
}

const glworm_t *glwdb_query_item(gldb_t *db, const char *label)
{
	gldb_item_t *i;

	if (!(i = gldb_query_item(db, label)))
		return NULL;
	return (const glworm_t *)i;
}
