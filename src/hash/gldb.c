/*
 * gldb.c
 * $Id: gldb.c,v 1.8 2001/09/17 11:47:12 nold Exp $
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
#include "gldb.h"


/* Item list wrappers. */
#define glame_list_add_item(i, db)   glame_list_add(&(i)->list, &(db)->items)
#define glame_list_del_item(i)       glame_list_del(&(i)->list)
#define item_in_db(i)          (!glame_list_empty(&(i)->list))


int _gldb_add_item(gldb_t *db, gldb_item_t *item, gldb_item_t *source, const char *label)
{
	if (gldb_query_item(db, label))
		return -1;
	item->db = db;
	if (!(item->label = strdup(label)))
		return -1;
	glame_list_add_item(item, db);
	if (db->ops->add(db, item, source) == -1) {
		gldb_remove_item(item);
		return -1;
	}
	return 0;
}


void gldb_init(gldb_t *db, struct gldb_ops *ops)
{
	GLAME_INIT_LIST_HEAD(&db->items);
	db->ops = ops;
}

void gldb_delete(gldb_t *db)
{
	gldb_item_t *item;
	while ((item = glame_list_gethead(&db->items, gldb_item_t, list)))
		gldb_delete_item(item);
}

int gldb_copy(gldb_t *dest, gldb_t *source)
{
	gldb_item_t *item, *copy, *existing;

	gldb_foreach_item(source, item) {
		if (!(copy = gldb_copy_item(item)))
			return -1;
		if ((existing = gldb_query_item(dest, item->label))) {
			gldb_delete_item(existing);
		}
		if (_gldb_add_item(dest, copy, item, item->label) == -1) {
			gldb_delete_item(copy);
			return -1;
		}
	}
	return 0;
}

void gldb_init_item(gldb_item_t *item)
{
	GLAME_INIT_LIST_HEAD(&item->list);
	item->db = NULL;
	item->label = NULL;
}

gldb_item_t *gldb_copy_item(gldb_item_t *item)
{
	return item->db->ops->copy(item);
}

int gldb_add_item(gldb_t *db, gldb_item_t *item, const char *label)
{
	return _gldb_add_item(db, item, NULL, label);
}

void gldb_remove_item(gldb_item_t *item)
{
	glame_list_del_item(item);
	GLAME_INIT_LIST_HEAD(&item->list);
	free((char *)item->label);
	item->label = NULL;
}

void gldb_delete_item(gldb_item_t *item)
{
	if (item_in_db(item))
		gldb_remove_item(item);
	item->db->ops->del(item);
	free(item);
}

gldb_item_t *gldb_query_item(gldb_t *db, const char *label)
{
	gldb_item_t *item;

	gldb_foreach_item(db, item) {
		if (strcmp(item->label, label) == 0)
			return item;
	}
	return NULL;
}
