/*
 * gldb.c
 * $Id: gldb.c,v 1.3 2000/05/19 09:27:34 richi Exp $
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

/* Item hash & list wrappers.
 */
#ifdef USE_HASH
#define hash_add_item(i, l, d) _hash_add(&(i)->hash, \
                             _hash((l), (d)))
#define hash_remove_item(i)  _hash_remove(&(i)->hash)
#define hash_find_item(l, d) __hash_entry(_hash_find((l), (d), _hash((l), (d)), \
			     __hash_pos(gldb_item_t, hash, label, db)), gldb_item_t, hash)
#else
#define hash_add_item(i, l, d)
#define hash_remove_item(i)
#endif

#define list_add_item(i, db)   list_add(&(i)->list, &(db)->items)
#define list_del_item(i)       list_del(&(i)->list)
#define item_in_db(i)          (!list_empty(&(i)->list))



void gldb_init(gldb_t *db, struct gldb_ops *ops)
{
	INIT_LIST_HEAD(&db->items);
	db->ops = ops;
}

void gldb_delete(gldb_t *db)
{
	gldb_item_t *item;
	while ((item = list_gethead(&db->items, gldb_item_t, list)))
		gldb_delete_item(item);
}

int gldb_copy(gldb_t *dest, gldb_t *source)
{
	gldb_item_t *item, *copy;

	gldb_foreach_item(source, item) {
		if ((copy = gldb_query_item(dest, item->label)))
			gldb_copy_item_data(copy, item);
		else {
			if (!(copy = gldb_copy_item(item)))
				return -1;
			if (gldb_add_item(dest, copy, item->label) == -1) {
				gldb_delete_item(copy);
				return -1;
			}
		}
	}
	return 0;
}

void gldb_init_item(gldb_item_t *item)
{
	INIT_LIST_HEAD(&item->list);
#ifdef USE_HASH
	INIT_HASH_HEAD(&item->hash);
#endif
	item->db = NULL;
	item->label = NULL;
}

void gldb_copy_item_data(gldb_item_t *dest, gldb_item_t *source)
{
	source->db->ops->copy(dest, source);
}

gldb_item_t *gldb_copy_item(gldb_item_t *item)
{
	return item->db->ops->copy(NULL, item);
}

int gldb_add_item(gldb_t *db, gldb_item_t *item, const char *label)
{
	if (gldb_query_item(db, label))
		return -1;
	item->db = db;
	if (!(item->label = strdup(label)))
		return -1;
	hash_add_item(item, label, db);
	list_add_item(item, db);
	if (db->ops->add(db, item) == -1) {
		gldb_remove_item(item);
		free((char *)item->label);
		item->label = NULL;
		return -1;
	}
	return 0;
}

void gldb_remove_item(gldb_item_t *item)
{
	hash_remove_item(item);
	list_del_item(item);
	INIT_LIST_HEAD(&item->list);
}

void gldb_delete_item(gldb_item_t *item)
{
	if (item_in_db(item))
		gldb_remove_item(item);
	item->db->ops->delete(item);
	free((char *)item->label);
	item->label = NULL;
}

gldb_item_t *gldb_query_item(gldb_t *db, const char *label)
{
#ifdef USE_HASH
	return hash_find_item(label, db);
#else
	gldb_item_t *item;

	gldb_foreach_item(db, item) {
		if (strcmp(item->label, label) == 0)
			return item;
	}
	return NULL;
#endif
}
