#ifndef _GLDB_H
#define _GLDB_H

/*
 * gldb.h
 * $Id: gldb.h,v 1.7 2000/12/12 17:11:25 richi Exp $
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
/*
 * The purpose for this generic small database framework is to
 * be able to have many databases with a very small footprint
 * for their hook and reasonable minimum item size. The item query
 * time is O(N).
 * Note that there is no locking internal to a database - at least
 * no guaranteed one, so you may want to have per database mutexes.
 *
 * Only one "type" of items may be stored in the database - type
 * is destinguished by the database operations, the copy and the
 * delete operation. These operate on all items and such either
 * need to find about the items type themself or assume equal types.
 *
 * Note that embedding a db_t* rather than a struct db_ops* in the
 * item does allow more flexibility such as a single linked list
 * implementation or some other tricky use of the db/item framework.
 *
 * The presented framework is not intended for direct use but rather
 * for use as a basis for own item types with corresponding wrappers
 * to the db API.
 */

#include "list.h"


struct gldb;
typedef struct gldb gldb_t;

struct gldb_item;
typedef struct gldb_item gldb_item_t;


/* Database item operations - same operations for each
 * item of one database. I.e. database scope, not item
 * scope. */
struct gldb_ops {
	/* The del operation should free all local
	 * storage associated with the item - but not
	 * the item itself. */
	void (*del)(gldb_item_t *);

	/* The copy operation should copy all local
	 * storage associated with the source item to
	 * a fresh allocated destination item which
	 * should be returned. */
	gldb_item_t *(*copy)(gldb_item_t *source);

	/* After add operations. May reject the item by
	 * returning -1. The last param is the copied
	 * item if the add was resulted by a item/db copy
	 * or NULL otherwise. Can be used to do special
	 * after add copy operations. */
	int (*add)(gldb_t *, gldb_item_t *, gldb_item_t *);
};

/* Database. Just a hook for items, &items provides
 * the head of a linked list of items and a namespace
 * for the hash. Also the per database item-operations
 * are stored here. */
struct gldb {
	struct list_head items;
	struct gldb_ops *ops;
};

/* Database item. Linked list and hash hooks. label and
 * db provide the unique name/namespace key. Item data
 * is embedded in and after data. The size element allows
 * copying of items. 28 bytes with hash, 16 without. */
struct gldb_item {
	struct list_head list;
	const char *label;
	gldb_t *db;
};


#ifdef __cplusplus
extern "C" {
#endif

/* Init previously allocated/embedded database. */
void gldb_init(gldb_t *db, struct gldb_ops *ops);

/* Delete database and all its entries - does not free
 * db itself. */
void gldb_delete(gldb_t *db);

/* Copies all database entries from source to dest database,
 * entries with same name already existing in dest are deleted
 * first, then recreated. */
int gldb_copy(gldb_t *dest, gldb_t *source);

/* Iterator over all database items. */
#define gldb_foreach_item(db, i) list_foreach(&(db)->items, gldb_item_t, list, i)

/* Get number of database items. */
#define gldb_nritems(db) list_count(&(db)->items)


/* Initializes a previously allocated/embedded item. */
void gldb_init_item(gldb_item_t *item);

/* Deletes item - removes it first if necessary. Frees the
 * item memory after issuing the destructor. */
void gldb_delete_item(gldb_item_t *item);

/* Copies an item. */
gldb_item_t *gldb_copy_item(gldb_item_t *item);


/* Adds previously allocated item to the database using
 * the specified label as key. */
int gldb_add_item(gldb_t *db, gldb_item_t *item, const char *label);

/* Removes the specified item from the database. */
void gldb_remove_item(gldb_item_t *item);

/* Queries an item from the database using label as key. */
gldb_item_t *gldb_query_item(gldb_t *db, const char *label);

#ifdef __cplusplus
}
#endif


#endif
