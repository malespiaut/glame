#ifndef _GLDB_H
#define _GLDB_H

/*
 * gldb.h
 * $Id: gldb.h,v 1.1 2000/04/25 08:58:00 richi Exp $
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
 * for their hook and reasonable minimum item size. Without use
 * of a hash item query is O(N) with hash it is O(1). For ~4 items
 * the O(N) query is better than the optimal O(1) one, with
 * a full hashtable there may be a higher breakeven. For more than
 * 16 items not using a hash is not recommended. Note that the
 * locking costs for the global hashtable have not been considered!
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

#undef USE_HASH

#include "list.h"
#ifdef USE_HASH
#include "glame_hash.h"
#endif


struct gldb_item;
typedef struct gldb_item gldb_item_t;


/* Database item operations - same operations for each
 * item of one database. I.e. database scope, not item
 * scope. */
struct gldb_ops {
	/* The delete operation should free all local
	 * storage associated with the item - but not
	 * the item itself. */
	void (*delete)(gldb_item_t *);

	/* The copy operation should copy all local
	 * storage associated with the source item to
	 * the destination item and create that, if NULL
	 * is supplied. The destination item should be
	 * returned. */
	gldb_item_t *(*copy)(gldb_item_t *dest, gldb_item_t *source);
};

/* Database. Just a hook for items, &items provides
 * the head of a linked list of items and a namespace
 * for the hash. Also the per database item-operations
 * are stored here. */
typedef struct {
	struct list_head items;
	struct gldb_ops *ops;
} gldb_t;

/* Database item. Linked list and hash hooks. label and
 * db provide the unique name/namespace key. Item data
 * is embedded in and after data. The size element allows
 * copying of items. 28 bytes with hash, 16 without. */
struct gldb_item {
	struct list_head list;
#ifdef USE_HASH
	struct hash_head hash;
#endif
	const char *label;
	gldb_t *db;
};



/* Init previously allocated/embedded database. */
void gldb_init(gldb_t *db, struct gldb_ops *ops);

/* Delete database and all its entries - does not free
 * db itself. */
void gldb_delete(gldb_t *db);

/* Copies all database entries from source to dest database. */
int gldb_copy(gldb_t *dest, gldb_t *source);

/* Iterator over all database items. */
#define gldb_foreach_item(db, i) list_foreach(&(db)->items, gldb_item_t, list, i)


/* Initializes a previously allocated/embedded item. */
void gldb_init_item(gldb_item_t *item);

/* Deletes item - removes it first if necessary. Does not
 * free item itself. */
void gldb_delete_item(gldb_item_t *item);

/* Copies an item. */
gldb_item_t *gldb_copy_item(gldb_item_t *item);

/* Copies item data from source to dest. */
void gldb_copy_item_data(gldb_item_t *dest, gldb_item_t *source);


/* Adds previously allocated item to the database using
 * the specified label as key. */
int gldb_add_item(gldb_t *db, gldb_item_t *item, const char *label);

/* Removes the specified item from the database. */
void gldb_remove_item(gldb_item_t *item);

/* Queries an item from the database using label as key. */
gldb_item_t *gldb_query_item(gldb_t *db, const char *label);


#endif
