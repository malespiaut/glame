#ifndef _GLAMEHASH_H
#define _GLAMEHASH_H

/*
 * glame_hash.h
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


/* This is the framework for a global name[namespace] hash.
 * To use this framework with your own type, define the following
 * set of macros (as f.i. done in channel/channel.c for the
 * channel group hash and the channel[channel group] hash):
 * - hash_find_XXX(name [, namespace])
 * - hash_add_XXX(var)
 * - hash_remove_XXX(var)
 * - hash_first_XXX([namespace])
 * - hash_next_XXX(var)
 * - hash_init_XXX(var)
 * - is_hashed_XXX(var)
 * You have to embed the following required entries into your type:
 * - a struct hash_head
 * - a char * name specifier
 * - a void * namespace specifier (or any other equal pointer type)
 */

struct hash_head;
struct hash_head {
	struct hash_head *next_hash;
	struct hash_head **pprev_hash;
};


/* Global namespaces. You should define them to values not valid
 * for any usable c-pointer. */
#define CG_NAMESPACE ((void *)(0xffff0010))
#define FILTER_NAMESPACE ((void *)(0xffff0020))

/* initialize global hashtable.
 * returns -1 on error */
int hash_alloc();

/* dump hashtable statistics. */
void hash_dump();



/* The following variables/functions/macros are the internal
 * used ones, i.e. those which you should wrap with your own
 * type-specific macros.
 */
extern struct hash_head **hash_table;

int _hashfn(const char *name, const void *namespace);
#define _hash(name, namespace) (hash_table + _hashfn(name, namespace))

#define __hash_pos(type, head, name, namespace) (unsigned long)(&((type *)0)->head), (unsigned long)(&((type *)0)->name), (unsigned long)(&((type *)0)->namespace)
#define __hash_entry(ptr, type, head) ((type *)((char *)(ptr)-(unsigned long)(&((type *)0)->head)))

struct hash_head *_hash_find(const char *name, const void *namespace,
			     struct hash_head *entry,
			     unsigned long _head, unsigned long _name,
			     unsigned long _namespace);

void _hash_add(struct hash_head *entry, struct hash_head **loc);

void _hash_remove(struct hash_head *entry);

struct hash_head *_hash_next(struct hash_head *entry, void *namespace,
			     unsigned long _head, unsigned long _name,
			     unsigned long _namespace);

#define _hash_init(hhead) do { (hhead)->pprev_hash = NULL; } while (0)

#define _is_hashed(hhead) ((hhead)->pprev_hash != NULL)


#endif

