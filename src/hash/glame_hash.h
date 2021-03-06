#ifndef _GLAMEHASH_H
#define _GLAMEHASH_H

/*
 * glame_hash.h
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


/* This is the framework for a global name[namespace] hash.
 * To use this framework with your own type, define the following
 * set of macros (as f.i. done in channel/channel.c for the
 * channel group hash and the channel[channel group] hash):
 * - hash_find_XXX(name [, namespace])
 * - hash_next_XXX(var)
 * - hash_add_XXX(var)
 * - hash_remove_XXX(var)
 * - hash_first_XXX([namespace])
 * - hash_walk_XXX(var)
 * - hash_unique_name_XXX(prefix, [namespace])
 * - hash_init_XXX(var)
 * - is_hashed_XXX(var)
 * and the corresponding _hash_*_* with no locking.
 * You have to embed the following required entries into your type:
 * - a struct hash_head
 * - a char * name specifier
 * - a void * namespace specifier (or any other equal pointer type)
 *
 * So suppose, you have a type
 * typedef struct {
 *    struct hash_head hash;
 *    char *name;
 *    foobar_t *group;
 *    ...
 * } foo_t;
 * where name is the unique identifier and group the namespace.
 * You would then define the above macros like
 * #define hash_find_foo(n, g)  __hash_entry(_hash_find((n), (g), _hash((n), (g)), __hash_pos(foo_t, hash, name, group)), foo_t, hash)
 * #define _hash_find_foo(n, g) __hash_entry(__hash_find((n), (g), _hash((n), (g)), __hash_pos(foo_t, hash, name, group)), foo_t, hash)
 * #define hash_next_foo(f)     __hash_entry(_hash_find((f)->name, (f)->group, &(f)->hash.next_hash, __hash_pos(foo_t, hash, name, group)), foo_t, hash)
 * #define _hash_next_foo(f)    __hash_entry(__hash_find((f)->name, (f)->group, &(f)->hash.next_hash, __hash_pos(foo_t, hash, name, group)), foo_t, hash)
 * #define hash_add_foo(f)      _hash_add(&(f)->hash, _hash((f)->name, (f)->group))
 * #define _hash_add_foo(f)     __hash_add(&(f)->hash, _hash((f)->name, (f)->group))
 * #define hash_remove_foo(f)   _hash_remove(&(f)->hash)
 * #define _hash_remove_foo(f)  __hash_remove(&(f)->hash)
 * #define hash_first_foo(g)    __hash_entry(_hash_walk(NULL, (g), __hash_pos(foo_t, hash, name, group)), foo_t, hash)
 * #define _hash_first_foo(g)   __hash_entry(__hash_walk(NULL, (g), __hash_pos(foo_t, hash, name, group)), foo_t, hash)
 * #define _hash_walk_foo(f)    __hash_entry(__hash_walk(&(f)->hash, (f)->group, __hash_pos(foo_t, hash, name, group)), foo_t, hash)
 * #define hash_unique_name_foo(prefix, ns)  _hash_unique_name((prefix), (ns), __hash_pos(foo_t, hash, name, group))
 * #define _hash_unique_name_foo(prefix, ns) __hash_unique_name((prefix), (ns), __hash_pos(foo_t, hash, name, group))
 * #define hash_init_foo(f)     _hash_init(&(f)->hash)
 * #define _hash_init_foo(f)    hash_init_foo(f)
 * #define is_hashed_foo(f)     _is_hashed(&(f)->hash)
 * #define _is_hashed_foo(f)    is_hashed_foo(f)
 */

struct hash_head;
struct hash_head {
	struct hash_head *next_hash;
	struct hash_head **pprev_hash;
};
#define INIT_HASH_HEAD(hp) do { (hp)->next_hash = NULL; \
	(hp)->pprev_hash = NULL; } while (0)

/* Global namespaces. You should define them to values not valid
 * for any usable c-pointer. */
#define TG_NAMESPACE ((void *)(0xffff0010))
#define FILTER_NAMESPACE ((void *)(0xffff0020))
#define PLUGIN_NAMESPACE ((void *)(0xffff0030))


#ifdef __cplusplus
extern "C" {
#endif

/* initialize global hashtable.
 * returns -1 on error */
int hash_alloc(void);

/* dump hashtable statistics. */
void hash_dump(void);


/* With the following primitives you can lock the hash against
 * modifications and parallel read by other threads. Inside a
 * locked section you have to use the _hash_*_* macros which
 * do not do internal locking.
 */

/* lock the hash against modification */
void hash_lock(void);

/* unlock the hash */
void hash_unlock(void);



/* The following variables/functions/macros are the internal
 * used ones, i.e. those which you should wrap with your own
 * type-specific macros.
 */
extern struct hash_head **hash_table;

int _hashfn(const char *name, const void *nmspace);
#define _hash(name, nmspace) (hash_table + _hashfn(name, nmspace))

#define __hash_pos(type, head, name, nmspace) offsetof(type, head), offsetof(type, name), offsetof(type, nmspace)
#define __hash_entry(ptr, type, head) ((ptr) ? (type *)((char *)(ptr)-offsetof(type, head)) : NULL)

struct hash_head *__hash_find(const char *name, const void *nmspace,
			      struct hash_head **entry,
			      unsigned long _head, unsigned long _name,
			      unsigned long _nmspace);
struct hash_head *_hash_find(const char *name, const void *nmspace,
			     struct hash_head **entry,
			     unsigned long _head, unsigned long _name,
			     unsigned long _nmspace);

void __hash_add(struct hash_head *entry, struct hash_head **loc);
void _hash_add(struct hash_head *entry, struct hash_head **loc);

void __hash_remove(struct hash_head *entry);
void _hash_remove(struct hash_head *entry);

struct hash_head *__hash_walk(struct hash_head *entry, void *nmspace,
			      unsigned long _head, unsigned long _name,
			      unsigned long _nmspace);
struct hash_head *_hash_walk(struct hash_head *entry, void *nmspace,
			     unsigned long _head, unsigned long _name,
			     unsigned long _nmspace);

/* generate an unique name in the namespace using a provided
 * prefix.
 */
const char *__hash_unique_name(const char *prefix, void *nmspace,
			       unsigned long _head, unsigned long _name,
			       unsigned long _nmspace);

const char *_hash_unique_name(const char *prefix, void *nmspace,
			      unsigned long _head, unsigned long _name,
			      unsigned long _nmspace);



#define _hash_init(hhead) do { (hhead)->pprev_hash = NULL; } while (0)

#define _is_hashed(hhead) ((hhead)->pprev_hash != NULL)

#ifdef __cplusplus
}
#endif


#endif
