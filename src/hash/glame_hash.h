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


/* This is the global name/namespace hash.
 * To add a new type to the hash, prepend your type
 * with a hash_head.
 */
struct hash_head;
struct hash_head {
	struct hash_head *next_hash;
	struct hash_head **pprev_hash;
	void *namespace;
	char *name;
};

extern struct hash_head **hash_table;
#define _hash(name, namespace) (hash_table + _hashfn(name, namespace))

int _hashfn(const char *name, const void *namespace);
struct hash_head *_hash_find(const char *name, const void *namespace,
			     struct hash_head *entry);
void _hash_add(struct hash_head *entry, struct hash_head **loc);
void _hash_remove(struct hash_head *entry);


/* initialize global hashtable.
 * returns -1 on error */
int hash_init();

/* dump hashtable statistics. */
void hash_dump();


/* below are use-EXAMPLES! dont put the actual macros here, instead
 * put them into your subsystem headerfile
 */
#if 0
/* filter (global) hash */
#define FILTER_NAMESPACE 0xffff0010
#define hash_find_filter(name) _hash_find((name), FILTER_NAMESPACE, (*(_hash((name), FILTER_NAMESPACE))))
#define hash_add_filter(f) do { (f)->namespace = FILTER_NAMESPACE; _hash_add((struct hash_head *)(f), _hash((f)->name, FILTER_NAMESPACE)); } while (0)
#define hash_remove_filter(f) _hash_remove((struct hash_head *)(f))

/* cluster in cluster-group hash */
#define hash_find_cluster(name, group) _hash_find((name), (group), (*(_hash((name), (group)))))
#define hash_add_cluster(c) _hash_add((struct hash_head *)(c), _hash((c)->name, (c)->group))
#define hash_remove_cluster(c) _hash_remove((struct hash_head *)(c))
#endif


#endif
