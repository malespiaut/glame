/*
 * glame_hash.c
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "glame_hash.h"


/* One page sized hashtable. */
#define HASH_BITS (10)
#define HASH_SIZE (1 << HASH_BITS)

struct hash_head **hash_table = NULL;


int hash_alloc()
{
	if (!(hash_table = (struct hash_head **)calloc(HASH_SIZE+1, sizeof(void *))))
		return -1;

	hash_table[HASH_SIZE] = NULL;

	return 0;
}

#define STEP 16
void hash_dump()
{
	struct hash_head *h;
	int i, j, cnt;

	for (i=0; i<HASH_SIZE; i+=STEP) {
		cnt = 0;
		for (j=i; j<i+STEP; j++) {
			h = hash_table[i];
			while (h) {
				cnt++;
				h = h->next_hash;
			}
		}
		printf("%.5i-%.5i: %.5i  ", i, i+STEP-1, cnt);
		if (cnt > 50)
			cnt = 50;
		for (j=0; j<cnt; j++)
			printf("*");
		printf("\n");
	}
}


/* convenience macros */
#define NAME(entry, head, name) (*(char **)((char *)(entry) - (head) + (name)))
#define NAMESPACE(entry, head, namespace) (*(void **)((char *)(entry) - (head) + (namespace)))

int _hashfn(const char *name, const void *namespace)
{
	int len, val = 0;

	len = strlen(name);
	while (len--)
		val += *(name++);

	val += ((int)namespace)/sizeof(void *);

	return (val & (HASH_SIZE-1));
}

struct hash_head *_hash_find(const char *name, const void *namespace,
			     struct hash_head *entry,
			     unsigned long _head, unsigned long _name,
			     unsigned long _namespace)
{
	while (entry) {
		if (NAMESPACE(entry, _head, _namespace) == namespace
		    && strcmp(NAME(entry, _head, _name), name) == 0)
			break;
		entry = entry->next_hash;
	}
	return entry;
}

void _hash_add(struct hash_head *entry, struct hash_head **loc)
{
	if ((entry->next_hash = *loc) != NULL)
		(*loc)->pprev_hash = &entry->next_hash;
	*loc = entry;
	entry->pprev_hash = loc;
}

void _hash_remove(struct hash_head *entry)
{
	if (entry->pprev_hash) {
		if (entry->next_hash)
			entry->next_hash->pprev_hash = entry->pprev_hash;
		*entry->pprev_hash = entry->next_hash;
		entry->pprev_hash = NULL;
	}
}

struct hash_head *_hash_next(struct hash_head *entry, void *namespace,
			     unsigned long _head, unsigned long _name,
			     unsigned long _namespace)
{
	struct hash_head **slot;

	if (!entry) {
		slot = hash_table;
		entry = *slot;
		goto _slot;
	} else {
		slot = _hash(NAME(entry, _head, _name), namespace);
		goto _next;
	}

	do {
		if (NAMESPACE(entry, _head, _namespace) == namespace)
			break;
 _next:
    		entry = entry->next_hash;
 _slot:
		while (!entry && slot - hash_table < HASH_SIZE) {
			slot++;
			entry = *slot;
		}
	} while (entry);

	return entry;
}






