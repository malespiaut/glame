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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include "util.h"
#include "glame_sem.h"
#include "glame_hash.h"

/* One page sized hashtable. */
#define HASH_BITS (10)
#define HASH_SIZE (1 << HASH_BITS)

struct hash_head **hash_table = NULL;


/* Beware! this is not signal-safe!!
 * Add sigprocmask(all, SIGBLOCK,..) if you want to access
 * or modify the hash from signal handlers.
 */
#ifndef USE_PTHREADS
#define _lock()
#define _unlock()
#define _lock_w()
#define _unlock_w()
#else
static int semid = -1;
static int semnum = -1;
static inline void _lock()
{
	struct sembuf sop;
	sop.sem_num = semnum;
	sop.sem_op = -1;
	sop.sem_flg = 0;
	glame_semop(semid, &sop, 1);
}
static inline void _unlock()
{
	struct sembuf sop;
	sop.sem_num = semnum;
	sop.sem_op = 1;
	sop.sem_flg = IPC_NOWAIT;
	glame_semop(semid, &sop, 1);
}
static inline void _lock_w()
{
	struct sembuf sop;
	sop.sem_num = semnum;
	sop.sem_op = -10000;
	sop.sem_flg = 0;
	glame_semop(semid, &sop, 1);
}
static inline void _unlock_w()
{
	struct sembuf sop;
	sop.sem_num = semnum;
	sop.sem_op = 10000;
	sop.sem_flg = IPC_NOWAIT;
	glame_semop(semid, &sop, 1);
}
#endif


static void cleanup()
{
	union semun ssun;
	glame_semctl(semid, 0, IPC_RMID, ssun);
}

int hash_alloc()
{
	if (!(hash_table = (struct hash_head **)calloc(HASH_SIZE+1, sizeof(void *))))
		return -1;
	hash_table[HASH_SIZE] = NULL;

#ifdef USE_PTHREADS
	if ((semid = glame_semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) == -1)
		return -1;
	semnum = 0;
	{
		union semun ssun;
		ssun.val = 10000;
		if (glame_semctl(semid, semnum, SETVAL, ssun) == -1)
			return -1;
	}

	/* register cleanup handler */
	atexit(cleanup);
#endif

	return 0;
}

#define STEP 16
void hash_dump()
{
	struct hash_head *h;
	int i, j, cnt;

	_lock();
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
	_unlock();
}


void hash_lock()
{
	_lock_w();
}

void hash_unlock()
{
	_unlock_w();
}


/* convenience macros */
#define NAME(entry, head, name) (*(char **)((char *)(entry) - (head) + (name)))
#define NAMESPACE(entry, head, nmspace) (*(void **)((char *)(entry) - (head) + (nmspace)))

int _hashfn(const char *name, const void *nmspace)
{
	int len, val = 0;

	len = strlen(name);
	while (len--)
		val += *(name++);

	val += ((int)nmspace)/sizeof(void *);

	return (val & (HASH_SIZE-1));
}

struct hash_head *__hash_find(const char *name, const void *nmspace,
			      struct hash_head **e,
			      unsigned long _head, unsigned long _name,
			      unsigned long _nmspace)
{
	struct hash_head *entry = *e;

	while (entry) {
		if (NAMESPACE(entry, _head, _nmspace) == nmspace
		    && strcmp(NAME(entry, _head, _name), name) == 0)
			break;
		entry = entry->next_hash;
	}
	return entry;
}
struct hash_head *_hash_find(const char *name, const void *nmspace,
			     struct hash_head **e,
			     unsigned long _head, unsigned long _name,
			     unsigned long _nmspace)
{
	struct hash_head *entry;

	_lock();
	entry = __hash_find(name, nmspace, e, _head, _name, _nmspace);
	_unlock();
	return entry;
}

void __hash_add(struct hash_head *entry, struct hash_head **loc)
{
        if (_is_hashed(entry))
	        DERROR("Adding already added hash entry");
	if ((entry->next_hash = *loc) != NULL)
		(*loc)->pprev_hash = &entry->next_hash;
	*loc = entry;
	entry->pprev_hash = loc;
}
void _hash_add(struct hash_head *entry, struct hash_head **loc)
{
	_lock_w();
	__hash_add(entry, loc);
	_unlock_w();
}

void __hash_remove(struct hash_head *entry)
{
        if (!_is_hashed(entry))
                DERROR("Removing already removed hash entry");
	if (entry->pprev_hash) {
		if (entry->next_hash)
			entry->next_hash->pprev_hash = entry->pprev_hash;
		*entry->pprev_hash = entry->next_hash;
		entry->pprev_hash = NULL;
	}
}
void _hash_remove(struct hash_head *entry)
{
	_lock_w();
	__hash_remove(entry);
	_unlock_w();
}

struct hash_head *__hash_walk(struct hash_head *entry, void *nmspace,
			      unsigned long _head, unsigned long _name,
			      unsigned long _nmspace)
{
	struct hash_head **slot;

	if (!entry) {
		slot = hash_table;
		entry = *slot;
		goto _slot;
	} else {
		slot = _hash(NAME(entry, _head, _name), nmspace);
		goto _next;
	}

	do {
		if (NAMESPACE(entry, _head, _nmspace) == nmspace)
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


const char *__hash_unique_name(const char *prefix, void *nmspace,
			       unsigned long _head, unsigned long _name,
			       unsigned long _nmspace)
{
	char buf[256];
	int i;

	for (i=1;; i++) {
		snprintf(buf, 255, "%s-%i", prefix, i);
		if (!__hash_find(buf, nmspace, _hash(buf, nmspace),
				 _head, _name, _nmspace))
			return strdup(buf);
	}

	return NULL;
}

const char *_hash_unique_name(const char *prefix, void *nmspace,
			      unsigned long _head, unsigned long _name,
			      unsigned long _nmspace)
{
	const char *name;

	_lock();
	name = __hash_unique_name(prefix, nmspace, _head, _name, _nmspace);
	_unlock();

	return name;
}
