/*
 * pmap.c
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <pthread.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include "util.h"
#include "list.h"
#include "hash.h"
#include "pmap.h"


/* Switch to disable lru (make pmap operation synchronous, just like
 * using mmap/munmap directly). */
#define USE_LRU


/* Wheeeeee! HACK! Make it compile, but not work :) */
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS 0
#endif

struct mapping;
struct mapping {
	struct mapping *next_mappingv_hash;
	struct mapping **pprev_mappingv_hash;
	struct mapping *next_mappingp_hash;
	struct mapping **pprev_mappingp_hash;
	struct list_head lru;

	int refcnt;
	void *mapping;

	size_t size;
	int prot;
	int flags;
	int fd;
	off_t offset;
};


/* The mapping hashes
 * - one by virtual address
 * - one by offset, size, and fd */
HASH(mappingv, struct mapping, 8,
     (mappingv->mapping == addr),
     ((long)addr/4096),
     ((long)mappingv->mapping/4096),
     void *addr)
HASH(mappingp, struct mapping, 8,
     (mappingp->offset == offset && mappingp->size == size && mappingp->fd == fd),
     (offset/4096+size/4096+fd),
     (mappingp->offset/4096+mappingp->size/4096+mappingp->fd),
     size_t size, int fd, off_t offset)

/* The lru list and the backing store fd. */
static struct list_head pmap_lru;
static size_t pmap_lrusize;
static int pmap_lrucnt;
static size_t pmap_size;

/* Lock protecting the pmap hashes, the lru and the lru meta. */
static pthread_mutex_t pmap_mutex;

/* Statistics. */
static int pmap_created;
static int pmap_reused;
static int pmap_lruhit;

/* Tunables. */
static size_t pmap_maxsize;
static int pmap_maxlrucnt;


/* Internal prototypes. */
static void _map_free(struct mapping *m);


/* Alloc a new mapping struct and initialize it by creating a mapping
 * as specified. This is the only function that populates the hashes.
 * The lru is _not_ touched! */
static struct mapping *_map_alloc(void *start, size_t size, int prot,
				  int flags, int fd, off_t offset)
{
	struct mapping *m;

	/* Initialize struct mapping. */
	if (!(m = (struct mapping *)malloc(sizeof(struct mapping))))
		return NULL;
	INIT_LIST_HEAD(&m->lru);
	hash_init_mappingv(m);
	hash_init_mappingp(m);
	m->refcnt = 1;

	/* Try to create the mapping. */
	m->size = size;
	m->prot = prot;
	m->flags = flags;
	m->offset = offset;
	m->fd = fd;
	m->mapping = mmap(start, size, prot, flags, fd, offset);
	if (m->mapping == MAP_FAILED)
		goto err;

	/* Hash the mapping. */
	hash_add_mappingp(m);
	hash_add_mappingv(m);

	/* Account for it. */
	pmap_size += size;
	pmap_created++;

	return m;

 err:
	free(m);
	return NULL;
}


/* Frees (unmaps) a mapping. This is the only function that removes
 * a mapping from the hashes. The lru is also depopulated, if necessary.
 * Note, that the refcnt is ignored! */
static void _map_free(struct mapping *m)
{
	hash_remove_mappingv(m);
	if (is_hashed_mappingp(m))
		hash_remove_mappingp(m);
	if (!list_empty(&m->lru)) {
		list_del(&m->lru);
		pmap_lrusize -= m->size;
		pmap_lrucnt--;
	}
	pmap_size -= m->size;
	munmap(m->mapping, m->size);
	free(m);
}

/* Shrinks the lru until limits are fullified. */
static void _shrink_lru()
{
    	struct mapping *m;

	while (pmap_lrucnt > pmap_maxlrucnt
	       || pmap_size > pmap_maxsize) {
		m = list_gettail(&pmap_lru, struct mapping, lru);
		if (!m)
		    	return;
		_map_free(m);
	}
}

/* Completely zap the lru. */
static void _zap_lru()
{
    	struct mapping *m;

	while ((m = list_gettail(&pmap_lru, struct mapping, lru)))
		_map_free(m);
}

static inline void _map_ref(struct mapping *m)
{
	m->refcnt++;
	pmap_reused++;
	if (!list_empty(&m->lru)) {
	    	list_del(&m->lru);
		pmap_lrusize -= m->size;
		pmap_lrucnt--;
		pmap_lruhit++;
	}
}

static inline void _map_unref(struct mapping *m)
{
	if (--(m->refcnt) == 0) {
#ifdef USE_LRU
		if (!is_hashed_mappingp(m))
			_map_free(m);
		else {
#ifdef DEBUG
                        /* Protect the mapping from accidential use. */
			mprotect(m->mapping, m->size, PROT_NONE);
			m->prot = PROT_NONE;
#endif
			list_add(&m->lru, &pmap_lru);
			pmap_lrusize += m->size;
			pmap_lrucnt++;
			if (pmap_size > pmap_maxsize
			    || pmap_lrucnt > pmap_maxlrucnt)
				_shrink_lru();
		}
#else
		_map_free(m);
#endif
	}
}

/* Uncaches all mappings of the specified type from the lru. */
void _pmap_uncache(int fd, off_t offset, size_t size)
{
	struct list_head *mn;
	struct mapping *m;

	/* Scan the lru for mappings of the specified type. */
	mn = pmap_lru.next;
	while (mn != &pmap_lru) {
		m = list_entry(mn, struct mapping, lru);
		mn = mn->next;
		if (m->fd == fd
		    && ((m->offset>=offset && m->offset<offset+size)
			|| (offset>=m->offset && offset<m->offset+m->size)))
			_map_free(m);
	}
}

/* Invalidates mappings of the specified type - does not uncache
 * mappings, so do an _pmap_uncache first. If do_invalidate is 0
 * matching mappings are only counted. The number of invalidated
 * mappings is returned. */
int _pmap_invalidate(int fd, off_t offset, size_t size, int do_invalidate)
{
	struct mapping *m;
	int i, cnt = 0;

	/* Scan the mappingv hash. */
	for (i=0; i<((1<<8)-1); i++) {
		m = hash_getslot_mappingv(i);
		while (m) {
			if (m->fd == fd
			    && ((m->offset>=offset && m->offset<offset+size)
				|| (offset>=m->offset
				    && offset<m->offset+m->size))) {
				cnt++;
				if (do_invalidate) {
					hash_remove_mappingp(m);
					m->fd = -1;
				}
			}
			m = hash_next_mappingv(m);
		}
	}

	return cnt;
}


/* API.
 */

int pmap_init(size_t maxsize)
{
    	INIT_LIST_HEAD(&pmap_lru);
	pmap_lrusize = 0;
	pmap_lrucnt = 0;
	pmap_size = 0;
	pmap_maxsize = maxsize;
	pmap_maxlrucnt = 64;
	pthread_mutex_init(&pmap_mutex, NULL);

	pmap_created = 0;
	pmap_reused = 0;
	pmap_lruhit = 0;

	return 0;
}

void pmap_close()
{
    	struct mapping *m;
	int i;

	/* Walk the virtual hash to remove all mappings (includes lru). */
    	for (i=0; i<(1<<8); i++) {
	    while ((m = mappingv_hash_table[i]))
		_map_free(m);
	}

	pthread_mutex_destroy(&pmap_mutex);

	DPRINTF("PMAP statistics: %i created, %i reused, %i lruhit\n",
		pmap_created, pmap_reused, pmap_lruhit);
}


void *pmap_map(void *start, size_t size, int prot, int flags, int fd, off_t offset)
{
	struct mapping *m;

	/* We dont (want to) support MAP_FIXED. */
	if (flags != MAP_SHARED && flags != MAP_PRIVATE) {
		errno = EINVAL;
		return MAP_FAILED;
	}

	pthread_mutex_lock(&pmap_mutex);

	/* We may not share MAP_PRIVATE mappings at all. Never. */
#ifdef USE_LRU /* if not set, always create a new mapping */
	if (flags == MAP_PRIVATE)
#endif
		goto new_mapping;
    
	/* A shared mapping is requested. Return a cached mapping,
	 * if available. */
	m = NULL;
	while ((m = hash_find_next_mappingp(size, fd, offset, m))) {
		/* Flags (MAP_SHARED/MAP_PRIVATE) have to match. */
		if (m->flags != flags)
			continue;
		/* Prot match? Ref it and ok. */
		if (m->prot == prot) {
			_map_ref(m);
			goto ok;
		}
	}

	/* In second run, we can fix protection, if refcnt is
	 * zero. */
	while ((m = hash_find_next_mappingp(size, fd, offset, m))) {
		/* Flags (MAP_SHARED/MAP_PRIVATE) have to match. */
		if (m->flags != flags)
			continue;
		/* In use? Cant do anything about it. */
		if (m->prot != prot && m->refcnt != 0)
			continue;
		/* Fix protection, ref and ok. */
		if (mprotect(m->mapping, m->size, prot) == -1)
			continue; /* umm... */
		m->prot = prot;
		_map_ref(m);
		goto ok;
	}

	/* Still no go? Create a new mapping. */

new_mapping:
	m = _map_alloc(start, size, prot, flags, fd, offset);

ok:
	pthread_mutex_unlock(&pmap_mutex);
	if (!m)
		return MAP_FAILED;
	return m->mapping;
}

void *pmap_zeromap(void *start, size_t length, int prot,
		   int flags, int fd, off_t offset)
{
	struct mapping *m;
	void *mem;

	/* We dont (want to) support MAP_FIXED. */
	if (flags != MAP_SHARED && flags != MAP_PRIVATE) {
		errno = EINVAL;
		return MAP_FAILED;
	}

	/* Real mapping of the file case. */
	if ((prot & PROT_WRITE)
	    && (flags == MAP_SHARED)) {
		mem = pmap_map(start, length, prot, flags, fd, offset);
		if (mem == MAP_FAILED)
			return MAP_FAILED;
		memset(mem, 0, length);
		return mem;
	}

	/* In any other case we're faster getting an anonymous mapping. */
	pthread_mutex_lock(&pmap_mutex);
	m = _map_alloc(start, length, prot, flags|MAP_ANONYMOUS, -1, offset);
	pthread_mutex_unlock(&pmap_mutex);
	if (!m)
		return MAP_FAILED;

	return m->mapping;
}


int pmap_unmap(void *start)
{
	struct mapping *m;

	pthread_mutex_lock(&pmap_mutex);
	if (!(m = hash_find_mappingv(start))) {
		pthread_mutex_unlock(&pmap_mutex);
		return -1;
	}
	_map_unref(m);
	pthread_mutex_unlock(&pmap_mutex);
	return 0;
}

int pmap_discard(void *start)
{
	struct mapping *m;

	pthread_mutex_lock(&pmap_mutex);
	if (!(m = hash_find_mappingv(start))) {
		pthread_mutex_unlock(&pmap_mutex);
		return -1;
	}
	if (--(m->refcnt) == 0) {
		if ((m->prot & PROT_WRITE)
		    && (m->flags & MAP_SHARED))
			msync(m->mapping, m->size, MS_INVALIDATE);
		_map_free(m);
	}
	pthread_mutex_unlock(&pmap_mutex);
	return 0;
}

void pmap_shrink()
{
	pthread_mutex_lock(&pmap_mutex);
	_zap_lru();
	pthread_mutex_unlock(&pmap_mutex);
}

int pmap_uncache(int fd, off_t offset, size_t size)
{
	int res;

	pthread_mutex_lock(&pmap_mutex);
	_pmap_uncache(fd, offset, size);
	res = _pmap_invalidate(fd, offset, size, 0);
	pthread_mutex_unlock(&pmap_mutex);

	return res;
}

void pmap_invalidate(int fd, off_t offset, size_t size)
{
	pthread_mutex_lock(&pmap_mutex);
	_pmap_uncache(fd, offset, size);
	_pmap_invalidate(fd, offset, size, 1);
	pthread_mutex_unlock(&pmap_mutex);
}

int pmap_has_mappings(int fd, off_t offset, size_t size)
{
	int res;

	pthread_mutex_lock(&pmap_mutex);
	res = _pmap_invalidate(fd, offset, size, 0);
	pthread_mutex_unlock(&pmap_mutex);

	return res > 0 ? 1 : 0;
}
