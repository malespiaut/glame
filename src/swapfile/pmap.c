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
#include <fcntl.h>
#include <errno.h>
#include "util.h"
#include "list.h"
#include "hash.h"
#include "pmap.h"


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


/* The mapping hashes, one by virtual address, one by pfn, prot
 * and flags. */
HASH(mappingv, struct mapping, 8,
     (mappingv->mapping == addr),
     ((long)addr/4096),
     ((long)mappingv->mapping/4096),
     void *addr)
HASH(mappingp, struct mapping, 8,
     (mappingp->offset == offset && mappingp->size == size && mappingp->flags == flags && mappingp->prot == prot && mappingp->fd == fd),
     (offset/4096+size/4096+flags+prot+fd),
     (mappingp->offset/4096+mappingp->size/4096+mappingp->flags+mappingp->prot+mappingp->fd),
     size_t size, int flags, int prot, int fd, off_t offset)


/* The lru list and the backing store fd. */
static struct list_head pmap_lru;
static size_t pmap_lrusize;
static int pmap_lrucnt;
static size_t pmap_minfree;
static size_t pmap_minlrusize;
static int pmap_lrumaxcnt;
static pthread_mutex_t pmap_mutex;


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
	munmap(m->mapping, m->size);
	free(m);
}

#ifndef MAP_ANONYMOUS
static int _fms_fd = -1;
#endif
static inline void *_fast_mmap_something(size_t size)
{
#ifdef MAP_ANONYMOUS
	return mmap(0, size, PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
#else
	return mmap(0, size, PROT_NONE, MAP_PRIVATE, _fms_fd, 0);
#endif
}

/* Try to shrink lru list, if necessary. */
static int _shrink_lru(size_t minfree)
{
    	struct mapping *m;
	void *mem;

	while (pmap_lrucnt > pmap_lrumaxcnt
	       || (mem = _fast_mmap_something(minfree)) == MAP_FAILED) {
		m = list_gettail(&pmap_lru, struct mapping, lru);
		if (!m)
		    	return -1;
		_map_free(m);
	}
	munmap(mem, minfree);
	return 0;
}

static inline void _map_ref(struct mapping *m)
{
	m->refcnt++;
	if (!list_empty(&m->lru)) {
	    	list_del(&m->lru);
		pmap_lrusize -= m->size;
		pmap_lrucnt--;
	}
}

static inline void _map_unref(struct mapping *m)
{
	if (--(m->refcnt) == 0) {
		if (!is_hashed_mappingp(m))
			_map_free(m);
		else {
			list_add(&m->lru, &pmap_lru);
			pmap_lrusize += m->size;
			pmap_lrucnt++;
			if (pmap_lrusize > pmap_minlrusize
			    || pmap_lrucnt > pmap_lrumaxcnt)
				_shrink_lru(pmap_minfree);
		}
	}
}

/* Uncaches all mappings of the specified type from the lru. */
void _pmap_uncache(size_t size, int prot, int flags, int fd, off_t offset)
{
	struct list_head *mn;
	struct mapping *m;

	/* Scan the lru for mappings of the specified type. */
	mn = pmap_lru.next;
	while (mn != &pmap_lru) {
		m = list_entry(mn, struct mapping, lru);
		mn = mn->next;
		if (m->fd == fd
		    && (m->prot & prot) && (m->flags & flags)
		    && ((m->offset>=offset && m->offset<offset+size)
			|| (offset>=m->offset && offset<m->offset+m->size)))
			_map_free(m);
	}
}

/* Invalidates mappings of the specified type - does not uncache
 * mappings, so do an _pmap_uncache first. If do_invalidate is 0
 * matching mappings are only counted. The number of invalidated
 * mappings is returned. */
int _pmap_invalidate(size_t size, int prot, int flags, int fd,
		     off_t offset, int do_invalidate)
{
	struct mapping *m;
	int i, cnt = 0;

	/* Scan the mappingv hash. */
	for (i=0; i<((1<<8)-1); i++) {
		m = hash_getslot_mappingv(i);
		while (m) {
			if (m->fd == fd
			    && (m->prot & prot) && (m->flags & flags)
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

int pmap_init(size_t minfree)
{
    	INIT_LIST_HEAD(&pmap_lru);
	pmap_lrusize = 0;
	pmap_lrucnt = 0;
	pmap_minfree = minfree;
	pmap_minlrusize = 8*1024*1024;
	pmap_lrumaxcnt = 32;
	pthread_mutex_init(&pmap_mutex, NULL);

#ifndef MAP_ANONYMOUS
	_fms_fd = open("/dev/zero", O_RDONLY);
#endif

	return 0;
}

void pmap_close()
{
    	struct mapping *m;
	int i;

	/* Walk the virtual hash to remove all mappings. */
    	for (i=0; i<(1<<8); i++) {
	    while ((m = mappingv_hash_table[i]))
		_map_free(m);
	}

#ifndef MAP_ANONYMOUS
	close(fd);
#endif

	pthread_mutex_destroy(&pmap_mutex);
}


void *pmap_map(void *start, size_t size, int prot, int flags, int fd, off_t offset)
{
	struct mapping *m;

	pthread_mutex_lock(&pmap_mutex);

	/* We may not share MAP_PRIVATE mappings at all. Never. */
	if (flags & MAP_PRIVATE)
		goto new_mapping;
	
	/* MAP_FIXED mappings have to be looked up through the
	 * virtual hash. */
	if (flags & MAP_FIXED
	    && (m = hash_find_mappingv(start))
	    && m->offset == offset
	    && m->size == size
	    && m->fd == fd
	    && m->prot == prot
	    && m->flags == flags) {
		_map_ref(m);
		goto ok;
	}
    
	/* If a shared mapping is requested, return a cached mapping,
	 * if available. */
	if (flags & MAP_SHARED
	    && (m = hash_find_mappingp(size, flags, prot, fd, offset))) {
		_map_ref(m);
		goto ok;
	}

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

	/* Real mapping of the file case. */
	if ((prot & PROT_WRITE)
	    && (flags & MAP_SHARED)) {
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
	_shrink_lru((size_t)1 << (sizeof(size_t)*8-2));
	pthread_mutex_unlock(&pmap_mutex);
}

int pmap_uncache(size_t size, int prot, int flags, int fd, off_t offset)
{
	int res;

	pthread_mutex_lock(&pmap_mutex);
	_pmap_uncache(size, prot, flags, fd, offset);
	res = _pmap_invalidate(size, prot, flags, fd, offset, 0);
	pthread_mutex_unlock(&pmap_mutex);

	return res;
}

void pmap_invalidate(size_t size, int prot, int flags, int fd, off_t offset)
{
	pthread_mutex_lock(&pmap_mutex);
	_pmap_uncache(size, prot, flags, fd, offset);
	_pmap_invalidate(size, prot, flags, fd, offset, 1);
	pthread_mutex_unlock(&pmap_mutex);
}
