/*
 * swapfile.c
 * $Id: swapfile.c,v 1.18 2000/08/07 13:46:57 richi Exp $
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "swapfile.h"
#include "util.h"
#include "glame_sem.h"

/* TODO:
 *
 * - optimized cluster allocation [partly done]
 * - optimized cluster deallocation (merge with free previous and
 *   next cluster)
 *
 * - redo logentry stuff:
 *   + global logentry list (chronologically, for garbage collecting)
 *   + per filehead logentry list, i.e. get rid of the fh->logpos
 *     entry and the _logentry_prev/next mess
 *   + begin/end logentries are strictly local to filehead (not in
 *     global list)
 *   + perhaps per begin/end list of logentries? begin/end are queued
 *     in filehead local logentry-group-list?
 *   + get rid of begin/end completely?
 * - fix the file_use/file_unuse mess
 *
 */


typedef struct {
	int fd;            /* file descriptor */
	soff_t size;       /* full size */

	soff_t data_off;   /* offsett for raw data */
	soff_t data_size;  /* size for raw data */

	soff_t meta_off;   /* offsett for meta data */
	soff_t meta_size;  /* size for meta data */

	/* last used ids for clusters, logentries and fileheads */
	int cid, lid, fid;

	/* cluster list, sorted by offset in swapfile.
	 * free and used clusters! */
	struct list_head map;
	/* transaction log, sorted by time of transaction. */
	struct list_head log;
	/* list of files in swap, unsorted */
	struct list_head files;

	/* mmap-cache - clusters, LRU */
	struct list_head mapped;
	off_t mapped_size;
	off_t mapped_max;

	/* global lock - ugh! */
	int semid, semnum;
} swap_t;


static swap_t *swap = NULL;

/* "visible" files hashtable */
#define SWAPFILE_HASH_BITS (8)
#define SWAPFILE_HASH_SIZE (1 << SWAPFILE_HASH_BITS)
filehead_t **swapfile_hash_table;
#include "swapfile_hash.h"


/************************************************************
 * forward declarations
 */
static void cluster_unref(cluster_t *c);
static void _log_clear(filehead_t *f, logentry_t *start, int direction);
static logentry_t *_logentry_findbyid(filehead_t *f, int id);
static void _ffree(filehead_t *f);
static void __fcpopulate(filecluster_t *fc, cluster_t *c, int coff);



/************************************************************
 * locking helpers
 */
static inline void _swap_lock()
{
	struct sembuf sop;
	sop.sem_num = swap->semnum;
	sop.sem_op = -1;
	sop.sem_flg = 0;
	glame_semop(swap->semid, &sop, 1);
}

static inline void _swap_unlock()
{
	struct sembuf sop;
	sop.sem_num = swap->semnum;
	sop.sem_op = 1;
	sop.sem_flg = IPC_NOWAIT;
	glame_semop(swap->semid, &sop, 1);
}



/************************************************************
 * debug helpers
 */

#ifdef NDEBUG
#define _filecluster_check(c)
#else
static void _filecluster_check(filecluster_t *fc)
{
	filecluster_t *fc2;

	if (!fc)
		return;

	/* check weird data */
	if (fc->off < 0 || fc->size < 0 || fc->coff < 0)
		DERROR("weird fc contents");

	/* check position/size consistency wrt next/previous
	 * cluster */
	if (!list_empty(&fc->fc_list) && (fc2 = fc_prev(fc)))
		if (fc2->off + fc2->size != fc->off)
			DERROR("weird linkage prev <-> fc");
	if (!list_empty(&fc->fc_list) && (fc2 = fc_next(fc)))
		if (fc->off + fc->size != fc2->off)
			DERROR("weird linkage fc <-> next");

	if (!fc->cluster)
		return;

	/* check consistency with underlying cluster */
	if (fc->coff >= fc->cluster->size)
		DERROR("weird cluster offset");
	if (fc->size > fc->cluster->size - fc->coff)
		DERROR("oversized filecluster");
}
#endif



/************************************************************
 * allocation helpers with id tracking
 */

static cluster_t *__cluster_alloc(int id)
{
	cluster_t *c;

	if (!(c = ALLOC(cluster_t)))
		return NULL;

	if (id == -1)
		id = ++swap->cid;
	else if (id > swap->cid)
		swap->cid = id;
	c->id = id;

	INIT_LIST_HEAD(&c->c_list);
	INIT_LIST_HEAD(&c->rfc_list);
	INIT_LIST_HEAD(&c->map_list);
	ATOMIC_INIT(c->refcnt, 0);
	ATOMIC_INIT(c->mmapcnt, 0);

	return c;
}

static filehead_t *__filehead_alloc(int id)
{
	filehead_t *fh;

	if (!(fh = ALLOC(filehead_t)))
		return NULL;

	if (id == -1)
		id = ++swap->fid;
	else if (id > swap->fid)
		swap->fid = id;
	fh->fid = id;

	INIT_LIST_HEAD(&fh->fh_list);
	INIT_LIST_HEAD(&fh->fc_list);

	return fh;
}

static filecluster_t *__filecluster_alloc()
{
	filecluster_t *fc;

	if (!(fc = ALLOC(filecluster_t)))
		return NULL;

	INIT_LIST_HEAD(&fc->fc_list);
	INIT_LIST_HEAD(&fc->rfc_list);

	return fc;
}

static logentry_t *__logentry_alloc(int id)
{
	logentry_t *le;

	if (!(le = ALLOC(logentry_t)))
		return NULL;

	if (id == -1)
		id = ++swap->lid;
	else if (id > swap->lid)
		swap->lid = id;
	le->lid = id;

	INIT_LIST_HEAD(&le->le_list);

	return le;
}



/************************************************************
 * list addition/removal helpers
 * addition takes hint (add before hint element), else
 * (hint == NULL) in correct order.
 */

static void _cluster_add(cluster_t *c, struct list_head *hint)
{
	cluster_t *c2;

	if (!hint) {
		hint = &swap->map;
		while (hint = hint->next, hint != &swap->map) {
			c2 = list_entry(hint, cluster_t, c_list);
			if (c2->off >= c->off + c->size)
				break;
		}
	}

	list_add_tail(&c->c_list, hint);
}

static void _filehead_add(filehead_t *fh, struct list_head *hint)
{
	filehead_t *fh2;

	if (!hint) {
		hint = &swap->files;
		while (hint = hint->next, hint != &swap->files) {
			fh2 = list_entry(hint, filehead_t, fh_list);
			if (fh2->fid < fh->fid)
				break;
		}
	}

	list_add_tail(&fh->fh_list, hint);
}

static void __filecluster_add(filehead_t *file,
			      filecluster_t *fc, struct list_head *hint)
{
	filecluster_t *fc2;

	fc->f = file;

	if (!hint) {
		hint = &fc->f->fc_list;
		while (hint = hint->next, hint != &fc->f->fc_list) {
			fc2 = list_entry(hint, filecluster_t, fc_list);
			if (fc2->off >= fc->off + fc->size)
				break;
		}
	}

	list_add_tail(&fc->fc_list, hint);
}
static inline void _filecluster_add(filehead_t *file,
				    filecluster_t *fc, struct list_head *hint)
{
	__filecluster_add(file, fc, hint);
	_filecluster_check(fc);
}


static void _filecluster_remove(filecluster_t *fc)
{
	list_del(&fc->fc_list);
	if (fc->cluster) {
		list_del(&fc->rfc_list);
		cluster_unref(fc->cluster);
	}
}

static void _logentry_remove(logentry_t *le)
{
	list_del(&le->le_list);

	switch (le->op) {
	case LOGENTRY_INSERT:
		if (!is_hashed_file(le->u.insert.f))
			_ffree(le->u.insert.f);
		break;
	case LOGENTRY_DELETE:
		_ffree(le->u.delete.f);
		break;
	default:
	}
}

static void _logentry_add(logentry_t *le, struct list_head *hint)
{
	logentry_t *le2;

	if (!hint) {
		hint = &swap->log;
		while (hint = hint->next, hint != &swap->log) {
			le2 = list_entry(hint, logentry_t, le_list);
			if (le2->lid < le->lid)
				break;
		}
	}

	/* at addition we have to clear the log wrt undone logentries */
	if (le->f->logpos != -1) {
		_log_clear(le->f, _logentry_findbyid(le->f, le->f->logpos), 1);
		le->f->logpos = -1;
	}

	list_add_tail(&le->le_list, hint);
}


/************************************************************
 * other lists helpers
 */

static cluster_t *_cluster_findbyid(int id)
{
	struct list_head *lh;
	cluster_t *c = NULL;

	lh = &swap->map;
	while (lh = lh->next, lh != &swap->map) {
		c = list_entry(lh, cluster_t, c_list);
		if (c->id == id)
			break;
	}

	return c;
}

static filehead_t *_filehead_findbyid(int id)
{
	struct list_head *lh;
	filehead_t *fh = NULL;

	lh = &swap->files;
	while (lh = lh->next, lh != &swap->files) {
		fh = list_entry(lh, filehead_t, fh_list);
		if (fh->fid == id)
			return fh;
	}

	return NULL;
}

static soff_t _filehead_size(filehead_t *f)
{
	filecluster_t *first, *last;

	if (list_empty(&f->fc_list))
		return 0;

	first = list_entry(f->fc_list.next, filecluster_t, fc_list);
	last = list_entry(f->fc_list.prev, filecluster_t, fc_list);

	return last->off + last->size - first->off;
}

static filecluster_t *_filecluster_findbyoff(filehead_t *f, soff_t off, filecluster_t *hint)
{
	struct list_head *lh;
	filecluster_t *fc;

	if (hint)
		lh = hint->fc_list.prev;
	else
		lh = &f->fc_list;
	while (lh = lh->next, lh != &f->fc_list) {
		fc = list_entry(lh, filecluster_t, fc_list);
		if (fc->off > off)
			return NULL;
		if (filecluster_end(fc) >= off)
			return fc;
	}

	return NULL;
}

static logentry_t *_logentry_findbyid(filehead_t *f, int id)
{
	struct list_head *lh;
	logentry_t *l;

	lh = &swap->log;
	while (lh = lh->next, lh != &swap->log) {
		l = list_entry(lh, logentry_t, le_list);
		if (id == -1 && l->f == f)
			return l;
		if (l->lid == id)
			return l;
	}

	return NULL;
}

static logentry_t *_logentry_prev(filehead_t *f, logentry_t *l)
{
	struct list_head *lh;
	logentry_t *le;

	lh = &l->le_list;
	while (lh = lh->next, lh != &swap->log) {
		le = list_entry(lh, logentry_t, le_list);
		if (le->f == f)
			return le;
	}

	return NULL;
}

static logentry_t *_logentry_next(filehead_t *f, logentry_t *l)
{
	struct list_head *lh;
	logentry_t *le;

	lh = &l->le_list;
	while (lh = lh->prev, lh != &swap->log) {
		le = list_entry(lh, logentry_t, le_list);
		if (le->f == f)
			return le;
	}

	return NULL;
}

/* direction -1 is towards older, 1 is towards newer entries, start
 * is included, can be NULL in which case all entries are discarded */
static void _log_clear(filehead_t *f, logentry_t *start, int direction)
{
	struct list_head *lh, *lhnext;
	logentry_t *l;

	if (start)
		lhnext = &start->le_list;
	else
		lhnext = direction == -1 ? swap->log.next : swap->log.prev;
	while (lh = lhnext, lhnext = direction == -1 ? lh->next : lh->prev, lh != &swap->log) {
		l = list_entry(lh, logentry_t, le_list);
		if (l->f == f) {
			_logentry_remove(l);
			free(l);
		}
	}
}



/************************************************************
 * swapfile on-disk helpers for read/write of entries
 * no check for magic/type is done here
 */

static void _swapd_record_prepare(swapd_record_t *r, int type)
{
	memcpy(r->magic, RECORD_MAGIC, 4);
	r->type = type;
}

static int _cluster_read(swapd_record_t *r)
{
	cluster_t *c;

	if (!(c = __cluster_alloc(r->u.cluster.id)))
		return -1;
	c->off = r->u.cluster.off;
	c->size = r->u.cluster.size;
	ATOMIC_VAL(c->refcnt) = r->u.cluster.refcnt;
	ATOMIC_VAL(c->mmapcnt) = 0;
	c->buf = NULL;

	_cluster_add(c, NULL);

	return 0;
}

static void _cluster_write(swapd_record_t *r, cluster_t *c)
{
	_swapd_record_prepare(r, RECORD_TYPE_CLUSTER);

	r->u.cluster.off = c->off;
	r->u.cluster.size = c->size;
	r->u.cluster.refcnt = ATOMIC_VAL(c->refcnt);
	r->u.cluster.id = c->id;
}

static int _filehead_read(swapd_record_t *r)
{
	filehead_t *fh;

	if (!(fh = __filehead_alloc(r->u.filehead.fid)))
		return -1;
	fh->usecnt = r->u.filehead.usecnt;
	fh->begincnt = r->u.filehead.begincnt;
	fh->logpos = r->u.filehead.logpos;

	_filehead_add(fh, NULL);
	if (fh->usecnt == 0)
		hash_add_file(fh);

	return 0;
}

static void _filehead_write(swapd_record_t *r, filehead_t *fh)
{
	_swapd_record_prepare(r, RECORD_TYPE_FILEHEAD);

	r->u.filehead.fid = fh->fid;
	r->u.filehead.usecnt = fh->usecnt;
	r->u.filehead.begincnt = fh->begincnt;
	r->u.filehead.logpos = fh->logpos;
}

static int _filecluster_read(swapd_record_t *r)
{
	filecluster_t *fc;
	filehead_t *fh;
	cluster_t *c;

	if (!(fc = __filecluster_alloc()))
		return -1;
	if (!(fh = _filehead_findbyid(r->u.filecluster.fid))) {
		fprintf(stderr, "stale filecluster for non-existing file %i!\n",
			r->u.filecluster.fid);
		goto _err;
	}
	fc->off = r->u.filecluster.off;
	fc->size = r->u.filecluster.size;
	if (r->u.filecluster.cid == -1) /* hole */
		c = NULL;
	else
		if (!(c = _cluster_findbyid(r->u.filecluster.cid))) {
			fprintf(stderr, "stale filecluster with non-existent cluster!\n");
			goto _err;
		}

	__fcpopulate(fc, c, r->u.filecluster.coff);
	__filecluster_add(fh, fc, NULL);

	return 0;

_err:
	free(fc);
	return -1;
}

static void _filecluster_write(swapd_record_t *r, filecluster_t *fc)
{
	_swapd_record_prepare(r, RECORD_TYPE_FILECLUSTER);

	r->u.filecluster.fid = fc->f->fid;
	r->u.filecluster.off = fc->off;
	r->u.filecluster.size = fc->size;
	if (fc->cluster)
		r->u.filecluster.cid = fc->cluster->id;
	else /* hole */
		r->u.filecluster.cid = -1;
	r->u.filecluster.coff = fc->coff;
}

static int _logentry_read(swapd_record_t *r)
{
	logentry_t *le;
	filehead_t *fh;

	if (!(le = __logentry_alloc(r->u.logentry.lid)))
		return -1;
	if (!(fh = le->f = _filehead_findbyid(r->u.logentry.fid))) {
		fprintf(stderr, "stale logentry without file\n");
		goto _err;
	}
	le->op = r->u.logentry.op;
	switch (le->op) {
	case LOGENTRY_INSERT:
		le->u.insert.pos = r->u.logentry.u.insert.pos;
		le->u.insert.size = r->u.logentry.u.insert.size;
		if (!(le->u.insert.f = _filehead_findbyid(r->u.logentry.u.insert.fid))) {
			fprintf(stderr, "stale insert logentry without file\n");
			goto _err;
		}
		break;
	case LOGENTRY_DELETE:
		le->u.delete.pos = r->u.logentry.u.delete.pos;
		le->u.delete.size = r->u.logentry.u.delete.size;
		if (r->u.logentry.u.delete.fid == -1)
			le->u.delete.f = NULL;
		else
			if (!(le->u.delete.f = _filehead_findbyid(r->u.logentry.u.delete.fid))) {
				fprintf(stderr, "stale delete logentry without file\n");
				goto _err;
			}
		break;
	default:
	}
  
	_logentry_add(le, NULL);

	return 0;

_err:
	free(le);
	return -1;
}

static void _logentry_write(swapd_record_t *r, logentry_t *le)
{
	_swapd_record_prepare(r, RECORD_TYPE_LOGENTRY);

	r->u.logentry.fid = le->f->fid;
	r->u.logentry.lid = le->lid;
	r->u.logentry.op = le->op;
	switch (le->op) {
	case LOGENTRY_INSERT:
		r->u.logentry.u.insert.pos = le->u.insert.pos;
		r->u.logentry.u.insert.size = le->u.insert.size;
		r->u.logentry.u.insert.fid = le->u.insert.f->fid;
		break;
	case LOGENTRY_DELETE:
		r->u.logentry.u.delete.pos = le->u.delete.pos;
		r->u.logentry.u.delete.size = le->u.delete.size;
		if (le->u.delete.f)
			r->u.logentry.u.delete.fid = le->u.delete.f->fid;
		else
			r->u.logentry.u.delete.fid = -1;
		break;
	default:
	}
}



/************************************************************
 * cluster helper - they do _no_ reference counting!
 * (apart from mmap/munmap)
 */

#undef ZEROMAP_READ
static int __cluster_zeromap(cluster_t *c, int prot)
{
	int fd;
	char *buf;

	/* This is supposed to mmap the cluster while zeroing it,
	 * i.e. ideally generate a cow page of the zero page and
	 * associate the swapfile as backingstore, the page would
	 * have to be marked dirty at mapping time. Unfortunately
	 * this would require os-support.
	 * For Linux we can at least have the zero-page mapping
	 * trick by reading page-aligned from /dev/zero - this
	 * is from reading the source, so it could be wrong, too.
	 */
	buf = mmap(NULL, c->size, prot, MAP_SHARED, swap->fd, c->off);
	if (buf == MAP_FAILED)
		return -1;

#if defined ZEROMAP_READ
	if ((fd = open("/dev/zero", O_RDONLY)) != -1) {
		read(fd, buf, c->size);
		close(fd);
	} else
#endif
		memset(buf, 0, c->size);

	c->buf = buf;
	list_add(&c->map_list, &swap->mapped);
	swap->mapped_size += c->size;

	return 0;
}

static int __cluster_mmap(cluster_t *c, int prot)
{
	char *buf;

	buf = mmap(NULL, c->size, prot, MAP_SHARED, swap->fd, c->off);
	if (buf == MAP_FAILED)
		return -1;
#if (defined HAVE_MADVISE) && (defined SWAPFILE_MADV_MMAP)
	madvise(buf, c->size, SWAPFILE_MADV_MMAP);
#endif

	c->buf = buf;
	list_add(&c->map_list, &swap->mapped);
	swap->mapped_size += c->size;

	return 0;
}

static inline void __cluster_forgetmap(cluster_t *c)
{
	/* This is supposed to throw away any possibly dirty
	 * mapping of the file away - possibly creating a
	 * hole in the backing-store file, too.
	 * Well, of course this needs os-support...
	 */
	if (!cluster_is_mapped(c))
		return;
#if 0
#if (defined HAVE_MADVISE) && (defined SWAPFILE_MADV_FORGET)
	madvise(c->buf, c->size, SWAPFILE_MADV_FORGET);
#endif
#else	
	msync(c->buf, c->size, MS_INVALIDATE);
#endif
	munmap(c->buf, c->size);
	c->buf = NULL;
	swap->mapped_size -= c->size;
	list_del(&c->map_list);
	INIT_LIST_HEAD(&c->map_list);
}

static inline void __cluster_munmap(cluster_t *c)
{
	if (!cluster_is_mapped(c))
		return;
	munmap(c->buf, c->size);
	c->buf = NULL;
	swap->mapped_size -= c->size;
	list_del(&c->map_list);
	INIT_LIST_HEAD(&c->map_list);
}

/* we do __cluster_forgetmap on unref and refcnt == 0 */
static void cluster_unref(cluster_t *c)
{
	/* if we are about to release our last reference,
	 * we can do special things and sanity checks.
	 */
	if (ATOMIC_VAL(c->refcnt) == 1) {
		if (ATOMIC_VAL(c->mmapcnt) > 0)
			DERROR("cluster has still mappings!");
		__cluster_forgetmap(c);

		/* FIXME: try to merge cluster with previous/next
		 * one. */
	}

	atomic_dec(&c->refcnt);
}

static int _cluster_try_munmap(cluster_t *c)
{
	if (ATOMIC_VAL(c->mmapcnt) > 0)
		return -1;

	__cluster_munmap(c);

	return 0;
}

static void _drain_mmapcache()
{
	struct list_head *lh, *lhprev;
	cluster_t *c;

	/* What we try to do here is walking the LRU sorted
	 * list of mapped clusters from tail to head.
	 * For reach mapped cluster we check if it is just
	 * cached in which case we unmap it and remove it
	 * from the mapped cluster list <- so we have to be
	 * careful about vanishing elements!
	 */
	lhprev = swap->mapped.prev;
	while (lh = lhprev, lhprev = lh->prev, lh != &swap->mapped) {
		c = list_entry(lh, cluster_t, map_list);
		if (!cluster_is_mapped(c))
			DERROR("cluster with no mapping in maplist!");
		_cluster_try_munmap(c);
		if (swap->mapped_size <= swap->mapped_max)
			break;
	}
}


/* pos is relative to c, has to be CLUSTER_MINSIZE aligned!,
 * c->buf[pos] is in tail cluster. */
static cluster_t *_cluster_split_aligned(cluster_t *c, int pos)
{
	cluster_t *tail;

	if (pos & CLUSTER_MINMASK)
		DERROR("split of cluster at unaligned position!");
	if (pos == 0 || pos == c->size)
		DERROR("split of cluster at unnecessary position! check caller!");
	if (pos < 0 || pos > c->size)
		DERROR("split of cluster at weird position!");
	if (_cluster_try_munmap(c) == -1)
		DERROR("split of mapped (used!) cluster");

	if (!(tail = __cluster_alloc(-1)))
		return NULL;
	tail->off = c->off + pos;
	tail->size = c->size - pos;

	c->size = pos;

	_cluster_add(tail, c->c_list.next);

	return tail;
}

/* Size is a hint only (i.e. usually returned clustersize is less)! 
 * Hint is a hint for the position of the cluster in the swapfile, i.e.
 * search for a cluster is started just after hint. If hint is NULL,
 * search is started at the beginning of the swapfile.
 * We basically walk the cluster list and search for the biggest cluster */
cluster_t *_cluster_alloc(soff_t size, cluster_t *hint)
{
	cluster_t *best, *c;
	struct list_head *le, *hintle;
	soff_t aligned_size;

	if (!hint)
		hintle = &swap->map;
	else
		hintle = &hint->c_list;

	best = NULL;
	le = hintle;
	while (le = le->next, le != hintle) {
		if (le == &swap->map)
			continue;
		c = list_entry(le, cluster_t, c_list);
		if (ATOMIC_VAL(c->refcnt) > 0)  /* FIXME - race */
			continue;
		if (!best
		    || ((best->size < size) && (c->size > best->size))
		    || ((best->size > size) && (c->size >= size) && (c->size < best->size)))
			best = c;
	}

	if (!best)
		return NULL;

	/* if we have got a too big one, we split it */
	aligned_size = (size + CLUSTER_MINSIZE) & ~CLUSTER_MINMASK;
	if (best->size > aligned_size) {
		if (!(c = _cluster_split_aligned(best, aligned_size)))
			/* umm, we just ignore it and waste memory */;
	}

	return best;
}

char *_cluster_mmap(cluster_t *cluster, int zero, int prot)
{
	if (cluster_is_mapped(cluster)) {
		/* LRU */
		list_del(&cluster->map_list);
		list_add(&cluster->map_list, &swap->mapped);
	} else if (zero) {
		if (__cluster_zeromap(cluster, prot) == -1)
			return NULL;
	} else {
		if (__cluster_mmap(cluster, prot) == -1)
			return NULL;
	}

	cluster_ref(cluster);
	atomic_inc(&cluster->mmapcnt);

	if (swap->mapped_size > swap->mapped_max)
		_drain_mmapcache();

	return cluster->buf;
}

/* We do late unmapping, i.e. this is just a reference
 * count drop.
 * The real unmapping is done by _drain_mmapcache() if the amount
 * of mapped memory is too high, of by cluster_unref() if we
 * released the last reference to the cluster.
 * As a hint to the OS we will mark the pages of the cluster as
 * probably not needed if the mmapcnt goes to zero.
 */
void _cluster_munmap(cluster_t *cluster)
{
#if (defined HAVE_MADVISE) && (defined SWAPFILE_MADV_MUNMAP)
	if (atomic_dec_and_test(&cluster->mmapcnt))
		madvise(cluster->buf, cluster->size, SWAPFILE_MADV_MUNMAP);
#else
	atomic_dec(&cluster->mmapcnt);
#endif
	cluster_unref(cluster);

	if (swap->mapped_size > swap->mapped_max)
		_drain_mmapcache();
}




/********************************************************
 * file helpers
 * the fc helpers do reference counting for the clusters
 */

static void __fcpopulate(filecluster_t *fc, cluster_t *c, int coff)
{
	fc->cluster = c;
	fc->coff = coff;
	if (c) {
		if (fc->size > c->size - coff)
			DERROR("fc populated with too small cluster");
		cluster_ref(c);
		list_add(&fc->rfc_list, &fc->cluster->rfc_list);
	}
}
static inline void _fcpopulate(filecluster_t *fc, cluster_t *c, int coff)
{
	__fcpopulate(fc, c, coff);
	_filecluster_check(fc);
}

/* splits filecluster at position pos relative to filecluster start!
 * populates resulting tail-filecluster with provided tail-cluster.
 * cannot fail by definition.
 * returns the filecluster containing pos.
 */
static filecluster_t *__filecluster_split(filecluster_t *fc, int pos, cluster_t *tail)
{
	filecluster_t *fctail = NULL;
	int coff = 0;

	if (pos == 0 || pos == fc->size)
		DERROR("Check caller - possible value, but unnecessary!");
	if (pos < 0 || pos > fc->size)
		DERROR("Uh split of filecluster at weird size!");

	if (fc->cluster && fc->cluster == tail)
		coff = fc->coff + pos;

	if (!(fctail = __filecluster_alloc()))
		PANIC("no memory for fc");
	fctail->off = fc->off + pos;
	fctail->size = fc->size - pos;
	_fcpopulate(fctail, tail, coff);

	fc->size = pos;

	_filecluster_add(fc->f, fctail, fc->fc_list.next);
	_filecluster_check(fc);

	return fctail;
}

/* pos is relative to fc start, fc is split in 2 to 3 fcs,
 * returned is the pointer to the fc which contains pos,
 * or NULL on error (no memory usually) */
static filecluster_t *_filecluster_split(filecluster_t *fc, int pos)
{
	cluster_t *tail;
	filecluster_t *fc2;
	struct list_head *lh;
	int aligned_pos;

	if (pos < 0 || pos > fc->size)
		DERROR("filecluster split at weird size");

	if (pos == 0)
		return fc;
	if (pos == fc->size)
		return filecluster_next(fc);

	/* split of a hole is easy */
	if (!fc->cluster) {
		return __filecluster_split(fc, pos, NULL);
	}

	aligned_pos = (pos + CLUSTER_MINSIZE-1) & ~CLUSTER_MINMASK;

	/* three cases, do the filecluster split for every cluster in
	 * the reverse cluster list of fc->cluster */
	if (aligned_pos == pos) {
		if (!(tail = _cluster_split_aligned(fc->cluster, aligned_pos)))
			return NULL;
		lh = &fc->cluster->rfc_list;
		while (lh = lh->next, lh != &fc->cluster->rfc_list) {
			fc2 = list_entry(lh, filecluster_t, rfc_list);
			if (!(fc2->coff == fc->coff
			      && fc2->size == fc->size))
				continue;
			__filecluster_split(fc2, pos, tail);
		}
	} else if (aligned_pos >= fc->size) {
		lh = &fc->cluster->rfc_list;
		while (lh = lh->next, lh != &fc->cluster->rfc_list) {
			fc2 = list_entry(lh, filecluster_t, rfc_list);
			if (!(fc2->coff == fc->coff
			      && fc2->size == fc->size))
				continue;
			__filecluster_split(fc2, pos, fc->cluster);
		}
	} else {
		if (!(tail = _cluster_split_aligned(fc->cluster, aligned_pos)))
			return NULL;
		lh = &fc->cluster->rfc_list;
		while (lh = lh->next, lh != &fc->cluster->rfc_list) {
			fc2 = list_entry(lh, filecluster_t, rfc_list);
			if (!(fc2->coff == fc->coff
			      && fc2->size == fc->size))
				continue;
			__filecluster_split(fc2, aligned_pos, tail);
			__filecluster_split(fc2, pos, fc->cluster);
		}
	}

	return filecluster_next(fc);
}


static filehead_t *_fnew()
{
	filehead_t *f;

	if (!(f = __filehead_alloc(-1)))
		return NULL;

	_filehead_add(f, NULL);

	return f;
}

static void _ffree(filehead_t *f)
{
	filecluster_t *fc;

	if (!f)
		return;

	list_del(&f->fh_list);

	while (!list_empty(&f->fc_list)) {
		fc = list_entry(f->fc_list.next, filecluster_t, fc_list);
		_filecluster_remove(fc);
		free(fc);
	}

	_log_clear(f, NULL, -1);
	free(f);
}

void _file_fixoff(filehead_t *f, filecluster_t *from)
{
	struct list_head *lh;
	filecluster_t *fc;
	soff_t pos;

	if (!from) {
		pos = 0;
		lh = &f->fc_list;
	} else {
		pos = filecluster_end(from) + 1;
		lh = &from->fc_list;
	}
	while (lh = lh->next, lh != &f->fc_list) {
		fc = list_entry(lh, filecluster_t, fc_list);
		fc->off = pos;
		pos += fc->size;
	}
}

/* inserts all fileclusters from fi into f after filecluster after 
 * or at the head of f if after == NULL. */
void _file_insert(filehead_t *f, filecluster_t *after, filehead_t *fi)
{
	struct list_head *lh, *lhi;
	filecluster_t *fc;

	if (!after)
		lh = &f->fc_list;
	else
		lh = &after->fc_list;

	while (lhi = fi->fc_list.prev, lhi != &fi->fc_list) {
		list_del(lhi);
		fc = list_entry(lhi, filecluster_t, fc_list);
		fc->f = f;
		list_add(lhi, lh);
	}

	_file_fixoff(f, after);
}

filehead_t *_file_copy(filehead_t *f, filecluster_t *from, filecluster_t *to)
{
	struct list_head *lh;
	filehead_t *c;
	filecluster_t *fc, *fcc;

	if (!(c = _fnew()))
		return NULL;

	lh = &from->fc_list;
	do {
		fc = list_entry(lh, filecluster_t, fc_list);
		if (!(fcc = __filecluster_alloc()))
			goto _killit;
		fcc->off = fc->off;
		fcc->size = fc->size;
		_fcpopulate(fcc, fc->cluster, fc->coff);
		_filecluster_add(c, fcc, &c->fc_list);

		lh = lh->next;
	} while (fc != to);

	_file_fixoff(c, NULL);

	return c;

_killit:
	_ffree(c);
	return NULL;
}

/* Moves all file clusters [from...to] to the file c (or a new file
 * if NULL) and returns c or the new file.
 * The returned file is not ready for further use, but if the given
 * filehead c was not NULL, without a fixoff call!
 */
filehead_t *_file_delete(filehead_t *f, filecluster_t *from, filecluster_t *to, filehead_t *c)
{
	struct list_head *lh;
	filecluster_t *fc;
	int fix = 0;

	if (c)
		fix = 1;
	else if (!(c = _fnew()))
		return NULL;

	lh = &from->fc_list;
	from = fc_prev(from); /* for fixoff */
	do {
		fc = list_entry(lh, filecluster_t, fc_list);
		lh = lh->next;

		list_del(&fc->fc_list);
		fc->f = c;
		list_add_tail(&fc->fc_list, &c->fc_list);
	} while (fc != to);

	_file_fixoff(f, from);
	if (fix)
		_file_fixoff(c, NULL);

	return c;
}


/*********************************************************************
 * log helpers
 */

static logentry_t *_lenew(filehead_t *f, int op)
{
	logentry_t *l;

	if (!(l = __logentry_alloc(-1)))
		return NULL;
	l->f = f;
	l->op = op;

	return l;
}



/*************************************************************
 * swap file helper
 */

static int _swap_open_blkdev(swap_t *s, int flags)
{
	struct stat sbuf;

#if 0   /* flock is broken on solaris */
      	if (flock(s->fd, LOCK_EX|LOCK_NB) == -1)
		goto _nolock;
#endif
	if (fstat(s->fd, &sbuf) == -1)
		goto _nostat;
	s->size = sbuf.st_blocks*512;

	return 0;

_nostat:
#if 0
	flock(s->fd, LOCK_SH);
_nolock:
#endif
	return -1;
}

static int _swap_open_file(swap_t *s, int flags)
{
	struct stat sbuf;

#if 0 /* flock is broken on solaris */
       	if (flock(s->fd, LOCK_EX|LOCK_NB) == -1)
		goto _nolock;
#endif
	if (fstat(s->fd, &sbuf) == -1)
		goto _nostat;
	s->size = sbuf.st_size;

	return 0;

_nostat:
#if 0
      	flock(s->fd, LOCK_SH);
_nolock:
#endif
	return -1;
}

static int _init_swap(swap_t *s)
{
	swapd_header_t header;

	if (lseek(s->fd, 0, SEEK_SET) == -1)
		return -1;
	if (read(s->fd, &header, sizeof(header)) != sizeof(header))
		return -1;

	if (memcmp(SWAP_MAGIC, header.magic, 16) != 0)
		return -1;
	s->data_off = header.u.header.data_off;
	s->data_size = header.u.header.data_size;
	s->meta_off = header.u.header.meta_off;
	s->meta_size = header.u.header.meta_size;

	if (s->data_off + s->data_size > s->size
	    || s->meta_off + s->meta_size > s->size)
		return -1;

	return 0;
}

static int _build_swap_mem(swap_t *s, swapd_record_t *records)
{
	swapd_record_t *r;

	/* build cluster list */
	r = records;
	do {
		if (memcmp(RECORD_MAGIC, r->magic, 4) != 0)
			return -1;
		if (r->type == RECORD_TYPE_EOF)
			break;
		if (r->type != RECORD_TYPE_CLUSTER)
			continue;
		if (_cluster_read(r) == -1)
			return -1;
	} while (r++, 1);

	/* build filehead list - no magic checking needed anymore */
	r = records;
	do {
		if (r->type == RECORD_TYPE_EOF)
			break;
		if (r->type != RECORD_TYPE_FILEHEAD)
			continue;
		if (_filehead_read(r) == -1)
			return -1;
	} while (r++, 1);

	/* build filecluster lists */
	r = records;
	do {
		if (r->type == RECORD_TYPE_EOF)
			break;
		if (r->type != RECORD_TYPE_FILECLUSTER)
			continue;
		if (_filecluster_read(r) == -1)
			return -1;
	} while (r++, 1);

	/* build logentry list */
	r = records;
	do {
		if (r->type == RECORD_TYPE_EOF)
			break;
		if (r->type != RECORD_TYPE_LOGENTRY)
			continue;
		if (_logentry_read(r) == -1)
			return -1;
	} while (r++, 1);

	return 0;
}

static int _build_swap_disk(swap_t *s, swapd_record_t *r)
{
	cluster_t *c;
	filehead_t *fh;
	filecluster_t *fc;
	logentry_t *le;
	struct list_head *lh, *lh2;

	/* write clusters to disk */
	lh = &s->map;
	while (lh = lh->next, lh != &s->map) {
		c = list_entry(lh, cluster_t, c_list);
		_cluster_write(r++, c);
	}

	/* write fileheads and fileclusters to disk */
	lh = &s->files;
	while (lh = lh->next, lh != &s->files) {
		fh = list_entry(lh, filehead_t, fh_list);
		_filehead_write(r++, fh);
		lh2 = &fh->fc_list;
		while (lh2 = lh2->next, lh2 != &fh->fc_list) {
			fc = list_entry(lh2, filecluster_t, fc_list);
			_filecluster_check(fc);
			_filecluster_write(r++, fc);
		}
	}

	/* write logentries to disk */
	lh = &s->log;
	while (lh = lh->next, lh != &s->log) {
		le = list_entry(lh, logentry_t, le_list);
		_logentry_write(r++, le);
	}

	/* write EOF record */
	_swapd_record_prepare(r, RECORD_TYPE_EOF);

	return 0;
}



/**********************************************************************
 * public API
 */

int swap_open(char *name, int flags)
{
	swapd_record_t *r;
	struct stat sbuf;

	if (swap || !name)
		return -1;

	hash_alloc_swapfile();

	if (!(swap = ALLOC(swap_t))) {
		errno = ENOMEM;
		goto _nomem;
	}
	swap->cid = 0;
	swap->lid = 0;
	swap->fid = 0;
	INIT_LIST_HEAD(&swap->map);
	INIT_LIST_HEAD(&swap->log);
	INIT_LIST_HEAD(&swap->files);
	swap->mapped_size = 0;
	swap->mapped_max = CLUSTER_MAXSIZE<<4;
	INIT_LIST_HEAD(&swap->mapped);

	if ((swap->semid = glame_semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) == -1)
		goto _nosem;
	swap->semnum = 0;
	{
		union semun sun;
		sun.val = 0;
		if (glame_semctl(swap->semid, swap->semnum, SETVAL, sun) == -1)
			return -1;
	}

	if ((swap->fd = open(name, O_RDWR)) == -1)
		goto _nofd;
	if (fstat(swap->fd, &sbuf) == -1)
		goto _nostat;
	if (S_ISREG(sbuf.st_mode)) {
		if (_swap_open_file(swap, flags) == -1)
			goto _err;
	} else if (S_ISBLK(sbuf.st_mode)) {
		if (_swap_open_blkdev(swap, flags) == -1)
			goto _err;
	} else {
		goto _err;
	}


	if (_init_swap(swap) == -1)
		goto _err;

	r = mmap(NULL, swap->meta_size, PROT_READ, MAP_PRIVATE,
		 swap->fd, swap->meta_off);
	if (r == MAP_FAILED)
		goto _err;
	if (_build_swap_mem(swap, r) == -1)
		goto _err;
	munmap(r, swap->meta_size);
	
	_swap_unlock();
  
	return 0;


_err:
_nostat:
	close(swap->fd);
_nofd:
	{
		union semun sun;
		glame_semctl(swap->semid, 0, IPC_RMID, sun);
	}
_nosem:
	free(swap);
	swap = NULL;
_nomem:
	return -1;
}


void swap_close()
{
	swapd_record_t *r;

	if (!swap)
		return;

	_swap_lock();

	r = mmap(NULL, swap->meta_size, PROT_READ|PROT_WRITE, MAP_SHARED,
		 swap->fd, swap->meta_off);
	if (r == MAP_FAILED)
		return;
	_build_swap_disk(swap, r);
	munmap(r, swap->meta_size);
	fsync(swap->fd);

#if 0 /* flock is broken on solaris */
      	flock(swap->fd, LOCK_SH);
#endif
	close(swap->fd);

	{
		union semun sun;
		glame_semctl(swap->semid, 0, IPC_RMID, sun);
	}

	/* aieee! FIXME! we should free the lists in mem?? (sure!) */
	/* and we should unmap all mappings... FIXME! */
	free(swap);
	swap = NULL;
}


/* ok, we do lazy allocation on mmap :)
 * the bottom side is that we do not check against overcommited memory :( */
fileid_t file_alloc(soff_t size)
{
	filehead_t *f;
	filecluster_t *fc;

	_swap_lock();

	if (!(f = _fnew()))
		goto _err;

	/* zero-sized files are allowed, but have no filecluster yet */
	if (size == 0)
		goto _out;

	if (!(fc = __filecluster_alloc()))
		goto _freef;
	fc->off = 0;
	fc->size = size;

	_filecluster_add(f, fc, &f->fc_list);

 _out:
	/* finally hash the file */
	hash_add_file(f);

	_swap_unlock();
	return f->fid;


_freef:
	errno = ENOMEM;
	_ffree(f);
_err:
	_swap_unlock();
	return -1;
}

fileid_t file_copy(fileid_t fid, soff_t pos, soff_t size)
{
	filecluster_t *fcstart, *fcend;
	filehead_t *f, *c;

	_swap_lock();

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _err;
	}
	if (!(fcstart = _filecluster_findbyoff(f, pos, NULL))
	    || !(fcend = _filecluster_findbyoff(f, pos+size-1, fcstart))) {
		errno = EINVAL;
		goto _err;
	}

	/* split clusters if necessary */
	if (!(fcstart = _filecluster_split(fcstart, pos - fcstart->off)))
		goto _errnomem;
	fcend = _filecluster_findbyoff(f, pos+size-1, fcstart);
	_filecluster_split(fcend, pos+size - fcend->off);

	if (!(c = _file_copy(f, fcstart, fcend)))
		goto _errnomem;

	hash_add_file(c);
	_swap_unlock();
	return c->fid;

 _errnomem:
	errno = ENOMEM;
 _err:
	_swap_unlock();
	return -1;
}

void file_unref(fileid_t fid)
{
	filehead_t *f;

	_swap_lock();

	if (!(f = hash_find_file(fid)))
		goto _out;

	hash_remove_file(f);
	_ffree(f);

 _out:
	_swap_unlock();
}

soff_t file_size(fileid_t fid)
{
	filehead_t *f;
	soff_t size = -1;

	_swap_lock();

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _out;
	}

	size = _filehead_size(f);

 _out:
	_swap_unlock();
	return size;
}

int file_truncate(fileid_t fid, soff_t size)
{
	filehead_t *f;
	filecluster_t *fc, *nfc;
	soff_t oldsize;

	_swap_lock();

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _err;
	}

	if (f->usecnt != 0) {
		errno = EINVAL;
		goto _err;
	}

	oldsize = _filehead_size(f);
	if (oldsize == size)
		goto _out;

	/* an "append" is easy, we update oldsize as we are adding space
	 * to the file. */
	if (size > oldsize) {
		/* get the last filecluster of the file to start expanding */
		fc = list_gettail(&f->fc_list, filecluster_t, fc_list);

		/* first fill up the last filecluster, if its non-virtual */
		if (fc && fc->cluster) {
			/* we may not do anything with not private clusters.
			 * you may think we need not to zero the extra space, but
			 * we do have to do it! think of a splitted dual used
			 * cluster of which one part got freed...
			 */
			if (ATOMIC_VAL(fc->cluster->refcnt) == 1
			    && fc->size != fc->cluster->size) {
				/* map the cluster - its a non-ref, just in
				 * cache map */
				if (!cluster_is_mapped(fc->cluster))
					__cluster_mmap(fc->cluster, PROT_READ|PROT_WRITE);
				/* we are the exclusive user of the cluster, so
				 * we may just move our part to the front. */
				if (fc->coff > 0)
					memmove(fc->cluster->buf, fc->cluster->buf+fc->coff, fc->size);
				fc->coff = 0;
				/* we're set, if all needed space was there */
				if (oldsize + fc->cluster->size - fc->size >= size) {
					memset(fc->cluster->buf+fc->size, 0, size - oldsize);
					fc->size += size - oldsize;
					goto _out;
				}
				memset(fc->cluster->buf+fc->size, 0, fc->cluster->size-fc->size);
				oldsize += fc->cluster->size - fc->size;
				fc->size = fc->cluster->size;
			}
		}
		/* if the last filecluster is a virtual one, we can just expand
		 * it, else we need to alloc one. */
		if (!fc || fc->cluster) {
			nfc = __filecluster_alloc();
			nfc->off = fc ? filecluster_end(fc)+1 : 0;
			nfc->size = 0;
			nfc->coff = 0;
			_filecluster_add(f, nfc, NULL);
			fc = nfc;
		}
		/* now, fc is guaranteed to be a virtual filecluster,
		 * just adjust the size of it. */
		fc->size += size - oldsize;


	/* truncate file */
	} else {
		if (!(fc = _filecluster_findbyoff(f, size, NULL)))
			DERROR("Uh! Corrupted filecluster list and/or offsets!");
		/* we have to be careful to not have any not discardable
		 * mappings of a to be truncated filecluster. so we do
		 * two walks through the to be truncated fileclusters.
		 */
		nfc = fc;  /* remember start of walk */
		do {
			if (fc->cluster
			    && _cluster_try_munmap(fc->cluster) == -1) {
				errno = EAGAIN;
				goto _err;
			}
			fc = filecluster_next(fc);
		} while (fc);
		/* do the actual work in the second walk */
		fc = nfc;
		/* kill part of the first filecluster, if necessary */
		if (fc->off < size) {
			/* FIXME! we could save cluster memory space by
			 * conditionally splitting a non-virtual cluster!
			 */
			fc->size -= oldsize - size;
			fc = filecluster_next(fc);
		}
		while (fc) {
			/* throw away filecluster, watch out! list modification! */
			nfc = filecluster_next(fc);
			_filecluster_remove(fc);
			free(fc);
			fc = nfc;
		}
	}

	/* DEBUG */
	if ((fc = list_gethead(&f->fc_list, filecluster_t, fc_list)))
		do {
			_filecluster_check(fc);
		} while ((fc = fc_next(fc)));

 _out:
	_swap_unlock();
	return 0;

 _err:
	_swap_unlock();
	return -1;
}


fileid_t file_next(fileid_t fid)
{
	filehead_t *fh;

	_swap_lock();

	if (fid == -1) {
		if (!(fh = list_gethead(&swap->files, filehead_t, fh_list)))
			goto _err;
		while (!is_hashed_file(fh)) {
			if (fh->fh_list.next == &swap->files)
				goto _err;
			fh = list_entry(fh->fh_list.next, filehead_t, fh_list);
		}
	} else {
		if (!(fh = hash_find_file(fid)))
			goto _err;
		do {
			if (fh->fh_list.next == &swap->files)
				goto _err;
			fh = list_entry(fh->fh_list.next, filehead_t, fh_list);
		} while (!is_hashed_file(fh));
	}

	_swap_unlock();
	return fh->fid;

 _err:
	_swap_unlock();
	return -1;
}


int file_op_insert(fileid_t fid, soff_t pos, fileid_t file)
{
	filehead_t *fd, *fs;
	logentry_t *l;
	filecluster_t *at;

	_swap_lock();
	
	if (!(fd = hash_find_file(fid))
	    || !(fs = hash_find_file(file))) {
		errno = ENOENT;
		goto _err;
	}
	if (pos == _filehead_size(fd))
		at = NULL;
	else if (!(at = _filecluster_findbyoff(fd, pos, NULL))) {
		errno = EINVAL;
		goto _err;
	}
	if (!(l = _lenew(fd, LOGENTRY_INSERT)))
		goto _errnomem;
	if (at && (at = _filecluster_split(at, pos - at->off)) == NULL)
		goto _freele;

	/* build logentry */
	l->u.insert.pos = pos;
	l->u.insert.size = _filehead_size(fs);
	l->u.insert.f = fs;

	/* do the operation */
	if (at)
		at = fc_prev(at);
	else
		at = fclist_tail(fd);
	_file_insert(fd, at, fs);

	/* finish it */
	file_use(fs);
	_logentry_add(l, swap->log.next);

	_swap_unlock();
	return 0;


_freele:
	free(l);
_errnomem:
	errno = ENOMEM;
_err:
	_swap_unlock();
	return -1;
}

int file_op_cut(fileid_t fid, soff_t pos, soff_t size)
{
	filehead_t *f;
	logentry_t *l;
	filecluster_t *fcstart, *fcend;

	_swap_lock();

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _err;
	}
	if (!(fcstart = _filecluster_findbyoff(f, pos, NULL))
	    || !(fcend = _filecluster_findbyoff(f, pos+size-1, fcstart))) {
		errno = EINVAL;
		goto _err;
	}

	/* split clusters if necessary */
	if (!(fcstart = _filecluster_split(fcstart, pos - fcstart->off)))
		goto _errnomem;
	fcend = _filecluster_findbyoff(f, pos+size-1, fcstart);
	_filecluster_split(fcend, pos+size - fcend->off);

	/* first set up the logentry */
	if (!(l = _lenew(f, LOGENTRY_DELETE)))
		goto _errnomem;
	l->u.delete.pos = pos;
	l->u.delete.size = size;
	if (!(l->u.delete.f = _file_delete(f, fcstart, fcend, NULL)))
		goto _freele;

	_logentry_add(l, swap->log.next);

	_swap_unlock();
	return 0;

_freele:
	free(l);
_errnomem:
	errno = ENOMEM;
_err:
	_swap_unlock();
	return -1;
}

int file_transaction_begin(fileid_t fid)
{
	filehead_t *f;
	logentry_t *l;
	int res = -1;

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _out;
	}

	res = 0;
	if (f->begincnt++)
		goto _out;

	res = -1;
	if (!(l = _lenew(f, LOGENTRY_BEGIN))) {
		errno = ENOMEM;
		goto _out;
	}
	res = 0;
	_logentry_add(l, swap->log.next);

 _out:
	_swap_unlock();
	return res;
}

int file_transaction_end(fileid_t fid)
{
	filehead_t *f;
	logentry_t *l;
	int res = -1;

	_swap_lock();

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _out;
	}
	if (!f->begincnt) {
		errno = EINVAL;
		goto _out;
	}

	res = 0;
	if (--(f->begincnt))
		goto _out;

	res = -1;
	if (!(l = _lenew(f, LOGENTRY_END))) {
		errno = ENOMEM;
		goto _out;
	}
	res = 0;
	_logentry_add(l, swap->log.next);

 _out:
	_swap_unlock();
	return res;
}

int file_transaction_undo(fileid_t fid)
{
	filehead_t *f;
	filecluster_t *fcstart, *fcend;
	logentry_t *l;
	int res = -1;

	_swap_lock();

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _out;
	}
	if (f->begincnt) {
		errno = EINVAL;
		goto _out;
	}

	res = 0;

	/* We need the end-entry of the transaction before the one
	 * with the id in f->logpos. For f->logpos == -1 this is
	 * just the latest available record, for other f->logpos
	 * we need the previous from the one identified with
	 * f->logpos */
	if (!(l = _logentry_findbyid(f, f->logpos))) {
		errno = EINVAL;
		goto _out;
	}
	if (f->logpos != -1)
		if (!(l = _logentry_prev(f, l))) {
			errno = EINVAL;
			goto _out;
		}
	/* we now should have the end record of the to be undone
	 * transaction. */
	if (l->op != LOGENTRY_END)
		DERROR("uh, log messed up!?");

	/* reverse actions */
	do {
		l = _logentry_prev(f, l);
		switch (l->op) {
		case LOGENTRY_INSERT:
			fcstart = _filecluster_findbyoff(f, l->u.insert.pos, NULL);
			fcend = _filecluster_findbyoff(f, l->u.insert.pos + l->u.insert.size - 1,
						       fcstart);
			if (!fcstart || fcstart->off != l->u.insert.pos
			    || !fcend || filecluster_end(fcend) != l->u.insert.pos+l->u.insert.size-1)
				DERROR("uh!? what happened?");
			_file_delete(f, fcstart, fcend, l->u.insert.f);
			/* FIXME!! for redo we may not rehash the file!
			 * but undo means rehashing it... uh!
			 * We need some dependency like "if you use this file,
			 * the logentries of the other files are gone"... UH! */
			file_unuse(l->u.insert.f);
			break;
		case LOGENTRY_DELETE:
			fcstart = _filecluster_findbyoff(f, l->u.delete.pos - 1, NULL);
			if ((!fcstart && l->u.delete.pos != 0)
			    || (fcstart && filecluster_end(fcstart) != l->u.delete.pos-1))
				DERROR("Uh!? what happened?");
			_file_insert(f, fcstart, l->u.delete.f);
			_ffree(l->u.delete.f);
			l->u.delete.f = NULL;
			break;
		default:
		}
	} while (l->op != LOGENTRY_BEGIN);

	/* remember the begin-id */
	f->logpos = l->lid;

 _out:
	_swap_unlock();
	return res;
}

int file_transaction_redo(fileid_t fid)
{
	filehead_t *f;
	filecluster_t *fcstart, *fcend;
	logentry_t *l;
	int res = -1;

	_swap_lock();

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _out;
	}
	res = 0;
	if (f->logpos == -1)
		goto _out;

	/* lookup the remembered begin entry (sanity-check can be removed) */
	if (!(l = _logentry_findbyid(f, f->logpos))
	    || l->op != LOGENTRY_BEGIN)
		DERROR("uh, log messed up!?");

	/* redo actions */
	do {
		l = _logentry_next(f, l);
		switch (l->op) {
		case LOGENTRY_INSERT:
			fcstart = _filecluster_findbyoff(f, l->u.insert.pos - 1, NULL);
			if ((!fcstart && l->u.insert.pos != 0)
			    || (fcstart && filecluster_end(fcstart) != l->u.insert.pos - 1))
				DERROR("uh? what happened??");
			_file_insert(f, fcstart, l->u.insert.f);
			file_use(l->u.insert.f);
			break;
		case LOGENTRY_DELETE:
			fcstart = _filecluster_findbyoff(f, l->u.delete.pos, NULL);
			fcend = _filecluster_findbyoff(f, l->u.delete.pos + l->u.delete.size - 1,
						       fcstart);
			if (!fcstart || fcstart->off != l->u.delete.pos
			    || !fcend || filecluster_end(fcend) != l->u.delete.pos + l->u.delete.size-1)
				DERROR("uh? what happened??");
			l->u.delete.f = _file_delete(f, fcstart, fcend, NULL);
			break;
		default:
		}
	} while (l->op != LOGENTRY_END);

	/* remember the next begin-id */
	l = _logentry_next(f, l);
	if (l)
		f->logpos = l->lid;
	else
		f->logpos = -1;

 _out:
	_swap_unlock();
	return res;
}

filecluster_t *filecluster_get(fileid_t fid, soff_t pos)
{
	filecluster_t *fc = NULL;
	filehead_t *f;

	_swap_lock();

	if (!(f = hash_find_file(fid))) {
		errno = ENOENT;
		goto _out;
	}
	fc = _filecluster_findbyoff(f, pos, NULL);

 _out:
	_swap_unlock();
	return fc;
}

char *filecluster_mmap(filecluster_t *fc)
{
	cluster_t *c;
	filecluster_t *pfc;
	int zero = 0;
	char *mem = NULL;

	_swap_lock();

	if (!is_hashed_file(fc->f)) {
		errno = EINVAL;
		goto _out;
	}

	/* do late allocation of cluster */
	if (!fc->cluster) {
		pfc = fc_prev(fc);
		if (!(c = _cluster_alloc(fc->size, pfc ? pfc->cluster : NULL))) {
			errno = ENOMEM;
			goto _out;
		}
		if (c->size < fc->size)
			_filecluster_split(fc, c->size);
		_fcpopulate(fc, c, 0);
		zero = 1;
	}

	mem = _cluster_mmap(fc->cluster, zero,
			    fc->f->usecnt == 0 ? PROT_READ|PROT_WRITE : PROT_READ);
	mem += fc->coff;

 _out:
	_swap_unlock();
	return mem;
}

void filecluster_munmap(filecluster_t *fc)
{
	_swap_lock();
	_cluster_munmap(fc->cluster);
	_swap_unlock();
}
