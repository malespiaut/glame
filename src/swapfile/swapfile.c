/*
 * swapfile.c
 * $Id: swapfile.c,v 1.2 2000/01/24 10:21:54 richi Exp $
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
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "swapfile.h"
#include "util.h"


#if defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)
/* union semun is defined by including <sys/sem.h> */
#else
#if !defined(_NO_XOPEN4)
/* according to X/OPEN we have to define it ourselves */
union semun {
	int val;                    /* value for SETVAL */
	struct semid_ds *buf;       /* buffer for IPC_STAT, IPC_SET */
	unsigned short int *array;  /* array for GETALL, SETALL */
	struct seminfo *__buf;      /* buffer for IPC_INFO */
};
#endif
#endif


/* TODO:
 * - fix fsplit
 * - preserve "holes" on delete and insert! (partly holes, too!)
 *
 * - optimized cluster allocation
 * - optimized cluster deallocation (merge with free previous and
 *   next cluster)
 */


typedef struct {
	int fd;           /* file descriptor */
	off_t size;       /* full size */

	off_t data_off;   /* offsett for raw data */
	off_t data_size;  /* size for raw data */

	off_t meta_off;   /* offsett for meta data */
	off_t meta_size;  /* size for meta data */

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
static void _log_clear(filehead_t *f, logentry_t *start, int direction);
static logentry_t *_logentry_findbyid(filehead_t *f, int id);
static void _ffree(filehead_t *f);
static void _fcpopulate(filecluster_t *fc, cluster_t *c, off_t coff);



/************************************************************
 * locking helpers
 */
static inline void _swap_lock()
{
	struct sembuf sop;

	sop.sem_num = swap->semnum;
	sop.sem_op = -1;
	sop.sem_flg = 0;
	while (semop(swap->semid, &sop, 1) == -1 && errno == EINTR)
		;
}

static inline void _swap_unlock()
{
	struct sembuf sop;

	sop.sem_num = swap->semnum;
	sop.sem_op = 1;
	sop.sem_flg = IPC_NOWAIT;
	semop(swap->semid, &sop, 1);
}


/************************************************************
 * mmap cache helpers
 */
static int _cluster_is_mapped(cluster_t *c)
{
	if (!c->buf)
		return 0;
	if (c->mmapcnt>0)
		return 1;

	list_del(&c->map_list);
	munmap(c->buf, c->size);
	c->buf = NULL;
	swap->mapped_size -= c->size;

	return 0;
}

static void _drain_mmapcache()
{
	struct list_head *lh, *lhnext;
	cluster_t *c;

	lhnext = &swap->mapped;
	while (lh = lhnext->prev, lh != &swap->mapped) {
		c = list_entry(lh, cluster_t, map_list);
		if (_cluster_is_mapped(c))
			lhnext = lh;
		else if (swap->mapped_size <= swap->mapped_max)
			break;
	}
}


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

	INIT_LIST_HEAD(&c->c_list);
	INIT_LIST_HEAD(&c->rfc_list);
	INIT_LIST_HEAD(&c->map_list);

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

static void _filecluster_add(filecluster_t *fc, struct list_head *hint)
{
	filecluster_t *fc2;

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

static void _filecluster_remove(filecluster_t *fc)
{
	list_del(&fc->rfc_list);
	list_del(&fc->fc_list);
	if (fc->cluster)
		cluster_unref(fc->cluster);
}

static void _logentry_remove(logentry_t *le)
{
	list_del(&le->le_list);

	switch (le->op) {
	case LOGENTRY_INSERT:
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

static off_t _filehead_size(filehead_t *f)
{
	filecluster_t *first, *last;

	if (list_empty(&f->fc_list))
		return 0;

	first = list_entry(f->fc_list.next, filecluster_t, fc_list);
	last = list_entry(f->fc_list.prev, filecluster_t, fc_list);

	return last->off + last->size - first->off;
}

static filecluster_t *_filecluster_findbyoff(filehead_t *f, off_t off)
{
	struct list_head *lh;
	filecluster_t *fc;

	lh = &f->fc_list;
	while (lh = lh->next, lh != &f->fc_list) {
		fc = list_entry(lh, filecluster_t, fc_list);
		if (filecluster_start(fc) > off)
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
	struct list_head *lh;
	logentry_t *l;

	if (start)
		lh = direction == -1 ? start->le_list.prev : start->le_list.next;
	else
		lh = &swap->log;
	while (lh = direction == -1 ? lh->next : lh->prev, lh != &swap->log) {
		l = list_entry(lh, logentry_t, le_list);
		if (l->f == f) {
			lh = direction == -1 ? lh->prev : lh->next;
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
	c->refcnt = r->u.cluster.refcnt;
	c->mmapcnt = 0;
	c->buf = NULL;

	_cluster_add(c, NULL);

	return 0;
}

static void _cluster_write(swapd_record_t *r, cluster_t *c)
{
	_swapd_record_prepare(r, RECORD_TYPE_CLUSTER);

	r->u.cluster.off = c->off;
	r->u.cluster.size = c->size;
	r->u.cluster.refcnt = c->refcnt;
	r->u.cluster.id = c->id;
}

static int _filehead_read(swapd_record_t *r)
{
	filehead_t *fh;

	if (!(fh = __filehead_alloc(r->u.filehead.fid)))
		return -1;
	fh->begincnt = r->u.filehead.begincnt;
	fh->logpos = r->u.filehead.logpos;

	_filehead_add(fh, NULL);

	return 0;
}

static void _filehead_write(swapd_record_t *r, filehead_t *fh)
{
	_swapd_record_prepare(r, RECORD_TYPE_FILEHEAD);

	r->u.filehead.fid = fh->fid;
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
	if (!(fh = fc->f = _filehead_findbyid(r->u.filecluster.fid))) {
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

	_fcpopulate(fc, c, r->u.filecluster.coff);
	_filecluster_add(fc, NULL);

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
		r->u.logentry.u.delete.fid = le->u.delete.f->fid;
		break;
	default:
	}
}



/************************************************************
 * cluster helper - they do _no_ reference counting!
 * (apart from mmap/munmap)
 */

#define ZEROMAP_READ
void _zeromap(cluster_t *c)
{
	int fd;

#if defined ZEROMAP_READ
	fd = open("/dev/zero", O_RDONLY);
	read(fd, c->buf, c->size);
	close(fd);
#else
	memset(c->buf, 0, c->size);
#endif
}


/* pos is relative to c, has to be CLUSTER_MINSIZE aligned!,
 * c->buf[pos] is in tail cluster. */
static cluster_t *_cluster_split_aligned(cluster_t *c, off_t pos)
{
	cluster_t *tail;

	if (pos & CLUSTER_MINMASK)
		PANIC("split of cluster at unaligned position!");
	if (pos == 0 || pos == c->size)
		PANIC("split of cluster at weird position!");
	if (_cluster_is_mapped(c))
		PANIC("split of mapped (used!) cluster");

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
cluster_t *_cluster_alloc(off_t size, cluster_t *hint)
{
	cluster_t *best, *c;
	struct list_head *le, *hintle;
	off_t aligned_size;

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
		if (c->refcnt > 0)
			continue;
		if (!best
		    || ((best->size < size) && (c->size > best->size))
		    || ((best->size > size) && (c->size >= size) && (c->size < best->size)))
			best = c;
	}

	if (!best)
		return NULL;

	/* if we have got a too big one, we split it */
	aligned_size = (size + CLUSTER_MINMASK) & ~CLUSTER_MINMASK;
	if (best->size > aligned_size) {
		if (!(c = _cluster_split_aligned(best, aligned_size)))
			/* umm, we just ignore it and waste memory */;
	}

	return best;
}

char *_cluster_mmap(cluster_t *cluster, int zero, int prot)
{
	if (!cluster->mmapcnt) {
		if (!(cluster->buf = mmap(NULL, cluster->size, prot,
					  MAP_SHARED, swap->fd, cluster->off)))
			return NULL;
	} else {
		list_del(&cluster->map_list);
	}
	list_add(&cluster->map_list, &swap->mapped);

	cluster_ref(cluster);
	cluster->mmapcnt++;

	swap->mapped_size += cluster->size;
	if (swap->mapped_size > swap->mapped_max)
		_drain_mmapcache();

	if (zero)
		_zeromap(cluster);

	return cluster->buf;
}

/* we do late unmapping */
void _cluster_munmap(cluster_t *cluster)
{
	cluster_unref(cluster);
	cluster->mmapcnt--;
	if (swap->mapped_size > swap->mapped_max)
		_drain_mmapcache();
}




/********************************************************
 * file helpers
 * the fc helpers do reference counting for the clusters
 */

static void _fcpopulate(filecluster_t *fc, cluster_t *c, off_t coff)
{
	fc->cluster = c;
	fc->coff = coff;
	if (c) {
		cluster_ref(c);
		list_add(&fc->rfc_list, &fc->cluster->rfc_list);
	}
}

static filecluster_t *_fcnew(filehead_t *f, cluster_t *c, off_t coff)
{
	filecluster_t *fc;

	if (!(fc = __filecluster_alloc()))
		return NULL;

	fc->f = f;
	_fcpopulate(fc, c, coff);

	return fc;
}

static void __filecluster_split(filecluster_t *fc, off_t pos, cluster_t *tail)
{
	filecluster_t *fctail = NULL;
	off_t coff = 0;

	if (fc->cluster && fc->cluster == tail)
		coff = fc->coff + pos;

	if (!(fctail = _fcnew(fc->f, tail, coff)))
		PANIC("no memory for fc");
	fctail->off = fc->off + pos;
	fctail->size = fc->size - pos;
	fc->size = pos;

	list_add(&fctail->fc_list, &fc->fc_list);
}

/* pos is relative to fc start, fc is split in 2 to 3 fcs */
static int _filecluster_split(filecluster_t *fc, off_t pos)
{
	cluster_t *tail;
	filecluster_t *fc2;
	struct list_head *lh;
	off_t aligned_pos;

	if (pos < 0 || pos >= fc->size)
		PANIC("filecluster split at weird size");

	if (pos == 0)
		return 0;

	/* split of a hole is easy */
	if (!fc->cluster) {
		__filecluster_split(fc, pos, NULL);
		return 0;
	}

	aligned_pos = (pos + CLUSTER_MINSIZE-1) & ~CLUSTER_MINMASK;

	/* three cases, do the filecluster split for every cluster in
	 * the reverse cluster list of fc->cluster */
	if (aligned_pos == pos) {
		if (!(tail = _cluster_split_aligned(fc->cluster, aligned_pos)))
			return -1;
		lh = &fc->cluster->rfc_list;
		while (lh = lh->next, lh != &fc->cluster->rfc_list) {
			fc2 = list_entry(lh, filecluster_t, rfc_list);
			__filecluster_split(fc2, pos, tail);
		}
	} else if (aligned_pos >= fc->size) {
		lh = &fc->cluster->rfc_list;
		while (lh = lh->next, lh != &fc->cluster->rfc_list) {
			fc2 = list_entry(lh, filecluster_t, rfc_list);
			__filecluster_split(fc2, pos, fc->cluster);
		}
	} else {
		if (!(tail = _cluster_split_aligned(fc->cluster, aligned_pos)))
			return -1;
		lh = &fc->cluster->rfc_list;
		while (lh = lh->next, lh != &fc->cluster->rfc_list) {
			fc2 = list_entry(lh, filecluster_t, rfc_list);
			__filecluster_split(fc2, aligned_pos, tail);
			__filecluster_split(fc2, pos, fc->cluster);
		}
	}

	return 0;
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
	off_t pos;

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

void _file_insert(filehead_t *f, filecluster_t *after, filehead_t *fi)
{
	struct list_head *lh, *lhi;

	if (!after)
		lh = &f->fc_list;
	else
		lh = &after->fc_list;

	while (lhi = fi->fc_list.prev, lhi != &fi->fc_list) {
		list_del(lhi);
		list_add(lhi, lh);
	}
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
		if (!(fcc = _fcnew(c, fc->cluster, fc->coff)))
			goto _killit;
		fcc->off = fc->off;
		fcc->size = fc->size;
		list_add_tail(&fcc->fc_list, &c->fc_list);

		lh = lh->next;
	} while (fc != to);

	return c;

_killit:
	_ffree(c);
	return NULL;
}

filehead_t *_file_delete(filehead_t *f, filecluster_t *from, filecluster_t *to, filehead_t *c)
{
	struct list_head *lh;
	filecluster_t *fc;

	if (!c && !(c = _fnew()))
		return NULL;

	lh = &from->fc_list;
	do {
		fc = list_entry(lh, filecluster_t, fc_list);
		lh = lh->next;

		list_del(&fc->fc_list);
		fc->f = c;
		list_add_tail(&fc->fc_list, &c->fc_list);
	} while (fc != to);

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

	if (flock(s->fd, LOCK_EX|LOCK_NB) == -1)
		goto _nolock;
	if (fstat(s->fd, &sbuf) == -1)
		goto _nostat;
	s->size = sbuf.st_blocks*512;

	return 0;

_nostat:
	flock(s->fd, LOCK_SH);
_nolock:
	return -1;
}

static int _swap_open_file(swap_t *s, int flags)
{
	struct stat sbuf;

	if (flock(s->fd, LOCK_EX|LOCK_NB) == -1)
		goto _nolock;
	if (fstat(s->fd, &sbuf) == -1)
		goto _nostat;
	s->size = sbuf.st_size;

	return 0;

_nostat:
	flock(s->fd, LOCK_SH);
_nolock:
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

	errno = ENOMEM;
	if (!(swap = ALLOC(swap_t)))
		goto _nomem;
	swap->cid = 0;
	swap->lid = 0;
	swap->fid = 0;
	INIT_LIST_HEAD(&swap->map);
	INIT_LIST_HEAD(&swap->log);
	INIT_LIST_HEAD(&swap->files);
	swap->mapped_size = 0;
	swap->mapped_max = CLUSTER_MAXSIZE<<4;
	INIT_LIST_HEAD(&swap->mapped);

	if ((swap->semid = semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) == -1)
		goto _nosem;
	swap->semnum = 0;
	semctl(swap->semid, swap->semnum, SETVAL, 0);

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

	if (!(r = mmap(NULL, swap->meta_size, PROT_READ, MAP_PRIVATE,
		       swap->fd, swap->meta_off)))
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
	semctl(swap->semid, 0, IPC_RMID, 0);
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

	if (!(r = mmap(NULL, swap->meta_size, PROT_READ|PROT_WRITE, MAP_SHARED,
		       swap->fd, swap->meta_off)))
		return;
	_build_swap_disk(swap, r);
	munmap(r, swap->meta_size);
	fsync(swap->fd);

	flock(swap->fd, LOCK_SH);
	close(swap->fd);

	/* aieee! FIXME! we should free the lists in mem?? (sure!) */
	/* and we should unmap all mappings... FIXME! */
	free(swap);
	swap = NULL;
}


/* ok, we do lazy allocation on mmap :)
 * the bottom side is that we do not check against overcommited memory :( */
fileid_t file_alloc(off_t size)
{
	filehead_t *f;
	filecluster_t *fc;

	_swap_lock();

	errno = ENOMEM;
	if (!(f = _fnew()))
		goto _err;

	/* zero-sized files are allowed, but have no filecluster yet */
	if (size == 0)
		goto _out;

	if (!(fc = _fcnew(f, NULL, 0)))
		goto _freef;
	fc->off = 0;
	fc->size = size;

	_filecluster_add(fc, &f->fc_list);

	/* finally hash the file */
	hash_add_file(f);

 _out:
	_swap_unlock();
	return f->fid;

_freef:
	_ffree(f);
_err:
	_swap_unlock();
	return -1;
}

fileid_t file_copy_part(fileid_t fid, off_t pos, off_t size)
{
	filecluster_t *fcstart, *fcend;
	filehead_t *f, *c;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _err;
	errno = EINVAL;
	if (!(fcstart = _filecluster_findbyoff(f, pos))
	    || !(fcend = _filecluster_findbyoff(f, pos+size)))
		goto _err;

	/* split clusters if necessary */
	if (_filecluster_split(fcstart, pos - filecluster_start(fcstart)) == -1
	    || _filecluster_split(fcend, pos+size - filecluster_start(fcend)) == -1)
		goto _err;
	fcstart = _filecluster_findbyoff(f, pos);
	fcend = _filecluster_findbyoff(f, pos+size);

	if (!(c = _file_copy(f, fcstart, fcend)))
		goto _err;

	hash_add_file(c);
	_swap_unlock();
	return c->fid;

 _err:
	_swap_unlock();
	return -1;
}

fileid_t file_copy(fileid_t fid)
{
	filehead_t *f, *c;
	fileid_t fidc = -1;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _out;

	errno = ENOMEM;
	if (!(c = _file_copy(f, fclist_head(f), fclist_tail(f))))
		return -1;

	fidc = c->fid;

	/* hash the file copy */
	hash_add_file(c);

 _out:
	_swap_unlock();
	return fidc;
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

off_t file_size(fileid_t fid)
{
	filehead_t *f;
	off_t size = -1;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _out;

	size = _filehead_size(f);

 _out:
	_swap_unlock();
	return size;
}

int file_truncate(fileid_t fid, off_t size)
{
	filehead_t *f;
	filecluster_t *fc, *nfc;
	off_t oldsize;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _err;

	oldsize = _filehead_size(f);
	if (oldsize == size)
		goto _out;

	/* an "append" is easier */
	if (size > oldsize) {
		size = size - oldsize;
		fc = list_gettail(&f->fc_list, filecluster_t, fc_list);
		if (fc) {
			/* if its a virtual filecluster or the additional
			 * size fits into the real cluster, just fix fc->size */
			if (!fc->cluster
			    || (fc->cluster->size - fc->size) < size) {
				fc->size += size;
				goto _out;
			}
			/* else fill filecluster to raw clustersize */
			size -= fc->cluster->size - fc->size;
			fc->size = fc->cluster->size;
		}
		/* generate new (virtual) filecluster for rest of size */
		nfc = __filecluster_alloc();
		nfc->f = f;
		nfc->off = fc ? filecluster_end(fc)+1 : 0;
		nfc->size = size;
		_filecluster_add(nfc, NULL);
	} else {
		/* truncate file */
		while ((fc = _filecluster_findbyoff(f, size))) {
			if (fc->off == size) {
				/* throw away filecluster */
				_filecluster_remove(fc);
				free(fc);
			} else {
				fc->size = size - fc->off;
			}
		}
	}

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
	} else {
		if (!(fh = hash_find_file(fid)))
			goto _err;
	}

	do {
		if (fh->fh_list.next == &swap->files)
			goto _err;
		fh = list_entry(fh->fh_list.next, filehead_t, fh_list);
	} while (!is_hashed_file(fh));

	_swap_unlock();
	return fh->fid;

 _err:
	_swap_unlock();
	return -1;
}


int file_op_insert(fileid_t fid, off_t pos, fileid_t file)
{
	filehead_t *fd, *fs;
	logentry_t *l;
	filecluster_t *at;

	_swap_lock();
	
	errno = ENOENT;
	if (!(fd = hash_find_file(fid))
	    || !(fs = hash_find_file(file)))
		goto _err;
	errno = EINVAL;
	if (!(at = _filecluster_findbyoff(fd, pos)))
		goto _err;
	errno = ENOMEM;
	if (!(l = _lenew(fd, LOGENTRY_INSERT)))
		goto _err;
	if (_filecluster_split(at, pos - filecluster_start(at)) == -1)
		goto _freele;

	/* build logentry */
	l->u.insert.pos = pos;
	l->u.insert.size = _filehead_size(fs);
	l->u.insert.f = fs;

	/* do the operation */
	at = fc_prev(_filecluster_findbyoff(fs, pos));
	_file_insert(fd, at, fs);
	_file_fixoff(fd, at);

	/* finish it */
	hash_remove_file(fs);
	_logentry_add(l, &swap->log);

	_swap_unlock();
	return 0;


_freele:
	free(l);
_err:
	_swap_unlock();
	return -1;
}

fileid_t file_op_delete(fileid_t fid, off_t pos, off_t size)
{
	filehead_t *f, *fd;
	filecluster_t *fcstart, *fcend;
	logentry_t *l;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _err;
	errno = EINVAL;
	if (!(fcstart = _filecluster_findbyoff(f, pos))
	    || !(fcend = _filecluster_findbyoff(f, pos+size)))
		goto _err;

	/* split clusters if necessary */
	if (_filecluster_split(fcstart, pos - filecluster_start(fcstart)) == -1
	    || _filecluster_split(fcend, pos+size - filecluster_start(fcend)) == -1)
		goto _err;
	fcstart = _filecluster_findbyoff(f, pos);
	fcend = _filecluster_findbyoff(f, pos+size);

	/* create user copy of the to be deleted part */
	if (!(fd = _file_copy(f, fcstart, fcend)))
		goto _err;
	_file_fixoff(fd, NULL);

	/* create the log entry */
	errno = ENOMEM;
	if (!(l = _lenew(f, LOGENTRY_DELETE)))
		goto _freefd;
	l->u.delete.pos = pos;
	l->u.delete.size = size;
	if (!(l->u.delete.f = _file_delete(f, fcstart, fcend, NULL)))
		goto _freele;

	/* finish the operation */
	_file_fixoff(f, NULL);
	hash_add_file(fd);
	_logentry_add(l, &swap->log);

	_swap_unlock();
	return fd->fid;


_freele:
	free(l);
_freefd:
	_ffree(fd);
_err:
	_swap_unlock();
	return -1;
}

int file_op_cut(fileid_t fid, off_t pos, off_t size)
{
	filehead_t *f;
	logentry_t *l;
	filecluster_t *fcstart, *fcend;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _err;
	errno = EINVAL;
	if (!(fcstart = _filecluster_findbyoff(f, pos))
	    || !(fcend = _filecluster_findbyoff(f, pos+size)))
		goto _err;

	/* split clusters if necessary */
	if (_filecluster_split(fcstart, pos - filecluster_start(fcstart)) == -1
	    || _filecluster_split(fcend, pos+size - filecluster_start(fcend)) == -1)
		goto _err;
	fcstart = _filecluster_findbyoff(f, pos);
	fcend = _filecluster_findbyoff(f, pos+size);

	/* first set up the logentry */
	errno = ENOMEM;
	if (!(l = _lenew(f, LOGENTRY_DELETE)))
		goto _err;
	l->u.delete.pos = pos;
	l->u.delete.size = size;
	if (!(l->u.delete.f = _file_delete(f, fcstart, fcend, NULL)))
		goto _freele;

	_file_fixoff(f, NULL);
	_logentry_add(l, &swap->log);

	_swap_unlock();
	return 0;

_freele:
	free(l);
_err:
	_swap_unlock();
	return -1;
}

int file_transaction_begin(fileid_t fid)
{
	filehead_t *f;
	logentry_t *l;
	int res = -1;

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _out;

	res = 0;
	if (f->begincnt++)
		goto _out;

	res = -1;
	errno = ENOMEM;
	if (!(l = _lenew(f, LOGENTRY_BEGIN)))
		goto _out;
	res = 0;
	_logentry_add(l, &swap->log);

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

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _out;
	errno = EINVAL;
	if (!f->begincnt)
		goto _out;

	res = 0;
	if (--(f->begincnt))
		goto _out;

	res = -1;
	errno = ENOMEM;
	if (!(l = _lenew(f, LOGENTRY_END)))
		goto _out;
	res = 0;
	_logentry_add(l, &swap->log);

 _out:
	_swap_unlock();
	return res;
}

int file_transaction_undo(fileid_t fid)
{
	filehead_t *f;
	logentry_t *l;
	int res = -1;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _out;
	errno = EINVAL;
	if (f->begincnt)
		goto _out;

	res = 0;

	/* lookup the previous begin entry (or get null, if f->logpos was -1) */
	l = _logentry_findbyid(f, f->logpos);

	/* we need the next end-entry (sanity-check can be removed) */
	if (!(l = _logentry_prev(f, l)))
		goto _out;
	if (l->op != LOGENTRY_END)
		PANIC("uh, log messed up!?");

	/* reverse actions */
	do {
		l = _logentry_prev(f, l);
		switch (l->op) {
		case LOGENTRY_INSERT:
			_file_delete(f, _filecluster_findbyoff(f, l->u.insert.pos),
				     _filecluster_findbyoff(f, l->u.insert.pos + l->u.insert.size),
				     l->u.insert.f);
			hash_add_file(l->u.insert.f);
			break;
		case LOGENTRY_DELETE:
			_file_insert(f, _filecluster_findbyoff(f, l->u.delete.pos - 1),
				     l->u.delete.f);
			_ffree(l->u.delete.f);
			break;
		default:
		}
		_file_fixoff(l->f, NULL);
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
	logentry_t *l;
	int res = -1;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _out;

	res = 0;
	if (f->logpos == -1)
		goto _out;

	/* lookup the remembered begin entry (sanity-check can be removed) */
	if (!(l = _logentry_findbyid(f, f->logpos))
	    || l->op != LOGENTRY_BEGIN)
		PANIC("uh, log messed up!?");

	/* redo actions */
	do {
		l = _logentry_next(f, l);
		switch (l->op) {
		case LOGENTRY_INSERT:
			_file_insert(f, _filecluster_findbyoff(f, l->u.insert.pos - 1),
				     l->u.insert.f);
			hash_remove_file(l->u.insert.f);
			break;
		case LOGENTRY_DELETE:
			l->u.delete.f = _file_delete(f, _filecluster_findbyoff(f, l->u.delete.pos),
						     _filecluster_findbyoff(f, l->u.delete.size),
						     NULL);
			break;
		default:
		}
		_file_fixoff(l->f, NULL);
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

filecluster_t *filecluster_get(fileid_t fid, off_t pos)
{
	filecluster_t *fc = NULL;
	filehead_t *f;

	_swap_lock();

	errno = ENOENT;
	if (!(f = hash_find_file(fid)))
		goto _out;
	fc = _filecluster_findbyoff(f, pos);

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

	errno = EINVAL;
	if (!is_hashed_file(fc->f))
		goto _out;

	/* do late allocation of cluster */
	if (!fc->cluster) {
		errno = ENOMEM;
		pfc = fc_prev(fc);
		if (!(c = _cluster_alloc(fc->size, pfc ? pfc->cluster : NULL)))
			goto _out;
		if (c->size < fc->size)
			_filecluster_split(fc, c->size);
		_fcpopulate(fc, c, 0);
		zero = 1;
	}

	mem = _cluster_mmap(fc->cluster, zero,
			    fc->cluster->refcnt == 1 ? PROT_READ|PROT_WRITE : PROT_READ);
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
