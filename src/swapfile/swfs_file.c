/*
 * swfs_file.c
 *
 * Copyright (C) 2000, 2001 Richard Guenther
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
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include "util.h"
#include "hash.h"
#include "swfs_cluster.h"
#include "swfs_file.h"


/* The in-memory files hash and its lock. */
HASH(swfile, struct swfile, 8,
     (swfile->name == name),
     (name),
     (swfile->name),
     long name)
static pthread_mutex_t filesmx = PTHREAD_MUTEX_INITIALIZER;
#define LOCKFILES do { pthread_mutex_lock(&filesmx); } while (0)
#define UNLOCKFILES do { pthread_mutex_unlock(&filesmx); } while (0)

#define LOCKFILE(f) do { pthread_mutex_lock(&f->mx); } while (0)
#define LOCK2FILE(f1, f2) do { \
	goto entry; \
	do { \
		pthread_mutex_unlock(&f1->mx); \
	entry: \
		pthread_mutex_lock(&f1->mx); \
	} while (pthread_mutex_trylock(&f2->mx) != 0); \
} while (0)
#define UNLOCKFILE(f) do { pthread_mutex_unlock(&f->mx); } while (0)



/* Helpers for opening the metadata file. */
static inline void __file_name(char *s, int len, long name)
{
	snprintf(s, len, "%s/%lX", swap.files_base, name);
}
static inline int __file_open(long name, int flags)
{
	char s[256];

	__file_name(s, 255, name);
	return open(s, flags, 0666);
}

/* Other helpers, see bottom of file. */
static struct swfile *_file_creat(long name);
static struct swfile *_file_stat(long name);
static void _file_readclusters(struct swfile *f);
static void _file_writeclusters(struct swfile *f);
#define _file_needclusters(f) do { if (f->flags & SWF_NOT_IN_CORE) _file_readclusters(f); } while (0)
static void _file_cluster_truncatehead(struct swfile *f, long cpos, s32 size);
static void _file_cluster_truncatetail(struct swfile *f, long cpos, s32 size);
static void _file_cluster_split(struct swfile *f, long cpos,
				s32 offset, s32 cutcnt);
static void _file_cluster_delete(struct swfile *f, long pos, long cnt);
static void _file_cluster_insert(struct swfile *df, long dpos,
				 struct swfile *sf, long spos, long cnt);
static int _file_grow(struct swfile *f, s64 delta);
#ifndef SWDEBUG
#define file_check(f) do {} while (0)
#define _file_check(f) do {} while (0)
#else
static void _file_check(struct swfile *f);
#define file_check(f) do { LOCKFILE(f); _file_check(f); UNLOCKFILE(f); } while (0)
#endif



/************************************************************************
 * API functions.
 */

static struct swfile *file_get(long name, int flags)
{
	struct swfile *f;

	LOCKFILES;
	if (!(f = hash_find_swfile(name))) {
		if (!(f = _file_stat(name)) && (flags & FILEGET_CREAT))
			f = _file_creat(name);
	} else
		f->usage++;
	UNLOCKFILES;
	if (!f)
		return NULL;

	/* Read the cluster list, if required. */
	if ((flags & FILEGET_READCLUSTERS)
	    && (f->flags & SWF_NOT_IN_CORE)) {
		LOCKFILE(f);
		if (f->flags & SWF_NOT_IN_CORE)
			_file_readclusters(f);
		UNLOCKFILE(f);
	}
	file_check(f);

	return f;
}

static void file_put(struct swfile *f, int flags)
{
	struct swcluster *c;
	char s[256];
	int i;

	if (!f)
		return;
	file_check(f);

	/* Check, if we are instructed to unlink the file. */
	if (flags & FILEPUT_UNLINK) {
		/* Bring in the cluster tree, if necessary
		 * and mark the file unlinked. */
		LOCKFILE(f);
		_file_needclusters(f);
		f->flags |= SWF_UNLINKED;
		UNLOCKFILE(f);

		/* Unlink the on disk representation. */
		__file_name(s, 255, f->name);
		unlink(s);
	}

	/* Release the reference and do resource cleanup, if
	 * this was the last one. */
	LOCKFILES;
	if (--(f->usage) == 0) {
		hash_remove_swfile(f);
		UNLOCKFILES;

		if (f->flags & SWF_UNLINKED) {
			/* SWF_UNLINKED files have the ctree in core. */
			for (i=0; i<f->clusters->cnt; i++) {
				if ((c = cluster_get(CID(f->clusters, i),
						     CLUSTERGET_READFILES,
						     CSIZE(f->clusters, i)))) {
					if (cluster_delfileref(c, f->name) == -1)
						SWAPFILE_MARK_UNCLEAN("Cannot del fileref");
					cluster_put(c, 0);
				} else
					SWAPFILE_MARK_UNCLEAN("Cannot get cluster");
			}
		} else {
			if (f->flags & SWF_DIRTY)
				_file_writeclusters(f);
		}
		if (f->clusters)
			free(f->clusters);
		pthread_mutex_destroy(&f->mx);
		free(f);
		return;
	}
	UNLOCKFILES;

	/* Check, if we are instructed to sync the file. */
	if ((flags & FILEPUT_SYNC) && (f->flags & SWF_DIRTY)) {
		LOCKFILE(f);
		if (f->flags & SWF_DIRTY)
			_file_writeclusters(f);
		UNLOCKFILE(f);
	}
}

static struct swcluster *file_getcluster(struct swfile *f,
					 s64 offset, s64 *cstart,
					 int flags)
{
	struct swcluster *c;
	long pos;
	u32 cid;
	s32 csize;

	LOCKFILE(f);
	_file_needclusters(f);
	pos = ctree_find(f->clusters, offset, cstart);
	if (pos != -1) {
		cid = CID(f->clusters, pos);
		csize = CSIZE(f->clusters, pos);
	}
	if (pos == -1) {
		UNLOCKFILE(f);
		return NULL;
	}

	/* The cluster is now required to exist and have a
	 * correct reference on the file. */
	if (!(c = cluster_get(cid, flags, csize)))
		SWAPFILE_MARK_UNCLEAN("Cannot get cluster");
	UNLOCKFILE(f);
	if (!c)
		return NULL;
#ifdef SWDEBUG /* FIXME: s/PANIC/SWAPFILE_MARK_UNCLEAN()/ ?? */
	if (cluster_checkfileref(c, f->name) == -1)
		SWPANIC("Cluster in ctree w/o correct fileref");
#endif

	return c;
}

static struct swcluster *file_getcluster_private(struct swfile *f,
						 s64 offset, s64 *cstart,
						 int flags)
{
	struct swcluster *c, *cc;
	long pos;
	u32 cid;
	s32 csize;

	LOCKFILE(f);
	_file_needclusters(f);
	pos = ctree_find(f->clusters, offset, cstart);
	if (pos != -1) {
		cid = CID(f->clusters, pos);
		csize = CSIZE(f->clusters, pos);
	}
	if (pos == -1) {
		UNLOCKFILE(f);
		return NULL;
	}

	/* The cluster is now required to exist and have a
	 * correct reference on the file. */
	if (!(c = cluster_get(cid, flags, csize)))
		SWAPFILE_MARK_UNCLEAN("Cannot get cluster");
	if (!c) {
		UNLOCKFILE(f);
		return NULL;
	}
#ifdef SWDEBUG /* FIXME: s/SWPANIC/SWAPFILE_MARK_UNCLEAN/ ?? */
	if (cluster_checkfileref(c, f->name) == -1)
		SWPANIC("Cluster in ctree w/o correct fileref");
#endif

	/* Now, we have to check if we need to copy the cluster. */
	if ((cc = cluster_unshare(c)) != c) {
		cluster_addfileref(cc, f->name);
		ctree_replace1(f->clusters, pos, cc->name, cc->size);
		f->flags |= SWF_DIRTY;
		cluster_delfileref(c, f->name);
		_file_check(f);

		/* Drop the original cluster, re-get the copied cluster
		 * with right flags (and drop the reference from unshare). */
		cluster_put(c, 0);
		c = cluster_get(cc->name, flags, csize);
		cluster_put(cc, 0);
	}
	UNLOCKFILE(f);

	return c;
}

/* This function is protected by a global lock, so no locking
 * necessary (against ctree modifying). */
static int file_truncate(struct swfile *f, s64 size)
{
	struct swcluster *c;
	long cpos;
	s32 csize;

	file_check(f);
	if (f->flags & SWF_NOT_IN_CORE)
		DERROR("called with ctree not in core");

	LOCKFILE(f);

	/* Well :) Simple cases first... */
	if (f->clusters->size == size) {
		UNLOCKFILE(f);
		return 0;
	}

	/* File grows. This is the easy case. At least for now,
	 * as we do this rather stupid and inefficient (read: we
	 * really want to avoid creating new clusters) */
	if (f->clusters->size < size) {
		if (_file_grow(f, size - f->clusters->size) == -1) {
			UNLOCKFILE(f);
			return -1;
		}
		UNLOCKFILE(f);
		file_check(f);
		return 0;
	}

	/* File shrinks. Ugh - this is complicated. */
	if (f->clusters->size > size) {
		/* Start from the last cluster, removing complete
		 * truncated clusters. */
		while (f->clusters->cnt > 0
		       && (f->clusters->size
			   - CSIZE(f->clusters, f->clusters->cnt-1)) >= size) {
			/* Get the cluster for later deleting the file
			 * reference. */
			if (!(c = cluster_get(CID(f->clusters, f->clusters->cnt-1), CLUSTERGET_READFILES, CSIZE(f->clusters, f->clusters->cnt-1))))
				SWAPFILE_MARK_UNCLEAN("cannot get cluster");
			/* Remove the cluster from the tree. */
			ctree_remove(f->clusters, f->clusters->cnt-1,
				     1, NULL, NULL);
			f->flags |= SWF_DIRTY;
			/* Delete the file reference. */
			if (c && cluster_delfileref(c, f->name) == -1)
				SWAPFILE_MARK_UNCLEAN("cannot delete fileref");
			if (c)
				cluster_put(c, 0);
		}

		/* If necessary, split the last cluster and remove
		 * the second part. */
		if (f->clusters->size != size) {
			cpos = f->clusters->cnt-1;
			csize = CSIZE(f->clusters, cpos)
				- (f->clusters->size-size);
			_file_cluster_truncatetail(f, cpos, csize);
		}
		UNLOCKFILE(f);
		file_check(f);
		return 0;
	}
	UNLOCKFILE(f);
	return -1;
}

static int file_insert(struct swfile *df, s64 dpos,
		       struct swfile *sf, s64 spos, s64 count)
{
	long cpos_sfirst, cpos_slast, cpos_dest;
	s64 off;

	file_check(df);
	file_check(sf);
	if (df != sf)
		LOCK2FILE(df, sf);
	else
		LOCKFILE(df);

	if (dpos > df->clusters->size
            || spos+count > sf->clusters->size)
		DERROR("Invalid arguments");

	/* First we need to potentially split the first and last
	 * affected clusters from the source file. Also remember
	 * the first/last affected cluster for later copy. Like
	 * file_cut try to keep the search results invariant 
	 * after the splits. */
	cpos_sfirst = ctree_find(sf->clusters, spos, &off);
	if (off != spos) {
		_file_cluster_split(sf, cpos_sfirst, spos - off, 0);
		cpos_sfirst++;
	}
	cpos_slast = ctree_find(sf->clusters, spos+count, &off);
	if (cpos_slast != -1 && off != spos+count) {
		_file_cluster_split(sf, cpos_slast, spos+count - off, 0);
		cpos_slast++;
	}

	/* Second we need to potentially split the cluster of the
	 * insertion point inside the destination cluster. Remember
	 * the destination position. */
	cpos_dest = ctree_find(df->clusters, dpos, &off);
	if (cpos_dest != -1 && off != dpos) {
		_file_cluster_split(df, cpos_dest, dpos - off, 0);
		cpos_dest++;
	}

	/* Dont try to be clever - just redo the search if we do
	 * self-insertion... - destination stuff should be ok. */
	if (df == sf) {
		cpos_sfirst = ctree_find(sf->clusters, spos, &off);
		cpos_slast = ctree_find(sf->clusters, spos+count, &off);
	}

	/* Now insert the clusters from cpos_sfirst to cpos_slast
	 * from the source file into the destination file at cpos_dest. */
	if (cpos_slast == -1)
		cpos_slast = sf->clusters->cnt;
	if (cpos_dest == -1)
		cpos_dest = df->clusters->cnt;
	_file_cluster_insert(df, cpos_dest,
			     sf, cpos_sfirst, cpos_slast-cpos_sfirst);

	if (df != sf) {
		UNLOCKFILE(df);
		UNLOCKFILE(sf);
	} else
		UNLOCKFILE(df);
	file_check(df);
	file_check(sf);

	return 0;
}

static int file_cut(struct swfile *f, s64 pos, s64 count)
{
	long cpos_first, cpos_last;
	s64 coff_first, coff_last;
	s32 dcnt;

	file_check(f);
	LOCKFILE(f);

	if (pos+count > f->clusters->size)
		DERROR("Invalid arguments");

	/* Note that the following is subtle - we do the thing
	 * step by step, trying to keep the following search
	 * results as invariants after each step. */
	cpos_first = ctree_find(f->clusters, pos, &coff_first);
	cpos_last = ctree_find(f->clusters, pos+count, &coff_last);
	if (cpos_first == -1 || f->clusters->size < pos+count)
		DERROR("pos/count outside of file");

	/* It can be that cpos_last is -1 and coff_last undefined (cut of
	 * tail of the file) - now we have to set it to the "virtually"
	 * one-after-last cluster id / size of the file. */
	if (cpos_last == -1) {
		cpos_last = f->clusters->cnt;
		coff_last = f->clusters->size;
	}

	/* First we cut off the tail of the first cluster if this is
	 * possible/necessary so the cut-start after this operation is
	 * on a cluster boundary.
	 * The condition is "start not already on cluster boundary" and
	 * "end not in the first cluster". */
	if (coff_first != pos
	    && coff_first+CSIZE(f->clusters, cpos_first) <= pos+count) {
		dcnt = CSIZE(f->clusters, cpos_first) - (pos-coff_first);
		_file_cluster_truncatetail(f, cpos_first, pos - coff_first);
		count -= dcnt;
		coff_last -= dcnt;
		coff_first = pos;
		cpos_first++;
	}

	/* For the last cluster we can do the same (cutting off the head
	 * if the start position is not inside (at exactly the first
	 * byte is allowed) the last cluster. */
	if (coff_last != pos+count
	    && pos <= coff_last) {
		dcnt = CSIZE(f->clusters, cpos_last)
			- (coff_last+CSIZE(f->clusters, cpos_last)
			   - (pos+count));
		_file_cluster_truncatehead(f, cpos_last,
					   CSIZE(f->clusters, cpos_last)-dcnt);
		count -= dcnt;
	}

	/* There are now five cases left:
	 * - nothing left to do, count is zero
	 * - start position is on cluster boundary and end position is
	 *   not, but is in the same cluster
	 * - end position is on cluster boundary and start position is
	 *   not, but is in the previous cluster
	 * - start and end position are not on cluster boundary but
	 *   in the same cluster
	 * - start and end position are on cluster boundaries
	 */
	if (count == 0)
		;
	else if (coff_first == pos && coff_last != pos + count
		 && cpos_first == cpos_last)
		_file_cluster_truncatehead(f, cpos_first,
					   CSIZE(f->clusters, cpos_first)-count);
	else if (coff_first != pos && coff_last == pos + count
		 && cpos_first + 1 == cpos_last)
		_file_cluster_truncatetail(f, cpos_first,
					   CSIZE(f->clusters, cpos_first)-count);
	else if (coff_first != pos && coff_last != pos + count
		 && cpos_first == cpos_last)
		_file_cluster_split(f, cpos_first, pos - coff_first, count);
	else if (coff_first == pos && coff_last == pos + count
		 && cpos_first < cpos_last)
		_file_cluster_delete(f, cpos_first, cpos_last-cpos_first);
	else
		DERROR("Duh! Failed to handle case!?");

	UNLOCKFILE(f);
	file_check(f);

	return 0;
}

static int file_replace(struct swfile *df, s64 dpos,
			struct swfile *sf, s64 spos, s64 count)
{
	if (dpos > df->clusters->size
	    || spos+count > sf->clusters->size)
		DERROR("Invalid arguments");

	/* To be optimized? - This is not really atomic - FIXME? */
	if (file_cut(df, dpos, count) == -1
	    || file_insert(df, dpos, sf, spos, count) == -1)
		return -1;
	return 0;
}



/***************************************************************************
 * Internal functions.
 */

/* Create a new file. Only the in memory representation is created,
 * the on-disk one gets created on demand. A reference is returned
 * or NULL on error. 
 * Needs the FILES lock. */
static struct swfile *_file_creat(long name)
{
	struct swfile *f;

	/* Allocate file structure. */
	if (!(f = (struct swfile *)malloc(sizeof(struct swfile))))
		return NULL;
	hash_init_swfile(f);
	f->name = name;
	f->usage = 1;
	f->flags = SWF_DIRTY;

	/* Allocate a minimal cluster tree. */
	if (!(f->clusters = ctree_alloc(4))) {
		free(f);
		return NULL;
	}
	pthread_mutex_init(&f->mx, NULL);

	hash_add_swfile(f);
	return f;
}

/* A new in memory representation is created, if the file exists
 * in an on-disk version. A reference is returned, or NULL on 
 * error.
 * Needs the FILES lock. */
static struct swfile *_file_stat(long name)
{
	struct swfile *f;
	struct stat st;
	char s[256];

	/* Try to stat file metadata. */
	__file_name(s, 255, name);
	if (stat(s, &st) == -1
	    || !S_ISREG(st.st_mode))
		return NULL;

	/* Allocate file structure. */
	if (!(f = (struct swfile *)malloc(sizeof(struct swfile))))
		return NULL;

	hash_init_swfile(f);
	pthread_mutex_init(&f->mx, NULL);
	f->name = name;
	f->flags = SWF_NOT_IN_CORE;
	f->usage = 1;
	f->clusters = NULL;
	hash_add_swfile(f);

	return f;
}

/* Reads the cluster tree into memory. Needs to have the
 * on-disk file already open. SWF_DIRTY state and internal
 * errors will cause a PANIC. !SWF_NOT_IN_CORE results in a
 * nop.
 * Needs the FILE lock. */
static void _file_readclusters(struct swfile *f)
{
	struct stat st;
	int fd;

	if (f->flags & SWF_DIRTY)
		DERROR("Read into dirty state");
	if (!(f->flags & SWF_NOT_IN_CORE))
		return;

	/* Load cluster tree. */
	if ((fd = __file_open(f->name, O_RDONLY)) == -1)
		SWPANIC("Cannot open file metadata");
	if (fstat(fd, &st) == -1)
		SWPANIC("Cannot stat file metadata");
	if (!(f->clusters = (struct ctree *)malloc(st.st_size)))
		SWPANIC("No memory for cluster tree");
	if (read(fd, f->clusters, st.st_size) != st.st_size)
		SWPANIC("Cannot read cluster tree");
	if (st.st_size != CTREESIZE(f->clusters->height))
		SWPANIC("Corrupted cluster tree file");
	f->flags &= ~SWF_NOT_IN_CORE;
	close(fd);
}

/* Write the clusters tree to the on-disk representation of
 * the file. Creates this file, if necessary. Calling with
 * SWF_DIRTY not set or SWF_NOT_IN_CORE or SWF_UNLINKED set
 * results in a nop.
 * On internal failures a PANIC is caused.
 * Needs the FILE lock. */
static void _file_writeclusters(struct swfile *f)
{
	int fd, size, res;
	char *m;

	if (!(f->flags & SWF_DIRTY)
	    || (f->flags & SWF_NOT_IN_CORE)
	    || (f->flags & SWF_UNLINKED))
		return;

	_file_check(f);
	if ((fd = __file_open(f->name, O_RDWR|O_CREAT)) == -1)
		SWPANIC("Cannot open/creat file metadata");
	size = CTREESIZE(f->clusters->height);
	if (ftruncate(fd, size) == -1)
		SWPANIC("Cannot truncate file metadata");
	m = (char *)(f->clusters);
	do {
		res = write(fd, m, size);
		if (res == -1 && errno != EINTR) {
			SWPANIC("Cannot write file metadata");
		} else {
			size -= res;
			m += res;
		}
	} while (size>0);
	f->flags &= ~SWF_DIRTY;
	close(fd);
}


/* Splits the cluster at CID/CSIZE position cpos at the specified offset
 * inside the cluster (offset belongs to the tail).
 * Needs the FILE lock. */
static void _file_cluster_split(struct swfile *f, long cpos,
				s32 offset, s32 cutcnt)
{
	struct swcluster *c, *ch, *ct;

	if (!(c = cluster_get(CID(f->clusters, cpos), CLUSTERGET_READFILES, -1)))
		SWPANIC("Cannot get cluster");
	cluster_split(c, offset, cutcnt, &ch, &ct);
	cluster_addfileref(ch, f->name);
	cluster_addfileref(ct, f->name);
	ctree_replace1(f->clusters, cpos, ch->name, ch->size);
	f->clusters = ctree_insert1(f->clusters, cpos+1, ct->name, ct->size);
	f->flags |= SWF_DIRTY;
	if (cluster_delfileref(c, f->name) == -1)
		SWPANIC("Cannot delete fileref");
	cluster_put(ch, 0);
	cluster_put(ct, 0);
	cluster_put(c, 0);
	_file_check(f);
}

/* Truncates the cluster at CID/CSIZE position cpos to the specified size,
 * truncating from the tail of the cluster.
 * Needs the FILE lock. */
static void _file_cluster_truncatetail(struct swfile *f, long cpos, s32 size)
{
	struct swcluster *c, *ch;

	if (!(c = cluster_get(CID(f->clusters, cpos), CLUSTERGET_READFILES,
			      CSIZE(f->clusters, cpos))))
		SWPANIC("Cannot get cluster");
	cluster_split(c, size, 0, &ch, NULL);
	cluster_addfileref(ch, f->name);
	ctree_replace1(f->clusters, cpos, ch->name, ch->size);
	f->flags |= SWF_DIRTY;
	if (cluster_delfileref(c, f->name) == -1)
		SWPANIC("Cannot delete fileref");
	cluster_put(c, 0);
	cluster_put(ch, 0);
	_file_check(f);
}

/* Truncates the cluster at CID/CSIZE position cpos to the specified size,
 * truncating from the head of the cluster.
 * Needs the FILE lock. */
static void _file_cluster_truncatehead(struct swfile *f, long cpos, s32 size)
{
	struct swcluster *c, *ct;

	if (!(c = cluster_get(CID(f->clusters, cpos), CLUSTERGET_READFILES,
			      CSIZE(f->clusters, cpos))))
		SWPANIC("Cannot get cluster");
	cluster_split(c, c->size - size, 0, NULL, &ct);
	cluster_addfileref(ct, f->name);
	ctree_replace1(f->clusters, cpos, ct->name, ct->size);
	f->flags |= SWF_DIRTY;
	if (cluster_delfileref(c, f->name) == -1)
		SWPANIC("Cannot delete fileref");
	cluster_put(c, 0);
	cluster_put(ct, 0);
	_file_check(f);
}

/* Deletes cnt clusters from CID/CSIZE position pos.
 * Needs the FILE lock. */
static void _file_cluster_delete(struct swfile *f, long pos, long cnt)
{
	struct swcluster *c;
	u32 *cid;
	s32 *csize;
	long i;

	cid = (u32 *)alloca(sizeof(u32)*cnt);
	csize = (s32 *)alloca(sizeof(s32)*cnt);
	f->clusters = ctree_remove(f->clusters, pos, cnt, cid, csize);
	f->flags |= SWF_DIRTY;
	for (i=0; i<cnt; i++) {
		if (!(c = cluster_get(cid[i], CLUSTERGET_READFILES, csize[i])))
			SWPANIC("Cannot get cluster");
		if (cluster_delfileref(c, f->name) == -1)
			SWPANIC("Cannot delete fileref");
		cluster_put(c, 0);
	}
	_file_check(f);
}

/* Inserts cnt clusters from file sf at position spos into file df
 * at position dpos.
 * Needs the FILE lock on both files. */
static void _file_cluster_insert(struct swfile *df, long dpos,
				 struct swfile *sf, long spos, long cnt)
{
	struct swcluster *c;
	long i;

	if (spos+cnt > sf->clusters->cnt
	    || dpos > df->clusters->cnt)
		DERROR("Invalid arguments");

	for (i=spos; i<spos+cnt; i++) {
		if (!(c = cluster_get(CID(sf->clusters, i), CLUSTERGET_READFILES, CSIZE(sf->clusters, i))))
			SWPANIC("Cannot get cluster");
		cluster_addfileref(c, df->name);
		cluster_put(c, 0);
	}
	df->clusters = ctree_insert(df->clusters, dpos, cnt,
				    &CID(sf->clusters, spos),
				    &CSIZE(sf->clusters, spos));
	df->flags |= SWF_DIRTY;
	_file_check(sf);
	_file_check(df);
}

/* Grow the file by delta bytes - either with new clusters or
 * by expanding existing ones. */
static int _file_grow(struct swfile *f, s64 delta)
{
	struct swcluster *c;
	s64 size_goal;
	s32 dc;

	/* If we do have clusters already, we can at least try to
	 * expand the last one to SWCLUSTER_MAXSIZE. */
	if (f->clusters->cnt > 0) {
		if (!(c = cluster_get(CID(f->clusters, f->clusters->cnt-1),
				      CLUSTERGET_READFILES,
				      CSIZE(f->clusters, f->clusters->cnt-1)))) {
			SWAPFILE_MARK_UNCLEAN("Cannot get cluster");			
			return -1;
		}
		size_goal = MIN(SWCLUSTER_MAXSIZE, c->size + delta);
		dc = size_goal - c->size;
		if (cluster_truncate(c, size_goal) == 0) {
			ctree_replace1(f->clusters, f->clusters->cnt-1,
				       c->name, c->size);
			f->flags |= SWF_DIRTY;
			delta -= dc;
		}
		cluster_put(c, 0);
	}

	/* Allocate new clusters, if delta is still > 0. */
	while (delta > 0) {
		size_goal = MIN(SWCLUSTER_MAXSIZE, delta);
		if (!(c = cluster_alloc(size_goal)))
			return -1;
		cluster_addfileref(c, f->name);
		f->clusters = ctree_insert1(f->clusters, f->clusters->cnt,
					    c->name, c->size);
		f->flags |= SWF_DIRTY;
		cluster_put(c, 0);
		delta -= size_goal;
	}
	return 0;
}

#ifdef SWDEBUG
/* Checks the clusters of the file. */
static void _file_check(struct swfile *f)
{
	struct swcluster *c;
	int i;

	if (!f || swap.fsck)
		return;

	/* Check ctree consistency. */
	if (ctree_check(f->clusters) == -1)
		SWPANIC("Inconsistent cluster tree");

	/* Loop through files clusters and "touch" them. */
	for (i=0; i<f->clusters->cnt; i++) {
		if (!(c = cluster_get(CID(f->clusters, i),
				      CLUSTERGET_READFILES,
				      CSIZE(f->clusters, i))))
			SWPANIC("Unaccesible cluster in ctree");
		if (cluster_checkfileref(c, f->name) == -1)
			SWPANIC("Cluster in ctree w/o valid fileref");
		cluster_put(c, 0);
	}
}
#endif
