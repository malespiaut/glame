/*
 * swfs_file.c
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
#include <sys/stat.h>
#include <fcntl.h>
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

#define LOCKFILE(f) do {} while (0)
#define UNLOCKFILE(f) do {} while (0)


static int __file_open(long name, int flags);
static struct swfile *_file_creat(long name);
static struct swfile *_file_stat(long name);
static void _file_readclusters(struct swfile *f);
static void _file_writeclusters(struct swfile *f);




/* Gets a reference to the file with the specified name -
 * creates it, if it does not exist and FILEGET_CREAT is
 * set. If FILEGET_READCLUSTERS is set, the cluster tree
 * of the file is brought into the core.
 * Returns a reference to the file or NULL on error. */
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

	return f;
}


/* Releases the file reference, if FILEPUT_SYNC is set,
 * the actual cluster tree is synced to the disk, if
 * FILEPUT_UNLINK is specified, the on-disk representation
 * is unlinked and as the last reference goes away, the
 * file and its name is freed. */
static void file_put(struct swfile *f, int flags)
{
	struct swcluster *c;
	char s[256];
	int i;

	if (!f)
		return;

	/* Check, if we are instructed to unlink the file. */
	if (flags & FILEPUT_UNLINK) {
		/* Bring in the cluster tree, if necessary
		 * and mark the file unlinked. */
		LOCKFILE(f);
		if (f->flags & SWF_NOT_IN_CORE)
			_file_readclusters(f);
		f->flags |= SWF_UNLINKED;
		UNLOCKFILE(f);

		/* Unlink the on disk representation. */
		snprintf(s, 255, "%s/%li", swap.files_base, f->name);
		unlink(s);
	}

	/* Release the reference and do resource cleanup, if
	 * this was the last one. */
	LOCKFILES;
	if (--(f->usage) == 0) {
		hash_remove_swfile(f);
		UNLOCKFILES;

		if (f->flags & SWF_UNLINKED) {
			/* SWF_UNLINKED files have the clusters in core. */
			for (i=0; i<f->clusters->cnt; i++) {
				if (!(c = cluster_get(CID(f->clusters, i),
						      CLUSTERGET_READFILES,
						      CSIZE(f->clusters, i))))
					PANIC("Cannot get filecluster");
				if (cluster_delfileref(c, f->name) == -1)
					PANIC("Cannot del fileref");
				cluster_put(c, 0);
			}
		} else {
			if (f->flags & SWF_DIRTY)
				_file_writeclusters(f);
		}
		close(f->fd);
		if (f->clusters)
			free(f->clusters);
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
	long pos;
	u32 cid;
	s32 csize;

	LOCKFILE(f);
	if (f->flags & SWF_NOT_IN_CORE)
		_file_readclusters(f);
	pos = _find_cluster(f->clusters, offset, cstart);
	if (pos != -1) {
		cid = CID(f->clusters, pos);
		csize = CSIZE(f->clusters, pos);
	}
	UNLOCKFILE(f);

	if (pos == -1)
		return NULL;
	return cluster_get(cid, flags, csize);
}

/* This function is protected by a global lock, so no locking
 * necessary (against ctree modifying). */
static int file_truncate(struct swfile *f, s64 size)
{
	struct swcluster *c, *c2;
	char *m, *m2;
	long cid;
	s32 csize;

	if (f->flags & SWF_NOT_IN_CORE)
		DERROR("called with ctree not in core");

	/* Well :) Simple cases first... */
	if (f->clusters->total_size == size)
		return 0;

	/* File grows. This is the easy case. At least for now,
	 * as we do this rather stupid and inefficient (read: we
	 * really want to avoid creating new clusters) */
	if (f->clusters->total_size < size) {
		/* Allocate a new cluster, append it to the file
		 * and insert the file reference. */
		c = cluster_alloc(size - f->clusters->total_size);
		cluster_addfileref(c, f->name);
		LOCKFILE(f);
		cid = c->name;
		csize = c->size;
		f->clusters = _insert_clusters(f->clusters, f->clusters->cnt,
					       1, &cid, &csize);
		UNLOCKFILE(f);
		cluster_put(c, 0);
		return 0;
	}

	/* File shrinks. Ugh - this is complicated. */
	if (f->clusters->total_size > size) {
		/* Start from the last cluster, removing complete
		 * truncated clusters. */
		while (f->clusters->total_size - CSIZE(f->clusters, f->clusters->cnt-1)) {
			/* Get the cluster for later deleting the file
			 * reference. */
			if (!(c = cluster_get(CID(f->clusters, f->clusters->cnt-1), CLUSTERGET_READFILES, CSIZE(f->clusters, f->clusters->cnt-1))))
				PANIC("cannot get cluster");
			/* Remove the cluster from the tree. */
			_replace_cluster(f->clusters, f->clusters->cnt-1,
					 0, 0);
			f->clusters->cnt--;
			/* Delete the file reference. */
			if (cluster_delfileref(c, f->name) == -1)
				PANIC("cannot delete fileref");
			cluster_put(c, 0);
		}

		/* If necessary, split the last cluster and remove
		 * the second part. */
		if (f->clusters->total_size == size)
			return 0;
		if (!(c = cluster_get(CID(f->clusters, f->clusters->cnt-1), CLUSTERGET_READFILES, CSIZE(f->clusters, f->clusters->cnt-1))))
			PANIC("cannot get cluster");
		/* Simple case of only one user of the to be split
		 * cluster - just truncate it to the right length. */
		if (c->files_cnt == 1) {
			/* Correct cluster and the ctree. */
			c->size -= f->clusters->total_size - size;
			_replace_cluster(f->clusters, f->clusters->cnt-1,
					 c->name, c->size);
			/* Correct the on-disk file, if necessary. */
			if (c->flags & SWC_CREAT) {
				cluster_put(c, 0);
				return 0;
			}
			if (c->fd == -1)
				if ((c->fd = __cluster_data_open(c->name, O_RDWR)) == -1)
					PANIC("cannot open cluster");
			if (ftruncate(c->fd, c->size) == -1)
				PANIC("cannot truncate cluster file");
			cluster_put(c, 0);
			return 0;
		}

		/* Umm, now we have to either split the cluster to
		 * allow it to be shared afterwards - or - for now
		 * just to be simple - copy the cluster. */
		LOCKFILE(f);
		c2 = cluster_alloc(c->size - (f->clusters->total_size - size));
		cluster_addfileref(c2, f->name);
		_replace_cluster(f->clusters, f->clusters->cnt-1,
				 c2->name, c2->size);
		cluster_delfileref(c, f->name);
		m = cluster_mmap(c, NULL, PROT_READ, MAP_SHARED);
		m2 = cluster_mmap(c2, NULL, PROT_WRITE, MAP_SHARED);
		memcpy(m2, m, c2->size);
		cluster_munmap(m);
		cluster_munmap(m2);
		cluster_put(c, 0);
		cluster_put(c2, 0);
		UNLOCKFILE(f);

		return -1;
	}

	return -1;
}



/***************************************************************************
 * Internal functions.
 */

/* Helper for opening the metadata file. */
static int __file_open(long name, int flags)
{
	char s[256];

	snprintf(s, 255, "%s/%li", swap.files_base, name);
	return open(s, flags, 0666);
}

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
	f->fd = -1;

	/* Allocate a minimal cluster tree. */
	if (!(f->clusters = (struct ctree *)malloc(CTREESIZE(1)))) {
		free(f);
		return NULL;
	}
	memset(f->clusters, 0, CTREESIZE(1));
	f->clusters->height = 1;

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

	/* Allocate file structure. */
	if (!(f = (struct swfile *)malloc(sizeof(struct swfile))))
		return NULL;

	/* Open file metadata. */
	if ((f->fd = __file_open(name, O_RDWR)) == -1) {
		free(f);
		return NULL;
	}

	hash_init_swfile(f);
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
 * nop. */
static void _file_readclusters(struct swfile *f)
{
	struct stat st;

	if (f->flags & SWF_DIRTY)
		PANIC("Read into dirty state");
	if (!(f->flags & SWF_NOT_IN_CORE))
		return;

	/* Load cluster tree. */
	if (fstat(f->fd, &st) == -1)
		PANIC("Cannot stat");
	if (!(f->clusters = (struct ctree *)malloc(st.st_size)))
		PANIC("No memory for cluster tree");
	if (read(f->fd, f->clusters, st.st_size) != st.st_size)
		PANIC("Cannot read cluster tree");
	if (st.st_size != CTREESIZE(f->clusters->height))
		PANIC("Corrupted cluster tree file");
	f->flags &= ~SWF_NOT_IN_CORE;
}

/* Write the clusters tree to the on-disk representation of
 * the file. Creates this file, if necessary. Calling with
 * SWF_DIRTY not set or SWF_NOT_IN_CORE or SWF_UNLINKED set
 * results in a nop.
 * On internal failures a PANIC is caused. */
static void _file_writeclusters(struct swfile *f)
{
	if (!(f->flags & SWF_DIRTY)
	    || (f->flags & SWF_NOT_IN_CORE)
	    || (f->flags & SWF_UNLINKED))
		return;

	if (f->fd == -1
	    && (f->fd = __file_open(f->name, O_RDWR|O_CREAT|O_EXCL)) == -1)
		PANIC("Inconsistent in-core/on-disk state");

	if (ftruncate(f->fd, CTREESIZE(f->clusters->height)) == -1
	    || lseek(f->fd, 0, SEEK_SET) != 0
	    || (write(f->fd, f->clusters, CTREESIZE(f->clusters->height))
		!= CTREESIZE(f->clusters->height)))
		PANIC("Cannot write the cluster tree");
	f->flags &= ~SWF_DIRTY;
}
