/*
 * swfs_cluster.c
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
#include <pthread.h>
#include <unistd.h>
#include <fcntl.h>
#include "util.h"
#include "hash.h"
#include "pmap.h"
#include "swfs_cluster.h"


/* The in-memory cluster hash/lru and a lock protecting it. */
HASH(swcluster, struct swcluster, 10,
     (swcluster->name == name),
     (name),
     (swcluster->name),
     long name)
static struct list_head clusterslru;
static int clusterslrucnt;
static int clustersmaxlru;
static pthread_mutex_t clustersmx;
#define LOCKCLUSTERS do { pthread_mutex_lock(&clustersmx); } while (0)
#define UNLOCKCLUSTERS do { pthread_mutex_unlock(&clustersmx); } while (0)

/* Per cluster lock that protects the files list/cnt and the
 * cluster size/flags. */
#define LOCKCLUSTER(c) do {} while (0)
#define UNLOCKCLUSTER(c) do {} while (0)


/* The internal helpers (with no locking) */
static int __cluster_data_open(long name, int flags);
static int __cluster_meta_open(long name, int flags);
static struct swcluster *_cluster_creat(long name);
static struct swcluster *_cluster_stat(long name, s32 known_size);
static void _cluster_readfiles(struct swcluster *c);
static void _cluster_writefiles(struct swcluster *c);
static void _cluster_createdata(struct swcluster *c);
static void _cluster_truncate(struct swcluster *c, s32 size);
static void _cluster_put(struct swcluster *c);


/***********************************************************************
 * API stuff. For documentation see headerfile.
 */

static int cluster_init(int maxlru, size_t pmapminfree)
{
	/* Initialize pmap subsystem. */
	if (pmap_init(pmapminfree) == -1)
		return -1;

	/* Init the lru. */
	clustersmaxlru = maxlru;
	clusterslrucnt = 0;
	INIT_LIST_HEAD(&clusterslru);

	/* The global cluster mx protecting hash & lru. */
	pthread_mutex_init(&clustersmx, NULL);
	srand(getpid()); /* hehe... */

	return 0;
}

static void cluster_cleanup()
{
	struct swcluster *c;

	/* Clear the cluster lru. */
	LOCKCLUSTERS;
	while ((c = list_gethead(&clusterslru, struct swcluster, lru))) {
		hash_remove_swcluster(c);
		list_del(&c->lru);
		_cluster_put(c);
	}
	clusterslrucnt = 0;
	UNLOCKCLUSTERS;

	/* Cleanup pmap subsystem. */
	pmap_close();

	/* Cleanup. */
	pthread_mutex_destroy(&clustersmx);
}

static struct swcluster *cluster_get(long name, int flags, s32 known_size)
{
	struct swcluster *c;

	LOCKCLUSTERS;
	if (!(c = hash_find_swcluster(name)))
		c = _cluster_stat(name, known_size);
	else {
		if (c->usage++ == 0)
			list_del_init(&c->lru);
	}
	UNLOCKCLUSTERS;
	if (!c)
		return NULL;
#ifdef DEBUG
	if (known_size != -1 && c->size != known_size)
		PANIC("User and cluster disagree about its size");
#endif

	/* Read the files list, if required. */
	if ((flags & CLUSTERGET_READFILES)
	    && (c->flags & SWC_NOT_IN_CORE)) {
		LOCKCLUSTER(c);
		if (c->flags & SWC_NOT_IN_CORE)
			_cluster_readfiles(c);
		UNLOCKCLUSTER(c);
	}

	return c;
}

static void cluster_put(struct swcluster *c, int flags)
{
	if (!c)
		return;

	/* Release the reference and do resouece cleanup, if
	 * this was the last one. */
	LOCKCLUSTERS;
	if (--(c->usage) == 0) {
		/* Free the cluster, if requested. */
		if (flags & CLUSTERPUT_FREE) {
			hash_remove_swcluster(c);
			UNLOCKCLUSTERS;
			_cluster_put(c);
			return;
		}

		/* Put cluster into the lru. */
		list_add(&c->lru, &clusterslru);
		clusterslrucnt++;
		if (clusterslrucnt > clustersmaxlru) {
			struct swcluster *c2;
			c2 = list_gettail(&clusterslru, struct swcluster, lru);
			hash_remove_swcluster(c2);
			list_del(&c2->lru);
			clusterslrucnt--;
			UNLOCKCLUSTERS;
			_cluster_put(c2);
			LOCKCLUSTERS;
		}
	}
	UNLOCKCLUSTERS;

	/* Check, if we are instructed to sync the metadata. */
	if ((flags & CLUSTERPUT_SYNC) && (c->flags & SWC_DIRTY)) {
		LOCKCLUSTER(c);
		if (c->flags & SWC_DIRTY)
			_cluster_writefiles(c);
		UNLOCKCLUSTER(c);
	}
}

static struct swcluster *cluster_alloc(s32 size)
{
	static pthread_mutex_t mx = PTHREAD_MUTEX_INITIALIZER;
	struct swcluster *c;
	long cid;

	/* This lock protects against racing free cid finding. */
	pthread_mutex_lock(&mx);

	/* Try, until we find an unallocated cid. This method is
	 * good if the cluster space is very sparse allocated and
	 * gets nearly unusable if more than the half of all clusters
	 * are allocated... */
	cid = rand();
	while ((c = cluster_get(cid, 0, -1))) {
		cluster_put(c, 0);
		cid = rand();
	}

	if ((c = _cluster_creat(cid)))
		c->size = size;

	pthread_mutex_unlock(&mx);

	return c;
}

static void cluster_addfileref(struct swcluster *c, long file)
{
	LOCKCLUSTER(c);
	/* Bring in the files list, if necessary. */
	if (c->flags & SWC_NOT_IN_CORE)
		_cluster_readfiles(c);

	/* Re-alloc the files array. - FIXME: inefficient */
	if (!(c->files = (long *)realloc(c->files, (c->files_cnt+1)*sizeof(long))))
		PANIC("cannot realloc files array");

	/* Append the file, fix the files count and mark it dirty. */
	c->files[c->files_cnt] = file;
	c->files_cnt++;
	c->flags |= SWC_DIRTY;
	UNLOCKCLUSTER(c);
}

static int cluster_delfileref(struct swcluster *c, long file)
{
	int i;

	LOCKCLUSTER(c);
	/* Bring in the files list, if necessary. */
	if (c->flags & SWC_NOT_IN_CORE)
		_cluster_readfiles(c);

	/* Search file from the back (this is most cache friendly
	 * for the necessary array move afterwards). */
	for (i=c->files_cnt-1; i>=0; i--) {
		if (c->files[i] == file) {
			/* Fix the files count. */
			c->files_cnt--;

			/* Start moving every file after i one
			 * position to the front of the list. */
			for (; i<c->files_cnt; i++)
				c->files[i] = c->files[i+1];

			c->flags |= SWC_DIRTY;
			UNLOCKCLUSTER(c);
			return 0;
		}
	}

	/* Not found!? */
	UNLOCKCLUSTER(c);
	return -1;
}

static int cluster_checkfileref(struct swcluster *c, long file)
{
	int i;

	LOCKCLUSTER(c);
	/* Bring in the files list, if necessary. */
	if (c->flags & SWC_NOT_IN_CORE)
		_cluster_readfiles(c);

	/* Search file from the back (this is most cache friendly
	 * for the necessary array move afterwards). */
	for (i=c->files_cnt-1; i>=0; i--) {
		if (c->files[i] == file) {
			UNLOCKCLUSTER(c);
			return 0;
		}
	}

	/* Not found!? */
	UNLOCKCLUSTER(c);
	return -1;
}

static char *cluster_mmap(struct swcluster *c,void *start, int prot, int flags)
{
	/* Need to create the file? Also truncate to the right size. */
	if (c->flags & SWC_CREAT)
		_cluster_createdata(c);
	/* Open the cluster datafile, if necessary. */
	if (c->fd == -1)
		if ((c->fd = __cluster_data_open(c->name, O_RDWR)) == -1)
			PANIC("Cluster vanished under us?");
	return (char *)pmap_map(start, c->size, prot, flags, c->fd, 0);
}

static int cluster_munmap(char *start)
{
	if (pmap_unmap(start) == -1) {
		DPRINTF("Unable to unmap %p\n", start);
		return -1;
	} else
		return 0;
}

static struct swcluster *cluster_truncatetail(struct swcluster *c, s32 size)
{
	struct swcluster *cc;
	char *m, *mc;

	LOCKCLUSTER(c);

	if (c->files_cnt == 1) {
		_cluster_truncate(c, size);
		cc = cluster_get(c->name, 0, -1);
	} else {
		if (!(cc = cluster_alloc(size))
		    || !(m = cluster_mmap(c, NULL, PROT_READ, MAP_SHARED))
		    || !(mc = cluster_mmap(cc, NULL, PROT_WRITE, MAP_SHARED)))
			PANIC("Cannot alloc/mmap cluster");
		memcpy(mc, m, size);
		cluster_munmap(mc);
		cluster_munmap(m);
	}

	UNLOCKCLUSTER(c);

	return cc;
}

static struct swcluster *cluster_truncatehead(struct swcluster *c, s32 size)
{
	struct swcluster *cc;
	char *m, *mc;

	LOCKCLUSTER(c);

	if (c->files_cnt == 1) {
		if (!(m = cluster_mmap(c, NULL, PROT_READ|PROT_WRITE, MAP_SHARED)))
			PANIC("Cannot mmap cluster");
		memmove(m, m + c->size - size, size);
		cluster_munmap(m);
		_cluster_truncate(c, size);
		cc = cluster_get(c->name, 0, c->size);
	} else {
		if (!(cc = cluster_alloc(size))
		    || !(m = cluster_mmap(c, NULL, PROT_READ, MAP_SHARED))
		    || !(mc = cluster_mmap(cc, NULL, PROT_WRITE, MAP_SHARED)))
			PANIC("Cannot alloc/mmap cluster");
		memcpy(mc, m + c->size - size, size);
		cluster_munmap(mc);
		cluster_munmap(m);
	}

	UNLOCKCLUSTER(c);

	return cc;
}

static void cluster_split(struct swcluster *c, s32 offset, s32 cutcnt,
			  struct swcluster **ch, struct swcluster **ct)
{
	char *m, *mh, *mt;

	LOCKCLUSTER(c);

	if (c->files_cnt == 1) {
		if (!(*ct = cluster_alloc(c->size - offset - cutcnt))
		    || !(m = cluster_mmap(c, NULL, PROT_READ, MAP_SHARED))
		    || !(mt = cluster_mmap(*ct, NULL, PROT_WRITE, MAP_SHARED)))
			PANIC("Cannot alloc/mmap cluster");
		memcpy(mt, m + offset + cutcnt, c->size - offset - cutcnt);
		cluster_munmap(mt);
		cluster_munmap(m);
		_cluster_truncate(c, offset);
		*ch = cluster_get(c->name, 0, c->size);
	} else {
		if (!(*ch = cluster_alloc(offset))
		    || !(*ct = cluster_alloc(c->size - offset - cutcnt))
		    || !(m = cluster_mmap(c, NULL, PROT_READ, MAP_SHARED))
		    || !(mh = cluster_mmap(*ch, NULL, PROT_WRITE, MAP_SHARED))
		    || !(mt = cluster_mmap(*ct, NULL, PROT_WRITE, MAP_SHARED)))
			PANIC("Cannot alloc/mmap clusters");
		memcpy(mh, m, offset);
		memcpy(mt, m + offset + cutcnt, c->size - offset - cutcnt);
		cluster_munmap(mt);
		cluster_munmap(mh);
		cluster_munmap(m);
	}

	UNLOCKCLUSTER(c);
}

static int cluster_truncate(struct swcluster *c, s32 size)
{
	LOCKCLUSTER(c);

	if (c->files_cnt > 1
	    || (c->fd != -1
		&& pmap_uncache(c->size, MAP_SHARED, c->fd, 0) == -1)) {
		UNLOCKCLUSTER(c);
		return -1;
	}
	_cluster_truncate(c, size);

	UNLOCKCLUSTER(c);

	return 0;
}


/***********************************************************************
 * Internal helpers. No locking.
 */

/* Helpers for opening the clusters metadata or data file on disk. */
static int __cluster_meta_open(long name, int flags)
{
	char s[256];

	snprintf(s, 255, "%s/%li", swap.clusters_meta_base, name);
	return open(s, flags, 0666);
}
static int __cluster_data_open(long name, int flags)
{
	char s[256];

	snprintf(s, 255, "%s/%li", swap.clusters_data_base, name);
	return open(s, flags, 0666);
}

/* Create a new in-memory representation of a cluster with name
 * entirely from scratch. To be useful you have to know no on-disk
 * or in-memory representation for this cluster exists. Initial
 * state is SWC_DIRTY|SWC_CREAT.
 * Returns a cluster reference or NULL if no memory was available. */
static struct swcluster *_cluster_creat(long name)
{
	struct swcluster *c;

	/* Allocate cluster structure. */
	if (!(c = (struct swcluster *)malloc(sizeof(struct swcluster))))
		return NULL;
	hash_init_swcluster(c);
	INIT_LIST_HEAD(&c->lru);
	c->name = name;
	c->usage = 1;
	c->flags = SWC_DIRTY|SWC_CREAT;
	c->size = 0;
	c->fd = -1;
	c->files_cnt = 0;
	c->files = NULL;

	hash_add_swcluster(c);
	return c;
}

/* Create a new in-memory representation of a cluster. Information
 * is generated using the on-disk representation - and an optional
 * provided size of the cluster (this will skip the stat of the
 * on-disk representation), which can be -1 if you dont know it or
 * if the cluster exists at all.
 * Returns a cluster reference, or NULL if anything went wrong.
 * Initial state of the cluster will be SWC_NOT_IN_CORE. */
static struct swcluster *_cluster_stat(long name, s32 known_size)
{
	struct swcluster *c;
	struct stat dstat;
	char sd[256];

	/* Allocate cluster structure. */
	if (!(c = (struct swcluster *)malloc(sizeof(struct swcluster))))
		return NULL;

	/* Stat metadata and data. */
#ifndef DEBUG
	if (known_size == -1) {
#endif
		snprintf(sd, 255, "%s/%li", swap.clusters_data_base, name);
		if (stat(sd, &dstat) == -1) {
			free(c);
			return NULL;
		}
#ifdef DEBUG
		if (known_size != -1 && known_size != dstat.st_size)
			PANIC("User and disk disagree about cluster size");
#endif
		known_size = dstat.st_size;
#ifndef DEBUG
	}
#endif

	hash_init_swcluster(c);
	INIT_LIST_HEAD(&c->lru);
	c->name = name;
	c->usage = 1;
	c->flags = SWC_NOT_IN_CORE;
	c->size = known_size;
	c->fd = -1;
	c->files_cnt = -1;
	c->files = NULL;

	hash_add_swcluster(c);
	return c;
}

/* Reads the list of cluster users into memory. It is an error
 * to call this operation on a SWC_DIRTY cluster. For clusters
 * which already have the list this is a nop. Any internal error
 * causes a PANIC. Clears SWC_NOT_IN_CORE flag. */
static void _cluster_readfiles(struct swcluster *c)
{
	int fd;
	struct stat stat;

	if (c->flags & SWC_DIRTY)
		PANIC("read into dirty state");
	if (!(c->flags & SWC_NOT_IN_CORE))
		return;

	/* Load files list. */
	if ((fd = __cluster_meta_open(c->name, O_RDWR)) == -1
	    || fstat(fd, &stat) == -1)
		PANIC("metadata vanished under us");
	if (!(c->files = (long *)malloc(stat.st_size)))
		PANIC("no memory for files list");
	c->files_cnt = stat.st_size/sizeof(long);
	if (read(fd, c->files, stat.st_size) != stat.st_size)
		PANIC("cannot read cluster metadata");
	c->flags &= ~SWC_NOT_IN_CORE;
	close(fd);
}

/* Writes the list of cluster users - the file list - to
 * the on-disk metadata file of the cluster. This file gets
 * created, if it is necessary.
 * If the cluster is not SWC_DIRTY this has no effect. The
 * SWC_DIRTY flag is cleared. Also for SWC_NOT_IN_CORE this
 * is a nop, too.
 * Failure is not permitted at any stage of this operation,
 * so we PANIC in this case. */
static void _cluster_writefiles(struct swcluster *c)
{
	int fd, size;

	if (!(c->flags & SWC_DIRTY)
	    || c->flags & SWC_NOT_IN_CORE)
		return;

	/* Open the metadatafile for writing (creating it, if it
	 * does not already exist), truncate it to the right
	 * size and write the file ids to the file. */
	size = c->files_cnt*sizeof(long);
	if ((fd = __cluster_meta_open(c->name, O_RDWR|O_CREAT)) == -1
	    || ftruncate(fd, size) == -1
	    || write(fd, c->files, size) != size)
		PANIC("cannot write cluster metadata");
	c->flags &= ~SWC_DIRTY;
	close(fd);
}

/* Create the clusters datafile, if necessary, and truncate it to
 * the right size. */
static void _cluster_createdata(struct swcluster *c)
{
	if (!(c->flags & SWC_CREAT))
		return;
	if ((c->fd == -1
	     && (c->fd = __cluster_data_open(c->name, O_RDWR|O_CREAT)) == -1)
	    || ftruncate(c->fd, c->size) == -1)
		PANIC("Cannot creat/truncate cluster");
	c->flags &= ~SWC_CREAT;
}

/* Set the clusters size and truncate the on-disk file, if it
 * is already open, else just postpone that via SWC_CREAT. */
static void _cluster_truncate(struct swcluster *c, s32 size)
{
	c->size = size;
	if (c->fd == -1)
		c->flags |= SWC_CREAT;
	else
		if (ftruncate(c->fd, c->size) == -1)
			PANIC("Cannot truncate cluster");
}

static void _cluster_put(struct swcluster *c)
{
	char s[256];

	/* Close the data fd - or create the file, if
	 * necessary. */
	if ((c->flags & SWC_CREAT) && c->files_cnt != 0)
		_cluster_createdata(c);
	/* Kill possible mappings and close the file. */
	if (c->fd != -1) {
		pmap_invalidate(c->fd);
		close(c->fd);
	}

	/* If there are no users (files) left, unlink the
	 * on-disk representations. */
	if (!(c->flags & SWC_NOT_IN_CORE) && c->files_cnt == 0) {
		snprintf(s, 255, "%s/%li", swap.clusters_meta_base, c->name);
		unlink(s);
		snprintf(s, 255, "%s/%li", swap.clusters_data_base, c->name);
		unlink(s);
	} else if (c->flags & SWC_DIRTY)
		_cluster_writefiles(c);
	if (c->files)
		free(c->files);
	free(c);
}
