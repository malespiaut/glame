/*
 * swfs_cluster.c
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
#include <sys/mman.h>
#include <pthread.h>
#include <unistd.h>
#include <fcntl.h>
#include "util.h"
#include "hash.h"
#include "swfs_cluster.h"


/* Hash for in-memory clusters, wether used or not. Protected
 * by the CLUSTERS lock. */
HASH(swcluster, struct swcluster, 10,
     (swcluster->name == name),
     (name),
     (swcluster->name),
     long name)

/* Clusters lru, where unused clusters are queued. Number of
 * lru'ed clusters. Protected by the CLUSTERS lock. */
static struct list_head clusterslru;
static int clusterslrucnt;
static int clustersmaxlru;

/* LRU for open fds. Protected by FDS lock. */
static struct list_head clusters_fdlru;
static int clusters_fdlru_cnt;
static int clusters_fdlru_maxcnt;
static pthread_mutex_t fdsmx;
#define LOCKFDS do { pthread_mutex_lock(&fdsmx); } while (0)
#define UNLOCKFDS do { pthread_mutex_unlock(&fdsmx); } while (0)

/* Hash for mapped (used) clusters, via virtual memory address.
 * Amount of memory mapped.
 * Protected by the CLUSTER lock (read) and the mappings lock (write). */
HASH(mapping, struct swcluster, 8,
     (mapping->map_addr == addr),
     ((long)addr/4096),
     ((long)mapping->map_addr/4096),
     void *addr)
static pthread_mutex_t mappingsmx;
#define LOCKMAPPINGS do { pthread_mutex_lock(&mappingsmx); } while (0)
#define UNLOCKMAPPINGS do { pthread_mutex_unlock(&mappingsmx); } while (0)
static long clusters_mappedsize;
static long clusters_maxmappedsize;

/* LRU for mappings, protected by MAPPINGS lock. */
static struct list_head clusters_maplru;
static int clusters_maplru_cnt;
static int clusters_maplru_maxcnt;

/* Statistics. Unprotected. */
static int clusters_getcnt;
static int clusters_lruhitcnt;
static int clusters_misscnt;
static int clusters_readfilescnt;
static int clusters_writefilescnt;
static int clusters_creatcnt;

/* The CLUSTERS lock. */
static pthread_mutex_t clustersmx;
#define LOCKCLUSTERS do { pthread_mutex_lock(&clustersmx); } while (0)
#define UNLOCKCLUSTERS do { pthread_mutex_unlock(&clustersmx); } while (0)

/* Per cluster lock that protects the files list/cnt and the
 * cluster size/flags. */
#define LOCKCLUSTER(c) do { pthread_mutex_lock(&c->mx); } while (0)
#define UNLOCKCLUSTER(c) do { pthread_mutex_unlock(&c->mx); } while (0)


/* The internal helpers (with no locking) */
static int __cluster_meta_open(long name, int flags);
static struct swcluster *_cluster_creat(long name);
static struct swcluster *_cluster_stat(long name, s32 known_size);
static int _cluster_readfiles(struct swcluster *c);
static void _cluster_writefiles(struct swcluster *c);
static void _cluster_truncate(struct swcluster *c, s32 size);
static void _cluster_put(struct swcluster *c);
#define _cluster_needfiles(c) do { if ((c)->flags & SWC_NOT_IN_CORE) if (_cluster_readfiles(c) == -1) SWPANIC("metadata vanished under us"); } while (0)
#define _cluster_needdata(c) do { if ((c)->flags & SWC_CREAT || (c)->fd == -1) __cluster_needdata(c); } while (0)
static void __cluster_needdata(struct swcluster *c);


/***********************************************************************
 * API stuff. For documentation see headerfile.
 */

static int cluster_init(int maxlru, int maxfds,
 			int maxmaps, size_t maxvm)
{
        /* Init the clusters lru - unused clusters only. */
	clustersmaxlru = maxlru;
	clusterslrucnt = 0;
	INIT_LIST_HEAD(&clusterslru);
	pthread_mutex_init(&clustersmx, NULL);
	
	/* Init the mappings lru - unused maps only. */
	clusters_maplru_maxcnt = maxmaps;
	clusters_maplru_cnt = 0;
	INIT_LIST_HEAD(&clusters_maplru);
	pthread_mutex_init(&mappingsmx, NULL);
	clusters_maxmappedsize = maxvm;

	/* Init the fd lru - all fds. */
	clusters_fdlru_maxcnt = maxfds;
	clusters_fdlru_cnt = 0;
	INIT_LIST_HEAD(&clusters_fdlru);
	pthread_mutex_init(&fdsmx, NULL);

	/* Initialize random number generator. */
	srand(getpid());

	/* Statistics. */
	clusters_getcnt = 0;
	clusters_lruhitcnt = 0;
	clusters_misscnt = 0;
	clusters_readfilescnt = 0;
	clusters_writefilescnt = 0;
	clusters_creatcnt = 0;

	return 0;
}

static void cluster_cleanup()
{
	struct swcluster *c;
	int i;

	/* Check the cluster lru accounting. */
	if (list_count(&clusterslru) != clusterslrucnt) {
		DPRINTF("LRU has %i elements, but %i accounted\n",
			list_count(&clusterslru), clusterslrucnt);
		PANIC("Bug in lru accounting");
	}

	/* Check the cluster fd accounting. */
	if (list_count(&clusters_fdlru) != clusters_fdlru_cnt) {
		DPRINTF("FD LRU has %i elements, but %i accounted\n",
			list_count(&clusters_fdlru), clusters_fdlru_cnt);
		PANIC("Bug in fd lru accounting");
	}

	/* Check the cluster map accounting. */
	if (list_count(&clusters_maplru) != clusters_maplru_cnt) {
		DPRINTF("MAP LRU has %i elements, but %i accounted\n",
			list_count(&clusters_maplru), clusters_maplru_cnt);
		PANIC("Bug in map lru accounting");
	}

	/* Clear the cluster lru. */
	LOCKCLUSTERS;
	while ((c = list_gethead(&clusterslru, struct swcluster, lru))) {
		hash_remove_swcluster(c);
		list_del_init(&c->lru);
		_cluster_put(c);
	}
	clusterslrucnt = 0;
	UNLOCKCLUSTERS;

	/* Check, if we still have clusters hashed (cluster leak). */
	for (i=0; i<((1<<10)-1); i++) {
		while ((c = hash_getslot_swcluster(i))) {
			if (c->usage != 0)
				DPRINTF("Cluster leak somewhere\n");
			DPRINTF("Unused cluster not in lru?\n");
			hash_remove_swcluster(c);
			_cluster_put(c);
		}
	}

	/* Check, if we still have fds/mappings in lrus. */
	if (clusters_maplru_cnt > 0)
		DPRINTF("Still entries in mappings lru?\n");
	if (clusters_fdlru_cnt > 0)
		DPRINTF("Still entries in fd lru?\n");

	/* Cleanup. */
	pthread_mutex_destroy(&clustersmx);
	pthread_mutex_destroy(&mappingsmx);
	pthread_mutex_destroy(&fdsmx);

	/* Statistics. */
	DPRINTF("SWFS_CLUSTER Statistics: %i gets, %i lru hits, %i misses\n",
		clusters_getcnt, clusters_lruhitcnt, clusters_misscnt);
	DPRINTF("\t%i meta reads, %i meta writes, %i data creates\n",
		clusters_readfilescnt, clusters_writefilescnt,
		clusters_creatcnt);
}

/* We need to be able to call cluster_get with the cluster lock held!
 * (but only for flags == 0) */
static struct swcluster *cluster_get(long name, int flags, s32 known_size)
{
	struct swcluster *c;

	LOCKCLUSTERS;
	if (!(c = hash_find_swcluster(name))) {
		c = _cluster_stat(name, known_size);
		clusters_misscnt++;
	} else {
		if (c->usage++ == 0) {
			list_del_init(&c->lru);
			clusterslrucnt--;
			clusters_lruhitcnt++;
		}
	}
	UNLOCKCLUSTERS;
	if (!c)
		return NULL;
#ifdef SWDEBUG
	if (known_size != -1 && c->size != known_size)
		SWPANIC("User and cluster disagree about its size");
#endif
	clusters_getcnt++;

	/* Read the files list, if required. */
	if ((flags & CLUSTERGET_READFILES)
	    && (c->flags & SWC_NOT_IN_CORE)) {
		LOCKCLUSTER(c);
		if (c->flags & SWC_NOT_IN_CORE)
			if (_cluster_readfiles(c) == -1) {
				if (c->usage > 1)
					SWPANIC("metadata vanished under us");
				UNLOCKCLUSTER(c);
				cluster_put(c, CLUSTERPUT_FREE);
				return NULL;
			}
		UNLOCKCLUSTER(c);
	}

	return c;
}

/* We need to be able to call this with the cluster lock held
 * (and flags == 0). */
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
			if (!c2)
				DERROR("Bug in lru accounting");
			if (&c2->lru == &clusterslru)
				PANIC("Corrupted lru list!");
			hash_remove_swcluster(c2);
			list_del_init(&c2->lru);
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
	_cluster_needfiles(c);

	/* Re-alloc the files array. - FIXME: inefficient */
	if (!(c->files = (long *)realloc(c->files, (c->files_cnt+1)*sizeof(long))))
		SWPANIC("cannot realloc files array");

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
	_cluster_needfiles(c);

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
	_cluster_needfiles(c);

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


static void _cluster_killmap(struct swcluster *c)
{
	if (is_hashed_mapping(c))
		DERROR("Killing used mapping");
	munmap(c->map_addr, c->size);
	c->map_addr = NULL;
	c->map_prot = PROT_NONE;
	LOCKMAPPINGS;
	clusters_mappedsize -= c->size;
	if (!list_empty(&c->maplru)) {
		list_del(&c->maplru);
		clusters_maplru_cnt--;
	}
	UNLOCKMAPPINGS;
}

static inline void __cluster_shrinkmaps()
{
	while (clusters_mappedsize > clusters_maxmappedsize
	       || clusters_maplru_cnt > clusters_maplru_maxcnt) {
		struct swcluster *c;
		c = list_gettail(&clusters_maplru, struct swcluster, maplru);
		if (!c)
			break;
		/* _cluster_killmap(c); */
		munmap(c->map_addr, c->size);
		c->map_addr = NULL;
		c->map_prot = PROT_NONE;
		clusters_mappedsize -= c->size;
		list_del_init(&c->maplru);
		clusters_maplru_cnt--;
	}
}

static char *_cluster_mmap(struct swcluster *c, int prot, int flags)
{
	/* Need a private mapping? Create one. */
	if (flags == MAP_PRIVATE) {
		/* FIXME: cannot unmap these later - need anon. cluster. */
		DERROR("unimplemented");
		_cluster_needdata(c);
		return mmap(NULL, c->size, prot, flags, c->fd, 0);
	}

	/* We can only MAP_SHARED now. */
	if (flags != MAP_SHARED)
		return MAP_FAILED;

	/* Do we have a mapping already? Maybe update it. */
	if (c->map_addr) {
		if ((prot & c->map_prot) != prot)
			mprotect(c->map_addr, c->size, c->map_prot | prot);
		c->map_prot |= prot;
		/* Get an additional reference. */
		cluster_get(c->name, 0, -1);
		if (c->map_cnt++ == 0) {
			LOCKMAPPINGS;
			hash_add_mapping(c);
			list_del_init(&c->maplru);
			clusters_maplru_cnt--;
			UNLOCKMAPPINGS;
		}
		return c->map_addr;
	}

	/* Ok, create a new shared mapping of the cluster. */
	_cluster_needdata(c);
	c->map_addr = mmap(NULL, c->size, prot, MAP_SHARED, c->fd, 0);
	if (c->map_addr == MAP_FAILED) {
		c->map_addr = NULL;
		return MAP_FAILED;
	}
	c->map_prot = prot;
	LOCKMAPPINGS;
	hash_add_mapping(c);
	clusters_mappedsize += c->size;
	__cluster_shrinkmaps();
	UNLOCKMAPPINGS;
	/* Get an additional reference. */
	cluster_get(c->name, 0, -1);
	c->map_cnt++;
	return c->map_addr;
}

static char *cluster_mmap(struct swcluster *c, int prot, int flags)
{
	char *mem;

	LOCKCLUSTER(c);
	mem = _cluster_mmap(c, prot, flags);
	UNLOCKCLUSTER(c);

	return mem;
}

static void _cluster_fixmap(struct swcluster *c, size_t size)
{
	void *addr;

	if (!c->map_addr)
		return;
#ifdef OS_LINUX
	if (c->map_cnt == 0)
		addr = mremap(c->map_addr, c->size, size, MREMAP_MAYMOVE);
	else
		addr = mremap(c->map_addr, c->size, size, 0);
#else
	addr = (void *)-1;
#endif
	if (addr == (void *)-1) {
		if (c->map_cnt == 0)
			_cluster_killmap(c);
		else
		        PANIC("umm, we are screwed now");
	} else {
		c->map_addr = addr;
		LOCKMAPPINGS;
		clusters_mappedsize += size - c->size;
		UNLOCKMAPPINGS;
	}
}

static int _cluster_munmap(struct swcluster *c)
{
	if (c->map_cnt == 0)
		DERROR("Unmapping not mapped cluster");
	if (--(c->map_cnt) == 0) {
		/* Remove the mapping and protect it. */
		LOCKMAPPINGS;
		hash_remove_mapping(c);
		list_add(&c->maplru, &clusters_maplru);
		clusters_maplru_cnt++;
		__cluster_shrinkmaps();
		UNLOCKMAPPINGS;
		if (c->map_addr) {
			c->map_prot = PROT_NONE;
			mprotect(c->map_addr, c->size, PROT_NONE);
		}
	}
	/* Drop the additional reference. */
	cluster_put(c, 0);

	return 0;
}

static int cluster_munmap(char *start)
{
	struct swcluster *c;
	int res;

	if (!(c = hash_find_mapping(start))) {
		errno = EINVAL;
		return -1;
	}
	LOCKCLUSTER(c);
	res = _cluster_munmap(c);
	UNLOCKCLUSTER(c);
	return res;
}

static void cluster_split(struct swcluster *c, s32 offset, s32 cutcnt,
			  struct swcluster **ch, struct swcluster **ct)
{
	char *m, *mh, *mt;

	if (!c || offset < 0 || offset > c->size
	    || cutcnt < 0 || offset+cutcnt > c->size
	    || (!ch && !ct))
		DERROR("Illegal use of cluster_split");

	LOCKCLUSTER(c);

	if (c->map_cnt != 0) {
		/* umm - fuck. we at least need to mremap to
		 * one part - or possibly "detach" the mapping,
		 * split it, too - hoo, humm. msync probably, etc. */
		PANIC("Splitting mapped cluster");
	}

	if (c->files_cnt == 1) {
		/* If the cluster is not shared, things are a lot
		 * simpler. */
		if (ct && !ch) {
			if ((m = _cluster_mmap(c, PROT_READ|PROT_WRITE, MAP_SHARED)) == MAP_FAILED)
				SWPANIC("Cannot mmap cluster");
			memmove(m, m + offset + cutcnt,
				c->size - offset - cutcnt);
			_cluster_munmap(c);
			_cluster_truncate(c, c->size - offset - cutcnt);
			*ct = cluster_get(c->name, 0, c->size);
		} else if (!ct && ch) {
			_cluster_truncate(c, offset);
			*ch = cluster_get(c->name, 0, c->size);
		} else /* if (ct && ch) */ {
			if (!(*ct = cluster_alloc(c->size - offset - cutcnt))
			    || (m = _cluster_mmap(c, PROT_READ,
						  MAP_SHARED)) == MAP_FAILED
			    || (mt = _cluster_mmap(*ct, PROT_WRITE,
						   MAP_SHARED)) == MAP_FAILED)
				SWPANIC("Cannot alloc/mmap cluster");
			memcpy(mt, m + offset + cutcnt,
			       c->size - offset - cutcnt);
			_cluster_munmap(*ct);
			_cluster_munmap(c);
			_cluster_truncate(c, offset);
			*ch = cluster_get(c->name, 0, c->size);
		}
	} else {
		/* Umm, now we have to copy in any case. */
		if ((m = _cluster_mmap(c, PROT_READ,
				       MAP_SHARED)) == MAP_FAILED)
			SWPANIC("Cannot mmap cluster");
		if (ch) {
			if (!(*ch = cluster_alloc(offset))
			    || (mh = _cluster_mmap(*ch, PROT_WRITE,
						   MAP_SHARED)) == MAP_FAILED)
				SWPANIC("Cannot alloc/mmap cluster");
			memcpy(mh, m, offset);
			_cluster_munmap(*ch);
		}
		if (ct) {
			if (!(*ct = cluster_alloc(c->size - offset - cutcnt))
			    || (mt = _cluster_mmap(*ct, PROT_WRITE,
						   MAP_SHARED)) == MAP_FAILED)
				SWPANIC("Cannot alloc/mmap cluster");
			memcpy(mt, m + offset + cutcnt,
			       c->size - offset - cutcnt);
			_cluster_munmap(*ct);
		}
		_cluster_munmap(c);
	}

	UNLOCKCLUSTER(c);
}

static int cluster_truncate(struct swcluster *c, s32 size)
{
	LOCKCLUSTER(c);
	_cluster_truncate(c, size);
	UNLOCKCLUSTER(c);

	return 0;
}

static struct swcluster *cluster_unshare(struct swcluster *c)
{
	struct swcluster *cc;
	char *m, *mc;

	LOCKCLUSTER(c);

	/* If we have only one reference to a file, we might just
	 * return the cluster. */
	_cluster_needfiles(c);
	if (c->files_cnt <= 1) {
		UNLOCKCLUSTER(c);
		return c;
	}

	/* Now we have to copy the whole cluster (and return the copy
	 * instead of the original). */
	if (!(cc = cluster_alloc(c->size))
	    || (m = _cluster_mmap(c, PROT_READ, MAP_SHARED)) == MAP_FAILED
	    || (mc = _cluster_mmap(cc, PROT_WRITE, MAP_SHARED)) == MAP_FAILED)
		SWPANIC("Cannot alloc/mmap clusters");
	memcpy(mc, m, c->size);
	_cluster_munmap(c);
	_cluster_munmap(cc);
	UNLOCKCLUSTER(c);

	return cc;
}


static ssize_t cluster_read(struct swcluster *c, void *buf,
			    size_t count, off_t offset)
{
	ssize_t res;

	LOCKCLUSTER(c);
	/* If we have a mapping with PROT_READ, we may memcpy
	 * to satisfy the read. */
	if (c->map_addr && (c->map_prot & PROT_READ)) {
		memcpy(buf, c->map_addr + offset, count);
		res = count;

	/* Else we're going to need the fd and do a pread. */
	} else {
		_cluster_needdata(c);
		res = pread(c->fd, buf, count, offset);
	}
	UNLOCKCLUSTER(c);

	return res;
}

static ssize_t cluster_write(struct swcluster *c, const void *buf,
			     size_t count, off_t offset)

{
	ssize_t res;

	LOCKCLUSTER(c);
	if (c->size < offset+count)
		DERROR("Write extends cluster size");

	/* If we have a mapping with PROT_WRITE, we may memcpy
	 * to satisfy the read. */
	if (c->map_addr && (c->map_prot & PROT_WRITE)) {
		memcpy(c->map_addr + offset, buf, count);
		res = count;

	/* Else we're going to need the fd and do a pwrite. */
	} else {
		_cluster_needdata(c);
		res = pwrite(c->fd, buf, count, offset);
	}
	UNLOCKCLUSTER(c);

	return res;
}



/***********************************************************************
 * Internal helpers. No locking.
 */

/* Helpers for opening the clusters metadata or data file on disk. */
static int __cluster_meta_open(long name, int flags)
{
	char s[256];

	snprintf(s, 255, "%s/%lX/%lX", swap.clusters_meta_base,
		 name & 0xff, name >> 8);
	return open(s, flags, 0666);
}

/* Alloc and init a cluster struct. */
static struct swcluster *_cluster_new()
{
	struct swcluster *c;

	if (!(c = (struct swcluster *)malloc(sizeof(struct swcluster))))
		return NULL;
	hash_init_swcluster(c);
	INIT_LIST_HEAD(&c->lru);
	c->name = -1;
	c->usage = 0;
	pthread_mutex_init(&c->mx, NULL);
	c->flags = SWC_NOT_IN_CORE;
	c->size = 0;
        INIT_LIST_HEAD(&c->fdlru);
	c->fd = -1;
	c->files_cnt = -1;
	c->files = NULL;
	hash_init_mapping(c);
        INIT_LIST_HEAD(&c->maplru);
	c->map_addr = NULL;
	c->map_prot = PROT_NONE;
	c->map_cnt = 0;

	return c;
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
	if (!(c = _cluster_new()))
		return NULL;
	c->name = name;
	c->usage = 1;
	c->flags = SWC_DIRTY|SWC_CREAT;
	c->size = 0;
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
	if (!(c = _cluster_new()))
		return NULL;

	/* Stat metadata and data. */
#ifndef SWDEBUG
	if (known_size == -1) {
#endif
		snprintf(sd, 255, "%s/%lX/%lX", swap.clusters_data_base,
			 name & 0xff, name >> 8);
		if (stat(sd, &dstat) == -1) {
			free(c);
			pthread_mutex_destroy(&c->mx);
			return NULL;
		}
		known_size = dstat.st_size;
#ifndef SWDEBUG
	}
#endif

	c->name = name;
	c->usage = 1;
	c->flags = SWC_NOT_IN_CORE;
	c->size = known_size;

	hash_add_swcluster(c);
	return c;
}

/* Reads the list of cluster users into memory. It is an error
 * to call this operation on a SWC_DIRTY cluster. For clusters
 * which already have the list this is a nop. Any internal error
 * causes a SWPANIC. Clears SWC_NOT_IN_CORE flag. */
static int _cluster_readfiles(struct swcluster *c)
{
	int fd;
	struct stat stat;

	if (c->flags & SWC_DIRTY)
		DERROR("read into dirty state");
	if (!(c->flags & SWC_NOT_IN_CORE))
		return 0;

	/* Load files list. */
	if ((fd = __cluster_meta_open(c->name, O_RDWR)) == -1
	    || fstat(fd, &stat) == -1) {
		if (fd != -1)
			close(fd);
		return -1;
	}
	if (!(c->files = (long *)malloc(stat.st_size)))
		SWPANIC("no memory for files list");
	c->files_cnt = stat.st_size/sizeof(long);
	if (read(fd, c->files, stat.st_size) != stat.st_size)
		SWPANIC("cannot read cluster metadata");
	c->flags &= ~SWC_NOT_IN_CORE;
	close(fd);
	clusters_readfilescnt++;

	return 0;
}

/* Writes the list of cluster users - the file list - to
 * the on-disk metadata file of the cluster. This file gets
 * created, if it is necessary.
 * If the cluster is not SWC_DIRTY this has no effect. The
 * SWC_DIRTY flag is cleared. Also for SWC_NOT_IN_CORE this
 * is a nop, too.
 * Failure is not permitted at any stage of this operation,
 * so we SWPANIC in this case. */
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
		SWPANIC("cannot write cluster metadata");
	c->flags &= ~SWC_DIRTY;
	close(fd);
	clusters_writefilescnt++;
}


static inline int __cluster_data_open(struct swcluster *c, int flags)
{
	char s[256];

	if (c->fd != -1)
		return 0;
	snprintf(s, 255, "%s/%lX/%lX", swap.clusters_data_base,
		 c->name & 0xff, c->name >> 8);
	c->fd = open(s, flags, 0666);
	if (c->fd == -1) {
		if (errno == EMFILE && clusters_fdlru_maxcnt > 16) {
			DPRINTF("WARNING! fixing clusters_fdlru_maxcnt (%i)\n", clusters_fdlru_maxcnt);
			clusters_fdlru_maxcnt /= 2;
		} else
			return -1;
	}
	LOCKFDS;
	if (c->fd != -1) {
		list_add(&c->fdlru, &clusters_fdlru);
		clusters_fdlru_cnt++;
	}
	while (clusters_fdlru_cnt > clusters_fdlru_maxcnt) {
		struct swcluster *c2;
		c2 = list_gettail(&clusters_fdlru, struct swcluster, fdlru);
		close(c2->fd);
		c2->fd = -1;
		list_del_init(&c2->fdlru);
		clusters_fdlru_cnt--;
	}
	UNLOCKFDS;
	if (c->fd == -1)
		return __cluster_data_open(c, flags);
	return 0;
}

/* Create the clusters datafile, if necessary, and truncate it to
 * the right size. */
static void __cluster_needdata(struct swcluster *c)
{
	if (!(c->flags & SWC_CREAT)) {
		if (__cluster_data_open(c, O_RDWR) == -1)
			SWPANIC("Cannot open cluster");
	} else {
		if (c->fd != -1)
			DERROR("SWC_CREAT, but file open!?");
		if (__cluster_data_open(c, O_RDWR|O_CREAT) == -1)
			SWPANIC("Cannot create cluster");
		if (ftruncate(c->fd, c->size) == -1)
			SWPANIC("Cannot truncate cluster");
		c->flags &= ~SWC_CREAT;
		clusters_creatcnt++;
	}
}

/* Set the clusters size and truncate the on-disk file, if it
 * is already open, else just postpone that via SWC_CREAT. */
static void _cluster_truncate(struct swcluster *c, s32 size)
{
	/* If SWC_CREAT is set, we can just adjust the in-memory
	 * size of the cluster and be done with it. */
	if (c->flags & SWC_CREAT) {
		c->size = size;
		return;
	}

	/* Next, truncate the on-disk data file. */
	_cluster_needdata(c);
	if (ftruncate(c->fd, size) == -1)
		SWPANIC("Cannot truncate cluster");

	/* Now, we have to adjust mappings of the cluster. */
	_cluster_fixmap(c, size);

	/* Update in-memory size. */
	c->size = size;
}

/* Free the cluster struct from memory and possibly cleanup. */
static void _cluster_put(struct swcluster *c)
{
	char s[256];

	if (!list_empty(&c->lru))
		DERROR("Freeing cluster in lru");
	if (is_hashed_swcluster(c))
		DERROR("Freeing hashed cluster");
	if (is_hashed_mapping(c))
		DERROR("Freeing mapped cluster");

	/* Close the data fd - or create the file, if
	 * necessary. */
	if ((c->flags & SWC_CREAT) && c->files_cnt != 0)
		_cluster_needdata(c);
	/* Kill possible mappings. */
	if (c->map_addr)
		_cluster_killmap(c);
        /* and close the file. */
	if (c->fd != -1) {
		LOCKFDS;
		list_del(&c->fdlru);
		clusters_fdlru_cnt--;
		close(c->fd);
		UNLOCKFDS;
	}

	/* If there are no users (files) left, unlink the
	 * on-disk representations. */
	if (!(c->flags & SWC_NOT_IN_CORE) && c->files_cnt == 0) {
		snprintf(s, 255, "%s/%lX/%lX", swap.clusters_meta_base,
			 c->name & 0xff, c->name >> 8);
		unlink(s);
		snprintf(s, 255, "%s/%lX/%lX", swap.clusters_data_base,
			 c->name & 0xff, c->name >> 8);
		unlink(s);
	} else if (c->flags & SWC_DIRTY)
		_cluster_writefiles(c);
	if (c->files)
		free(c->files);
	pthread_mutex_destroy(&c->mx);
	free(c);
}
