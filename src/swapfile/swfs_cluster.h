/*
 * swfs_cluster.h
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

#ifndef _SWFS_CLUSTER_H
#define _SWFS_CLUSTER_H

#include "swfs_ctree.h"


struct swcluster;


/* Cluster instance, flags are
 *   SWC_DIRTY - files list is dirty
 *   SWC_CREAT - the data file needs to be created
 *   SWC_NOT_IN_CORE - files_cnt and files is uninitialized */
#define SWC_DIRTY 1
#define SWC_CREAT 2
#define SWC_NOT_IN_CORE 4
struct swcluster {
	struct swcluster *next_swcluster_hash;
	struct swcluster **pprev_swcluster_hash;
	struct list_head lru;
	long name;
	int usage;     /* number of references to this struct cluster */
	int flags;     /* SWC_* */

	/* The size field is always initialized. */
	s32 size;      /* size of the cluster */

	/* The fd is not always open - check for it. */
	int fd;        /* cached fd of the on-disk _data_ */

	/* Fields created out of the cluster metadata, if
	 * SWC_NOT_IN_CORE is set, none of this fields is initialized. */
	int files_cnt; /* number of files that use this cluster */
	long *files;   /* list of files that use this cluster */
};

/* A maximum size goal we want to achieve for this inefficient
 * implementation via a native filesystem - else it would be S32_MAX. */
#define SWCLUSTER_MAXSIZE (1*1024*1024)


/* Initialize the cluster subsystem. Maxlru is the maximum number of
 * cluster descriptors cached in memory. */
static int cluster_init(int maxlru, size_t pmapminfree);

/* Cleanup from the cluster subsystem. */
static void cluster_cleanup();

/* Gets a reference to the specified cluster, if CLUSTERGET_READFILES
 * is set, the list of files that use this cluster is read in. Returns
 * a reference or NULL on error. You may specify the size of the cluster
 * data, if you know it to speed up an eventual readin of the cluster,
 * specify -1, if you dont known the size. */
#define CLUSTERGET_READFILES 1
static struct swcluster *cluster_get(long name, int flags, s32 known_size);

/* Releases the reference, if CLUSTERPUT_SYNC is set, the list of
 * files that use this cluster is synced back to disk. */
#define CLUSTERPUT_SYNC 1
#define CLUSTERPUT_FREE 2
static void cluster_put(struct swcluster *c, int flags);


/* Allocates a new cluster with room for size bytes of data.
 * Returns a cluster reference on success, NULL on failure. */
static struct swcluster *cluster_alloc(s32 size);


/* Adds the specified file to the list of users of this cluster. */
static void cluster_addfileref(struct swcluster *c, long file);

/* Deletes the specified file from the list of users of this
 * cluster. Returns 0 if this was succesful and -1 if there
 * was no such user file. */
static int cluster_delfileref(struct swcluster *c, long file);

/* Checks, if the cluster has a reference on the file file. Returns
 * 0 if that is the case, else -1. */
static int cluster_checkfileref(struct swcluster *c, long file);

/* Creates a memory map of the cluster c possibly at address
 * start with protection and flags like mmap(2). */
static char *cluster_mmap(struct swcluster *c,void *start,
			  int prot, int flags);

/* Unmaps a previously mmapped cluster. Returns 0 on success
 * and -1 on error (invalid supplied address) */
static int cluster_munmap(char *start);


/* Splits the cluster c at position offset storing the head inside
 * ch and the tail after omitting cutcnt bytes after offset inside ct.
 * The head cluster may be identical to c afterwards, if there was only
 * one user of c, but two references are returned. */
static void cluster_split(struct swcluster *c, s32 offset, s32 cutcnt,
			  struct swcluster **ch, struct swcluster **ct);

/* Truncates cluster c to the specified size, truncating from the head.
 * This will copy the cluster, if it is used more than once. */
static struct swcluster *cluster_truncatehead(struct swcluster *c, s32 size);

/* Truncates cluster c to the specified size, truncating from the tail.
 * This will copy the cluster, if it is used more than once. */
static struct swcluster *cluster_truncatetail(struct swcluster *c, s32 size);


/* Truncates the cluster to the specified size, if the cluster is not
 * shared and returns 0, else (shared cluster) -1 is returned. */
static int cluster_truncate(struct swcluster *c, s32 size);


#endif
