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


struct swfile;
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


/* Creates a memory map of the cluster c possibly at address
 * start with protection and flags like mmap(2). */
static void *cluster_mmap(struct swcluster *c,void *start,
			  int prot, int flags);

/* Unmaps a previously mmapped cluster. Returns 0 on success
 * and -1 on error (invalid supplied address) */
static int cluster_munmap(void *start);


#endif
