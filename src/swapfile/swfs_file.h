/*
 * swfs_file.h
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

#ifndef _SWFS_FILE_H
#define _SWFS_FILE_H

#include "swfs_ctree.h"


struct swfile;
struct swcluster;


/* File instance, flags are
 *   SWF_NOT_IN_CORE - swfile.{size,cluster_cnt,clusters} is not filled in
 *   SWF_UNLINKED - on disk file is unlinked
 *   SWF_DIRTY - cluster list is dirty */
#define SWF_NOT_IN_CORE 1
#define SWF_UNLINKED 2
#define SWF_DIRTY 4
struct swfile {
	struct swfile *next_swfile_hash;
	struct swfile **pprev_swfile_hash;
	pthread_mutex_t mx; /* protects flags & clusters */
	long name;
	int usage;    /* number of references to this struct swfile,
		       * protected by FILES lock. */
	int flags;    /* SWF_* */

	/* Fields created out of the file metadata */
	struct ctree *clusters; /* cluster tree */
};


/* Gets a reference to the file with the specified name -
 * creates it, if it does not exist and FILEGET_CREAT is
 * set. If FILEGET_READCLUSTERS is set, the cluster tree
 * of the file is brought into the core.
 * Returns a reference to the file or NULL on error. */
#define FILEGET_READCLUSTERS 1
#define FILEGET_CREAT 2
static struct swfile *file_get(long name, int flags);

/* Releases the file reference, if FILEPUT_SYNC is set,
 * the actual cluster tree is synced to the disk, if
 * FILEPUT_UNLINK is specified, the on-disk representation
 * is unlinked and as the last reference goes away, the
 * file and its name is freed. */
#define FILEPUT_UNLINK 1
#define FILEPUT_SYNC 2
static void file_put(struct swfile *f, int flags);


/* Get a reference to the cluster containing offset using the
 * specified flags. Stores the offset of the cluster start in cstart.
 * Return values/flags like get_cluster. */
static struct swcluster *file_getcluster(struct swfile *f,
					 s64 offset, s64 *cstart,
					 int flags);

/* Get a reference to the cluster containing offset using the
 * specified flags. Stores the offset of the cluster start in cstart.
 * Return values/flags like get_cluster. If the cluster is shared,
 * copy it and replace it with the copy. */
static struct swcluster *file_getcluster_private(struct swfile *f,
						 s64 offset, s64 *cstart,
						 int flags);


/* Truncate the file to the given size. Returns 0 on success
 * and -1 on error. */
static int file_truncate(struct swfile *f, s64 size);


/* Insert count bytes from position spos in file sf into file df
 * at position dpos. Returns -1 on error, 0 on success. */
static int file_insert(struct swfile *df, s64 dpos,
		       struct swfile *sf, s64 spos, s64 count);

/* Replaces count bytes of file df from position dpos with
 * data from position spos out of file sf. Returns 0 on success,
 * -1 on error. */
static int file_replace(struct swfile *df, s64 dpos,
			struct swfile *sf, s64 spos, s64 count);

/* Removes count bytes from position dpos out of file df. Returns
 * 0 on success, -1 on error. */
static int file_cut(struct swfile *df, s64 dpos, s64 count);


#endif
