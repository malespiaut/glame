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


HASH(swfile, struct swfile, 8,
     (swfile->name == name),
     (name),
     (swfile->name),
     long name)

#define LOCKFILES do {} while (0)
#define UNLOCKFILES do {} while (0)

#define LOCKFILE(f) do {} while (0)
#define UNLOCKFILE(f) do {} while (0)


static int __file_open(long name, int flags);
static struct swfile *_file_creat(long name);
static struct swfile *_file_stat(long name);
static int _file_readclusters(struct swfile *f);
static int _file_writeclusters(struct swfile *f);




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
				c = cluster_get(CID(f->clusters, i),
						CLUSTERGET_READFILES,
						CSIZE(f->clusters, i));
				cluster_delfileref(c, f->name);
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

static int file_truncate(struct swfile *f, s64 size)
{
	/* FIXME */
	return -1;
}



/***************************************************************************
 * Internal functions.
 */

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
	f->open_cnt = 0;
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
	f->open_cnt = 0;
	f->clusters = NULL;
	hash_add_swfile(f);

	return f;
}

static int _file_readclusters(struct swfile *f)
{
	struct stat st;

	if (f->flags & SWF_DIRTY)
		PANIC("read into dirty state");
	if (!(f->flags & SWF_NOT_IN_CORE)) {
		DPRINTF("called with cluster tree already in core");
		return 0;
	}

	/* Load cluster tree. */
	if (fstat(f->fd, &st) == -1)
		PANIC("Umm");
	f->clusters = (struct ctree *)malloc(st.st_size);
	if (!f->clusters)
		return -1;
	if (read(f->fd, f->clusters, st.st_size) != st.st_size)
		PANIC("Umm");
	if (st.st_size != CTREESIZE(f->clusters->height))
		PANIC("Corrupted file?");

	f->flags &= ~SWF_NOT_IN_CORE;
	return 0;
}

static int _file_writeclusters(struct swfile *f)
{
	if ((f->flags & SWF_NOT_IN_CORE) || !(f->flags & SWF_DIRTY))
		PANIC("Umm..");

	if (f->fd == -1
	    && (f->fd = __file_open(f->name, O_RDWR|O_CREAT|O_EXCL)) == -1)
		PANIC("Inconsistent in-core/on-disk state");

	ftruncate(f->fd, CTREESIZE(f->clusters->height));
	lseek(f->fd, 0, SEEK_SET);
	if (write(f->fd, f->clusters, CTREESIZE(f->clusters->height))
	    != CTREESIZE(f->clusters->height))
		return -1;

	f->flags &= ~SWF_DIRTY;
	return 0;
}
