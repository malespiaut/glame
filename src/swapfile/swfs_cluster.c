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
#include <unistd.h>
#include <fcntl.h>
#include "util.h"
#include "hash.h"
#include "swfs_cluster.h"


HASH(swcluster, struct swcluster, 10,
     (swcluster->name == name),
     (name),
     (swcluster->name),
     long name)

#define LOCKCLUSTERS do {} while (0)
#define UNLOCKCLUSTERS do {} while (0)

#define LOCKCLUSTER(c) do {} while (0)
#define UNLOCKCLUSTER(c) do {} while (0)


static int __cluster_data_open(long name, int flags);
static int __cluster_meta_open(long name, int flags);
static struct swcluster *_cluster_creat(long name);
static struct swcluster *_cluster_stat(long name, s32 known_size);
static int _cluster_readfiles(struct swcluster *c);
static int _cluster_writefiles(struct swcluster *c);


static struct swcluster *cluster_get(long name, int flags, s32 known_size)
{
	struct swcluster *c;

	LOCKCLUSTERS;
	if (!(c = hash_find_swcluster(name)))
		c = _cluster_stat(name, known_size);
	else
		c->usage++;
	UNLOCKCLUSTERS;
	if (!c)
		return NULL;

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
	char s[256];

	if (!c)
		return;

	/* Release the reference and do resouece cleanup, if
	 * this was the last one. */
	LOCKCLUSTERS;
	if (--(c->usage) == 0) {
		hash_remove_swcluster(c);
		UNLOCKCLUSTERS;

		/* Close the data fd. */
		if (c->fd != -1)
			close(c->fd);

		/* If there are no users (files) left, unlink the
		 * on-disk representations. */
		if (!(c->flags & SWC_NOT_IN_CORE)
		    && c->files_cnt == 0) {
			snprintf(s, 255, "%s/%li", swap.clusters_meta_base,
				 c->name);
			unlink(s);
			snprintf(s, 255, "%s/%li", swap.clusters_data_base,
				 c->name);
			unlink(s);
		} else if (c->flags & SWC_DIRTY)
			_cluster_writefiles(c);
		if (c->files)
			free(c->files);
		free(c);
		return;
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

static int cluster_addfileref(struct swcluster *c, long file)
{
	LOCKCLUSTER(c);
	/* Bring in the files list, if necessary. */
	if (c->flags & SWC_NOT_IN_CORE)
		_cluster_readfiles(c);

	/* Re-alloc the files array. - FIXME: inefficient */
	if (!(c->files = realloc(c->files, (c->files_cnt+1)*sizeof(long))))
		PANIC("cannot realloc files array");

	/* Append the file and fix the files count. */
	c->files[c->files_cnt] = file;
	c->files_cnt++;
	UNLOCKCLUSTER(c);

	return 0;
}

static int cluster_delfileref(struct swcluster *c, long file)
{
	int i;

	LOCKCLUSTER(c);
	/* Bring in the files list, if necessary. */
	if (c->flags & SWC_NOT_IN_CORE)
		_cluster_readfiles(c);

	/* Search file from the back (this is most cache friendly
	 * for the necessary array move afterwards. */
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

static struct swcluster *_cluster_creat(long name)
{
	struct swcluster *c;

	/* Allocate cluster structure. */
	if (!(c = (struct swcluster *)malloc(sizeof(struct swcluster))))
		return NULL;
	hash_init_swcluster(c);
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

static struct swcluster *_cluster_stat(long name, s32 known_size)
{
	struct swcluster *c;
	struct stat dstat;
	char sd[256];

	/* Allocate cluster structure. */
	if (!(c = (struct swcluster *)malloc(sizeof(struct swcluster))))
		return NULL;

	/* Stat metadata and data. */
	if (known_size == -1) {
		snprintf(sd, 255, "%s/%li", swap.clusters_data_base, name);
		if (stat(sd, &dstat) == -1) {
			free(c);
			return NULL;
		}
		known_size = dstat.st_size;
	}

	hash_init_swcluster(c);
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

static int _cluster_readfiles(struct swcluster *c)
{
	int fd;
	struct stat stat;

	if (c->flags & SWC_DIRTY)
		PANIC("read into dirty state");
	if (!(c->flags & SWC_NOT_IN_CORE)) {
		DPRINTF("called with files already in core");
		return 0;
	}

	/* Load files list. */
	if ((fd = __cluster_meta_open(c->name, O_RDWR)) == -1
	    || fstat(fd, &stat) == -1)
		PANIC("metadata vanished under us");
	if (!(c->files = (long *)malloc(stat.st_size)))
		return -1;
	c->files_cnt = stat.st_size/sizeof(long);
	if (read(fd, c->files, stat.st_size) != stat.st_size)
		PANIC("cannot read cluster metadata");
	close(fd);

	c->flags &= ~SWC_NOT_IN_CORE;
	return 0;
}

static int _cluster_writefiles(struct swcluster *c)
{
	int fd;

	if (!(c->flags & SWC_DIRTY)) {
		DPRINTF("called non dirty");
		return 0;
	}
	if (c->flags & SWC_NOT_IN_CORE)
		PANIC("called with files not in core");

	if ((fd = __cluster_meta_open(c->name, O_RDWR|O_CREAT)) == -1
	    || ftruncate(fd, c->files_cnt*sizeof(long)) == -1
	    || write(fd, c->files, c->files_cnt*sizeof(long)) != c->files_cnt*sizeof(long))
		PANIC("cannot write cluster metadata");
	close(fd);

	c->flags &= ~SWC_DIRTY;
	return 0;
}

