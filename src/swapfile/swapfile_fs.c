/*
 * swapfile_fs.c
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
/* Swapfile implementation using a native filesystem. The idea is to
 * implement cluster sharing by having seperate native files for clusters.
 * A swapfile file consists of a file containing the used cluster numbers.
 * A cluster consists of two files, one for the data and one for listing
 * all files that use this cluster.
 * Main directory layout is
 *   clusters/
 *     {long}
 *     {long}.meta
 *   {long}.meta
 * The {long} indexed clusters and files are hashed and the metadata
 * is loaded on demand.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include "list.h"
#include "hash.h"
#include "swapfile.h"


/* Main directory (swapfile) is the FAT
 *   Every subdirectory is a file (named by the long) which
 *   contains the clusters as files (named by the cluster id)
 *   and one file describing the connection of the clusters.
 * The special directory clusters contains metadata of all used
 * clusters (list of files that use the cluster).
 */

struct swfile;
struct swcluster;

/* Cluster instance, flags are
 *   SWC_DIRTY - files list is dirty */
#define SWC_DIRTY 1
struct swcluster {
	struct swcluster *next_swcluster_hash;
	struct swcluster **pprev_swcluster_hash;
	long name;
	size_t size;
	int flags;     /* SWC_* */
	int files_cnt; /* number of files that use this cluster */
	long *files;   /* list of files that use this cluster */
	int usage;     /* number of references to this struct cluster */
};
HASH(swcluster, struct swcluster, 10,
     (swcluster->name == name),
     (name),
     (swcluster->name),
     long name)

/* File instance, flags are
 *   SWF_NOT_IN_CORE - swfile.{size,cluster_cnt,clusters} is not filled in
 *   SWF_UNLINK_ON_CLOSE - unlink the file after last close
 *   SWF_DIRTY - cluster list is dirty */
#define SWF_NOT_IN_CORE 1
#define SWF_UNLINK_ON_CLOSE 2
#define SWF_DIRTY 4
struct swfile {
	struct swfile *next_swfile_hash;
	struct swfile **pprev_swfile_hash;
	long name;
	int flags;    /* SWF_* */
	int usage;    /* number of references to this struct swfile */
	int open_cnt; /* number of current opens (fd's) */

	size_t size;     /* current total file size */
	int cluster_cnt; /* number of clusters in the file */
	long *clusters;  /* clusters of the file (in order) */
};
HASH(swfile, struct swfile, 8,
     (swfile->name == name),
     (name),
     (swfile->name),
     long name)


struct swfd {
	struct list_head list;
	struct swfile *file;
	int mode;
	off_t offset;       /* file pointer position */
	struct swcluster *c;  /* current cluster */
};
#define SWFD(s) ((struct swfd *)(s))

static struct {
	int fatfd;
	char *base;
	int maxnamelen;
	struct list_head fds;
} swap;


#define LOCK
#define UNLOCK


static struct swfile *_get_file(long name, int need_clusters);
static void _put_file(struct swfile *f);
static struct swfile *_stat_file(long name);
static int _read_file(struct swfile *f);
static int _write_file(struct swfile *f);

static struct swcluster *_get_cluster(long name);
static void _put_cluster(struct swcluster *c);
static struct swcluster *_read_cluster(long name);
static int _write_cluster(struct swcluster *c);

static struct swfd *_sw_open(long name, int flags);
static void _sw_close(struct swfd *fd);


/**********************************************************************
 * Initialization/cleanup
 */

/* Tries to open an existing swap file/partition.
 * Returns 0 on success, -1 on failure.
 * Failures can be
 *  - missing swap file/parition
 *  - in use swap
 *  - unclean swap */
int swapfile_open(char *name, int flags)
{
	struct stat s;
	int fd;

	/* Open name and check if its a directory. */
	fd = open(name, 0);
	if (fd == -1)
		goto err;
	if (fstat(fd, &s) == -1)
		goto err_close;
	if (!S_ISDIR(s.st_mode))
		goto err_close;

	/* Initialize swap structure. */
	swap.fatfd = fd;
	swap.base = strdup(name);
	swap.maxnamelen = strlen(name)+32;
	INIT_LIST_HEAD(&swap.fds);

	return 0;

 err_close:
	close(fd);
 err:
	return -1;
}

/* Closes and updates a previously opened swap file/partition
 * and marks it clean. */
void swapfile_close()
{
	struct swfd *f;

	/* close all files */
	while ((f = list_gethead(&swap.fds, struct swfd, list))) {
		sw_close(f);
	}
	close(swap.fatfd);
}

/* Tries to create an empty swapfile on name of size size. */
int swapfile_creat(char *name, size_t size)
{
	char *s;

	if (mkdir(name, 0777) == -1)
		return -1;
	s = alloca(strlen(name)+32);
	sprintf(s, "%s/clusters.meta", name);
	if (mkdir(s, 0777) == -1)
		return -1;
	sprintf(s, "%s/clusters", name);
	if (mkdir(s, 0777) == -1)
		return -1;

	return 0;
}



/**********************************************************************
 * Operations on the swapfile namespace. Unlike a unix filesystem
 * the swapfile filesystem has names composed out of a single "long".
 * Also the swapfile name hierarchy is flat - i.e. no directories.
 * Names are >=0, negative values are reserved.
 * All namespace operations are atomic (i.e. thread safe) and not
 * undoable (well - just sw_unlink is not undoable).
 */

/* Deletes a name from the filesystem. Like unlink(2).
 * We need to (atomically!)
 * - open the file
 * - mark it unlink-on-close
 * - close it */
int sw_unlink(long name)
{
	struct swfd *fd;

	LOCK;
	if (!(fd = _sw_open(name, O_RDWR))) {
		UNLOCK;
		return -1;
	}
	fd->file->flags |= SWF_UNLINK_ON_CLOSE;
	_sw_close(fd);
	UNLOCK;

	return 0;
}

/* Creates a new name to the file with name oldname. Like link(2).
 * We need to (atomically!)
 * - open the file
 * - create the new file
 * - link(2) all clusters
 * - update the clusters metadata */
int sw_link(long oldname, long newname)
{
	return -1;
}

/* As the namespace is rather simple the equivalent to readdir(3) is
 * just returning the names in sorted order - just start with name
 * (long)-1 and sw_readdir will get you the first used name. -1 is
 * returned, if no further entry is available. */
long sw_readdir(long previous)
{
	return -1;
}


/**********************************************************************
 * Operations on a single file. Files are organized in variable sized
 * clusters. Access to the file is limited to mapping those clusters.
 */

/* Open a file like open(2) - flags can be O_CREAT, O_EXCL,
 * O_RDWR, O_RDONLY, O_WRONLY with same semantics as open(2).
 * Returns a file descriptor on success, -1 on error.
 * The optional transaction id is used for all operations
 * operating on the swfd_t, they can be undone and redone
 * this way. */
swfd_t sw_open(long name, int flags, tid_t tid)
{
	return NULL;
}

/* Closes a file descriptor. Like close(2). Also closes the transaction
 * given to sw_open. */
int sw_close(swfd_t fd)
{
	return -1;
}

/* Changes the size of the file fd like ftruncate(2). */
int sw_ftruncate(swfd_t fd, off_t length)
{
	return -1;
}

/* Tries to copy count bytes from the current position of in_fd
 * to the current position of out_fd (updating both file pointer
 * positions). The actual number of copied bytes is returned, or
 * -1 on an error.
 * Two different modes are supported (may be or'ed together):
 * - SWSENDFILE_INSERT inserts into, rather than overwrites/extends
 *   the destination file
 * - SWSENDFILE_CUT removes copied data from the source file
 * The destination file descriptor may be SW_NOFILE, in that case
 * no data is actually written (useful with SWSENDFILE_CUT). */
ssize_t sw_sendfile(swfd_t out_fd, swfd_t in_fd, size_t count, int mode)
{
	return -1;
}

/* Update the file pointer position like lseek(2). */
off_t sw_lseek(swfd_t fd, off_t offset, int whence)
{
	return -1;
}

/* Obtain information about the file - works like fstat(2), but
 * with different struct stat. Also included is information about
 * the actual (file pointer position, see sw_lseek) cluster which
 * can be mapped using sw_mmap. */
int sw_fstat(swfd_t fd, struct sw_stat *buf)
{
	return -1;
}

/* Maps the actual (file pointer position, see sw_lseek and sw_fstat)
 * cluster into memory with parameters like mmap(2) - no size/offset
 * as they are determined by the actual cluster offset/size. */
void *sw_mmap(void *start, int prot, int flags, swfd_t fd)
{
	return NULL;
}

/* Unmaps a previously mapped part of a file. Like munmap(2). */
int sw_munmap(void *start)
{
	return -1;
}



/******************************************************************
 * Internal stuff.
 */

static struct swfile *_get_file(long name, int need_clusters)
{
	struct swfile *f;

	if (!(f = hash_find_swfile(name)))
		f = _stat_file(name);
	else
		f->usage++;
	if (need_clusters && (f->flags & SWF_NOT_IN_CORE))
		_read_file(f);
	return f;
}

static void _put_file(struct swfile *f)
{
	if (--(f->usage) == 0) {
		hash_remove_swfile(f);
		if (f->flags & SWF_DIRTY)
			_write_file(f);
		if (f->clusters)
			free(f->clusters);
		free(f);
	}
}

static struct swfile *_stat_file(long name)
{
	struct swfile *f;
	char *s;
	int fd;

	/* Open file metadata. */
	s = alloca(swap.maxnamelen);
	sprintf(s, "%s/%li", swap.base, name);
	if ((fd = open(s, O_RDONLY)) == -1)
		return NULL;
	close(fd);

	/* Allocate file structure. */
	if (!(f = (struct swfile *)malloc(sizeof(struct swfile))))
		return NULL;
	hash_init_swfile(f);
	f->name = name;
	f->flags = SWF_NOT_IN_CORE;
	f->usage = 1;
	f->open_cnt = 0;
	f->size = 0;
	f->cluster_cnt = 0;
	f->clusters = NULL;

	hash_add_swfile(f);
	return f;
}

static int _read_file(struct swfile *f)
{
	char *s;
	struct stat st;
	int fd, i;
	struct swcluster *c;

	if (!(f->flags & SWF_NOT_IN_CORE))
		return 0;

	/* Open file metadata. */
	s = alloca(swap.maxnamelen);
	sprintf(s, "%s/%li", swap.base, f->name);
	if ((fd = open(s, O_RDONLY)) == -1)
		goto err;

	/* Load cluster list. */
	if (fstat(fd, &st) == -1)
		goto err_close;
	f->cluster_cnt = st.st_size/sizeof(long);
	f->clusters = (long *)malloc(st.st_size);
	if (!f->clusters)
		goto err_close;
	if (read(fd, f->clusters, st.st_size) != st.st_size)
		goto err_free;

	/* Stat the clusters, compute file size. */
	for (i=0; i<f->cluster_cnt; i++) {
		c = _get_cluster(f->clusters[i]);
		f->size += c->size;
		_put_cluster(c);
	}

	f->flags &= ~SWF_NOT_IN_CORE;
	return 0;

 err_free:
	free(f->clusters);
 err_close:
	close(fd);
 err:
	return -1;
}

static int _write_file(struct swfile *f)
{
	char *s;
	int fd;

	s = alloca(swap.maxnamelen);
	sprintf(s, "%s/%li", swap.base, f->name);
	if ((fd = open(s, O_RDWR|O_CREAT, 0666)) == -1)
		return -1;
	if (!(f->flags & SWF_NOT_IN_CORE)) {
		ftruncate(fd, f->cluster_cnt*sizeof(long));
		write(fd, f->clusters, f->cluster_cnt*sizeof(long));
	}
	close(fd);

	return 0;
}


static struct swcluster *_get_cluster(long name)
{
	struct swcluster *c;

	if (!(c = hash_find_swcluster(name)))
		c = _read_cluster(name);
	else
		c->usage++;
	return c;
}

static void _put_cluster(struct swcluster *c)
{
	if (--(c->usage) == 0) {
		hash_remove_swcluster(c);
		if (c->flags & SWC_DIRTY)
			_write_cluster(c);
		free(c->files);
		free(c);
	}
}

static struct swcluster *_read_cluster(long name)
{
	struct swcluster *c;
	char *s;
	int fd;
	struct stat st;

	/* Open cluster metadata. */
	s = alloca(swap.maxnamelen);
	sprintf(s, "%s/clusters/%li", swap.base, name);
	if (stat(s, &st) == -1)
		goto err;
	sprintf(s, "%s/clusters.meta/%li", swap.base, name);
	fd = open(s, O_RDONLY);
	if (fd == -1)
		goto err;

	/* Allocate cluster structure. */
	if (!(c = (struct swcluster *)malloc(sizeof(struct swcluster))))
		goto err_close;
	hash_init_swcluster(c);
	c->name = name;
	c->usage = 1;
	c->size = st.st_size;
	c->flags = 0;
	c->files_cnt = 0;
	c->files = NULL;

	/* Read the files list. */
	if (fstat(fd, &st) == -1)
		goto err_free;
	c->files_cnt = st.st_size/sizeof(long);
	c->files = (long *)malloc(st.st_size);
	if (!c->files)
		goto err_free;
	if (read(fd, c->files, st.st_size) != st.st_size)
		goto err_free2;

	/* Stat the files?
	for (i=0; i<c->files_cnt; i++) {
		_get_file(c->files[i], 0);
	}
	*/

	hash_add_swcluster(c);
	return c;

 err_free2:
	free(c->files);
 err_free:
	free(c);
 err_close:
	close(fd);
 err:
	return NULL;
}

static int _write_cluster(struct swcluster *c)
{
	char *s;
	int fd;

	s = alloca(swap.maxnamelen);
	sprintf(s, "%s/clusters.meta/%li", swap.base, c->name);
	if ((fd = open(s, O_RDWR|O_CREAT, 0666)) == -1)
		return -1;
	ftruncate(fd, c->files_cnt*sizeof(long));
	write(fd, c->files, c->files_cnt*sizeof(long));
	close(fd);

	return 0;
}


static struct swfd *_sw_open(long name, int flags)
{
	return NULL;
}

static void _sw_close(struct swfd *fd)
{
}
