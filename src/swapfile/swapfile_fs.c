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
#include <dirent.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include "list.h"
#include "hash.h"
#include "swapfile.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* The global "state" of the swapfile and its locks. The
 * locks are for namespace operations atomicy. */
static struct {
	int fatfd;
	char *files_base;
	char *clusters_data_base;
	char *clusters_meta_base;
	int maxnamelen;
	struct list_head fds;
} swap;

#define LOCK do {} while (0)
#define UNLOCK do {} while (0)


/* We want to have a clean (static) namespace... */
#include "swfs_ctree.c"
#include "swfs_cluster.c"
#include "swfs_file.c"


/* The FILE equivalent - the swapfile filedescriptor. No locking here
 * as it is done by fd->file locking. Also the user has to ensure
 * consistency himself. */
struct swfd {
	struct list_head list;
	struct swfile *file;
	int mode;
	s64 offset;          /* file pointer position */
	s64 c_start;         /* start of current cluster */
	struct swcluster *c; /* current cluster */
};
#define SWFD(s) ((struct swfd *)(s))
#define list_add_swfd(fd) list_add(&(fd)->list, &swap.fds)
#define list_del_swfd(fd) list_del_init(&(fd)->list)



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
	char str[512];
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
	swap.files_base = strdup(name);
	sprintf(str, "%s/clusters.data", name);
	swap.clusters_data_base = strdup(str);
	sprintf(str, "%s/clusters.meta", name);
	swap.clusters_meta_base = strdup(str);
	swap.maxnamelen = strlen(swap.clusters_meta_base)+32;
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
	sprintf(s, "%s/clusters.data", name);
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

/* Deletes a name from the filesystem. Like unlink(2). */
int sw_unlink(long name)
{
	struct swfile *f;

	/* No locking needed!? */
	if (!(f = file_get(name, FILEGET_READCLUSTERS)))
		return -1;
	file_put(f, FILEPUT_UNLINK);

	return 0;
}


/* Open the (flat) swapfile directory for reading. The stream
 * is positioned at the first file. Like opendir(3), but w/o
 * directory specification for obvious reason. */
SWDIR *sw_opendir()
{
	return (SWDIR *)opendir(swap.files_base);
}

/* As the namespace is rather simple the equivalent to readdir(3) is
 * just returning the names, no directory entry. Anything else
 * is like readdir(3). If no further entries are available, -1 is returned. */
long sw_readdir(SWDIR *d)
{
    	struct dirent *e;
	long name;

	while ((e = readdir((DIR *)d))) {
		if (sscanf(e->d_name, "%li", &name) == 1)
		    	return name;
	}
	return -1;
}

/* Like closedir(3). */
int sw_closedir(SWDIR *d)
{
    	return closedir((DIR *)d);
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
swfd_t sw_open(long name, int flags, txnid_t tid)
{
	struct swfile *f;
	struct swfd *fd;

	/* Detect illegal/unsupported flag combinations. */
	if (((flags & O_EXCL) && !(flags & O_CREAT))
	    || (flags & ~(O_CREAT|O_EXCL|O_TRUNC|O_RDONLY|O_WRONLY|O_RDWR))) {
		errno = EINVAL;
		return NULL;
	}

	/* Alloc fd. */
	if (!(fd = (struct swfd *)malloc(sizeof(struct swfd))))
		return NULL;

	LOCK;
	f = file_get(name, 0);

	/* File is unlinked. */
	if (f && (f->flags & SWF_UNLINKED)) {
		if (flags & O_CREAT)
			errno = EAGAIN;
		else
			errno = ENOENT;
		goto err;
	}

	/* File exists and is requested to be created exclusively? */
	if (f && (flags & O_EXCL)) {
		errno = EEXIST;
		goto err;
	}
	/* FIle does not exist and is not requested to being created? */
	if (!f && !(flags & O_CREAT)) {
		errno = ENOENT;
		goto err;
	}

	/* Create file, if necessary and requested. */
	if (!f && (flags & O_CREAT)) {
		if (!(f = file_get(name, FILEGET_CREAT)))
			goto err; /* FIXME: which errno? */
	}
	/* Truncate file, if requested. */
	if (flags & O_TRUNC)
		file_truncate(f, 0);
	UNLOCK;

	INIT_LIST_HEAD(&fd->list);
	fd->file = f;
	fd->mode = flags & (O_RDONLY|O_WRONLY|O_RDWR);
	fd->offset = 0;
	fd->c = NULL;
	fd->c_start = 0;
	list_add_swfd(fd);

	errno = 0;
	return (swfd_t)fd;

 err:
	if (f)
		file_put(f, 0);
	UNLOCK;
	free(fd);
	return NULL;
}

/* Closes a file descriptor. Like close(2). */
int sw_close(swfd_t fd)
{
	struct swfd *_fd = SWFD(fd);

	/* FIXME: locking against broken close() close() on same fd. */
	list_del_swfd(_fd);
	cluster_put(_fd->c, 0);
	file_put(_fd->file, 0);
	free(_fd);

	return 0;
}

/* Changes the size of the file fd like ftruncate(2). */
int sw_ftruncate(swfd_t fd, off_t length)
{
	struct swfd *_fd = SWFD(fd);
	int res;

	/* Check if its a valid truncate command. */
	if (!(_fd->mode & O_WRONLY)) {
		errno = EPERM;
		return -1;
	}
	if (length < 0) {
		errno = EINVAL;
		return -1;
	}

	/* Umm, what should happen to the file pointer posistion? */
	if (_fd->offset >= length) {
		cluster_put(_fd->c, 0);
		_fd->c = NULL;
	}

	LOCK;
	res = file_truncate(_fd->file, length);
	UNLOCK;

	/* File pointer position may be unspecified now... */
	return res;
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
	struct swfd *_fd = SWFD(fd);

	/* We convert all seeks to SEEK_SET seeks which can be
	 * handled by common code. */
	switch (whence) {
	case SEEK_END:
		offset = _fd->file->clusters->total_size + offset - 1;
		break;
	case SEEK_CUR:
		offset += _fd->offset;
		break;
	case SEEK_SET:
		break;
	default:
		errno = EINVAL;
		return -1;
	}

	/* Just offset query? */
	if (_fd->offset == offset)
		return _fd->offset;
	/* Seek inside the current cluster? */
	if (_fd->c && _fd->c_start <= offset && _fd->c_start+_fd->c->size > offset) {
		_fd->offset = offset;
		return _fd->offset;
	}

	/* Now we really need to seek. Ugh. Linear search. */
	cluster_put(_fd->c, 0);
	_fd->c = file_getcluster(_fd->file, offset, &_fd->c_start, 0);
	if (!_fd->c)
		_fd->c_start = -1;
	_fd->offset = offset;

	return _fd->offset;
}

/* Obtain information about the file - works like fstat(2), but
 * with different struct stat. Also included is information about
 * the actual (file pointer position, see sw_lseek) cluster which
 * can be mapped using sw_mmap. */
int sw_fstat(swfd_t fd, struct sw_stat *buf)
{
	struct swfd *_fd = SWFD(fd);

	if (!buf)
		return -1;
	buf->name = _fd->file->name;
	buf->size = _fd->file->clusters->total_size;
	buf->mode = _fd->mode;
	buf->offset = _fd->offset;
	if (_fd->c) {
		buf->cluster_start = _fd->c_start;
		buf->cluster_end = _fd->c_start + _fd->c->size;
		buf->cluster_size = _fd->c->size;
	} else {
		buf->cluster_start = -1;
		buf->cluster_end = -1;
		buf->cluster_size = -1;
	}
	return 0;
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



static struct swfd *_sw_open(long name, int flags)
{
	return NULL;
}

static void _sw_close(struct swfd *fd)
{
}
