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
	off_t offset;         /* file pointer position */
	struct swcluster *c;  /* current cluster */
	off_t c_start;        /* file pointer position of start of current cluster */
};
#define SWFD(s) ((struct swfd *)(s))
#define list_add_swfd(fd) list_add(&(fd)->list, &swap.fds)
#define list_del_swfd(fd) list_del_init(&(fd)->list)

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
static struct swfile *_creat_file(long name);
static int _truncate_file(struct swfile *f, size_t size);

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
	return -1; /* FIXME - necessary?? sw_open(O_CREAT) && sw_sendfile(dest, source)! */
}

/* Open the (flat) swapfile directory for reading. The stream
 * is positioned at the first file. Like opendir(3), but w/o
 * directory specification for obvious reason. */
SWDIR *sw_opendir()
{
	return (SWDIR *)opendir(swap.base);
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
swfd_t sw_open(long name, int flags, tid_t tid)
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
	f = _get_file(name, 0);

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
	if (!f && (flags & O_CREAT))
		f = _creat_file(name);
	/* Truncate file, if requested. */
	if (flags & O_TRUNC)
		_truncate_file(f, 0);
	UNLOCK;

	INIT_LIST_HEAD(&fd->list);
	fd->file = f;
	fd->mode = flags & (O_RDONLY|O_WRONLY|O_RDWR);
	fd->offset = 0;
	fd->c = NULL;
	fd->c_start = 0;
	if (f->cluster_cnt > 0)
		fd->c = _get_cluster(f->clusters[0]);
	list_add_swfd(fd);

	errno = 0;
	return (swfd_t)fd;

 err:
	if (f)
		_put_file(f);
	UNLOCK;
	free(fd);
	return NULL;
}

/* Closes a file descriptor. Like close(2). Also closes the transaction
 * given to sw_open. */
int sw_close(swfd_t fd)
{
	struct swfd *_fd = SWFD(fd);

	_put_file(_fd->file);
	_put_cluster(_fd->c);
	list_del_swfd(_fd);
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
		_put_cluster(_fd->c);
		_fd->c = NULL;
	}

	LOCK;
	res = _truncate_file(_fd->file, length);
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
		offset = _fd->file->size + offset - 1;
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
	LOCK;
	_put_cluster(_fd->c);
	_fd->c_start = 0;
	_fd->c = _get_cluster(_fd->file->clusters[0]);
	do {

	} while (_fd->c && (_fd->c_start < offset || _fd->c_start+_fd->c->size >= offset));
	if (!_fd->c)
		_fd->c_start = -1;
	UNLOCK;

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
	buf->size = _fd->file->size;
	buf->mode = _fd->mode;
	buf->offset = _fd->offset;
	if (_fd->c) {
		buf->cluster_start = _fd->c_start;
		buf->cluster_end = _fd->c_start + _fd->c->size;
		buf->cluster_size = _fd->c->size;
		buf->cluster_nlink = _fd->c->usage;
	} else {
		buf->cluster_start = -1;
		buf->cluster_end = -1;
		buf->cluster_size = -1;
		buf->cluster_nlink = -1;
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
	if (!f)
		return;
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

static struct swfile *_creat_file(long name)
{
	return NULL;
}

static int _truncate_file(struct swfile *f, size_t size)
{
	return -1;
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
	if (!c)
		return;
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






/* Cluster binary tree routines. The clusters of a file are
 * organized in a binary tree consisting of nodes that contain
 * the size of its siblings:
 * t[0] = height of the tree (0, 1, 2... - example is 2)
 * t[1]                       total sizeA
 * t[2][3]             sizeB                sizeA-sizeB
 * t[4][5][6][7]    sC     sB-sC         sD       sA-sB-sD
 * t[8][9][10][11]  cID     cID          cID         cID
 * (trees with larger height constructed the same way)
 * For tree traversal a left branch is done with i->2*i, a right
 * branch with i->2*i+1. The height of the tree is stored as the
 * 0th component of the array containing the tree.
 * The tree is always filled "from the left", i.e. leaf nodes
 * span the tree.
 */

/* Finds the cluster with offset inside it, returns the
 * offset of the cluster id in the tree array. Copies
 * the cluster start position and its size to cstart/csize. */
static int _find_cluster(long *tree, off_t offset,
			 off_t *cstart, size_t *csize)
{
	long height = tree[0];
	off_t pos;
	int i;

	pos = 0;
	i = 1;
	while (height--) {
		if (pos+tree[2*i] > offset) {
			i = 2*i;
		} else {
			pos += tree[2*i];
			i = 2*i+1;
		}
	}
	if (pos > offset || pos+tree[i] <= offset)
		return -1;
	if (cstart)
		*cstart = pos;
	if (csize)
		*csize = tree[i];
	return i+(1<<tree[0]);
}

/* Correct the tree structure to correct the size of the cluster
 * which id is at pos in the tree structure. */
static void _resize_cluster(long *tree, int pos, long size)
{
	int height, i;
	long delta;

	height = tree[0];
	i = pos - (1<<height);
	delta = size-tree[i];
	do {
		tree[i] += delta;
		i >>= 1;
	} while (height--);
}

/* Inserts the clusters cid[] with sizes csize[] at tree array position pos
 * and returns the tree pointer as it may be necessary to reallocate
 * it to make room for the extra clusters. */
static long *_insert_clusters(long *tree, int pos, int cnt,
			      long *cid, off_t *csize)
{
	/* FIXME: resize tree, find last used position */
	int lastpos = 11223;

	/* Move right end of tree by cnt positions to the right. */
	int i, j, k, l;
	i = lastpos;
	j = lastpos+cnt;
	k = lastpos-pos+1;
	l = cnt-1;
	while (k--) {
		cid[l] = tree[i];
		csize[l] = tree[i-(1<<tree[0])];
		_resize_cluster(tree, i, 0);
		tree[j] = cid[l];
		_resize_cluster(tree, j, csize[l]);
		i--;
		j--;
		l--;
	}
	/* Insert clusters from j downward. */
	while (cnt--) {
		tree[j] = cid[cnt];
		_resize_cluster(tree, j, csize[cnt]);
		j--;
	}

	return tree;
}

static long *_remove_clusters(long *tree, int pos, int cnt,
			      long *cid, off_t *csize)
{
	/* First move (at most) cnt clusters after pos+cnt
	 * by cnt positions to the left, copying cluster
	 * information to cid/csize */
	long newsize, newid;
	int i = 0;
	while (tree[pos-(1<<tree[0])] != 0) {
		if (i<cnt) {
			cid[i] = tree[pos];
			csize[i] = tree[pos-(1<<tree[0])];
		}
		if (pos+cnt < 1<<tree[0] /* FIXME */) {
			newsize = tree[pos+cnt-(1<<tree[0])];
			newid = tree[pos+cnt];
		} else {
			newsize = 0;
			newid = -1;
		}
		tree[pos] = newid;
		_resize_cluster(tree, pos, newsize);
		i++;
		pos++;
	}

	/* Second, resize tree, if applicable. */
	/* FIXME */
	return tree;
}

/* FIXME: try to code _add_height(long *tree, int cnt) */
static long *_double_height(long *tree)
{
	int i, j, cnt, height;
	long *dest;

	dest = (long *)malloc(sizeof(long)*((1<<(tree[0]+1)) + (1<<tree[0])));
	if (!dest)
		return NULL;

	/* Fill tree height and total size */
	dest[0] = tree[0]+1;
	dest[1] = tree[1];
	/* Copy nodes, old tree is completely left child of root. */
	height = 0;
	i = 1; j = 2;
	while (height < dest[0]) {
		cnt = 1<<height;
		while (cnt--)
			dest[j++] = tree[i++];
		cnt = 1<<height;
		while (cnt--)
			dest[j++] = 0;
		height++;
	}
	/* Copy leaf nodes. */
	cnt = 1<<tree[0];
	while (cnt--)
		dest[j++] = tree[i++];
	cnt = 1<<tree[0];
	while (cnt--)
		dest[j++] = 0;

	return dest;
}
