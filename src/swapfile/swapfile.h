#ifndef _SWAPFILE_H
#define _SWAPFILE_H

/*
 * swapfile.h
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

/* There are some generic issues for an implementation of the
 * swapfile API to make it efficient for the desired tasks:
 * - blocks need to be able to be shared between files
 * - blocks need to be able to be split and continue to be
 *   shared between files
 * Those requirements result in the following difficulties
 * an implementation will run into:
 * - maintaining a simple bitmask of unallocated blocks is not
 *   possible, as blocks may be shared, i.e. sort of an usage
 *   count is needed, not just a true/false state
 * - mmapping a file is usually not (efficient) possible - only
 *   one block at a time can be mmapped due to block splitting
 *   block start offsets are not aligned to a minimum block size
 *   and block sizes are not multiples of a minimum block size
 * - organizing files block allocation table as a simple linear
 *   list or as extends is not efficient because of the same
 *   block alignment/size issues, so a tree-like structure needs
 *   to be used for this - and in this tree the size of the blocks
 *   or their offsets in the file need to be stored
 * - because of the splitting requirement one need information
 *   about which files use a given block - i.e. a reverse block
 *   allocation table needs to be maintained (optimize for the
 *   single file case, O(n) operations are acceptable)
 * So an efficient implementation with respect to speed _and_
 * storage requirement for metadata is quite difficult.
 */

#include "glame_types.h"
#include "txn.h"



typedef void *swfd_t;                 /* open file cookie, like fd/FILE */
typedef struct { int dummy; } SWDIR;  /* cookie for open directory, like DIR */

struct sw_stat {
	long name;           /* file name */
	size_t size;         /* file size in bytes */
	int mode;            /* active protection */
	off_t offset;        /* current file pointer position */
	off_t cluster_start; /* start of current cluster */
	off_t cluster_end;   /* end of current cluster */
	size_t cluster_size; /* size of current cluster */
};



#ifdef __cplusplus
extern "C" {
#endif


/**********************************************************************
 * Initialization/cleanup
 */

/* Tries to open an existing swap file/partition.
 * Returns 0 on success, -1 on failure.
 * Failures can be
 *  - missing swap file/parition
 *  - in use swap
 *  - unclean swap */
int swapfile_open(char *name, int flags);

/* Closes and updates a previously opened swap file/partition
 * and marks it clean. */
void swapfile_close();

/* Tries to create an empty swapfile on name of size size. */
int swapfile_creat(char *name, size_t size);



/**********************************************************************
 * Operations on the swapfile namespace. Unlike a unix filesystem
 * the swapfile filesystem has names composed out of a single "long".
 * Also the swapfile name hierarchy is flat - i.e. no directories.
 * Names are >=0, negative values are reserved.
 * All namespace operations are atomic (i.e. thread safe) and not
 * undoable (well - just sw_unlink is not undoable).
 */

/* Deletes a name from the filesystem. Like unlink(2). */
int sw_unlink(long name);

/* Open the (flat) swapfile directory for reading. The stream
 * is positioned at the first file. Like opendir(3), but w/o
 * directory specification for obvious reason. */
SWDIR *sw_opendir();

/* As the namespace is rather simple the equivalent to readdir(3) is
 * just returning the names, no directory entry. Anything else
 * is like readdir(3). If no further entries are available, -1 is returned. */
long sw_readdir(SWDIR *dir);

/* Like closedir(3). */
int sw_closedir(SWDIR *d);


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
swfd_t sw_open(long name, int flags, txnid_t tid);

/* Closes a file descriptor. Like close(2). */
int sw_close(swfd_t fd);

/* Changes the size of the file fd like ftruncate(2). */
int sw_ftruncate(swfd_t fd, off_t length);

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
ssize_t sw_sendfile(swfd_t out_fd, swfd_t in_fd, size_t count, int mode);

/* Update the file pointer position like lseek(2). */
off_t sw_lseek(swfd_t fd, off_t offset, int whence);

/* Obtain information about the file - works like fstat(2), but
 * with different struct stat. Also included is information about
 * the actual (file pointer position, see sw_lseek) cluster which
 * can be mapped using sw_mmap. */
int sw_fstat(swfd_t fd, struct sw_stat *buf);

/* Maps the actual (file pointer position, see sw_lseek and sw_fstat)
 * cluster into memory with parameters like mmap(2) - no size/offset
 * as they are determined by the actual cluster offset/size. */
void *sw_mmap(void *start, int prot, int flags, swfd_t fd);

/* Unmaps a previously mapped part of a file. Like munmap(2). */
int sw_munmap(void *start);


#ifdef __cplusplus
}
#endif

#endif
