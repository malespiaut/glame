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


struct cluster_s;
typedef struct cluster_s cluster_t;

struct filecluster_s;
typedef struct filecluster_s filecluster_t;

typedef int fileid_t;

#include "swapfileI.h"



#ifdef __cplusplus
extern "C" {
#endif

/**************************
 * init
 */

/* Tries to open an existing swap file/partition.
 * Returns 0 on success, -1 on failure.
 * Failures can be
 *  - missing swap file/parition
 *  - in use swap
 *  - unclean swap
 */
int swap_open(char *name, int flags);

/* Closes and updates a previously opened swap file/partition
 * and marks it clean
 */
void swap_close();



/**************************
 * file
 */

/* Creates a new file and reserves size space for data (initially 0).
 * Returns file id or -1 on error */
fileid_t file_alloc(off_t size);

/* Creates a new _ro_(!) copy of the file.
 * Returns file id or -1 on error.
 * THIS WILL MARK fid RO, TOO! */
fileid_t file_copy(fileid_t fid);

/* Copies part [pos...pos+size] of fid into a new file
 * and returns its file id.
 * Returns -1 on error.
 * RETURNED FILE IS RO!
 * THIS WILL MARK fid RO, TOO! */
fileid_t file_copy_part(fileid_t fid, off_t pos, off_t size);

/* Deletes a file and its corresponding data */
void file_unref(fileid_t fid);

/* get size of file */
off_t file_size(fileid_t fid);

/* Changes the size (creating trailing 0s or truncating the tail)
 * of the file. Works only on rw files, NOT UNDOABLE! */
int file_truncate(fileid_t fid, off_t size);


/* tool to loop through the set of files in swapfile */
fileid_t file_next(fileid_t fid);


/* Inserts file into fid at position pos (i.e. first byte of
 * file will be at position pos of fid).
 * Returns -1 on error.
 * THIS WILL MARK fid RO!
 * THIS WILL EAT file! YOU HAVE TO file_copy file, IF
 * YOU WANT TO CONTINUE TO USE file!
 */
int file_op_insert(fileid_t fid, off_t pos, fileid_t file);

/* Deletes part [pos...pos+size] of fid and returns it
 * as new file.
 * Returns -1 on error.
 * RETURNED FILE IS RO! IF YOU DONT NEED THE RETURNED
 * FILE USE file_op_cut INSTEAD!
 */
fileid_t file_op_delete(fileid_t fid, off_t pos, off_t size);

/* Cuts part [pos...pos+size] of fid.
 * Returns -1 on error.
 * If you need the deleted part, use file_op_delete
 * instead.
 */
int file_op_cut(fileid_t fid, off_t pos, off_t size);



/* transaction grouping:
 * transactions are for undo/redo support and for future
 * always-consistent swap.
 */
/* Begin starts a new transaction. Returns -1 on error. */
int file_transaction_begin(fileid_t fid);
/* End ends the last transaction. Returns -1 on error. */
int file_transaction_end(fileid_t fid);
/* Undo undos the last transaction. Returns -1 on error. */
int file_transaction_undo(fileid_t fid);
/* Redo redos the last undone transaction. Returns -1 on error. */
int file_transaction_redo(fileid_t fid);


/* Returns the filecluster of fid containing the byte at
 * position pos.
 * Returns NULL on error.
 */
filecluster_t *filecluster_get(fileid_t fid, off_t pos);

/* filecluster_next returns the next cluster or NULL.
 * filecluster_start returns the position of the first byte.
 * filecluster_end returns the position of the last byte.
 * filecluster_size returns the number of bytes.
 */
#define filecluster_next(fc) (fc_next(fc))
#define filecluster_prev(fc) (fc_prev(fc))
#define filecluster_start(fc) ((fc)->off)
#define filecluster_end(fc) ((fc)->off + (fc)->size - 1)
#define filecluster_size(fc) ((fc)->size)

/* Generates a temporary mapping of the fileclusters data.
 * Returns NULL on error.
 * WATCH OUT! CREATING THE MAPPING CAN ALTER THE FILECLUSTERS
 * END AND SIZE!
 */
char *filecluster_mmap(filecluster_t *fc);

/* Unmaps previously mapped data. */
void filecluster_munmap(filecluster_t *fc);



#ifdef __cplusplus
}
#endif

#endif
