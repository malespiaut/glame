#ifndef _GPSM_H
#define _GPSM_H

/*
 * gpsm.h
 *
 * Copyright (C) 2001 Richard Guenther
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

/* The project structure management including track metadata
 * and coupling with the swapfile/filter API.
 *
 * The structure is build out of gpsm-items which can be either
 * groups (gpsm_grp_t) or swapfile files (gpsm_swfile_t). Think
 * of the structure as of a tree with leafs all being swapfile files
 * (or equivalent types, if they ever will appear) and the interior
 * nodes being groups. Groups (generic: items) have horizontal (samples)
 * and vertical (tracks) extend, items have horizontal (samples) and
 * vertical (track) positions.
 *  [FIXME: insert nice ascii-art picture of tree here.]
 * 
 * Usage of the gpsm API is by the various GUI widgets that handle
 * the swapfile and the filters such as the swapfile gui and the
 * timeline gui. Also use by the scheme scripts is the preferred way
 * to work on the swapfile.
 * Consistent updating of all interfaces can be realized by registering
 * appropriate signal handlers to the gpsm-items emitter. Signals are
 * sent out by all gpsm API functions and have to be sent out manually
 * by everyone operating on the swapfile/gpsm-items _not_ using the
 * gpsm API functions.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <unistd.h>
#include "list.h"
#include "glsignal.h"


struct gpsm_item_s;
typedef struct gpsm_item_s gpsm_item_t;
struct gpsm_grp_s;
typedef struct gpsm_grp_s gpsm_grp_t;
struct gpsm_swfile_s;
typedef struct gpsm_swfile_s gpsm_swfile_t;


/*
 * Signals sent out by the gpsm-items.
 */

/* GPSM_SIG_ITEM_CHANGED has one parameter, the gpsm-item.
 * The signal will be sent out _after_ a change to any of the
 * items data elements.
 * Note that GPSM_SIG_ITEM_CHANGED delivery is not suppressed, if
 * the change has a semantically more specific signal like one of
 * the GPSM_SIG_GRP_* or GPSM_SIG_SWFILE_* signals. Also a
 * GPSM_SIG_ITEM_CHANGED signal is sent out on item re-position. */
#define GPSM_SIG_ITEM_CHANGED    (1<<0)

/* GPSM_SIG_ITEM_DESTROY has one parameter, the gpsm-item. The
 * signal will be sent out _before_ item destruction.
 * Note that items attached to a group will generally recieve
 * a GPSM_SIG_GRP_REMOVEITEM signal before destruction, i.e.
 * gpsm_item_destroy() will remove them first, then destruct. */
#define GPSM_SIG_ITEM_DESTROY    (1<<1)

/* GPSM_SIG_ITEM_REMOVE has one parameter, the gpsm-item. The
 * signal will be sent out _before_ item removal from its group.
 * The GPSM_SIG_GRP_REMOVEITEM signal will be send out to the
 * group after this signal. */
#define GPSM_SIG_ITEM_REMOVE    (1<<2)

/* Both GPSM_SIG_GRP_NEWITEM and GPSM_SIG_GRP_REMOVEITEM have
 * two parameters, the gpsm-grp as the first and the gpsm-item
 * to be inserted/removed as second one. The GPSM_SIG_GRP_REMOVEITEM
 * signal is sent out before item removal and after the item
 * recieved the GPSM_SIG_ITEM_REMOVE signal, the NEWITEM signal
 * after item addition.
 * NOTE: If the actual item is a group it is certainly possible
 *       for it to contain children!
 * NOTE2: You may want to attach/remove signal handlers to the
 *        item (and the possible childrens of a group) */
#define GPSM_SIG_GRP_NEWITEM     (1<<3)
#define GPSM_SIG_GRP_REMOVEITEM  (1<<4)

/* The GPSM_SIG_SWFILE_* have three parameters, the first is
 * the gpsm-swfile itself, the second is a long position, the
 * third a long size specifying position and size of the inserted
 * / cutted / changed data in samples. This signal is sent _after_
 * the actual operation was carried out on the swapfile. */
#define GPSM_SIG_SWFILE_INSERT   (1<<5)
#define GPSM_SIG_SWFILE_CUT      (1<<6)
#define GPSM_SIG_SWFILE_CHANGED  (1<<7)


#define GPSM_ITEM_TYPE_GRP    0
#define GPSM_ITEM_TYPE_SWFILE 1
#define GPSM_ITEM_IS_GRP(i) (((gpsm_item_t *)(i))->type == GPSM_ITEM_TYPE_GRP)
#define GPSM_ITEM_IS_SWFILE(i) (((gpsm_item_t *)(i))->type == GPSM_ITEM_TYPE_SWFILE)
struct gpsm_item_s {
	struct list_head list;
	gpsm_grp_t *parent;
	glsig_emitter_t emitter;
	int type;

	/* Items have (not necessarily distinct) labels, i.e.
	 * the following is guaranteed to be non-NULL. */
	char *label;

	/* Item positions are _relative_ positions to the direct
	 * parent [or to (0,0) if the parent is NULL]. The item
	 * bounding box is guaranteed to not extend the parents
	 * bounding box and to not overlap with any child of the
	 * parent. */
	long hposition;
	long vposition;
	long hsize;
	long vsize;
};
struct gpsm_grp_s {
	/* An gpsm-grp is an item with a list containing further
         * items. The gpsm-item positions/sizes are unions of the
         * groups children. */
	gpsm_item_t item;
	struct list_head items;
};
struct gpsm_swfile_s {
	/* An gpsm-swfile is an item with additional information
         * about the swapfile file location/metadata. The gpsm-swfile
         * vsize is always 1. */
	gpsm_item_t item;
	gpsm_swfile_t *next_swfile_hash;
	gpsm_swfile_t **pprev_swfile_hash;
	struct timeval last_op_time;
	long filename;
	int samplerate;
	float position;
};

#define gpsm_item_parent(item) (((gpsm_item_t *)item)->parent)
#define gpsm_item_emitter(item) (&((gpsm_item_t *)item)->emitter)
#define gpsm_item_label(item) (((gpsm_item_t *)item)->label)
#define gpsm_item_hposition(item) (((gpsm_item_t *)item)->hposition)
#define gpsm_item_vposition(item) (((gpsm_item_t *)item)->vposition)
#define gpsm_item_hsize(item) (((gpsm_item_t *)item)->hsize)
#define gpsm_item_vsize(item) (((gpsm_item_t *)item)->vsize)

#define gpsm_grp_foreach_item(grp, item) list_foreach(&((gpsm_grp_t *)(grp))->items, gpsm_item_t, list, item)
#define gpsm_grp_safe_foreach_item(grp, dummy, item) list_safe_foreach(&((gpsm_grp_t *)(grp))->items, gpsm_item_t, list, dummy, item)

#define gpsm_swfile_filename(swfile) (((gpsm_swfile_t *)swfile)->filename)
#define gpsm_swfile_samplerate(swfile) (((gpsm_swfile_t *)swfile)->samplerate)
#define gpsm_swfile_position(swfile) (((gpsm_swfile_t *)swfile)->position)


/* Initializes the gpsm subsystem using the specified swapfile
 * (the gpsm subsystem metadata is stored in swapfile file 0).
 * Returns 0 on success and -1 on error (which is the sign for
 * swapfile corruption or already initialized gpsm). */
int gpsm_init(const char *swapfile);

/* Changes (or just queries, if max < 0) the maximum number of states
 * saved for undo/redo. Returns the actual set value. */
int gpsm_set_max_saved_ops(int max);

/* Syncs the gpsm metadata on disk (swapfile) with the gpsm
 * metadata in memory. Also syncs all swapfile data cached. */
void gpsm_sync();

/* Syncs the gpsm metadata and the swapfile and closes the
 * subsystem. Further usage will require a call to gpsm_init()
 * again. */
void gpsm_close();



/* Returns the gpsm root group (or NULL on non initialized
 * gpsm subsystem). */
gpsm_grp_t *gpsm_root(void);


/* Creates a new spare swapfile to operate with. You have to
 * insert it into a gpsm_grp yourself. Returns a gpsm-swfile
 * or NULL on error. */
gpsm_swfile_t *gpsm_newswfile(const char *label);

/* Creates a new swapfile with contents from the swapfile
 * specified by the gpsm-swfile. Returns a gpsm-swfile or
 * NULL on error. */
gpsm_swfile_t *gpsm_swfile_cow(gpsm_swfile_t *swfile);

/* Creates a new gpsm-swfile with the swapfile of the specified
 * gpsm-swfile as backing store. Returns a gpsm-swfile or
 * NULL on error. */
gpsm_swfile_t *gpsm_swfile_link(gpsm_swfile_t *swfile);


/* Creates a new empty gpsm-grp. You have to insert it into
 * a gpsm_grp yourself. Returns a gpsm-grp or NULL on error. */ 
gpsm_grp_t *gpsm_newgrp(const char *label);


/* Destroys a gpsm-item (gpsm-swfile including the backing store
 * if the gpsm-swfile was the last reference to the swapfile,
 * gpsm-grp including all children). */
void gpsm_item_destroy(gpsm_item_t *item);


/* Inserts the specified gpsm-item into the group at the specified
 * position. Random (non-overlapping) {hv}positioning is performed if
 * you pass -1 to {hv}position.
 * May fail, as overlapping items are not allowed. Returns
 * 0 on success and -1 on error. */
int gpsm_grp_insert(gpsm_grp_t *group, gpsm_item_t *item,
		    long hposition, long vposition);

/* Removes the specified gpsm-item from its current group. The
 * items position will be (0,0) after this operation. If the
 * item was not member of a group this is a NOP. */
void gpsm_item_remove(gpsm_item_t *item);


/* Find a gpsm-grp by label in the subtree specified by root. The
 * search is started at the item start (or at the root, if you specify
 * NULL). You can find all occurences by specifying the previous
 * result as start. Returns a gpsm-grp, if found or NULL, if not. */
gpsm_grp_t *gpsm_find_grp_label(gpsm_grp_t *root, gpsm_item_t *start,
				const char *label);

/* Find a gpsm-swfile by label in the subtree specified by root. The
 * search is started at the item start (or at the root, if you specify
 * NULL). You can find all occurences by specifying the previous
 * result as start. Returns a gpsm-swfile, if found or NULL, if not. */
gpsm_swfile_t *gpsm_find_swfile_label(gpsm_grp_t *root, gpsm_item_t *start,
				      const char *label);

/* Find a gpsm-swfile by filename  in the subtree specified by root. The
 * search is started at the item start (or at the root, if you specify
 * NULL). You can find all occurences by specifying the previous
 * result as start. Returns a gpsm-swfile, if found or NULL, if not. */
gpsm_swfile_t *gpsm_find_swfile_filename(gpsm_grp_t *root, gpsm_item_t *start,
					 long filename);

/* Find a gpsm-swfile by vposition in the subtree specified by root. The
 * search is started at the item start (or at the root, if you specify
 * NULL). You can find all occurences by specifying the previous
 * result as start. Returns a gpsm-swfile, if found or NULL, if not. */
gpsm_swfile_t *gpsm_find_swfile_vposition(gpsm_grp_t *root, gpsm_item_t *start,
					  long vposition);


/* Updates the label of the specified gpsm-item. Note that this will
 * cause a GPSM_SIG_ITEM_CHANGED signal to be send out. */
void gpsm_item_set_label(gpsm_item_t *item, const char *label);

/* Updates the samplerate and position of the specified gpsm-swfile. Note
 * that this information is per gpsm-swfile, not per swapfile! Note that
 * this will cause a GPSM_SIG_ITEM_CHANGED signal to be send out. */
void gpsm_swfile_set(gpsm_swfile_t *swfile, int samplerate,
		     float position);

/* Updates the samplerate of the specified gpsm-swfile. Note
 * that this information is per gpsm-swfile, not per swapfile! Note that
 * this will cause a GPSM_SIG_ITEM_CHANGED signal to be send out. */
void gpsm_swfile_set_samplerate(gpsm_swfile_t *swfile, int samplerate);

/* Updates the position of the specified gpsm-swfile. Note
 * that this information is per gpsm-swfile, not per swapfile! Note that
 * this will cause a GPSM_SIG_ITEM_CHANGED signal to be send out. */
void gpsm_swfile_set_position(gpsm_swfile_t *swfile, float position);

/* _After_ you've done an operation on a swapfile such
 * as modifying or cutting/inserting via sw_sendfile() you have to notify
 * the GPSM about this change. The swfiles sizes will be updated and
 * appropriate signals will be send out.
 * Note that it is generally better to make changes to a swapfile through
 * gpsm functions (which dont exist at the moment...). */
void gpsm_notify_swapfile_change(long filename, long pos, long size);
void gpsm_notify_swapfile_cut(long filename, long pos, long size);
void gpsm_notify_swapfile_insert(long filename, long pos, long size);
void gpsm_invalidate_swapfile(long filename);



/* Undo/redo support.
 */

/* Save the current state of the provided subtree for later undo.
 * Returns 0 on success, -1 on failure. */
int gpsm_op_prepare(gpsm_item_t *item);

/* Returns 1 if undo is pending for the subtree item and can be
 * undone at this point, else returns 0. */
int gpsm_op_can_undo(gpsm_item_t *item);

/* Rolls back to the latest saved state of the provided subtree.
 * Returns 0 on success, -1 on error (such as no undo pending/possible).
 * Saves the actual state for later redo. */
int gpsm_op_undo(gpsm_item_t *item);

/* Rolls back to the latest saved state of the provided subtree.
 * Returns 0 on success, -1 on error (such as no undo pending/possible).
 * Does not save the actual state for later redo. */
int gpsm_op_undo_and_forget(gpsm_item_t *item);

/* Returns 1 if redo is pending for the subtree item and can be
 * redone at this point, else returns 0. */
int gpsm_op_can_redo(gpsm_item_t *item);

/* Rolls back to the state before the previous undo to the provided
 * subtree. Returns 0 on success, -1 on error (such as no redo pending
 * or possible). Saves the actual state for later undo. */
int gpsm_op_redo(gpsm_item_t *item);

/* Rolls back to the state before the previous undo to the provided
 * subtree. Returns 0 on success, -1 on error (such as no redo pending
 * or possible). Does not save the actual state for later undo. */
int gpsm_op_redo_and_forget(gpsm_item_t *item);

/* Kills off the latest saved state of the provided subtree. Returns
 * 0 on success, -1 on error (no pending undo/redo). */
int gpsm_op_forget(gpsm_item_t *item);


/* Useful complex operations.
 */

/* Collects all swfiles in the subtree item and creates a new
 * group with links to them in traversal order. Positions relative
 * to item are retained.
 * Returns NULL on failure (no swfiles), a new group on success. */
gpsm_grp_t *gpsm_collect_swfiles(gpsm_item_t *item);

/* Flattens a gpsm item, that is, out of a possible deep tree of
 * horizontally and vertically spreaded swfiles make a set of
 * vertically aligned (read: starting at position zero and ending
 * at the maximum position) swfiles.
 * Returns a new group with new swfiles, one for each vertical track.
 * The data is COWed from the original tree. In the special case of
 * providing a swfile as item a new group with a COW copy of this
 * item is returned (without paying attention to hposition of the item).
 * On failure NULL is returned.
 * Note that this feature greatly simplifies operations such as play
 * and export (i.e. where you only want to _read_ from the files). */
gpsm_grp_t *gpsm_flatten(gpsm_item_t *item);



#endif
