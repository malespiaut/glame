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

#include "list.h"
#include "glsignal.h"


struct gpsm_item_s;
struct gpsm_grp_s;
struct gpsm_swfile_s;
typedef struct gpsm_item_s gpsm_item_t;
typedef struct gpsm_grp_s gpsm_grp_t;
typedef struct gpsm_swfile_s gpsm_swfile_t;

/* Both GPSM_SIG_ITEM_CHANGED and GPSM_SIG_ITEM_DESTROY have
 * one parameter, the gpsm-item. GPSM_SIG_ITEM_DESTROY will
 * be sent out _before_ item destruction, GPSM_SIG_ITEM_CHANGED
 * _after_ the change. Note that GPSM_SIG_ITEM_CHANGED delivery
 * is not suppressed, if the change has a semantically more
 * specific signal like one of the GPSM_SIG_GRP or GPSM_SIG_SWFILE
 * signals. */
#define GPSM_SIG_ITEM_CHANGED    (1<<0)
#define GPSM_SIG_ITEM_DESTROY    (1<<1)
/* Both GPSM_SIG_GRP_NEWITEM and GPSM_SIG_GRP_REMOVEITEM have
 * two parameters, the gpsm-grp as the first and the gpsm-item
 * to be inserted/removed as second one. The GPSM_SIG_GRP_REMOVEITEM
 * signal is sent out before the actual action, the NEWITEM signal
 * after the action. */
#define GPSM_SIG_GRP_NEWITEM     (1<<2)
#define GPSM_SIG_GRP_REMOVEITEM  (1<<3)
/* GPSM_SIG_SWFILE_INSERT has three parameters, the first is
 * the gpsm-swfile itself, the second is a long position, the
 * third a long size specifying position and size of the inserted
 * data in samples. This signal is sent _after_ the actual operation
 * was carried out on the swapfile. The GPSM_SIG_SWFILE_CUT and
 * GPSM_SIG_SWFILE_CHANGED signals operate the same way. */
#define GPSM_SIG_SWFILE_INSERT   (1<<4)
#define GPSM_SIG_SWFILE_CUT      (1<<5)
#define GPSM_SIG_SWFILE_CHANGED  (1<<6)

#define GPSM_ITEM_TYPE_GRP    0
#define GPSM_ITEM_TYPE_SWFILE 1
#define GPSM_ITEM_IS_GRP(i) ((i)->type == GPSM_ITEM_TYPE_GRP)
#define GPSM_ITEM_IS_SWFILE(i) ((i)->type == GPSM_ITEM_TYPE_SWFILE)
struct gpsm_item_s {
	struct list_head list;
	gpsm_grp_t *parent;
	glsig_emitter_t emitter;
	int type;
	char *label;
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

/* Creates a new spare swfile to operate with. You have to
 * insert it into a gpsm_grp yourself. Returns a gpsm-swfile
 * or NULL on error. */
gpsm_swfile_t *gpsm_newswfile(const char *label);

gpsm_swfile_t *gpsm_swfile_cow(gpsm_swfile_t *swfile);

gpsm_swfile_t *gpsm_swfile_link(gpsm_swfile_t *swfile);

/* Creates a new empty gpsm-grp. You have to insert it into
 * a gpsm_grp yourself. Returns a gpsm-grp or NULL on error. */ 
gpsm_grp_t *gpsm_newgrp(const char *label);

/* Destroys a gpsm-item (gpsm-swfile including the backing store,
 * gpsm-grp including all children). */
void gpsm_item_destroy(gpsm_item_t *item);


/* May fail, as overlapping items are not allowed. Returns
 * 0 on success and -1 on error. */
int gpsm_grp_insert(gpsm_grp_t *group, gpsm_item_t *item,
		    long hposition, long vposition);
void gpsm_item_remove(gpsm_item_t *item);


void gpsm_item_set_label(gpsm_item_t *item, const char *label);


gpsm_grp_t *gpsm_find_group_label(gpsm_grp_t *root, const char *label);
gpsm_swfile_t *gpsm_find_swfile_label(gpsm_grp_t *root, const char *label);
gpsm_swfile_t *gpsm_find_swfile_filename(gpsm_grp_t *root, long filename);


#if 0
/* Applying filter stubs (with "in" and/or "out" port, multiple
 * or single), filter replication and automagically destination
 * creation. Returns a network for own use, delete it yourself.
 * NO SIGNALS ARE RAISED BY THIS FUNCTION! YOU HAVE TO RAISE SIGNALS
 * YOURSELF! You may use gpsm_item_invalidate for this purpose.
 * FIXME: this is br0ken? but we cant fix it... (dynamically create
 * completion function and return it!? whooo...)
 * Note, that all combinations of source/dest/cdest being NULL/not
 * NULL should be supported (but not dest & cdest being not NULL).
 */
filter_t *gpsm_apply_filter(filter_t *stub,
			    gpsm_item_t *source, gpsm_item_t *dest,
			    gpsm_item_t **cdest);
/* Invalidates (sends _CHANGED/_CUT/_INSERT signals) to all groups
 * and swfiles. */
void gpsm_item_invalidate(gpsm_item_t *item);


gpsm_grp_t *gpsm_newgrp_import(const char *filename);
int gpsm_item_export(gpsm_item_t *item, const char *filename);
#endif

#endif
