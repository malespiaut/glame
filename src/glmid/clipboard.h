#ifndef _CLIPBOARD_H
#define _CLIPBOARD_H

/*
 * clipboard.h
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

/* The clipboard implements correct cut&paste operations to and
 * from arbitrary gpsm subtrees.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gpsm.h"


/* If the clipboard does contain data, 1 is returned, else 0. */
int clipboard_is_empty();

/* Empties the clipboard. Useful for cleanup. */
void clipboard_empty();


/* Returns a COW copy of the current contents of the clipboard or
 * NULL, if it was empty. */
gpsm_grp_t *clipboard_get();


/* Returns 1, if the clipboard can paste into the provided subtree.
 * Pasting is only possible, if the number of tracks match. */
int clipboard_can_paste(gpsm_item_t *item);


/* Does a "delete" operation on the specified gpsm subtree in the
 * range [pos, pos + size[. The contents of this range are not
 * saved in the clipboard. Returns 0 on success, -1 on error. */
int clipboard_delete(gpsm_item_t *item, long pos, long size);

/* Does a "cut" operation on the specified gpsm subtree in the
 * range [pos, pos + size[. The contents of this range are
 * saved in the clipboard. Returns 0 on success, -1 on error. */
int clipboard_cut(gpsm_item_t *item, long pos, long size);

/* Does a "copy" operation on the specified gpsm subtree in the
 * range [pos, pos + size[. The contents of this range are
 * saved in the clipboard. Returns 0 on success, -1 on error. */
int clipboard_copy(gpsm_item_t *item, long pos, long size);

/* Does a "paste" operation on the specified gpsm subtree at the
 * position pos. The contents of the clipboard do not change.
 * Returns 0 on success, -1 on error. */
int clipboard_paste(gpsm_item_t *item, long pos);


#endif
