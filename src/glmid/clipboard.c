/*
 * clipboard.c
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "glame_types.h"
#include "swapfile.h"
#include "gpsm.h"
#include "clipboard.h"


static gpsm_grp_t *root = NULL;


void clipboard_empty()
{
	gpsm_item_destroy((gpsm_item_t *)root);
	root = NULL;
}

int clipboard_is_empty()
{
	return root == NULL;
}


gpsm_grp_t *clipboard_get()
{
	gpsm_grp_t *grp;
	gpsm_item_t *item;
	gpsm_swfile_t *swfile;

	if (!root)
		return NULL;

	grp = gpsm_newgrp("clipboard");
	gpsm_grp_foreach_item(root, item) {
		swfile = gpsm_swfile_cow((gpsm_swfile_t *)item);
		gpsm_item_place(grp, (gpsm_item_t *)swfile,
				0, gpsm_item_vsize(grp));
	}

	return grp;
}


int clipboard_can_paste(gpsm_item_t *item)
{
	gpsm_item_t *it;

	if (!item || !root)
		return 0;

	if (GPSM_ITEM_IS_SWFILE(item))
		return gpsm_grp_nritems(root) == 1;
	gpsm_grp_foreach_item(item, it)
		if (!GPSM_ITEM_IS_SWFILE(it))
			return 0;

	return gpsm_grp_nritems(root) == gpsm_grp_nritems(item);
}


/* Copies [pos, pos + size[ from source to the new file dest. Handles
 * out-of source file positions and sizes (pads/prepends with zeros).
 * Returns 0 on success, -1 on error. */
static int copy_one(gpsm_swfile_t *dest, gpsm_swfile_t *source,
		    long pos, long size, int extra_flags)
{
	swfd_t sfd = -1, dfd = -1;
	long start, length;
	int res = 0;

	sfd = sw_open(gpsm_swfile_filename(source), O_RDONLY);
	if (dest)
		dfd = sw_open(gpsm_swfile_filename(dest), O_WRONLY);
	if (sfd == -1 || (dest && dfd == -1))
		goto err;
	start = MAX(0, pos);
	if (sw_lseek(sfd, start*SAMPLE_SIZE, SEEK_SET) == -1)
		res = -1;
	if (dest)
		if (sw_lseek(dfd, -MIN(0, pos)*SAMPLE_SIZE, SEEK_SET) == -1)
			res = -1;
	length = MIN(gpsm_item_hsize(source) - MAX(0, pos),
		     (size + MIN(0, pos)));
	res |= sw_sendfile(dest ? dfd : SW_NOFILE, sfd, length*SAMPLE_SIZE,
			   SWSENDFILE_INSERT|extra_flags);
	if (dest)
		gpsm_invalidate_swapfile(gpsm_swfile_filename(dest));
	if (extra_flags == SWSENDFILE_CUT)
		gpsm_notify_swapfile_cut(gpsm_swfile_filename(source),
					 start, length);
	if (dest)
		res |= sw_ftruncate(dfd, size*SAMPLE_SIZE);
	if (res != 0)
		goto err;
	if (dest)
		sw_close(dfd);
	sw_close(sfd);
	return 0;

 err:
	if (dest) {
		sw_ftruncate(dfd, 0);
		sw_close(dfd);
	}
	sw_close(sfd);
	return -1;
}

/* Pastes from source to dest at position pos. Handles out-of dest file
 * positions. Returns 0 on success, -1 on error. */
static int paste_one(gpsm_swfile_t *dest, gpsm_swfile_t *source,
		     long pos)
{
	swfd_t sfd, dfd;
	int res = 0;

	sfd = sw_open(gpsm_swfile_filename(source), O_RDONLY);
	dfd = sw_open(gpsm_swfile_filename(dest), O_WRONLY);
	if (sfd == -1 || dfd == -1)
		goto err;
	/* The only difficult case is negative pos - others are handled
	 * by sw_sendfile. */
	if (pos < 0) {
		/* We need to prepend -pos "zeros" to dest. */
		swfd_t zfd;
		long name;
		while ((zfd = sw_open(name = rand(), O_RDWR|O_CREAT|O_EXCL)) == -1)
			;
		sw_unlink(name);
		sw_ftruncate(zfd, -pos*SAMPLE_SIZE);
		res |= sw_sendfile(dfd, zfd, -pos*SAMPLE_SIZE,
				   SWSENDFILE_INSERT);
		sw_close(zfd);
	}
	if (sw_lseek(dfd, abs(pos)*SAMPLE_SIZE, SEEK_SET) == -1)
		res = -1;
	res |= sw_sendfile(dfd, sfd, gpsm_item_hsize(source)*SAMPLE_SIZE,
			   SWSENDFILE_INSERT);
	gpsm_notify_swapfile_insert(gpsm_swfile_filename(dest),
				    MIN(gpsm_item_hsize(dest), MAX(0, pos)),
				    (-MIN(0, pos) + gpsm_item_hsize(source) + MAX(0, pos - gpsm_item_hsize(dest))));
	if (res != 0)
		goto err;
	sw_close(dfd);
	sw_close(sfd);
	return 0;

 err:
	sw_close(dfd);
	sw_close(sfd);
	return -1;
}


int clipboard_delete(gpsm_item_t *item, long pos, long size)
{
	gpsm_item_t *it;

	if (!item || pos < 0 || size <= 0)
		return -1;
	if (GPSM_ITEM_IS_GRP(item) && gpsm_grp_nritems(item) == 0)
		return -1;

	if (GPSM_ITEM_IS_SWFILE(item)) {
		return copy_one(NULL, (gpsm_swfile_t *)item,
				pos, size, SWSENDFILE_CUT);
	}

	gpsm_grp_foreach_item(item, it) {
		if (!GPSM_ITEM_IS_SWFILE(it))
			return -1;
		if (copy_one(NULL, (gpsm_swfile_t *)it,
			     pos - gpsm_item_hposition(it), size,
			     SWSENDFILE_CUT) == -1)
			return -1;
	}

	return 0;
}


/* Cut and copy operations. Mode 1 == cut, 2 == copy. */
static int clipboard_cut_copy(gpsm_item_t *item, long pos, long size,
			      int sendfile_flags)
{
	gpsm_grp_t *grp;
	gpsm_item_t *it;

	if (!item || pos < 0 || size <= 0)
		return -1;
	if (GPSM_ITEM_IS_GRP(item) && gpsm_grp_nritems(item) == 0)
		return -1;

	grp = gpsm_newgrp("clipboard");

	if (GPSM_ITEM_IS_SWFILE(item)) {
		gpsm_swfile_t *swfile;
		swfile = gpsm_newswfile("track");
		gpsm_swfile_set(swfile, gpsm_swfile_samplerate(item),
				gpsm_swfile_position(item));
		gpsm_item_place(grp, (gpsm_item_t *)swfile,
				0, gpsm_item_vsize(grp));
		if (copy_one(swfile, (gpsm_swfile_t *)item,
			     pos, size, sendfile_flags) == -1)
			goto err;
		goto ok;
	}

	gpsm_grp_foreach_item(item, it) {
		gpsm_swfile_t *swfile;
		if (!GPSM_ITEM_IS_SWFILE(it))
			goto err;
		swfile = gpsm_newswfile("track");
		gpsm_swfile_set(swfile, gpsm_swfile_samplerate(it),
				gpsm_swfile_position(it));
		gpsm_item_place(grp, (gpsm_item_t *)swfile,
				0, gpsm_item_vsize(grp));
		if (copy_one(swfile, (gpsm_swfile_t *)it,
			     pos - gpsm_item_hposition(it), size,
			     sendfile_flags) == -1)
			goto err;
	}

 ok:
	gpsm_item_destroy((gpsm_item_t *)root);
	root = grp;
	return 0;

 err:
	gpsm_item_destroy((gpsm_item_t *)grp);
	return -1;
}

int clipboard_cut(gpsm_item_t *item, long pos, long size)
{
	return clipboard_cut_copy(item, pos, size, SWSENDFILE_CUT);
}

int clipboard_copy(gpsm_item_t *item, long pos, long size)
{
	return clipboard_cut_copy(item, pos, size, 0);
}

int clipboard_paste(gpsm_item_t *item, long pos)
{
	gpsm_swfile_t *source, *dest;

	if (!item || pos < 0 || !clipboard_can_paste(item))
		return -1;

	/* paste from single file into single file is simple. */
	source = (gpsm_swfile_t *)gpsm_grp_first(root);
	if (GPSM_ITEM_IS_SWFILE(item))
		return paste_one((gpsm_swfile_t *)item, source, pos);

	/* now for the complicated multi-file to group case. */
	dest = (gpsm_swfile_t *)gpsm_grp_first(item);
	do {
		if (paste_one(dest, source,
			      pos - gpsm_item_hposition(dest)) == -1)
			return -1;

		source = (gpsm_swfile_t *)gpsm_grp_next(root, source);
		dest = (gpsm_swfile_t *)gpsm_grp_next(item, dest);
	} while (source && dest);

	return 0;
}
