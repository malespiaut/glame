/*
 * swfs_ctree.c
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

#include <stdlib.h>
#include "swfs_ctree.h"


static int log2(unsigned long n);
static struct ctree *ctree_alloc(long cnt);
static void ctree_build(struct ctree *t);



static long ctree_find(struct ctree *h, s64 offset, s64 *coff)
{
	s64 *tree64 = CTREE64(h);
	s32 *tree32 = CTREE32(h);
    	s64 pos;
	u32 height = h->height;
	long i;

	pos = 0;
	i = 1;
	/* FIXME - this will not work, as the last level of the
	 * tree (the cluster sizes) is not s64, but s32... so
	 * we need to seperate the last iteration. */
	while (--height) {
		if (pos+tree64[2*i] > offset) {
			i = 2*i;
		} else {
			pos += tree64[2*i];
			i = 2*i+1;
		}
	}
	/* Do the last chech with the sizes[] which is s32, not s64... */
	if (pos+tree32[2*i + (1<<h->height)] > offset) {
		i = 2*i + (1<<h->height);
	} else {
		pos += tree32[2*i + (1<<h->height)];
		i = 2*i+1 + (1<<h->height);
	}

	/* Outside of allocated area? */
	if (pos > offset || pos+tree32[i] <= offset)
		return -1;

	/* Fill start offset, size and return id */
	if (coff)
		*coff = pos;

	/* Offset inside size/cid subarray. */
	return i-2*(1<<h->height);
}


static void ctree_zero(struct ctree *h, long pos, long cnt)
{
	long i;

	/* For now _very_ inefficent. */
	for (i=pos; i<pos+cnt; i++)
		ctree_replace1(h, i, 0, 0);
}


static void ctree_replace1(struct ctree *h, long pos,
			  u32 cid, s32 size)
{
	u32 height = h->height;
	s32 delta = size - CSIZE(h, pos);

	/* Fix size and id of the cluster at position pos. */
	CSIZE(h, pos) = size;
	CID(h, pos) = cid;

	/* Fix the affected part of the tree. */
	while (height--) {
		pos = pos >> 1;
		CSUM(h, height, pos) += delta;
	}
}


static void ctree_replace(struct ctree *h, long pos, long cnt,
			  u32 *cid, s32 *size)
{
	long i;

	/* For now _very_ inefficient. */
	if (&CID(h, pos) < cid) {
		for (i=pos; i<pos+cnt; i++)
			ctree_replace1(h, i, cid[i-pos], size[i-pos]);
	} else {
		for (i=pos+cnt-1; i>=pos; i--)
			ctree_replace1(h, i, cid[i-pos], size[i-pos]);
	}
}

static struct ctree *ctree_insert1(struct ctree *h, long pos,
				   u32 cid, s32 size)
{
	return ctree_insert(h, pos, 1, &cid, &size);
}

static struct ctree *ctree_insert(struct ctree *h, long pos, long cnt,
				  u32 *cid, s32 *size)
{
	u32 target_height;
	struct ctree *dest;
	u32 *ccid, *csize;
	long i;

	if (!h || !cid || !size
	    || pos<0 || cnt<=0 || pos>h->cnt)
		PANIC("Invalid arguments");

	target_height = log2(h->cnt + cnt);
	if (h->height != target_height)
		dest = ctree_alloc(h->cnt + cnt);
	else
		dest = h;

	/* If cid/size array overlap the dest cid/size array,
	 * we need to copy it (could be done more efficient -
	 * but not now). */
	if (cid >= &CID(dest, 0) && cid < &CID(dest, dest->cnt)) {
		ccid = alloca(sizeof(u32)*cnt);
		csize = alloca(sizeof(s32)*cnt);
		memcpy(ccid, cid, sizeof(u32)*cnt);
		memcpy(csize, size, sizeof(s32)*cnt);
		cid = ccid;
		csize = csize;
	}

	/* Fill the cluster ids/sizes into the destination tree,
	 * first the unchanged tail-part of the source tree, */
	for (i=(long)(h->cnt)-1; i>=pos; i--) {
		CID(dest, i+cnt) = CID(h, i);
		CSIZE(dest, i+cnt) = CSIZE(h, i);
	}
	/* then the inserted part, */
	for (i=cnt-1; i>=0; i--) {
		CID(dest, pos+i) = cid[i];
		CSIZE(dest, pos+i) = size[i];
	}
	/* and then the unchanged head-part of the source tree,
	 * if necessary. */
	if (h != dest) {
		for (i=pos-1; i>=0; i--) {
			CID(dest, i) = CID(h, i);
			CSIZE(dest, i) = CSIZE(h, i);
		}
	}

	/* Build the tree over the cluster sizes. */
	dest->cnt = h->cnt + cnt;
	ctree_build(dest);

	return dest;
}


static struct ctree *ctree_remove(struct ctree *h, long pos, long cnt,
				  u32 *cid, s32 *csize)
{
	long i, replcnt;

	if (!h || pos<0 || cnt<=0 || pos+cnt>h->cnt)
		PANIC("Out of range params");

	/* Fill the to be deleted cluster ids/sizes into
	 * the provided buffers. */
	for (i=pos; i<pos+cnt; i++) {
		if (cid)
			cid[i-pos] = CID(h, i);
		if (csize)
			csize[i-pos] = CSIZE(h, i);
	}

	/* Number of entries after to be removed entries - these
	 * we will place (i.e. ctree_replace) at pos. */
	replcnt = h->cnt - pos - cnt;
	ctree_replace(h, pos, replcnt,
		      &CID(h, pos+cnt), &CSIZE(h, pos+cnt));

	/* Zero the tail of the tree - cnt entries starting at
	 * position pos + replcnt. Correct h->cnt. */
	ctree_zero(h, pos+replcnt, cnt);
	h->cnt -= cnt;

	return h;
}



/****************************************************************
 * Internal helpers.
 */

static struct ctree *ctree_alloc(long cnt)
{
	u32 height;
	int size;
	struct ctree *t;

	height = log2(cnt);
	size = CTREESIZE(height);
	if (!(t = (struct ctree *)malloc(size)))
		return NULL;
	memset(t, 0, size);
	t->height = height;
	t->cnt = cnt;

	return t;
}

static void ctree_build(struct ctree *t)
{
	u32 h = t->height;
	int i;

	/* Zero height tree is ready with sizes only. */
	if (h == 0)
		return;

	/* Span the tree over the csizes array, first s32 sizes sum. */
	for (i=0; i<(1<<(h-1)); i++)
		CSUM(t, h-1, i) = (CSIZE(t, 2*i)
				      + CSIZE(t, 2*i+1));

	/* Second the rest of the tree, s64 sums. */
	while (--h) {
		for (i=0; i<(1<<(h-1)); i++)
			CSUM(t, h-1, i) = (CSUM(t, h, 2*i)
					      + CSUM(t, h, 2*i+1));
	}
}


static int log2(unsigned long n)
{
	int l = 0, b = 0;

	while (n != 0) {
		l++;
		if (n & 1)
			b++;
		n = n >> 1;
	}

	if (b>=2)
		l++;

	return l;
}
