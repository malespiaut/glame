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


static struct ctree *ctree_alloc(long cnt);


static void ctree_mark_end(struct ctree *t);
#define ctree_set_cnt(t, c) do { \
        (t)->cnt = c; \
	ctree_mark_end(t); \
} while (0)
static void ctree_build_partial(struct ctree *t, long pos, long cnt);
#define ctree_build(t) ctree_build_partial(t, 0, (t)->cnt)



static long ctree_find(struct ctree *h, s64 offset, s64 *coff)
{
	s64 *tree64 = CTREE64(h);
	s32 *tree32 = CTREE32(h);
    	s64 pos;
	u32 height = h->height;
	long i;

	pos = 0;
	i = 1;
	/* Search the tree but the last level, as that
	 * (the cluster sizes) is not s64, but s32... so
	 * we need to seperate the last iteration. */
	while (--height) {
		if (tree64[2*i] == 0
		    || pos+tree64[2*i] > offset) {
			i = 2*i;
		} else {
			pos += tree64[2*i];
			i = 2*i+1;
		}
	}
	/* Do the last check with the sizes[] which is s32, not s64... */
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
			  u32 *cid, s32 *csize)
{
	/* Copy the id's and sizes - may overlap, so memmove. */
	memmove(&CID(h, pos), cid, cnt*sizeof(u32));
	memmove(&CSIZE(h, pos), csize, cnt*sizeof(s32));

	/* Rebuild partial tree. */
	ctree_build_partial(h, pos, cnt);
}


static struct ctree *ctree_insert1(struct ctree *h, long pos,
				   u32 cid, s32 size)
{
	return ctree_insert(h, pos, 1, &cid, &size);
}

static struct ctree *ctree_insert(struct ctree *h, long pos, long cnt,
				  u32 *cid, s32 *size)
{
	struct ctree *dest;
	u32 *ccid;
	s32 *csize;
	long i;

	if (!h || !cid || !size
	    || pos<0 || cnt<=0 || pos>h->cnt)
		PANIC("Invalid arguments");

	if (CTREEMAXCNT(h->height) < h->cnt + cnt) {
		if (!(dest = ctree_alloc(h->cnt + cnt)))
			PANIC("Cannot alloc ctree");
	} else
		dest = h;

	/* If cid/size array overlap the dest cid/size array,
	 * we need to copy it (could be done more efficient -
	 * but not now). */
	if (cid >= &CID(dest, 0) && cid < &CID(dest, dest->cnt)) {
		ccid = (u32 *)alloca(sizeof(u32)*cnt);
		csize = (s32 *)alloca(sizeof(s32)*cnt);
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
	ctree_set_cnt(dest, h->cnt + cnt);
	if (h != dest)
		ctree_build(dest);
	else
		ctree_build_partial(dest, pos, dest->cnt - pos);

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
	if (cid)
		for (i=pos; i<pos+cnt; i++)
			cid[i-pos] = CID(h, i);
	if (csize)
		for (i=pos; i<pos+cnt; i++)
			csize[i-pos] = CSIZE(h, i);

	/* Number of entries after to be removed entries - these
	 * we will place at pos. */
	replcnt = h->cnt - pos - cnt;

	/* Move the tail cid/csize over the deleted cid/csize part. */
	if (replcnt > 0) {
		memmove(&CID(h, pos), &CID(h, pos+cnt), replcnt*sizeof(u32));
		memmove(&CSIZE(h, pos), &CSIZE(h, pos+cnt), replcnt*sizeof(s32));
	}

	/* Truncate the tree and rebuild the partial tree.
	 * If we changed the tail (replcnt == 0, pos == h->cnt), we
	 * need to at least rebuild the last item. */
	ctree_set_cnt(h, h->cnt - cnt);
	if (replcnt == 0) {
		replcnt++;
		if (pos != 0)
			pos--;
	}
	ctree_build_partial(h, pos, replcnt);

	return h;
}


int ctree_check(struct ctree *t)
{
	u32 h = t->height;
	long from, to;
	long i;

	/* Check size/h/cnt consistency. */
	if (CTREEMAXCNT(h) < t->cnt || t->size < t->cnt) {
		DPRINTF("Inconsistent height %li, cnt %li, size %li\n",
			(long)h, (long)t->cnt, (long)t->size);
		return -1;
	}

	/* Zero height tree is ready with sizes only. */
	if (h == 0 || t->cnt == 0)
		return 0;

	/* Check the tree spanned over the csizes array,
	 * first s32 sizes sum. */
	from = 0;
	to = (t->cnt - 1)/2;
	for (i=from; i<=to; i++)
		if (CSUM(t, h-1, i) != (CSIZE(t, 2*i) + CSIZE(t, 2*i+1))) {
			DPRINTF("CSUM(t, %li, %li) != CSIZE(t, %li) + CSIZE(t, %li)\n", (long)h-1, i, 2*i, 2*i+1);
			return -1;
		}

	/* Second the rest of the tree, s64 sums. */
	while (--h) {
		from /= 2;
		to /= 2;
		for (i=from; i<=to; i++)
			if (CSUM(t, h-1, i) != (CSUM(t, h, 2*i)
						+ CSUM(t, h, 2*i+1))) {
				DPRINTF("CSUM(t, %li, %li) != CSUM(t, %li, %li) + CSUM(t, %li, %li)\n", (long)h-1, i, (long)h, 2*i, (long)h, 2*i+1);
				return -1;
			}
	}

	return 0;
}


/****************************************************************
 * Internal helpers.
 */

static struct ctree *ctree_alloc(long cnt)
{
	u32 height;
	int size;
	struct ctree *t;

	/* Start from minimum height, increase until cnt fits. */
	height = 2;
	while (CTREEMAXCNT(height) < cnt)
		height++;

	/* Alloc the tree and initialize it with zero elements. */
	size = CTREESIZE(height);
	if (!(t = (struct ctree *)malloc(size)))
		return NULL;
	/* memset(t, 0, size);  Is not necessary, if ctree_set_cnt used. */
	t->height = height;
	ctree_set_cnt(t, 0);

	return t;
}

static void ctree_build_partial(struct ctree *t, long pos, long cnt)
{
	u32 h = t->height;
	long from, to;
	long i;

	/* Zero height tree is ready with sizes only. Cnt == 0 is
	 * trivial, too. */
	if (h == 0 || cnt == 0)
		return;

	/* Span the tree over the csizes array, first s32 sizes sum. */
	from = pos/2;
	to = (pos+cnt-1)/2;
	for (i=from; i<=to; i++)
		CSUM(t, h-1, i) = (CSIZE(t, 2*i)
				      + CSIZE(t, 2*i+1));

	/* Second the rest of the tree, s64 sums. */
	while (--h) {
		from /= 2;
		to /= 2;
		for (i=from; i<=to; i++)
			CSUM(t, h-1, i) = (CSUM(t, h, 2*i)
					      + CSUM(t, h, 2*i+1));
	}
}

static void ctree_mark_end(struct ctree *t)
{
	long pos = t->cnt - 1;
	long h = t->height;

	/* Set the cluster id after the last one to 0. */
	if (t->cnt < CTREEMAXCNT(t->height))
		CID(t, t->cnt) = 0;

	/* Empty tree is special - we cant walk it for
	 * element -1... */
	if (t->cnt == 0) {
		CSIZE(t, 0) = 0;
		while (--h)
			CSUM(t, h-1, 0) = 0;
		return;
	}

	/* Walk up the tree for the last valid element and
	 * zero all right neighbours, if we are a left leaf. */
	if (!(pos & 1))
		CSIZE(t, pos+1) = 0;
	while (--h) {
		pos /= 2;
		if (!(pos & 1))
			CSUM(t, h, pos+1) = 0;
	}
}




#if 0
/* Zeroes the element at position pos. Does not change h->cnt. */
#define ctree_zero1(h, pos) ctree_replace1(h, pos, 0, 0)

/* Zeroes CID and CSIZE for cnt entries starting from position pos.
 * Does not change h->cnt. */
static void ctree_zero(struct ctree *h, long pos, long cnt)
{
	/* Zero id's and sizes. */
	memset(&CID(h, pos), 0, cnt*sizeof(u32));
	memset(&CSIZE(h, pos), 0, cnt*sizeof(s32));

	/* Rebuild partial tree. */
	ctree_build_partial(h, pos, cnt);
}
#endif
