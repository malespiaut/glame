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
static struct ctree* alloc_tree(long cnt);
static void build_tree(struct ctree *t);


/* Finds the cluster with offset inside it, returns the
 * offset of the cluster id in the tree array. Copies
 * the cluster start position and its size to cstart/csize. */
static long _find_cluster(struct ctree *h, s64 offset, s64 *coff)
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


/* Correct the tree structure to replace the cluster at cid[] position
 * pos with the specified cid/size. */
static void _replace_cluster(struct ctree *h, long pos,
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


static struct ctree *_insert_clusters(struct ctree *h, int pos,
				      int cnt, u32 *cid, s32 *size)
{
	u32 target_height;
	struct ctree *dest;
	long i;

	target_height = log2(h->cnt + cnt);
	if (h->height != target_height)
		dest = alloc_tree(h->cnt + cnt);
	else
		dest = h;

	/* Fill the cluster ids/sizes into the destination tree. */
	for (i=h->cnt-1; i>=h->cnt-pos; i--) {
		CID(dest, i+cnt) = CID(h, i);
		CSIZE(dest, i+cnt) = CSIZE(h, i);
	}
	for (i=cnt-1; i>=0; i--) {
		CID(dest, pos+i) = cid[i];
		CSIZE(dest, pos+i) = size[i];
	}
	for (i=pos-1; i>=0; i--) {
		CID(dest, i) = CID(h, i);
		CSIZE(dest, i) = CSIZE(h, i);
	}

	/* Build the tree over the cluster sizes. */
	build_tree(dest);

	return dest;
}


#if 0 /* below needs to be FIXED wrt s64/s32 */
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
#endif



static struct ctree* alloc_tree(long cnt)
{
	u32 height;
	int size;
	struct ctree *t;

	height = log2(cnt);
	size = sizeof(s64)*2*(1<<height);
	if (!(t = (struct ctree *)malloc(size)))
		return NULL;
	memset(t, 0, size);
	t->height = height;
	t->cnt = cnt;

	return t;
}

static void build_tree(struct ctree *t)
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
