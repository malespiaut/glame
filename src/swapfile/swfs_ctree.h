/*
 * swfs_ctree.h
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

#ifndef _SWFS_CTREE_H
#define _SWFS_CTREE_H

/* Cluster binary tree routines. The clusters of a file are
 * organized in a binary tree consisting of nodes that contain
 * the size of its siblings (example tree height is 3):
 *
 * s64 s00 [height, flags]
 *                            s01                    0  h-3
 *                  s02                 s03          1  h-2
 *             s04       s05       s06       s07     2  h-1
 * s32       s16 s17   s18 s19   s20 s21   s22 s23   3  h
 * u32       i24 i25   i26 i27   i28 i29   i30 i31
 *
 * where s01 is the total size of the clusters, s01 = s02+s03,
 * s04 = s16+17, s03=s06+s07, etc...
 *
 * The tree is always filled "from the left", i.e. leaf nodes
 * span the tree.
 * Its obvious that this way an O(logN) query of a cluster, its
 * starting offset in the file and its size can be obtained for
 * a given file offset. Also its obvious that O(NlogN) storage
 * is required.
 */


/* This, of course has to be FIXED. */
#define u32 unsigned long
#define s32 signed long
#define s64 signed long long


/* A tree head structure - dummy of course, and some macros
 * transforming a pointer to such structure to pointers to
 * the beginning of the cluster sizes/ids arrays and to the
 * beginning of the tree that is suited for good algorithm
 * implementation - tree[1] being the s64 total_size. Other
 * nice transforms are (in the s64 part of the tree):
 *           tree[i]                     tree[i>>1]
 *  tree[2*i]       tree[2*i+1]   tree[i]          tree[i+1]
 * for the transition from the s64 part to the s32 part the
 * situation is as follows:
 *                       s64 *tree[i]
 * s32 *tree[2*i + 1<<height]   s32 *tree[2*i+1 + 1<<height]
 * the reverse transition would be:
 *                 s64 *tree[(i - 1<<height)>>1]
 *      s32 *tree[i]                          s32 *tree[i+1]
 */
struct ctree {
    u32 height; /* we _need_ 32 bits here, as 1<<32 == max clusterid */
    u32 cnt;    /* maybe we need some flags - or store the number of *
		 * actual stored clusters which is <= 1<<height      */
    s64 size;   /* really the head node of the following tree        */
    /* s64 inner_tree[(1<<height) - 2];      -> max filesize         */
    /* s32 leafs_cluster_sizes[1<<height];   -> max clustersize      */
    /* u32 cluster_ids[1<<height];           -> max clusterid        */
};

#define CTREESIZE(height) (4*(1<<height)*sizeof(s64))

#define CTREE64(h) (s64 *)(h)
#define CTREE32(h) (s32 *)(h)

#define CSIZES(h) ((s32 *)(h) + 2*(1<<(h)->height))
#define CIDS(h) ((u32 *)(h) + 3*(1<<(h)->height))

#define CID(h, off) (((u32 *)(h))[3*(1<<(h)->height) + (off)])
#define CSIZE(h, off) (((s32 *)(h))[2*(1<<(h)->height) + (off)])
#define CSUM(h, level, off) (((s64 *)(h))[(1<<(level)) + off])


/* Find the position of the cluster that contains offset in the
 * cluster ids/sizes array of the cluster tree. Stores the cluster
 * start offset inside coff. Returns -1 if offset is not inside the
 * tree, else an array position 0 <= pos < h->cnt. The return value
 * can be used for CID() and CSIZE(). */
static long ctree_find(struct ctree *h, s64 offset, s64 *coff);

/* Zeroes CID and CSIZE for cnt entries starting from position pos.
 * Does not fix h->cnt. */
static void ctree_zero(struct ctree *h, long pos, long cnt);

/* Replaces the cluster at position pos inside the ids/sizes array
 * of the cluster tree with a cluster with id cid and size size. */
static void ctree_replace1(struct ctree *h, long pos,
			   u32 cid, s32 size);

/* Replaces cnt clusters starting at position pos inside the ids/sizes
 * array of the cluster tree with the clusters specified by the cid
 * and size arrays. This is designed to work with the cid and size
 * array overlapping with the cid and size array from the ctree h -
 * but if they do overlap, you should better ensure cid/size have the
 * same ID. */
static void ctree_replace(struct ctree *h, long pos, long cnt,
			  u32 *cid, s32 *size);

static struct ctree *ctree_insert1(struct ctree *h, long pos,
				   u32 cid, s32 size);

/* Inserts cnt clusters with ids/sizes inside the cid/size arrays
 * at position pos inside the ids/sizes array of the cluster tree.
 * Cnt clusters from position pos on are moved cnt positions to
 * the right. Returns the pointer to the updated struct ctree as
 * it may have to be reallocated to make room for the new clusters.
 * This is designed to allow for an insertion point inside the
 * cid/size array, i.e. self-insertion of a part of h. */
static struct ctree *ctree_insert(struct ctree *h, long pos, long cnt,
				  u32 *cid, s32 *size);

/* Removes cnt clusters from position pos inside the ids/sizes array
 * of the cluster tree. Stores the removed clusters ids/sizes into
 * the cid/csize arrays. The clusters starting at pos + cnt positions
 * are moved cnt positions to the left. Returns the pointer to the
 * updated struct ctree as it may be reallocated to free the space no
 * longer needed for the removed clusters. */
static struct ctree *ctree_remove(struct ctree *h, long pos, long cnt,
				  u32 *cid, s32 *csize);


#endif
