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
    u32 cnt;   /* maybe we need some flags - or store the number of  *
		* actual stored clusters which is <= 1<<height       */
    s64 total_size; /* really the head node of the following tree    */
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


static long _find_cluster(struct ctree *h, s64 offset, s64 *coff);

static void _replace_cluster(struct ctree *h, long pos,
			     u32 cid, s32 size);

static struct ctree *_insert_clusters(struct ctree *h, int pos,
				      int cnt, u32 *cid, s32 *size);


#endif
