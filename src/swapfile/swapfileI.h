#ifndef _SWAPFILEI_H
#define _SWAPFILEI_H

/*
 * swapfileI.h
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

#include <sys/types.h>
#include <sys/param.h>
#include "list.h"


#ifndef MIN
#define MIN(a, b) ((a)<(b)?(a):(b))
#endif

struct logentry_s;
typedef struct logentry_s logentry_t;

struct filehead_s;
typedef struct filehead_s filehead_t;


struct cluster_s {
	struct list_head c_list; /* global, swap-index sorted list */
	int refcnt;              /* reference-count, cluster is free if == 0 */
	off_t off;               /* offset in swap */
	off_t size;              /* raw size of cluster */

	int id;                  /* unique identifier (used at load/save time) */

	int mmapcnt;             /* count of mappings */
	char *buf;               /* address of mmapped region */

	struct list_head rfc_list; /* reverse filecluster-list, unsorted */
	struct list_head map_list; /* mmap LRU cache list */
};
#define cluster_ref(cluster) do { (cluster)->refcnt++; } while (0)



#define FILEFLAG_WRITABLE  1
#define FILEFLAG_USED      2
struct filehead_s {
	struct list_head fh_list;
	struct list_head fc_list;
	filehead_t **pprev_hash;
	filehead_t *next_hash;

	fileid_t fid;
	int usecnt;                 /* == 0 -> file writable */
	int begincnt;               /* level of begin() calls */
	int logpos;                 /* position in log after undo (begin pos) (or -1) */
	logentry_t *top;            /* latest transaction vs actual state */
};
#define fclist_head(fh) (list_gethead(&(fh)->fc_list, filecluster_t, fc_list))
#define fclist_tail(fh) (list_gettail(&(fh)->fc_list, filecluster_t, fc_list))
#define file_use(file) do { if ((file)->usecnt == 0) hash_remove_file(file); (file)->usecnt++; } while (0)
#define file_unuse(file) do { (file)->usecnt--; if ((file)->usecnt == 0) hash_add_file(file); } while (0)


struct filecluster_s {
	struct list_head fc_list;   /* file-cluster list */
	filehead_t *f;
	off_t off;                  /* offset in file */
	off_t size;                 /* size of filecluster */
	cluster_t *cluster;         /* corresponding cluster or NULL,
				       if hole */
	off_t coff;                 /* offset in cluster */
	struct list_head rfc_list;  /* reverse filecluster-list */
};
#define fc_prev(fc) (((fc)->fc_list.prev == &fc->f->fc_list) ? NULL : list_entry((fc)->fc_list.prev, filecluster_t, fc_list))
#define fc_next(fc) (((fc)->fc_list.next == &fc->f->fc_list) ? NULL : list_entry((fc)->fc_list.next, filecluster_t, fc_list))


struct logentry_s {
	struct list_head le_list;
	filehead_t *f;
	int lid;                    /* used for ordering on disk */
	int op;
	union {
		struct {
			off_t pos;
			off_t size;
			filehead_t *f;
		} insert;
		struct {
			off_t pos;
			off_t size;
			filehead_t *f;
		} delete;
		struct {
		} begin;
		struct {
		} end;
	} u;
};




/* on-disk structures */

/* clustersize min 16kB, max 4MB */
#define CLUSTER_MINORDER (14)
#define CLUSTER_MINMASK ((1<<CLUSTER_MINORDER) - 1)
#define CLUSTER_MINSIZE (1<<CLUSTER_MINORDER)
#define CLUSTER_MAXORDER (22)
#define CLUSTER_MAXMASK ((1<<CLUSTER_MAXORDER) - 1)
#define CLUSTER_MAXSIZE (1<<CLUSTER_MAXORDER)


#define SWAP_MAGIC "GLAMESWAP0000001"
typedef struct {
	char magic[16];
	union {
		struct {
			off_t size;
			off_t data_off, data_size;
			off_t meta_off, meta_size;
		} header;
		struct {
			char reserved[256-16];
		} reserved;
	} u;
} swapd_header_t;


#define LOGENTRY_BEGIN  1
#define LOGENTRY_END    2
#define LOGENTRY_DELETE 3
#define LOGENTRY_INSERT 4

#define RECORD_MAGIC "GLRC"
#define RECORD_TYPE_UNUSED      0
#define RECORD_TYPE_CLUSTER     1
#define RECORD_TYPE_LOGENTRY    2
#define RECORD_TYPE_FILECLUSTER 4
#define RECORD_TYPE_FILEHEAD    5
#define RECORD_TYPE_EOF         7
typedef struct {
	char magic[4];
	int type;
	union {
		struct {
			off_t off;
			off_t size;
			int refcnt;
			int id;
		} cluster;
		struct {
			fileid_t fid;
			int usecnt;
			int begincnt;
			int logpos;
		} filehead;
		struct {
			int fid;
			off_t off;
			off_t size;
			int cid;
			off_t coff;
		} filecluster;
		struct {
			int fid;
			int lid;
			int op;
			union {
				struct {
					off_t pos;
					off_t size;
					int fid;
				} insert;
				struct {
					off_t pos;
					off_t size;
					int fid;
				} delete;
				struct {
				} begin;
				struct {
				} end;
			} u;
		} logentry;
		struct {
			int reserved[32];
		} reserved;
	} u;
} swapd_record_t;



#endif


