#ifndef _SWAPFILE_HASH_H
#define _SWAPFILE_HASH_H

/*
 * swapfile_hash.h
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


#include "swapfileI.h"

/* #define inline */

#define hash_alloc_swapfile() do { \
  swapfile_hash_table = (filehead_t **)malloc(SWAPFILE_HASH_SIZE*sizeof(filehead_t *)); \
  memset(swapfile_hash_table, 0, SWAPFILE_HASH_SIZE*sizeof(filehead_t *)); \
} while (0)
#define hash_free_swapfile() do { \
  free(swapfile_hash_table); \
} while (0)


#define _file_hashfn(fileid) ((fileid) & (SWAPFILE_HASH_SIZE-1))
#define _file_compare(fileid, n) ((n)->fid == (fileid))

#define file_hash(fileid) (swapfile_hash_table + _file_hashfn(fileid))

static inline filehead_t *__find_file(fileid_t fid, filehead_t *t)
{
  goto inside;

  for (;;) { 
    t = t->next_hash;
inside:
    if (!t)
      break;
    if (_file_compare(fid, t))
      break;
  }
  return t;
}
#define hash_find_file(fileid) __find_file((fileid), *(file_hash(fileid)))

#define __hash_add_file(t, tt) do { \
  if (((t)->next_hash = *(tt)) != NULL) \
    (*(tt))->pprev_hash = &(t)->next_hash; \
  *(tt) = (t); \
  (t)->pprev_hash = (tt); \
} while (0)
#define hash_add_file(t) __hash_add_file(t, file_hash((t)->fid))

#define hash_remove_file(t) do { \
  if ((t)->pprev_hash) { \
    if ((t)->next_hash) \
      (t)->next_hash->pprev_hash = (t)->pprev_hash; \
    *(t)->pprev_hash = (t)->next_hash; \
    (t)->pprev_hash = NULL; \
  } \
} while (0)

#define hash_init_file(t) do { (t)->pprev_hash = NULL; } while (0)
#define is_hashed_file(t) ((t)->pprev_hash != NULL)

#endif
