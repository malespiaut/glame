#ifndef _FOOBAR_HASH_H
#define _FOOBAR_HASH_H

/*
 * foobar_hash.h
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "FOOBAR.h"

/* #define inline */

/* customize, i.e. duplicate/prefix the macros/functions
 *
 * basic functions are
 *  - hash_alloc_foobar
 *  - hash_free_foobar
 *  - hash_add_foobar
 *  - hash_remove_foobar
 *  - hash_find_foobar
 *
 * you need to fix
 *  - struct foobar
 *  - input, _foobar_hashfn, _foobar_compare
 *
 * foobartype needs the fields
 *  - foobartype *next_hash
 *  - foobartype **pprev_hash
 *
 * ATM the hash is set up as "string" hash
 */


#define FOOBAR_HASH_BITS (8)
#define FOOBAR_HASH_SIZE (1 << FOOBAR_HASH_BITS)
extern foobartype **foobar_hash_table;

#define hash_alloc_foobar() do { \
  foobar_hash_table = (foobartype **)malloc(FOOBAR_HASH_SIZE*sizeof(foobartype *)); \
  memset(foobar_hash_table, 0, FOOBAR_HASH_SIZE*sizeof(foobartype *)); \
} while (0)
#define hash_free_foobar() do { \
  free(foobar_hash_table); \
} while (0)

static inline int _foobar_hashfn(const char *str)
{
  int i, val = 0;

  for (i=0; i<strlen(str); i++)
    val += str[i];

  return (val & (FOOBAR_HASH_SIZE-1));
}
#define _foobar_compare(str, n) (strcmp((n)->name, str) == 0)


#define foobar_hash(str) (foobar_hash_table + _foobar_hashfn(str))

static inline foobartype *__find_foobar(const char *str, foobartype *t)
{
  goto inside;

  for (;;) { 
    t = t->next_hash;
inside:
    if (!t)
      break;
    if (_foobar_compare(str, t))
      break;
  }
  return t;
}
#define hash_find_foobar(str) __find_foobar(str, *(foobar_hash(str)))

#define __hash_add_foobar(t, tt) do { \
  if (((t)->next_hash = *(tt)) != NULL) \
    (*(tt))->pprev_hash = &(t)->next_hash; \
  *(tt) = (t); \
  (t)->pprev_hash = (tt); \
} while (0)
#define hash_add_foobar(t) __hash_add_foobar(t, foobar_hash((t)->name))

#define hash_remove_foobar(t) do { \
  if ((t)->pprev_hash) { \
    if ((t)->next_hash) \
      (t)->next_hash->pprev_hash = (t)->pprev_hash; \
    *(t)->pprev_hash = (t)->next_hash; \
    (t)->pprev_hash = NULL; \
  } \
} while (0)


#endif
