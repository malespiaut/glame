#ifndef _FILTERHASH_H
#define _FILTERHASH_H

/*
 * filter_hash.h
 * $Id: filter_hash.h,v 1.1 2000/01/20 14:54:19 richi Exp $
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


/* customize, i.e. duplicate/prefix the macros/functions
 *
 * basic functions are
 *  - hash_alloc_filter
 *  - hash_free_filter
 *  - hash_add_filter
 *  - hash_remove_filter
 *  - hash_find_filter
 *
 * you need to fix
 *  - filter_t
 *  - input, _filter_hashfn, _filter_compare
 *
 * filter_t needs the fields
 *  - filter_t *next_hash
 *  - filter_t **pprev_hash
 */


#define FILTER_HASH_BITS (8)
#define FILTER_HASH_SIZE (1 << FILTER_HASH_BITS)
extern filter_t **filter_hash_table;

#define hash_alloc_filter() do { \
  filter_hash_table = (filter_t **)malloc(FILTER_HASH_SIZE*sizeof(filter_t *)); \
} while (0)
#define hash_free_filter() do { \
  free(filter_hash_table); \
} while (0)

extern inline int _filter_hashfn(const char *input)
{
	int i, val = 0;

	for (i=0; i<strlen(input); i++)
		val += (input)[i];

	return (val & (FILTER_HASH_SIZE-1));
}

#define _filter_compare(input, t) ((strcmp((t)->name, input)) == 0)


#define filter_hash(input) (filter_hash_table + _filter_hashfn(input))

extern inline filter_t *__find_filter(const char *input, filter_t *t)
{
  goto inside;

  for (;;) { 
    t = t->next_hash;
inside:
    if (!t)
      break;
    if (_filter_compare(input, t))
      break;
  }
  return t;
}
#define hash_find_filter(input) __find_filter(input, (*(filter_hash(input))))

#define __hash_add_filter(t, tt) do { \
  if (((t)->next_hash = *(tt)) != NULL) \
    (*(tt))->pprev_hash = &(t)->next_hash; \
  *(tt) = (t); \
  (t)->pprev_hash = (tt); \
} while (0)
#define hash_add_filter(t) __hash_add_filter(t, filter_hash(t->name))

#define hash_remove_filter(t) do { \
  if ((t)->pprev_hash) { \
    if ((t)->next_hash) \
      (t)->next_hash->pprev_hash = (t)->pprev_hash; \
    *(t)->pprev_hash = (t)->next_hash; \
    (t)->pprev_hash = NULL; \
  } \
} while (0)


#endif
