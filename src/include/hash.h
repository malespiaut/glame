#ifndef _HASH_H
#define _HASH_H

/*
 * hash.h
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

/* Static hashtable generator, use like
 *   struct node {
 *       struct node **pprev_node_hash;
 *       struct node *next_node_hash;
 *       int id;
 *       ...
 *   };
 *   HASH(node, struct node, 8,
 *        (node->id == id),
 *        (id),
 *        (node->id),
 *        int id)
 * to have a struct node hash with id as the hash key and a
 * 1<<8 sized hashtable. The following functions are created:
 *   hash_add_node(struct node *)
 *   hash_remove_node(struct node *)
 *   hash_find_node(int id)
 *   hash_find_next_node(int id, struct node *)
 *   hash_next_node(struct node *)
 *   hash_init_node(struct node *)
 *   is_hashed_node(struct node *)
 *   hash_getslot_node(int slot)
 * 
 * So the HASH macro would have the following prototype:
 * HASH(symbol name, type recordtype, int hashbits, expr comparison,
 *      expr hash(...), expr hash(recordtype), hash arguments...)
 */



#define HASH(FOOBAR, FOOBARtype, HASH_BITS, HASH_COMPARE, HASH_HASHFN_FROM_PARAMS, HASH_HASHFN_FROM_TYPE, params...) \
\
static FOOBARtype *FOOBAR##_hash_table[1<<HASH_BITS]; \
\
static inline FOOBARtype *hash_getslot_##FOOBAR(int slot) \
{ \
    return FOOBAR##_hash_table[slot&((1<<HASH_BITS)-1)]; \
} \
\
static inline FOOBARtype *hash_find_##FOOBAR(params) \
{ \
  FOOBARtype *FOOBAR = FOOBAR##_hash_table[(HASH_HASHFN_FROM_PARAMS)&((1<<HASH_BITS)-1)]; \
  goto inside; \
\
  for (;;) { \
    FOOBAR = FOOBAR->next_##FOOBAR##_hash; \
inside: \
    if (!FOOBAR) \
      break; \
    if (HASH_COMPARE) \
      break; \
  } \
  return FOOBAR; \
} \
\
static inline FOOBARtype *hash_find_next_##FOOBAR(params, FOOBARtype *FOOBAR) \
{ \
  if (!FOOBAR) { \
    FOOBAR = FOOBAR##_hash_table[(HASH_HASHFN_FROM_PARAMS)&((1<<HASH_BITS)-1)]; \
    goto inside; \
  } \
\
  for (;;) { \
    FOOBAR = FOOBAR->next_##FOOBAR##_hash; \
inside: \
    if (!FOOBAR) \
      break; \
    if (HASH_COMPARE) \
      break; \
  } \
  return FOOBAR; \
} \
\
static inline FOOBARtype *hash_next_##FOOBAR(FOOBARtype *FOOBAR) \
{ \
        return (FOOBAR)->next_##FOOBAR##_hash; \
} \
\
static inline void hash_add_##FOOBAR(FOOBARtype *FOOBAR) \
{ \
        FOOBARtype **tt = &FOOBAR##_hash_table[(HASH_HASHFN_FROM_TYPE)&((1<<HASH_BITS)-1)]; \
	if (((FOOBAR)->next_##FOOBAR##_hash = *(tt)) != NULL) \
		(*(tt))->pprev_##FOOBAR##_hash = &(FOOBAR)->next_##FOOBAR##_hash; \
	*(tt) = (FOOBAR); \
	(FOOBAR)->pprev_##FOOBAR##_hash = (tt); \
} \
\
static inline void hash_remove_##FOOBAR(FOOBARtype *t) \
{ \
	if (!t->pprev_##FOOBAR##_hash) \
		return; \
	if (t->next_##FOOBAR##_hash) \
		t->next_##FOOBAR##_hash->pprev_##FOOBAR##_hash = t->pprev_##FOOBAR##_hash; \
	*t->pprev_##FOOBAR##_hash = t->next_##FOOBAR##_hash; \
	t->pprev_##FOOBAR##_hash = NULL; \
} \
\
static inline void hash_init_##FOOBAR(FOOBARtype *t) \
{ \
	t->pprev_##FOOBAR##_hash = NULL; \
} \
\
static inline int is_hashed_##FOOBAR(FOOBARtype *t) \
{ \
	return t->pprev_##FOOBAR##_hash != NULL; \
}


#endif
