#ifndef _CG_HASH_H
#define _CG_HASH_H

/* #define inline */

/* customize, i.e. duplicate/prefix the macros/functions
 *
 * basic functions are
 *  - hash_alloc_cg
 *  - hash_free_cg
 *  - hash_add_cg
 *  - hash_remove_cg
 *  - hash_find_cg
 *
 * you need to fix
 *  - struct cg
 *  - input, _cg_hashfn, _cg_compare
 *
 * cg_t needs the fields
 *  - cg_t *next_hash
 *  - cg_t **pprev_hash
 *
 * ATM the hash is set up as "string" hash
 */


#define CG_HASH_BITS (8)
#define CG_HASH_SIZE (1 << CG_HASH_BITS)
extern cg_t **cg_hash_table;

#define hash_alloc_cg() do { \
  cg_hash_table = (cg_t **)malloc(CG_HASH_SIZE*sizeof(cg_t *)); \
  memset(cg_hash_table, 0, CG_HASH_SIZE*sizeof(cg_t *)); \
} while (0)
#define hash_free_cg() do { \
  free(cg_hash_table); \
} while (0)

static inline int _cg_hashfn(const char *str)
{
  int i, val = 0;

  for (i=0; i<strlen(str); i++)
    val += str[i];

  return (val & (CG_HASH_SIZE-1));
}
#define _cg_compare(str, n) (strcmp((n)->cg_name, str) == 0)


#define cg_hash(str) (cg_hash_table + _cg_hashfn(str))

static inline cg_t *__find_cg(const char *str, cg_t *t)
{
  goto inside;

  for (;;) { 
    t = t->next_hash;
inside:
    if (!t)
      break;
    if (_cg_compare(str, t))
      break;
  }
  return t;
}
#define hash_find_cg(str) __find_cg(str, *(cg_hash(str)))

#define __hash_add_cg(t, tt) do { \
  if (((t)->next_hash = *(tt)) != NULL) \
    (*(tt))->pprev_hash = &(t)->next_hash; \
  *(tt) = (t); \
  (t)->pprev_hash = (tt); \
} while (0)
#define hash_add_cg(t) __hash_add_cg(t, cg_hash((t)->cg_name))

#define hash_remove_cg(t) do { \
  if ((t)->pprev_hash) { \
    if ((t)->next_hash) \
      (t)->next_hash->pprev_hash = (t)->pprev_hash; \
    *(t)->pprev_hash = (t)->next_hash; \
    (t)->pprev_hash = NULL; \
  } \
} while (0)


#endif
