#ifndef _LIST_H
#define _LIST_H

/*
 * list.h
 *
 * $Id: list.h,v 1.24 2004/05/19 08:49:49 richi Exp $
 * 
 * Copyright (C) 1999, 2000 Richard Guenther
 *
 * This code was taken from the Linux kernel source which is
 * Copyright (C) by Linus Torvalds and others
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

#include "util.h"

/*
 * Simple doubly linked list implementation.
 *
 * Some of the internal functions ("__xxx") are useful when
 * manipulating whole lists rather than single entries, as
 * sometimes we already know the next/prev entries and we can
 * generate better code by using them directly rather than
 * using the generic single-entry routines.
 *
 * Some of the operations are implemented as macros to allow
 * the debugging checks print the "right" file/function.
 */

struct glame_list_head {
	struct glame_list_head *next, *prev;
};

#define GLAME_LIST_HEAD_INIT(name) { &(name), &(name) }

#define GLAME_LIST_HEAD(name) \
	struct glame_list_head name = GLAME_LIST_HEAD_INIT(name)

#define GLAME_INIT_LIST_HEAD(ptr) do { \
	(ptr)->next = (ptr); (ptr)->prev = (ptr); \
} while (0)

/*
 * Insert a new entry between two known consecutive entries. 
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static inline void __list_add(struct glame_list_head * newel,
	struct glame_list_head * prev,
	struct glame_list_head * next)
{
	next->prev = newel;
	newel->next = next;
	newel->prev = prev;
	prev->next = newel;
}

/*
 * Insert a new entry after the specified head..
 */
#ifndef NDEBUG
#define glame_list_add(n, h) do { \
        struct glame_list_head *___node = (n); \
        struct glame_list_head *___head = (h); \
	if (!(___node->next == ___node)) \
		DERROR("Adding already added list item"); \
	__list_add(___node, ___head, ___head->next); \
} while (0)
#else
#define glame_list_add(n, h) do { \
        struct glame_list_head *___node = (n); \
        struct glame_list_head *___head = (h); \
	__list_add(___node, ___head, ___head->next); \
} while (0)
#endif

/*
 * Insert a new entry before the specified head..
 */
#ifndef NDEBUG
#define glame_list_add_tail(n, t) do { \
        struct glame_list_head *___node = (n); \
        struct glame_list_head *___tail = (t); \
	if (!(___node->next == ___node)) \
		DERROR("Adding already added list item"); \
	__list_add(___node, ___tail->prev, ___tail); \
} while (0)
#else
#define glame_list_add_tail(n, t) do { \
        struct glame_list_head *___node = (n); \
        struct glame_list_head *___tail = (t); \
	__list_add(___node, ___tail->prev, ___tail); \
} while (0)
#endif


/*
 * Delete a list entry by making the prev/next entries
 * point to each other.
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static inline void __list_del(struct glame_list_head * prev,
				  struct glame_list_head * next)
{
	next->prev = prev;
	prev->next = next;
}

/* Note! glame_list_del() does not reinitialize the item, so things
 * like glame_list_empty() do not return true afterwards! */
#ifndef NDEBUG
#define glame_list_del(e) do { \
        struct glame_list_head *___node = (e); \
	if (___node->next == ___node) \
		DERROR("Removing already removed list item"); \
	__list_del(___node->prev, ___node->next); \
	GLAME_INIT_LIST_HEAD(___node); \
} while (0)
#else
#define glame_list_del(e) do { \
        struct glame_list_head *___node = (e); \
	__list_del(___node->prev, ___node->next); \
} while (0)
#endif

/* glame_list_del_init() deletes the element from the list and
 * reinitializes it, so glame_list_empty() on it will return true. */
#ifndef NDEBUG
#define glame_list_del_init(e) do { \
        struct glame_list_head *___node = (e); \
	if (___node->next == ___node) \
		DERROR("Removing already removed list item"); \
	__list_del(___node->prev, ___node->next); \
	GLAME_INIT_LIST_HEAD(___node); \
} while (0)
#else
#define glame_list_del_init(e) do { \
        struct glame_list_head *___node = (e); \
	__list_del(___node->prev, ___node->next); \
	GLAME_INIT_LIST_HEAD(___node); \
} while (0)
#endif


/* glame_list_empty() returns true, if the element is not in any list,
 * i.e. there are no further elements associated with it. */
static inline int glame_list_empty(struct glame_list_head *head)
{
	return head->next == head;
}

/* Splice in "list" into "head" */
static inline void glame_list_splice(struct glame_list_head *list, struct glame_list_head *head)
{
	struct glame_list_head *first = list->next;

	if (first != list) {
		struct glame_list_head *last = list->prev;
		struct glame_list_head *at = head->next;

		first->prev = head;
		head->next = first;

		last->next = at;
		at->prev = last;
	}
}

/* Splice from-to out of "list" */
static inline void glame_list_unsplice(struct glame_list_head *list,
				     struct glame_list_head *from, struct glame_list_head *to)
{
        list->next = from;
	list->prev = to;
	from->prev->next = to->next;
	to->next->prev = from->prev;
	from->prev = list;
	to->next = list;
}

/* Get a pointer to the specified type out of a ptr which points to the
 * struct glame_list_head inside type at struct member member. */
#define glame_list_entry(ptr, type, member) \
	((type *)((char *)(ptr)-(unsigned long)(&((type *)0)->member)))

/* Get head/tail entry of list like glame_list_entry(), but NULL if list is empty. */
#define glame_list_gethead(listptr, type, member) ((glame_list_empty(listptr)) ? ((type *)NULL) : \
	(glame_list_entry((listptr)->next, type, member)))
#define glame_list_gettail(listptr, type, member) ((glame_list_empty(listptr)) ? ((type *)NULL) : \
	(glame_list_entry((listptr)->prev, type, member)))

/* Get next/prev item after/before itemptr out of list listptr like glame_list_entry(), but
 * NULL if no further item is available. */
#define glame_list_getnext(listptr, itemptr, type, member) ((itemptr)->member.next == (listptr) \
	? NULL : glame_list_entry((itemptr)->member.next, type, member))
#define glame_list_getprev(listptr, itemptr, type, member) ((itemptr)->member.prev == (listptr) \
	? NULL : glame_list_entry((itemptr)->member.prev, type, member))

/* Iterate through all list items using entryvar. This is not remove-safe!! */
#define glame_list_foreach(listp, type, member, entryvar) for (entryvar = glame_list_entry((listp)->next, type, member); &entryvar->member != (listp) || (entryvar = NULL); entryvar = glame_list_entry(entryvar->member.next, type, member))

/* Iterate through all list items using entryvar. It is safe to remove
 * entryvar, but _only_ entryvar. Provide an additional dummy pointer
 * for internal use. */
#ifdef __cplusplus
#define glame_list_safe_foreach(listptr, type, member, dummy_ignored, entryvar) for (glame_list_head *dummy = (entryvar = glame_list_entry((listptr)->next, type, member), (listptr)->next->next); (&entryvar->member != (listptr) && (!glame_list_empty(&entryvar->member) || DERROR_eval("glame_list_safe_foreach remove error"))) || (entryvar = NULL); entryvar = glame_list_entry(dummy, type, member), dummy = ((struct glame_list_head *)dummy)->next) 
#else
#define glame_list_safe_foreach(listptr, type, member, dummy, entryvar) for (entryvar = glame_list_entry((listptr)->next, type, member), dummy = (void *)(listptr)->next->next; (&entryvar->member != (listptr) && (!glame_list_empty(&entryvar->member) || DERROR_eval("glame_list_safe_foreach remove error"))) || (entryvar = NULL); entryvar = glame_list_entry((struct glame_list_head *)dummy, type, member), dummy = (void *)((struct glame_list_head *)dummy)->next) 
#endif

/* Count the number of items in the list. */
static inline int glame_list_count(struct glame_list_head *l)
{
	struct glame_list_head *lh = l;
	int cnt = 0;
	while ((lh = lh->next) != l)
		cnt++;
	return cnt;
}


#endif
