#ifndef _LINUX_LIST_H
#define _LINUX_LIST_H

/*
 * list.h
 *
 * $Id: list.h,v 1.9 2000/04/11 14:39:17 richi Exp $
 * 
 * Copyright (C) 1999, 2000 Richard Guenther
 *
 * This code was taken from the Linux kernel source which is
 * Copyright (C) by Linus Torvalds
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
 */

struct list_head {
	struct list_head *next, *prev;
};

#define LIST_HEAD_INIT(name) { &(name), &(name) }

#define LIST_HEAD(name) \
	struct list_head name = LIST_HEAD_INIT(name)

#define INIT_LIST_HEAD(ptr) do { \
	(ptr)->next = (ptr); (ptr)->prev = (ptr); \
} while (0)

/*
 * Insert a new entry between two known consecutive entries. 
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static inline void __list_add(struct list_head * new,
	struct list_head * prev,
	struct list_head * next)
{
	next->prev = new;
	new->next = next;
	new->prev = prev;
	prev->next = new;
}

/*
 * Insert a new entry after the specified head..
 */
#define list_add(n, h) do { \
	if (!((n)->next == (n))) \
		DERROR("Adding already added list item"); \
	__list_add((n), (h), (h)->next); \
} while (0)
#if 0
static inline void list_add(struct list_head *new, struct list_head *head)
{
        if (!(new->next == new))
	       DERROR("Adding already added list item");
	__list_add(new, head, head->next);
}
#endif

/*
 * Insert a new entry before the specified head..
 */
#define list_add_tail(n, h) do { \
	if (!((n)->next == (n))) \
		DERROR("Adding already added list item"); \
	__list_add((n), (h)->prev, (h)); \
} while (0)
#if 0
static inline void list_add_tail(struct list_head *new, struct list_head *head)
{
        if (!(new->next == new))
	       DERROR("Adding already added list item");
	__list_add(new, head->prev, head);
}
#endif

/*
 * Delete a list entry by making the prev/next entries
 * point to each other.
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static inline void __list_del(struct list_head * prev,
				  struct list_head * next)
{
	next->prev = prev;
	prev->next = next;
}

#define list_del(e) do { \
	if ((e)->next == (e)) \
		DERROR("Removing already removed list item"); \
	__list_del((e)->prev, (e)->next); \
	INIT_LIST_HEAD(e); \
} while (0)
#if 0
static inline void list_del(struct list_head *entry)
{
        if (entry->next == entry)
	       DERROR("Removing already removed list item");
	__list_del(entry->prev, entry->next);
#ifndef NDEBUG
	INIT_LIST_HEAD(entry);
#endif
}
#endif

static inline int list_empty(struct list_head *head)
{
	return head->next == head;
}

/*
 * Splice in "list" into "head"
 */
static inline void list_splice(struct list_head *list, struct list_head *head)
{
	struct list_head *first = list->next;

	if (first != list) {
		struct list_head *last = list->prev;
		struct list_head *at = head->next;

		first->prev = head;
		head->next = first;

		last->next = at;
		at->prev = last;
	}
}

/*
 * Splice from-to out of "list"
 */
static inline void list_unsplice(struct list_head *list,
				     struct list_head *from, struct list_head *to)
{
        list->next = from;
	list->prev = to;
	from->prev->next = to->next;
	to->next->prev = from->prev;
	from->prev = list;
	to->next = list;
}


#define list_entry(ptr, type, member) \
	((type *)((char *)(ptr)-(unsigned long)(&((type *)0)->member)))


#define list_gethead(ptr, type, member) ((list_empty(ptr)) ? ((type *)NULL) : (list_entry((ptr)->next, type, member)))
#define list_gettail(ptr, type, member) ((list_empty(ptr)) ? ((type *)NULL) : (list_entry((ptr)->prev, type, member)))


/* this is not remove-safe!! */
#define list_foreach(listp, type, member, entryvar) for (entryvar = list_entry((listp)->next, type, member); &entryvar->member != (listp); entryvar = list_entry(entryvar->member.next, type, member))


static inline int list_count(struct list_head *l)
{
	struct list_head *lh = l;
	int cnt = 0;
	while ((lh = lh->next) != l)
		cnt++;
	return cnt;
}


#endif
