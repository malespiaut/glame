#ifndef _GLDB_WORM_H
#define _GLDB_WORM_H

/*
 * gldb_worm.h
 * $Id: gldb_worm.h,v 1.6 2004/12/25 19:17:50 richi Exp $
 *
 * Copyright (C) 2000, 2001, 2004 Richard Guenther
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

#include "gldb.h"

/* Simple general purpose write once, read many database
 * for arbitrary (void * style) data.
 * Write once, read many means you may add an item and
 * after that never modify or remove it. This way shared
 * values between multiple databases works.
 */

typedef struct {
	gldb_item_t item;

	union {
		void *ptr;
		char *str;
		int i;
		float f;
	} u;
} glworm_t;


#ifdef __cplusplus
extern "C" {
#endif

void glwdb_init(gldb_t *db);

glworm_t *glworm_alloc(void);

int glwdb_add_item(gldb_t *db, glworm_t *item, const char *label);
const glworm_t *glwdb_query_item(gldb_t *db, const char *label);

#define glwdb_foreach_item(db, i) glame_list_foreach(&(db)->items, glworm_t, item.list, i)

#ifdef __cplusplus
}
#endif


#endif
