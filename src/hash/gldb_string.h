#ifndef _GLDB_STRING_H
#define _GLDB_STRING_H

/*
 * gldb_string.h
 * $Id: gldb_string.h,v 1.2 2000/05/01 12:22:16 richi Exp $
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

#include "gldb.h"

typedef struct {
	gldb_item_t entry;
	char *str;
} sitem_t;

#define sitem_label(si) ((si)->entry.label)
#define sitem_str(si) ((si)->str)

void glsdb_init(gldb_t *db);

#define glsdb_delete(db) gldb_delete(db)
#define glsdb_copy(d, s) gldb_copy((d), (s))

#define glsdb_foreach_item(pdb, i) list_foreach(&(pdb)->item, sitem_t, entry.list, i)

void glsdb_set(gldb_t *db, const char *str, const char *label);
void glsdb_remove(gldb_t *db, const char *label);
char *glsdb_query(gldb_t *db, const char *label);


#endif
