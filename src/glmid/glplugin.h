#ifndef _GLPLUGIN_H
#define _GLPLUGIN_H

/*
 * glplugin.h
 * $Id: glplugin.h,v 1.2 2000/03/15 16:29:32 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther
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

#include "list.h"
#include "glame_hash.h"


typedef struct {
	struct list_head list;
	const char *path;
} plugin_path_t;

typedef struct {
	struct list_head list;
	struct hash_head hash;
	void *namespace;

	void *handle;
	int (*reg_func)(void);

	const char *name;
	const char **description;
	void **pixmap;
} plugin_t;
#define plugin_name(p) ((p)->name)
#define plugin_description(p) (*((p)->description))
#define plugin_pixmap(p) (*((p)->pixmap))


#ifdef __cplusplus
extern "C" {
#endif


/* Adds the specified path to the plugin path. Plugins will be
 * searched using the specified name in all paths. 
 * Returns -1 on error, 0 on success. */
int plugin_add_path(const char *path);

/* Gets the handle of the plugin with the specified name. If not
 * already loaded, the plugin will be searched using the available
 * paths. Returns the plugin handle or NULL on error. */
plugin_t *plugin_get(const char *name);

/* Gets a non-standard symbol from the plugin shared object. Can
 * return NULL, if the symbol was not found or the value of the
 * symbol is NULL. */
void *plugin_get_symbol(plugin_t *p, const char *symbol);


#ifdef __cplusplus
}
#endif


#endif
