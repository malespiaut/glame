#ifndef _GLPLUGIN_H
#define _GLPLUGIN_H

/*
 * glplugin.h
 * $Id: glplugin.h,v 1.12 2000/12/07 16:07:03 richi Exp $
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
#include "gldb_worm.h"


typedef struct {
	struct list_head list;   /* linkage in plugin list/hash */
	struct hash_head hash;
	void *nmspace;

	const char *name;        /* plugin name */

	void *handle;            /* dlopen handle */

	/* Other than dlsym() information, "configurable".
	 */
	gldb_t db;
} plugin_t;

#define plugin_name(p) ((p) ? (p)->name : NULL)


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

/* Create and add a plugin manually. */
plugin_t *plugin_add(const char *name);

/* Remove a plugin. USE WITH EXTREME CARE! YOU HAVE TO ENSURE
 * NO USER IS LEFT YOURSELF! */
void _plugin_delete(plugin_t *p);


/* Set plugin key/value pair. */
int plugin_set(plugin_t *p, const char *key, const void *val);

/* Gets a information from the plugin shared object by key. Can
 * return NULL, if the information was not found or the value of the
 * information is NULL.
 * As you will obviously know the actual type of the information
 * you usually want to cast the (void *) to an appropriate type. */
void *plugin_query(plugin_t *p, const char *key);


/* Browse through the list of registered plugins. NULL gets
 * the first available plugin. */
plugin_t *plugin_next(plugin_t *plugin);


/* Macro to create the plugin set symbol & value. */
#define PLUGIN_SET(name, pset) char *name ## _set = pset;

/* Standard db entry keys. */
#define PLUGIN_DESCRIPTION "desc"
#define PLUGIN_PIXMAP "pixmap"
#define PLUGIN_CATEGORY "category"
#define PLUGIN_FILTER "filter"
#define PLUGIN_PARENT "parent"


#ifdef __cplusplus
}
#endif


#endif
