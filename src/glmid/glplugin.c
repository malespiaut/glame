/*
 * glplugin.c
 * $Id: glplugin.c,v 1.6 2000/03/25 21:16:14 richi Exp $
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

#include <string.h>
#include <dlfcn.h>
#include "util.h"
#include "list.h"
#include "glplugin.h"


static struct list_head plugin_path_list = LIST_HEAD_INIT(plugin_path_list);
static struct list_head plugin_list = LIST_HEAD_INIT(plugin_list);

#define list_add_plugin_path(p) list_add(&(p)->list, &plugin_path_list)
#define plugin_foreach_path(p) list_foreach(&plugin_path_list, plugin_path_t, \
        list, p)
#define list_add_plugin(p) list_add(&(p)->list, &plugin_list)
#define hash_add_plugin(p) _hash_add(&(p)->hash, _hash((p)->name, \
        PLUGIN_NAMESPACE))
#define hash_find_plugin(nm) __hash_entry(_hash_find((nm), PLUGIN_NAMESPACE, \
        _hash((nm), PLUGIN_NAMESPACE), __hash_pos(plugin_t, hash, name, \
        namespace)), plugin_t, hash)
#define hash_init_plugin(p) do { _hash_init(&p->hash); (p)->namespace = PLUGIN_NAMESPACE; } while (0)


int plugin_add_path(const char *path)
{
	plugin_path_t *p;

	if (!(p = ALLOC(plugin_path_t)))
		return -1;
	INIT_LIST_HEAD(&p->list);
	p->path = strdup(path);
	list_add_plugin_path(p);

	return 0;
}

static plugin_t *plugin_add(const char *name, const char *filename);
static plugin_t *plugin_load(const char *name, const char *filename)
{
        char s[256], *sp;
	plugin_t *p;

	if (!(p = ALLOC(plugin_t)))
		return NULL;
	INIT_LIST_HEAD(&p->list);
	hash_init_plugin(p);

	if (!(p->name = strdup(name)))
		goto err;
	if (!(p->handle = dlopen(filename, RTLD_NOW)))
		goto err;

	/* either register() or a plugin set string */
	snprintf(s, 255, "%s_register", name);
	p->reg_func = dlsym(p->handle, s);
	snprintf(s, 255, "%s_set", name);
	p->set = dlsym(p->handle, s);
	if (!p->reg_func && !p->set)
		goto err_close;

	snprintf(s, 255, "%s_pixmap", name);
	p->pixmap = dlsym(p->handle, s);
	snprintf(s, 255, "%s_description", name);
	p->description = dlsym(p->handle, s);

	if (p->reg_func && p->reg_func() == -1)
		goto err_close;
	if (p->set) {
		/* add all plugins contained in the plugin set */
		snprintf(s, 255, "%s", *(p->set));
		sp = s;
		do {
			name = sp;
			if ((sp = strchr(name, ' ')))
				*(sp++) = '\0';
			plugin_add(name, filename);
		} while (sp);
	}
	return p;

 err_close:
	dlclose(p->handle);
 err:
	free((char *)(p->name));
	free(p);
	return NULL;
}

static plugin_t *plugin_add(const char *name, const char *filename)
{
	plugin_t *p;

	if (!(p = plugin_load(name, filename)))
		return NULL;
	hash_add_plugin(p);
	list_add_plugin(p);
	return p;	
}

plugin_t *plugin_get(const char *name)
{
	plugin_t *p = NULL;
	plugin_path_t *path;
	char filename[256];

	if (!name)
		return NULL;

	/* already loaded? */
	if ((p = hash_find_plugin(name)))
		return p;

	/* first try to look for an "in-core" plugin */
	if ((p = plugin_add(name, NULL)))
		return p;

	/* try each path until plugin found */
	plugin_foreach_path(path) {
		sprintf(filename, "%s/%s.so", path->path, name);
		if ((p = plugin_add(name, filename)))
			return p;
	}

	/* last try LD_LIBRARY_PATH supported plugins */
	sprintf(filename, "%s.so", name);
	if ((p = plugin_add(name, filename)))
	    return p;

	return NULL;
}

void *plugin_get_symbol(plugin_t *p, const char *symbol)
{
	if (!p || !symbol)
		return NULL;

	return dlsym(p->handle, symbol);
}
