/*
 * glplugin.c
 * $Id: glplugin.c,v 1.17 2000/05/22 11:06:45 richi Exp $
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
#include "ladspa.h"


typedef struct {
	struct list_head list;
	const char *path;
} plugin_path_t;

static struct list_head plugin_path_list = LIST_HEAD_INIT(plugin_path_list);
static struct list_head plugin_list = LIST_HEAD_INIT(plugin_list);

#define list_add_plugin_path(p) list_add(&(p)->list, &plugin_path_list)
#define plugin_foreach_path(p) list_foreach(&plugin_path_list, plugin_path_t, \
        list, p)
#define list_add_plugin(p) list_add(&(p)->list, &plugin_list)
#define list_del_plugin(p) list_del(&(p)->list)
#define hash_add_plugin(p) _hash_add(&(p)->hash, _hash((p)->name, \
        PLUGIN_NAMESPACE))
#define hash_remove_plugin(p) _hash_remove(&(p)->hash)
#define hash_find_plugin(nm) __hash_entry(_hash_find((nm), PLUGIN_NAMESPACE, \
        _hash((nm), PLUGIN_NAMESPACE), __hash_pos(plugin_t, hash, name, \
        namespace)), plugin_t, hash)
#define hash_init_plugin(p) do { _hash_init(&p->hash); (p)->namespace = PLUGIN_NAMESPACE; } while (0)
#define is_hashed_plugin(p) _is_hashed(&(p)->hash)


/* Helpers.
 */

static void mangle_name(char *dest, const char *name)
{
	strncpy(dest, name, 32);
	while ((dest = strchr(dest, '-')))
		*dest = '_';
}


/* Plugin path & "initial load" (TODO) code.
 */

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


/* The plugin stuff itself.
 */

static plugin_t *_plugin_alloc(const char *name)
{
	plugin_t *p;

	if (!(p = (plugin_t *)malloc(sizeof(plugin_t))))
		return NULL;
	INIT_LIST_HEAD(&p->list);
	hash_init_plugin(p);
	if (!(p->name = strdup(name))) {
		free(p);
		return NULL;
	}
	p->handle = NULL;
	glwdb_init(&p->db);

	return p;
}

static void _plugin_free(plugin_t *p)
{
	if (!p)
		return;
	if (p->name)
		free((char *)p->name);
	if (p->handle)
		dlclose(p->handle);
	gldb_delete(&p->db);
	free(p);
}

static int _plugin_add(plugin_t *p)
{
	/* atomic check if name is occupied & add */
	hash_lock();
	if (__hash_find(p->name, PLUGIN_NAMESPACE,
			_hash(p->name, PLUGIN_NAMESPACE),
			__hash_pos(plugin_t, hash, name, namespace))) {
		hash_unlock();
		return -1;
	}
	__hash_add(&p->hash, _hash(p->name, PLUGIN_NAMESPACE));
	hash_unlock();

	list_add_plugin(p);

	return 0;
}


static int try_init_glame_plugin(plugin_t *p, const char *name,
				 const char *filename)
{
	int (*reg_func)(plugin_t *);
	char **set;
        char s[256], *sp, *psname;
	plugin_t *pn;

	/* GLAME plugins do have either name_register() or
	 * name_set symbols defined. */
	snprintf(s, 255, "%s_register", name);
	reg_func = dlsym(p->handle, s);
	snprintf(s, 255, "%s_set", name);
	set = dlsym(p->handle, s);
	if (!reg_func && !set)
		return -1;

	/* First call the name_register() function if
	 * provided. */
	if (reg_func && reg_func(p) == -1)
		return -1;

	/* If there is no name_set string, we are done now. */
	if (!set)
		return 0;

	/* If there is a name_set string try to initialize
	 * plugins with the space seperated names in the string. */
	snprintf(s, 255, "%s", *set);
	sp = s;
	do {
		psname = sp;
		if ((sp = strchr(psname, ' ')))
			*(sp++) = '\0';
		if (!(pn = _plugin_alloc(psname))
		    || !(pn->handle = dlopen(filename, RTLD_NOW))
		    || try_init_glame_plugin(pn, psname, filename) == -1
		    || _plugin_add(pn) == -1) {
			_plugin_free(pn);
			continue;
		}
		plugin_set(pn, PLUGIN_PARENT, p);
	} while (sp);

	return 0;
}

int installLADSPAPlugin(const LADSPA_Descriptor *desc, plugin_t *p);

static int try_init_ladspa_plugin(plugin_t *p, const char *name,
				  const char *filename)
{
	LADSPA_Descriptor_Function desc_func;
	const LADSPA_Descriptor *desc;
	plugin_t *lp;
	int i;

	/* First get the descriptor providing function of the
	 * plugin(set). If its not there, its no LADSPA plugin. */
	desc_func = (LADSPA_Descriptor_Function)dlsym(p->handle,
						      "ladspa_descriptor");
	if (!desc_func)
		return -1;

	/* Loop through all available descriptors and create
	 * GLAME wrappers for them. */
	for (i=0; (desc = desc_func(i)) != NULL; i++) {
		if (!(lp = _plugin_alloc(desc->Label))
		    || !(lp->handle = dlopen(filename, RTLD_NOW))
		    || installLADSPAPlugin(desc, lp) == -1
		    || _plugin_add(lp) == -1) {
			_plugin_free(lp);
			continue;
		}
		plugin_set(lp, PLUGIN_PARENT, p);
	}

	return 0;
}

static int try_load_plugin(plugin_t *p, const char *name, const char *filename)
{
	/* First try to open the specified shared object. */
	if (!(p->handle = dlopen(filename, RTLD_NOW))) {
		fprintf(stderr, "dlopen(%s): %s\n", filename, dlerror());
		return -1;
	}

	/* Then in the order stated below try to init it as plugin
	 * of the specified type (GLAME & LADSPA supported for now. */
	if (try_init_glame_plugin(p, name, filename) == 0)
		return 0;
	if (try_init_ladspa_plugin(p, name, filename) == 0)
		return 0;

	/* Too bad, thats obviously not a valid plugin. */
	dlclose(p->handle);
	p->handle = NULL;
	return -1;
}



plugin_t *plugin_get(const char *nm)
{
	plugin_t *p = NULL;
	plugin_path_t *path;
	char filename[256], name[32];

	if (!nm)
		return NULL;
	mangle_name(name, nm);

	/* already loaded? */
	if ((p = hash_find_plugin(name)))
		return p;

	/* allocate a new plugin struct */
	if (!(p = _plugin_alloc(name)))
		return NULL;

	/* first try to look for an "in-core" plugin */
	if (try_load_plugin(p, name, NULL) == 0)
		goto found;

	/* try each path until plugin found */
	plugin_foreach_path(path) {
		sprintf(filename, "%s/%s.so", path->path, name);
		if (try_load_plugin(p, name, filename) == 0)
			goto found;
	}

	/* last try LD_LIBRARY_PATH supported plugins */
	sprintf(filename, "%s.so", name);
	if (try_load_plugin(p, name, filename) == 0)
		goto found;

	_plugin_free(p);
	return NULL;

 found:
	if (_plugin_add(p) == -1) {
		_plugin_free(p);
		return NULL;
	}
	return p;
}

int plugin_set(plugin_t *p, const char *key, void *val)
{
	glworm_t *w;

	if (!p || !key)
		return -1;
	if (!(w = glworm_alloc()))
		return -1;
	w->u.ptr = val;
	if (glwdb_add_item(&p->db, w, key) == -1) {
		free(w);
		return -1;
	}
	return 0;
}

void *plugin_query(plugin_t *p, const char *key)
{
	const glworm_t *w;

	if (!p || !key)
		return NULL;

	if ((w = glwdb_query_item(&p->db, key)))
		return (void *)(w->u.ptr);
	return dlsym(p->handle, key);
}

plugin_t *plugin_add(const char *name)
{
	plugin_t *p;
	char nm[32];

	if (!name)
		return NULL;
	mangle_name(nm, name);

	if (!(p = _plugin_alloc(nm)))
		return NULL;

	if (_plugin_add(p) == -1) {
		_plugin_free(p);
		return NULL;
	}

	return p;
}

void _plugin_delete(plugin_t *p)
{
	if (is_hashed_plugin(p)) {
		list_del_plugin(p);
		hash_remove_plugin(p);
	}
	_plugin_free(p);
}


plugin_t *plugin_next(plugin_t *plugin)
{
	if (!plugin)
		return list_gethead(&plugin_list, plugin_t, list);
	if (plugin->list.next == &plugin_list)
		return NULL;
	return list_entry(plugin->list.next, plugin_t, list);
}
