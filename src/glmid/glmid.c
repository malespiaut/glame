/*
 * glmid.c
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <guile/gh.h>
#include "glame_hash.h"
#include "swapfile.h"
#include "filter.h"
#include "glplugin.h"
#include "glsimd.h"
#include "glscript.h"


/* Builtin plugins. */
PLUGIN_SET(glamebuiltins, "basic basic_sample audio_io file_io waveform "
	   "rms arithmetic swapfile_io ssp")


int glame_load_plugin(const char *fname)
{
    	/* Shared object plugin type? Just try to load it as such, will
	 * fail if it is not. */
	if (plugin_load(fname) == 0)
		return 0;

	/* Scheme macro plugin type? FIXME: more sanity check before. */
	if (strstr(fname, ".scm")) {
		SCM s_res;
	        glscript_load_mode = 0;
		s_res = glame_gh_safe_eval_file(fname);
		if (gh_boolean_p(s_res) && !gh_scm2bool(s_res)) {
			DPRINTF("Exception from .scm\n");
			return -1;
		}
		return 0;
	}

	return -1;
}

filter_t *glame_load_instance(const char *fname)
{
	SCM s_res;

	if (!strstr(fname, ".scm"))
		return NULL;

	glscript_load_mode = 1;
	s_res = glame_gh_safe_eval_file(fname);
	if (gh_boolean_p(s_res) && !gh_scm2bool(s_res)) {
		DPRINTF("Exception from .scm\n");
		return NULL;
	}
	return last_loaded_filter_instance; /* HACK... */
}

static void add_plugin_path(const char *path)
{
	char *tmp, *dir, *p;
	DIR *d;

	if (!path || !(tmp = p = strdup(path)))
		return;

	dir = p;
	do {
		if ((p = strchr(p, ':')))
			*p++ = '\0';

		/* Try to open the directory and add the path to
		 * the plugin subsystem. */
		if ((d = opendir(dir))) {
			closedir(d);
			plugin_add_path(dir);
		}

		dir = p;
	} while (dir);

	free(tmp);
}

static void load_plugins_from_directory(const char *dir)
{
	DIR *d;
	struct dirent *e;

	/* Try to open the directory. */
	if (!(d = opendir(dir)))
		return;

	/* Try to load every file as plugin. */
	while ((e = readdir(d))) {
	    	char fname[256];
		struct stat st;
		char *ext;

		/* Only take .so and .scm, but not glame.scm */
		if (!(ext = strrchr(e->d_name, '.'))
		    || !(strcmp(ext, ".scm")
			 || strcmp(ext, ".so"))
		    || strcmp(e->d_name, "glame.scm") == 0)
			continue;

		snprintf(fname, 255, "%s/%s", dir, e->d_name);
		if (stat(fname, &st) == -1)
		    	continue;
		if (!S_ISREG(st.st_mode) || !(st.st_mode & S_IRUSR))
		    	continue;
		glame_load_plugin(fname);
	}

	closedir(d);
}

static void load_plugins_from_path(const char *path)
{
	char *tmp, *dir, *p;

	if (!path || !(tmp = p = strdup(path)))
		return;

	dir = p;
	do {
		if ((p = strchr(p, ':')))
			*p++ = '\0';
		load_plugins_from_directory(dir);
		dir = p;
	} while (dir);

	free(tmp);
}

static int plugins_register()
{
	/* Add plugin paths. */
	add_plugin_path("./plugins/.libs");
	add_plugin_path(PKGLIBDIR);
#ifdef HAVE_LADSPA
	add_plugin_path(getenv("LADSPA_PATH"));
	add_plugin_path("/usr/lib/ladspa");
	add_plugin_path("/usr/local/lib/ladspa");
#endif
	
	/* First initialize the builtin plugins */
	plugin_get("glamebuiltins");

	/* Plugins from default paths - and "debug path" (first) */
	load_plugins_from_path("./plugins/.libs"); /* for .so */
	load_plugins_from_path(PKGLIBDIR);
#ifdef HAVE_LADSPA
	load_plugins_from_path(getenv("LADSPA_PATH"));
	load_plugins_from_path("/usr/lib/ladspa");
	load_plugins_from_path("/usr/local/lib/ladspa");
#endif

	return 0;
}

static void glame_cleanup()
{
	swapfile_close();
}


static void init_after_guile(int argc, char **argv)
{
#if !defined NDEBUG
	/* We dont like guiles signal handlers for debugging
	 * purposes. */
	scm_restore_signals();
#endif

	/* Init lowlevel GLAME subsystems. */
	glsimd_init(0);
	if (hash_alloc() == -1)
		exit(1);
	if (plugins_register() == -1)
		exit(1);
	atexit(glame_cleanup);

	/* Init scripting. */
	if (glscript_init() == -1)
		exit(1);
	add_plugin_path(PKGSCRIPTSDIR);
	load_plugins_from_path("./plugins");       /* for .scm */
	load_plugins_from_path(PKGSCRIPTSDIR);

	((void (*)(void))argv[1])();
}

int glame_init(void (*main)(void))
{
	char *argv[2];
	argv[0] = NULL;
	argv[1] = (char *)main;

	gh_enter(0, argv, init_after_guile);

	return 0;
}

plugin_t *glame_create_plugin(filter_t *filter, const char *name)
{
	plugin_t *plugin;

	if (!name || !filter)
		return NULL;
	if (!(plugin = plugin_add(name)))
		return NULL;
	if (filter_register(filter, plugin) == -1) {
		_plugin_delete(plugin);
		return NULL;
	}
	return plugin;
}
