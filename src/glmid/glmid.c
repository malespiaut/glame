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
#include "glame_hash.h"
#include "swapfile.h"
#include "filter.h"
#include "glplugin.h"
#include "glsimd.h"

#ifdef HAVE_GUILE
#include <guile/gh.h>
#include "glscript.h"
extern filter_t *scm2filter(SCM filter_smob);
extern long filter_smob_tag;
#define filter_p(s) (SCM_NIMP(s) && SCM_CAR(s) == filter_smob_tag)
#endif

/* Builtin plugins. */
PLUGIN_SET(glamebuiltins, "basic basic_sample audio_io file_io waveform "
	   "rms arithmetic swapfile_io")


int glame_load_plugin(const char *fname)
{
    	/* Shared object plugin type? Just try to load it as such, will
	 * fail if it is not. */
	if (plugin_load(fname) == 0)
		return 0;

#ifdef HAVE_GUILE
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
#endif

	return -1;
}

filter_t *glame_load_instance(const char *fname)
{
	SCM s_res;

	if (!strstr(fname, ".scm"))
		return NULL;

#ifdef HAVE_GUILE
	glscript_load_mode = 1;
	s_res = glame_gh_safe_eval_file(fname);
	if (gh_boolean_p(s_res) && !gh_scm2bool(s_res)) {
		DPRINTF("Exception from .scm\n");
		return NULL;
	}
	return last_loaded_filter_instance; /* HACK... */
#else
	return NULL;
#endif
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
	add_plugin_path("./plugins/_libs");
	add_plugin_path(PKGLIBDIR);
	add_plugin_path(getenv("LADSPA_PATH"));

	/* First initialize the builtin plugins */
	plugin_get("glamebuiltins");

	/* Plugins from default paths - and "debug path" (first) */
	load_plugins_from_path("./plugins/.libs"); /* for .so */
	load_plugins_from_path("./plugins/_libs"); /* for .so */
	load_plugins_from_path(PKGLIBDIR);
	load_plugins_from_path(getenv("LADSPA_PATH"));

	return 0;
}

static void glame_cleanup()
{
	swapfile_close();
}

int glame_init()
{
	glsimd_init(0);
	if (hash_alloc() == -1)
		return -1;
	if (plugins_register() == -1)
		return -1;
	atexit(glame_cleanup);

	return 0;
}

#ifdef HAVE_GUILE
static void init_after_guile(int argc, char **argv)
{
#ifndef NDEBUG
	/* We dont like guiles signal handlers for debugging
	 * purposes. */
	scm_restore_signals();
#endif
	if (glscript_init() == -1)
		exit(1);
	load_plugins_from_path("./plugins");       /* for .scm */
	load_plugins_from_path(PKGSCRIPTSDIR);
	((void (*)(void))argv[1])();
}
#endif

int glame_init_with_guile(void (*main)(void))
{
	if (glame_init() == -1)
		return -1;

#ifdef HAVE_GUILE
	{
		char *argv[2];
		argv[0] = NULL;
		argv[1] = (char *)main;
		/* scm_init_guile();
		   init_after_guile(0, argv); */
		gh_enter(0, argv, init_after_guile);
	}
#endif

	/* not reached if HAVE_GUILE */
	main();

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
