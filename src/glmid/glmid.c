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

#ifdef HAVE_GUILE
#include <guile/gh.h>
#include "glscript.h"
extern filter_t *scm2filter(SCM filter_smob);
extern long filter_smob_tag;
#define filter_p(s) (SCM_NIMP(s) && SCM_CAR(s) == filter_smob_tag)
#endif

/* Builtin plugins. */
PLUGIN_SET(glamebuiltins, "basic basic_sample audio_io file_io waveform "
	   "rms basic_midi midi_io midi_debug arithmetic basicfft swapfile_io")

int glame_load_plugin(const char *fname)
{
    	/* Shared object plugin type? Just try to load it as such, will
	 * fail if it is not. */
	if (plugin_load(fname) == 0)
		return 0;

#ifdef HAVE_GUILE
	/* Scheme macro plugin type? FIXME: more sanity check before. */
	if (strstr(fname, ".scm")) {
		SCM s_res = gh_eval_file(fname);
		if (gh_boolean_p(s_res) && gh_scm2bool(s_res))
		    	return 0;
	}
#endif

	return -1;
}

filter_t *glame_load_instance(const char *fname)
{
	SCM s_res;
	filter_t *f;

	if (!strstr(fname, ".scm"))
		return NULL;

#ifdef HAVE_GUILE
	glscript_load_mode = 1;
	s_res = gh_eval_file(fname);
	/* if (!filter_p(s_res)) {
		DPRINTF("gh_eval_file did not return a filter_t\n");
		return NULL;
	} */
	return filter_creat(last_loaded_filter_instance /* scm2filter(s_res) */);
#else
	return NULL;
#endif
}

static void plugins_process_directory(const char *dir)
{
	DIR *d;
	struct dirent *e;

	/* Try to open the directory. */
	if (!(d = opendir(dir)))
		return;

	/* Add the path to the plugin subsystem. */
	plugin_add_path(dir);

	/* Try to load every file as plugin. */
	while ((e = readdir(d))) {
	    	char fname[256];
		struct stat st;

		snprintf(fname, 255, "%s/%s", dir, e->d_name);
		if (stat(fname, &st) == -1)
		    	continue;
		if (!S_ISREG(st.st_mode) || !(st.st_mode & S_IRUSR))
		    	continue;
		glame_load_plugin(fname);
	}

	closedir(d);
}

static void plugins_process_env(const char *env)
{
	char *e, *path, *dir;

	if (!(e = getenv("LADSPA_PATH"))
	    || !(path = strdup(e)))
		return;
	e = path;

	dir = path;
	do {
		if ((path = strchr(path, ':')))
			*path++ = '\0';
		plugins_process_directory(dir);
		dir = path;
	} while (dir);

	free(e);
}

static int plugins_register()
{
	/* First initialize the builtin plugins */
	plugin_get("glamebuiltins");

	/* Plugins from default paths - and "debug path" (first) */
	plugins_process_directory("./plugins/.libs");
	plugins_process_directory(PKGLIBDIR);

	/* Paths from environment. */
	plugins_process_env("LADSPA_PATH");

	return 0;
}

static void glame_cleanup()
{
	swapfile_close();
}

int glame_init()
{
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
