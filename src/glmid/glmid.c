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

#include <sys/types.h>
#include <dirent.h>
#include <guile/gh.h>
#include <glame_hash.h>
#include <swapfile.h>
#include <glplugin.h>

extern int glscript_init();

/* builtin plugins */
PLUGIN_SET(glamebuiltins, "basic basic_sample track_io audio_io file_io waveform rms basic_midi midi_io midi_debug arithmetic")

static void plugins_process_directory(const char *dir)
{
	DIR *d;
	struct dirent *e;

	/* Try to open the directory. */
	if (!(d = opendir(dir)))
		return;

	/* Add the path to the plugin subsystem. */
	plugin_add_path(dir);

	/* Manually try to "get" every file as plugin. */
	while ((e = readdir(d))) {
		/* Get the plugins basename. */
		char *name, *p;
		name = strdup(e->d_name);
		if (!(p = strstr(name, ".so")))
			continue;
		*p = '\0';

		/* Try to get the plugin. */
		plugin_get(name);
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
	swap_close();
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

int glame_init_with_guile(void (*main)(void))
{
#ifdef HAVE_GUILE
	char *argv[2];
#endif

	if (glame_init() == -1)
		return -1;
#ifdef HAVE_GUILE
	argv[0] = NULL;
	argv[1] = (void *)main;
	gh_enter(0, argv, init_after_guile);
#endif

	/* not reached if HAVE_GUILE */
	main();
	return 0;
}
