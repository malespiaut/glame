/*
 * cglame.c
 *
 * Copyright (C) 1999-2001 Richard Guenther
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <guile/gh.h>
#include "glmid.h"
#include "swapfile.h"
#include "filter.h"
#include "util.h"
#include "gpsm.h"
#include "glconfig.h"


int cmd_argc;
char **cmd_argv;
char *swfname;



/* Update globals derived from preferences and provide defaults to
 * all configurables.
 * Returns -1 if it seems we didnt have a valid config for now. */
static int update_preferences()
{
	filter_t *filter;
	char *swappath = NULL;
	char *ainplugin = NULL, *aindev = NULL;
	char *aoutplugin = NULL, *aoutdev = NULL;
	char s[256];
	long maxundo, res = 0;
	long wbufsize, maxlru, maxfds, maxmaps, maxvm;
	long rate;

	/* Check, if we have anything configured already. */
	if (glame_config_get_string("swapfile/defaultpath", &swappath) == -1)
		res = -1;
	else
		free(swappath);

	/* Set default swapfile path and max. undo depth */
	snprintf(s, 255, "%s/.glameswap", getenv("HOME"));
	swappath = glame_config_get_string_with_default("swapfile/defaultpath", s);

	glame_config_get_long("swapfile/maxundo", &maxundo);
	gpsm_set_max_saved_ops(maxundo);

	maxlru = glame_config_get_long_with_default("swapfile/maxlru", 2048);
	maxfds = glame_config_get_long_with_default("swapfile/maxfds", 128);
	maxmaps = glame_config_get_long_with_default("swapfile/maxmaps", 256);
	maxvm = glame_config_get_long_with_default("swapfile/maxvm", 256*1024*1024);
	swcluster_set_cache(maxlru, maxfds, maxmaps, maxvm);

	/* GLAME_WBUFSIZE */
	glame_config_get_long("filter/wbufsize", &wbufsize);
	_GLAME_WBUFSIZE = wbufsize;

	/* Update IO plugin setup - audio_out */
	aoutplugin = glame_config_get_string_with_default(
		"audio_io/output_plugin", "audio_out");
	if (!plugin_get(aoutplugin)) {
		DPRINTF("No plugin %s - using audio_out\n", aoutplugin);
		free(aoutplugin);
		aoutplugin = strdup("audio_out");
		if (!plugin_get(aoutplugin))
			goto ain;
	}
	aoutdev = filterparam_val_string(filterparamdb_get_param(filter_paramdb((filter_t *)plugin_query(plugin_get(aoutplugin), PLUGIN_FILTER)), "device"));
	snprintf(s, 255, "%s", aoutdev ? aoutdev : "");
	aoutdev = glame_config_get_string_with_default("audio_io/output_dev", s);
	filter = filter_instantiate(plugin_get(aoutplugin));
	if (filter) {
		filterparam_set(filterparamdb_get_param(filter_paramdb(filter),
							"device"), &aoutdev);
		filter_register(filter, plugin_get("audio_out"));
	}

 ain:
	/* Update IO plugin setup - audio_in */
	rate = glame_config_get_long_with_default(
		"audio_io/input_rate", GLAME_DEFAULT_SAMPLERATE);
	ainplugin = glame_config_get_string_with_default(
		"audio_io/input_plugin", "audio_in");
	if (!plugin_get(ainplugin)) {
		DPRINTF("No plugin %s - using audio_in\n", ainplugin);
		free(ainplugin);
		ainplugin = strdup("audio_in");
		if (!plugin_get(ainplugin))
			goto sync;
	}
	aindev = filterparam_val_string(filterparamdb_get_param(filter_paramdb((filter_t *)plugin_query(plugin_get(ainplugin), PLUGIN_FILTER)), "device"));
	snprintf(s, 255, "%s", aindev ? aindev : "");
	aindev = glame_config_get_string_with_default("audio_io/input_dev", s);
	filter = filter_instantiate(plugin_get(ainplugin));
	if (filter) {
		filterparam_set(filterparamdb_get_param(filter_paramdb(filter),
							"device"), &aindev);
		filterparam_set(filterparamdb_get_param(filter_paramdb(filter),
							"rate"), &rate);
		filter_register(filter, plugin_get("audio_in"));
	}

 sync:
	/* Sync changes. */
	glame_config_sync();

	/* Display summary. */
	DPRINTF(
"Preferences:\n"
"\tSwapfile directory %s\n"
"\tUndo stack depth is %li\n"
"\tAudio input plugin %s, device \"%s\", rate %li\n"
"\tAudio output plugin %s, device \"%s\"\n"
"\tGLAME_WBUFSIZE %i\n",
                swappath, maxundo, ainplugin, aindev, rate,
                aoutplugin, aoutdev, _GLAME_WBUFSIZE);

	/* Free temp. storage. */
	free(swappath);
	free(aoutplugin);
	if (aoutdev)
		free(aoutdev);
	free(ainplugin);
	if (aindev)
		free(aindev);

	return res;
}


/* just enter a scheme command line.
 */
void sc_main()
{
	if (!swfname)
		glame_config_get_string("swapfile/defaultpath", &swfname);
	if (swfname && gpsm_init(swfname) == -1)
		exit(1);

	/* Update state from preferences. */
	update_preferences();

	/* Interactive mode. */
	if (!cmd_argv) {
		fprintf(stderr,
"    Quick help:\n"
"    (quit) gets you out of here.\n"
"    (help) gets you some additional help.\n\n");

		gh_eval_str_with_catch(
"(use-modules (ice-9 readline))"
"(activate-readline)", scm_handle_by_message_noexit);
		gh_eval_str(
"(let ((guile-user (resolve-module '(guile-user))))"
"  (module-use! guile-user (resolve-interface '(glame)))"
"  (set-repl-prompt! \"glame> \")"
"  (top-repl))");
		/* not reached. */

	/* Non-interactive mode. FIXME - somehow arguments dont
	 * reach the program-arguments procedure. */
	} else {
		char command[1024];
		snprintf(command, 1023,
			 "(load \"%s\")", cmd_argv[0]);
		gh_eval_str(command);
	}
}

void usage()
{
	fprintf(stderr,
"    Usage: cglame [-(c|s) swapfile] [script-file script-args...]\n\n");
}

int main(int argc, char **argv)
{
	int creat;

	fprintf(stderr, "\n"
"    CGLAME for GLAME version "VERSION", Copyright (C) 1999-2001\n"
"    Alexander Ehlert, Richard Guenther, Johannes Hirche, Daniel Kobras.\n"
"    CGLAME and GLAME come with ABSOLUTELY NO WARRANTY. This is free\n"
"    software, and you are welcome to redistribute it under certain\n"
"    conditions.\n\n");

	/* Check, if we have a usage request. */
	if (argc > 1
	    && (strcmp(argv[1], "--help") == 0
		|| strcmp(argv[1], "-h") == 0
		|| strcmp(argv[1], "-?") == 0)) {
		usage();
		exit(0);
	}

	/* Find out the swapfile filename and if we are allowed to
	 * create it. */
	if (argc > 2) {
		swfname = argv[2];
		creat = 0;
		if (!strcmp(argv[1], "-c"))
			creat = 1;
		else if (strcmp(argv[1], "-s"))
			swfname = NULL;
	}

	if (swfname && argc > 3) {
		cmd_argc = argc - 3;
		cmd_argv = &argv[3];
	} else if (!swfname && argc > 1) {
		cmd_argc = argc - 1;
		cmd_argv = &argv[1];
	} else {
		cmd_argc = 0;
		cmd_argv = NULL;
	}

	if (swfname && creat) {
		fprintf(stderr, "Creating swapfile on %s\n", swfname);
		if (swapfile_creat(swfname, 1024) == -1) {
			perror("ERROR: Cannot create swapfile");
			exit(1);
		}
	}


	if (glame_init(sc_main, cmd_argc, cmd_argv) == -1) {
	        fprintf(stderr, "glame init failed!\n");
		exit(1);
	}

	fprintf(stderr, "quitting.\n");
	gpsm_close();

	return 0;
}
