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

/* HACK */
extern void blafoobar(int);


int cmd_argc;
char **cmd_argv;
char *swfname;

/* just enter a scheme command line.
 */
void sc_main()
{
	if (!swfname)
		glame_config_get_string("swapfile/defaultpath", &swfname);
	if (swfname && gpsm_init(swfname) == -1)
		exit(1);

	/* Interactive mode. */
	if (!cmd_argv) {
		fprintf(stderr,
"    Quick help:\n"
"    (quit) gets you out of here.\n"
"    (help) gets you some additional help.\n\n");

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

	/* HACK */
	blafoobar(0);

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
	if (argc > 1 && strcmp(argv[1], "-c") == 0) {
		creat = 1;
		swfname = argv[2];
	} else if (argc > 1 && strcmp(argv[1], "-s") == 0) {
		creat = 0;
		swfname = argv[2];
	} else
		swfname = NULL;

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
