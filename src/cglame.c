/*
 * cglame.c
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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
#include "filter.h"
#include "util.h"


int init()
{
	if (hash_alloc() == -1)
		return -1;

	return 0;
}

void cleanup()
{
	swap_close();
}


/* just init the scripting subsystem and enter a scheme command line.
 */
void sc_main(int argc, char **argv)
{
	if (glmid_init(1) == -1)
		exit(1);

	gh_eval_str("(plugin_add_path \"plugins/.libs\")");

	scm_shell(argc, argv);
	/* not reached. */
}


int main(int argc, char **argv)
{
	fprintf(stderr, "\n"
"    CGLAME for GLAME version "VERSION", Copyright (C) 1999, 2000\n"
"    Alexander Ehlert, Jim Garrison, Richard Guenther, Johannes Hirche,\n"
"    Daniel Kobras.\n"
"    CGLAME and GLAME come with ABSOLUTELY NO WARRANTY. This is free\n"
"    software, and you are welcome to redistribute it under certain\n"
"    conditions.\n\n"
"    Usage: cglame [swapfile]\n\n");

	fprintf(stderr,
"Quick help:\n"
"(quit) gets you out of here.\n"
"(help) gets you some additional help.\n\n");

	if (init() == -1) {
		fprintf(stderr, "Error in init!");
		exit(1);
	}

	if (argc == 2) {
		if (swap_open(argv[1], 0) == -1) {
			perror("Unable to open swap");
			exit(0);
		}
		atexit(cleanup);
	}

	gh_enter(argc, argv, sc_main);
	/* not reached */

	return 0;
}
