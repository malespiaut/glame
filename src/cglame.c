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

#include <stdio.h>
#include <guile/gh.h>
#include "glscript.h"
#include "channel.h"
#include "filter.h"
#include "util.h"


int init()
{
	if (hash_alloc() == -1
	    || filter_init() == -1
	    || init_channel() == -1)
		return -1;

	return 0;
}


/* just init the scripting subsystem and enter a scheme command
 * line waiting for commands (or a NULL line).
 */
void sc_main(int argc, char **argv)
{
	char str[256];
	char str2[256];
	int done;

	if (glscript_init() == -1)
		exit(1);

	done = 0;
	fputs("glame> ", stdout);
	while (!done) {
		if (gets(str) == NULL) {
			done = 1;
		} else {
		        sprintf(str2, "(display %s)", str);
			gh_eval_str(str2);
			gh_eval_str("(newline)");
			fputs("glame> ", stdout);
		}
	}

	exit(0);
}


int main(int argc, char **argv)
{
	fprintf(stderr, "\n"
"    GLAME version "VERSION", Copyright (C) 1999, 2000 Alexander Ehlert,\n"
"    Richard Guenther, Johannes Hirche, Daniel Kobras.\n"
"    GLAME comes with ABSOLUTELY NO WARRANTY.\n"
"    This is free software, and you are welcome to redistribute it\n"
"    under certain conditions.\n\n");

	if (init() == -1)
		PANIC("Error in init!");

	if (argc != 2) {
	        fprintf(stderr, "Usage: %s swapfile\n\n", argv[0]);
	} else {
		if (swap_open(argv[1], 0) == -1) {
			perror("Unable to open swap");
			exit(0);
		}
	}

	gh_enter(argc, argv, sc_main);
	/* not reached */

	return 0;
}
