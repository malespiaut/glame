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



/* just enter a scheme command line.
 */
void sc_main()
{
	scm_shell(0, NULL);
	/* not reached. */
}


int main(int argc, char **argv)
{
	fprintf(stderr, "\n"
"    CGLAME for GLAME version "VERSION", Copyright (C) 1999-2001\n"
"    Alexander Ehlert, Richard Guenther, Johannes Hirche, Daniel Kobras.\n"
"    CGLAME and GLAME come with ABSOLUTELY NO WARRANTY. This is free\n"
"    software, and you are welcome to redistribute it under certain\n"
"    conditions.\n\n");

	if (argc == 2) {
		if (swapfile_open(argv[1], 0) == -1) {
			if (errno != EBUSY) {
				perror("ERROR: Unable to open swap");
				exit(1);
			}
			fprintf(stderr, "WARNING: Unclean swap - running fsck\n");
			if (swapfile_fsck(argv[1]) == -1) {
				perror("ERROR: Fsck failed");
				exit(1);
			}
			fprintf(stderr, "WARNING: Fsck successful\n");
			if (swapfile_open(argv[1], 0) == -1) {
				perror("ERROR: Still cannot open swap");
				exit(1);
			}
		}
	} else {
		fprintf(stderr,
"    WARNING: starting without a swapfile.\n"
"    Usage: cglame [swapfile]\n\n");
	}

        fprintf(stderr,
"    Quick help:\n"
"    (quit) gets you out of here.\n"
"    (help) gets you some additional help.\n\n");

	if (glame_init_with_guile(sc_main) == -1) {
	        fprintf(stderr, "glame init failed!\n");
		exit(1);
	}
	/* not reached */

	return 0;
}
