/*
 * swapfile_fsck.c
 *
 * Copyright (C) 2001 Richard Guenther
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
#include "swapfile.h"



int main(int argc, char **argv)
{
	char *swname;
	int force, res;

	if (argc < 2 || argc > 3)
		goto _usage;

	fprintf(stderr, "\n"
"    swapfile_fsck version "VERSION", Copyright (C) 2001 Richard Guenther\n"
"    swapfile_fsck comes with ABSOLUTELY NO WARRANTY.\n"
"    This is free software, and you are welcome to redistribute it\n"
"    under certain conditions.\n\n");

	force = 0;
	swname = argv[1];
	if (argc == 3) {
		if (!strcmp(argv[1], "-f") == 0)
			goto _usage;
		force = 1;
		swname = argv[2];
	}

	res = swapfile_fsck(swname, 0);
	if (res == 0)
		fprintf(stderr, "%s is clean.\n", swname);

	if (force)
		fprintf(stderr, "forcing fsck.\n");
	if (force || res == -1) {
		res = swapfile_fsck(swname, 1);
		if (res == -1)
			perror("unable to fsck");
		else if (res == 0)
			fprintf(stderr, "%s is clean.\n", swname);
		else if (res == 1)
			fprintf(stderr, "WARNING: %s was modified.\n", swname);
	}

	return 0;

 _usage:
	fprintf(stderr, "Usage: %s [-f] swapfile\n", argv[0]);
	exit(1);
}
