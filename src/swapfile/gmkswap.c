/*
 * gmkswap.c
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

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "swapfile.h"



int main(int argc, char **argv)
{
	char *swapname;
	size_t size;

	fprintf(stderr, "\n"
"    gmkswap version "VERSION", Copyright (C) 1999, 2000 Richard Guenther\n"
"    gmkswap comes with ABSOLUTELY NO WARRANTY.\n"
"    This is free software, and you are welcome to redistribute it\n"
"    under certain conditions.\n\n");

	if (argc<2 || argc>3)
		goto _usage;

	swapname = argv[1];
	size = -1;
	if (argc == 3)
		size = 1024*1024*atol(argv[2]);

	if (size < 1024*1024*4) {
		fprintf(stderr, "You don't want to have only %li bytes for swap!\n",
			(long)size);
		goto _usage;
	}

	fprintf(stderr, "Will create %li bytes swap in %s at once!\n",
		(long)size, swapname);
	fprintf(stderr, "\nSetting up swap...\n");

	/* Create swap. */
	if (swapfile_creat(swapname, size) == -1) {
		fprintf(stderr, "...failed (cannot create).\n");
		exit(1);
	}

	/* Try to open/close swap. */
	if (swapfile_open(swapname, 0) == -1) {
		fprintf(stderr, "...failed (cannot open created swap).\n");
		exit(1);
	}
	swapfile_close();

	fprintf(stderr, "...all done.\n");
	return 0;


 _usage:
	fprintf(stderr, "Usage: %s file [size in MB]\n", argv[0]);
	return 1;
}
