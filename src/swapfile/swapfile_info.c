/*
 * swapfile_info.c
 * $Id: swapfile_info.c,v 1.8 2000/10/09 08:39:38 richi Exp $
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

#include <stdio.h>
#include "swapfile.h"


int main(int argc, char **argv)
{
	SWDIR *dir;
	long fname;
	swfd_t fd;
	struct sw_stat st;
	size_t totalsize;

	if (argc < 2)
		goto _usage;

	if (swapfile_open(argv[1], 0) == -1) {
		perror("swap_open");
		return 1;
	}

	fprintf(stderr, "Swapfile %s:\n", argv[1]);

	totalsize = 0;
	dir = sw_opendir();
	while ((fname = sw_readdir(dir)) != -1) {
		if ((fd = sw_open(fname, O_RDONLY, TXN_NONE)) == -1) {
			printf("\t----\t%li", fname);
			continue;
		}
		sw_fstat(fd, &st);
		printf("\t%8i\t%li\n", st.size, fname);
		totalsize += st.size;
	}
	printf("total size %i\n", totalsize);
	sw_closedir(dir);

	fprintf(stderr, "done.\n");
	swapfile_close();

	return 0;

 _usage:
	fprintf(stderr, "Usage: %s swapfile\n", argv[0]);
	return 1;
}
