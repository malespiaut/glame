/*
 * swapfile_info.c
 * $Id: swapfile_info.c,v 1.2 2000/01/24 10:21:54 richi Exp $
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


void process_file(fileid_t fid)
{
	off_t size;

	printf("Found file with id %i\n", fid);

	size = file_size(fid);
	printf("\ttotal size is %i bytes\n", (int)size);
}

int main(int argc, char **argv)
{
	fileid_t fid;

	if (argc < 2)
		goto _usage;

	if (swap_open(argv[1], 0) == -1) {
		perror("swap_open");
		return 1;
	}

	/* find all files */
	fid = -1;
	while ((fid = file_next(fid)) != -1)
		process_file(fid);

	swap_close();

	return 0;

 _usage:
	fprintf(stderr, "Usage: %s swapfile\n", argv[0]);
	return 1;
}


