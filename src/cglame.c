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

	fprintf(stderr, "FIXME!\n");
	fprintf(stderr, "[You probably want to have a look at src/filter/ for "
			"some sample code\nthat actually works. Watch out for "
			"executables named test_*.]\n");
	exit(1);
}
