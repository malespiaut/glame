/*
 * test_play.c
 * $Id: test_play.c,v 1.2 2000/02/02 15:13:32 nold Exp $
 *
 * Copyright (C) 1999, 2000 Alexander Ehlert
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

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "glame_hash.h"
#include "filter.h"

void usage(char *name) {
	char *basename = strrchr(name, '/');
	if (!basename)
		basename = name;

	fprintf(stderr, "Usage: %s <file>\n", basename);
	exit(1);
}


int main(int argc, char **argv)
{
	filter_network_t *net;
	filter_node_t *audio_out, *read_file;

	if (argc!=2)
		usage(argv[0]);

	printf("Playing %s.\n",argv[1]);

	if (hash_alloc() == -1) {
		fprintf(stderr, "error in initting global hash\n");
		return -1;
	}
	
	if (filter_init() == -1) {
		fprintf(stderr, "error in filter_init()\n");
		return -1;
	}

	if (!(net = filternetwork_new())) {
		fprintf(stderr, "error in filternetwork_new()\n");
		return -1;
	}

	if (!(read_file = filternode_add(net, "read_file"))) {
		fprintf(stderr, "error in filternode_add(read_file)\n");
		return -1;
	}

	if (!(audio_out = filternode_add(net,"audio_out"))){
		fprintf(stderr, "error in filternode_add(audio_out)\n");
		return -1;
	}

	if (filternode_connect(read_file, "left", audio_out, "left") == -1) {
		fprintf(stderr, "error in connecting\n");
		return -1;
	}

	if (filternode_connect(read_file, "right", audio_out, "right") == -1) {
		fprintf(stderr, "error in connecting\n");
		return -1;
	}

	if (filternode_setparam(read_file,"filename",&argv[1]) == -1){
		fprintf(stderr,"error in setparam\n");
		return -1;
	}

	if (filternetwork_launch(net) == -1) {
		fprintf(stderr, "error in filternetwork_launch()\n");
	}

	fprintf(stderr, "network launched, waiting for completion\n");
	if (filternetwork_wait(net) == -1) {
		fprintf(stderr, "error in filternetwork_wait()\n");
		return -1;
	}

	fprintf(stderr, "all done.\n");

	return 0;
}



