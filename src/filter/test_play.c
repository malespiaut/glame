/*
 * test_play.c
 * $Id: test_play.c,v 1.6 2000/02/09 02:08:16 mag Exp $
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
	filter_node_t *audio_out, *read_file, *echo, *echo2;

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

	if (!(net = filternetwork_new("test"))) {
		fprintf(stderr, "error in filternetwork_new()\n");
		return -1;
	}

	if (!(read_file = filternetwork_add_node(net, "read_file", NULL))) {
		fprintf(stderr, "error in filternetwork_add_node(read_file)\n");
		return -1;
	}

	if (!(audio_out = filternetwork_add_node(net,"audio_out", NULL))){
		fprintf(stderr, "error in filternetwork_add_node(audio_out)\n");
		return -1;
	}

	if (!(echo = filternetwork_add_node(net,"echo",NULL))){
		fprintf(stderr,"error in filternetwork_add_node(echo)\n");
		return -1;
	}

	if (!(echo2 = filternetwork_add_node(net,"echo",NULL))){
                fprintf(stderr,"error in filternetwork_add_node(echo)\n");
                return -1;
        }

	if (!filternetwork_add_connection(read_file, "left_out", echo , "in")) {
		fprintf(stderr, "error in connecting\n");
		return -1;
	}

	if (!filternetwork_add_connection(read_file, "right_out", echo2 , "in")) {
		fprintf(stderr, "error in connecting\n");
		return -1;
	}
	
	if (!filternetwork_add_connection(echo, "out", audio_out, "left_in")) {
		fprintf(stderr, "error in connecting\n");
		return -1;
	}

	if (!filternetwork_add_connection(echo2, "out", audio_out, "right_in")) {
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

	filternetwork_save(net,"test_play.net");

	return 0;
}



