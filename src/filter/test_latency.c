/*
 * test_latency.c
 * $Id: test_latency.c,v 1.3 2000/01/24 11:43:22 richi Exp $
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

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "glame_hash.h"
#include "filter.h"



int main(int argc, char **argv)
{
	filter_network_t *net;
	filter_node_t *ping, *source, *dest;
	int cnt = 1;

	if (argc == 2)
		cnt = atoi(argv[1]) - 2;
	if (cnt<1)
		cnt = 1;

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

	if (!(ping = filternode_add(net, "ping"))) {
		fprintf(stderr, "error in filternode_add(ping)\n");
		return -1;
	}
	source = ping;

	while (cnt) {
		if (!(dest = filternode_add(net, "null"))) {
			fprintf(stderr, "error in filternode_add(null)\n");
			return -1;
		}
		if (filternode_connect(source, "out", dest, "in") == -1) {
			fprintf(stderr, "error in connecting\n");
			return -1;
		}
		source = dest;
		cnt--;
	}

	if (filternode_connect(source, "out", ping, "in") == -1) {
		fprintf(stderr, "error in connecting (final)\n");
		return -1;
	}

	/* we have created a cyclic network. ugh!
	 * lets "fix" it :)
	 */
	list_add(&ping->neti_list, &net->inputs);

	fprintf(stderr, "launching network\n");
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



