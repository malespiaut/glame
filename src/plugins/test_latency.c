/*
 * test_latency.c
 * $Id: test_latency.c,v 1.1 2000/03/15 13:07:10 richi Exp $
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

	if (!(net = filternetwork_new("test", "test"))) {
		fprintf(stderr, "error in filternetwork_new()\n");
		return -1;
	}

	if (!(ping = filternetwork_add_node(net, "ping", NULL))) {
		fprintf(stderr, "error in filternetwork_add_node(ping)\n");
		return -1;
	}
	source = ping;

	while (cnt) {
		if (!(dest = filternetwork_add_node(net, "null", NULL))) {
			fprintf(stderr, "error in filternetwork_add_node(null)\n");
			return -1;
		}
		if (!filternetwork_add_connection(source, "out", dest, "in")) {
			fprintf(stderr, "error in connecting\n");
			return -1;
		}
		source = dest;
		cnt--;
	}

	if (!filternetwork_add_connection(source, "out", ping, "in")) {
		fprintf(stderr, "error in connecting (final)\n");
		return -1;
	}


	fprintf(stderr, "launching network\n");
	if (filternetwork_launch(net) == -1) {
		fprintf(stderr, "error in filternetwork_launch()\n");
		goto err;
	}

	fprintf(stderr, "starting network\n");
	if (filternetwork_start(net) == -1) {
		fprintf(stderr, "error in filternetwork_start()\n");
		goto err;
	}

	fprintf(stderr, "network launched, waiting for completion\n");
	if (filternetwork_wait(net) == -1) {
		fprintf(stderr, "error in filternetwork_wait()\n");
		goto err;
	}

	fprintf(stderr, "all done.\n");

	fprintf(stderr, filternetwork_to_string(net));

	filternetwork_delete(net);

	return 0;

 err:
	filternetwork_terminate(net);
	filternetwork_delete(net);
	return -1;
}
