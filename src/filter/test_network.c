/*
 * test_latency.c
 * $Id: test_network.c,v 1.4 2000/02/10 11:07:19 richi Exp $
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
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "glame_hash.h"
#include "filter.h"


filter_network_t *net = NULL;

static void cleanup(int sig)
{
	filternetwork_terminate(net);
	filternetwork_delete(net);
	exit(0);
}

int main(int argc, char **argv)
{
	struct sigaction sa;
	filter_node_t *n;
	filter_t *f;
	int i;

	
	sa.sa_flags = 0;
	sa.sa_handler = cleanup;
	sigemptyset(&sa.sa_mask);
	sa.sa_sigaction = NULL;
	if (sigaction(SIGINT, &sa, NULL) == -1)
		perror("sigaction");
	if (sigaction(SIGTERM, &sa, NULL) == -1)
		perror("sigaction");

	if (argc < 2) {
		fprintf(stderr, "Usage: %s network {param value}\n", argv[0]);
		exit(1);
	}

	if (hash_alloc() == -1) {
		fprintf(stderr, "error in initting global hash\n");
		return -1;
	}
	
	if (filter_init() == -1) {
		fprintf(stderr, "error in filter_init()\n");
		return -1;
	}

	if (!(f = filternetwork_load(argv[1]))) {
		fprintf(stderr, "error in filternetwork_load(%s)\n", argv[1]);
		return -1;
	}
	if (filter_add(f) == -1) {
		fprintf(stderr, "error in filter_add()\n");
		return -1;
	}

	if (!(net = filternetwork_new("test"))) {
		fprintf(stderr, "error in filternetwork_new()\n");
		return -1;
	}

	if (!(n = filternetwork_add_node(net, f->name, NULL))) {
		fprintf(stderr, "error in filternetwork_add_node(%s)\n", f->name);
		return -1;
	}

	for (i=2; i<argc; i+=2) {
		fprintf(stderr, "setting parameter %s to %s\n",
			argv[i], argv[i+1]);
		filternode_setparamstring(n, argv[i], argv[i+1]);
	}

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

	filternetwork_delete(net);

	return 0;
}



