/*
 * test_network.c
 * $Id: test_network.c,v 1.4 2000/03/17 13:57:21 richi Exp $
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
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "glame_hash.h"
#include "filter.h"
#include "glmid.h"


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
	struct stat sbuf;
	int i;
	int fd;
	char *network, str[32];
	void *val;
	
	sa.sa_flags = 0;
	sa.sa_handler = cleanup;
	sigemptyset(&sa.sa_mask);
#ifdef HAVE_POSIX_RTSIG
	sa.sa_sigaction = NULL;
#endif
	if (sigaction(SIGINT, &sa, NULL) == -1)
		perror("sigaction");
	if (sigaction(SIGTERM, &sa, NULL) == -1)
		perror("sigaction");

	if (argc < 2) {
		fprintf(stderr, "Usage: %s network {param value}\n", argv[0]);
		exit(1);
	}

	if (glame_init() == -1) {
		fprintf(stderr, "error in glame_init()\n");
		return -1;
	}

	/* argh! */
	if ((fd = open(argv[1], O_RDONLY)) == -1)
		return -1;
	fstat(fd, &sbuf);
	network = mmap(NULL, sbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);

	if (!(net = filternetwork_from_string(network))) {
		fprintf(stderr, "error in string_to_filternetwork()\n");
		return -1;
	}

	for (i=2; i<argc; i+=2) {
		fprintf(stderr, "setting parameter %s to %s\n",
			argv[i], argv[i+1]);
		if (!(val = filterparamval_from_string(filter_get_paramdesc(net->node.filter, argv[i]), argv[i+1]))) {
		        fprintf(stderr, "Invalid parameter string %s for parameter %s\n",
				argv[i+1], argv[i]);
		}
		if (filternode_set_param(&net->node, argv[i], val) == -1) {
			fprintf(stderr, "Cannot set parameter %s\n", argv[i]);
		}
		free(val);
	}

	fprintf(stderr, "launching network\n");
	if (filternetwork_launch(net) == -1) {
		fprintf(stderr, "error in filternetwork_launch()\n");
	}

	fprintf(stderr, "network launched, press ENTER to start\n");
	fgets(str, 32, stdin);

	fprintf(stderr, "starting network\n");
	if (filternetwork_start(net) == -1) {
		fprintf(stderr, "error starting network!\n");
		return -1;
	}

	fprintf(stderr, "waiting for network completion\n");
	if (filternetwork_wait(net) == -1) {
		fprintf(stderr, "error in filternetwork_wait()\n");
		return -1;
	}

	fprintf(stderr, "all done.\n");

	filternetwork_delete(net);

	return 0;
}



