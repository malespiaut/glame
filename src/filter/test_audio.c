/*
 * test_sound.c
 * $Id: test_audio.c,v 1.1 2000/02/01 09:35:55 mag Exp $
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
#include "glame_hash.h"
#include "filter.h"



int main(int argc, char **argv)
{
	filter_network_t *net;
	filter_node_t *audio_out, *sinus;

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

	if (!(sinus = filternode_add(net, "sinus"))) {
		fprintf(stderr, "error in filternode_add(sinus)\n");
		return -1;
	}

	if (!(audio_out = filternode_add(net,"audio_out"))){
		fprintf(stderr, "error in filternode_add(audio_out)\n");
		return -1;
	}

	if (filternode_connect(sinus, "output", audio_out, "left") == -1) {
		fprintf(stderr, "error in connecting\n");
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



