/*
 * filter_network.c
 * $Id: filter_network.c,v 1.1 2000/01/20 14:54:19 richi Exp $
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

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <errno.h>
#include "filter.h"
#include "util.h"


#define STATE_UNDEFINED 0
#define STATE_PREPROCESSED 1
#define STATE_INITIALIZED 2
#define STATE_PRELAUNCHED 3
#define STATE_LAUNCHED 4


/* prototypes of internal functions */
static int preprocess_node(filter_node_t *n);
static int init_node(filter_node_t *n);
static int launch_node(filter_node_t *n);
static void postprocess_node(filter_node_t *n);



filter_network_t *filternetwork_new()
{
	filter_network_t *net;

	if (!(net = ALLOC(filter_network_t)))
		return NULL;

	INIT_LIST_HEAD(&net->inputs);
	INIT_LIST_HEAD(&net->outputs);

	pthread_mutex_init(&net->mx, NULL);
	net->state = STATE_UNDEFINED;

	return net;
}

/* ok, we have to launch the network back-to-front
 * (i.e. it is a _directed_ network)
 * we do this recursievely - ugh(?)
 */
int filternetwork_launch(filter_network_t *net)
{
	filter_node_t *n;
	sigset_t sigs;

	/* block EPIPE */
	sigemptyset(&sigs);
	sigaddset(&sigs, SIG_BLOCK);
	sigprocmask(SIG_BLOCK, &sigs, NULL);

	/* lock state&result */
	pthread_mutex_lock(&net->mx);
	if (net->state != STATE_UNDEFINED) {
		pthread_mutex_unlock(&net->mx);
		return -1;
	}

	net->result = 0;
	net->state = STATE_LAUNCHED;
	pthread_mutex_unlock(&net->mx);

	__list_foreach(&net->inputs, filter_node_t, neti_list, n,
		       { if (preprocess_node(n) == -1)
			       goto out;
		       } );

	__list_foreach(&net->inputs, filter_node_t, neti_list, n,
		       { if (init_node(n) == -1)
			       goto out;
		       } );

	__list_foreach(&net->inputs, filter_node_t, neti_list, n,
		       { if (launch_node(n) == -1)
			       goto out;
		       } );

	return 0;

 out:
	__list_foreach(&net->inputs, filter_node_t, neti_list, n,
		       { postprocess_node(n);
		       } );

	pthread_mutex_lock(&net->mx);
	net->state = STATE_UNDEFINED;
	pthread_mutex_unlock(&net->mx);

	return -1;
}

/* wait for the network to finish processing */
int filternetwork_wait(filter_network_t *net)
{
	int res, wait_again;
	filter_node_t *n;

	pthread_mutex_lock(&net->mx);
	if (net->state != STATE_LAUNCHED) {
		pthread_mutex_unlock(&net->mx);
		return -1;
	}
	net->state = STATE_UNDEFINED;
	pthread_mutex_unlock(&net->mx);

	do {
		wait_again = 0;
		__list_foreach(&net->inputs, filter_node_t, neti_list, n,
			       {
				       if ((res = pthread_join(n->thread, NULL)) != 0
					   && (res != ESRCH))
					       wait_again = 1;
			       } );
	} while (wait_again);

	__list_foreach(&net->inputs, filter_node_t, neti_list, n,
		       { postprocess_node(n);
		       } );

	return net->result;
}

/* kill the network */
void filternetwork_terminate(filter_network_t *net)
{
	filter_node_t *n;

	__list_foreach(&net->inputs, filter_node_t, neti_list, n,
		       { postprocess_node(n);
		       } );
}



/* we preprocess the network recursively - so it has to be
 * non cyclic.
 * preprocessing does memory allocation for the file-descriptors */
static int preprocess_node(filter_node_t *n)
{
	int i;

	if (!n || n->state == STATE_PREPROCESSED)
		return 0;
	if (n->state != STATE_UNDEFINED)
		return -1;
	n->state = STATE_PREPROCESSED;

	for (i=0; i<n->nr_inputs; i++)
		n->inputs[i]->dest_fd = -1;

	for (i=0; i<n->nr_outputs; i++)
		n->outputs[i]->source_fd = -1;

	/* recurse on output filters */
	for (i=0; i<n->nr_outputs; i++)
		if (preprocess_node(n->outputs[i]->dest) == -1)
			return -1;

	return 0;
}

/* we initialize the network recursively.
 * initialization does alloc the pipes */
static int init_node(filter_node_t *n)
{
	int i, fds[2];

	if (!n || n->state == STATE_INITIALIZED)
		return 0;
	if (n->state != STATE_PREPROCESSED)
		return -1;
	n->state = STATE_INITIALIZED;

	for (i=0; i<n->nr_outputs; i++) {
		if (pipe(fds) == -1)
			return -1;
		n->outputs[i]->source_fd = fds[1];
		n->outputs[i]->dest_fd = fds[0];
	}
	
	/* recurse on output filters */
	for (i=0; i<n->nr_outputs; i++)
		if (init_node(n->outputs[i]->dest) == -1)
			return -1;

	return 0;
}


static void *launcher(void *node)
{
	filter_node_t *n = (filter_node_t *)node;
	int i;

	if (n->filter->f(n) == 0)
		return NULL;

	/* set result */
	pthread_mutex_lock(&n->net->mx);
	n->net->result = -1;
	pthread_mutex_unlock(&n->net->mx);

	/* send EOFs */
	for (i=0; i<n->nr_outputs; i++)
		fbuf_queue(n->outputs[i], NULL);

	return NULL;
}

/* we launch the network recursively, but we have to do
 * this back to front (really??) */
static int launch_node(filter_node_t *n)
{
	int i;

	if (!n || n->state >= STATE_PRELAUNCHED)
		return 0;

	n->state = STATE_PRELAUNCHED;

	/* recurse on output filters */
	for (i=0; i<n->nr_outputs; i++)
		if (launch_node(n->outputs[i]->dest) == -1)
			return -1;

	/* launch filter thread */
	if (pthread_create(&n->thread, NULL, launcher, n) != 0)
		return -1;

	n->state = STATE_LAUNCHED;

	return 0;
}

/* we postprocess the network recursively - so it has to be
 * non cyclic.
 * postprocessing does memory deallocation and fd-closing */
static void postprocess_node(filter_node_t *n)
{
	int i;

	if (!n || n->state == STATE_UNDEFINED)
		return;

	if (n->state == STATE_LAUNCHED
	    && pthread_cancel(n->thread) != ESRCH) {
		pthread_join(n->thread, NULL);
		n->state = STATE_INITIALIZED;
	}

	for (i=0; i<n->nr_inputs; i++)
		if (n->inputs[i]->source_fd != -1)
			close(n->inputs[i]->source_fd);

	for (i=0; i<n->nr_outputs; i++)
		if (n->outputs[i]->dest_fd != -1)
			close(n->outputs[i]->dest_fd);

	n->state = STATE_UNDEFINED;

	/* recurse on output filters */
	for (i=0; i<n->nr_outputs; i++)
		postprocess_node(n->outputs[i]->dest);
}

