/*
 * filter_ops.c
 * $Id: filter_ops.c,v 1.11 2000/03/24 11:08:14 richi Exp $
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

#include <unistd.h>
#include "util.h"
#include "filter.h"


static int init_node(filter_node_t *n)
{
	filter_pipe_t *p;
	int fds[2];

	if (!n || n->state == STATE_INITIALIZED)
		return 0;
	if (n->state != STATE_UNDEFINED)
		return -1;
	if (filternode_has_error(n))
	        return -1;

	filternode_foreach_output(n, p) {
		if (pipe(fds) == -1)
			return -1;
		p->source_fd = fds[1];
		p->dest_fd = fds[0];
	}
	n->state = STATE_INITIALIZED;

	return 0;
}
static int init_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;

	if (n->net)
		net->launch_context = n->net->launch_context;

	filternetwork_foreach_node(net, n)
		if (n->ops->init(n) == -1)
			return -1;
	net->node.state = STATE_INITIALIZED;

	return 0;
}

static void *launcher(void *node)
{
	filter_node_t *n = (filter_node_t *)node;
	filter_pipe_t *p;

	DPRINTF("%s launched (pid %i)\n", n->name, (int)getpid());

	n->glerrno = n->filter->f(n);
	if (n->glerrno == 0) {
		/* either finish or terminate -> drain pipes, send EOFs */
		filternode_foreach_input(n, p)
			fbuf_drain(p);
		filternode_foreach_output(n, p)
			fbuf_queue(p, NULL);
	        filternode_clear_error(n);
		DPRINTF("filter %s completed.\n", n->name);
		pthread_exit(NULL);
	}

	DPRINTF("%s had failure (errstr=\"%s\")\n",
		n->name, n->glerrstr);

	/* allow failure of "unused" nodes (without connections) */
	if (n->nr_inputs != 0 || n->nr_outputs != 0) {
		/* signal net failure */
		atomic_inc(&n->net->launch_context->result);
	} else {
		DPRINTF("ignoring failure of %s\n", n->name);
	}

	/* increment filter ready semaphore */
	sem_op(n->net->launch_context->semid, 0, 1);

	pthread_exit((void *)-1);

	return NULL;
}
static int launch_node(filter_node_t *n)
{
	if (!n || n->state == STATE_LAUNCHED)
		return 0;
	if (n->state != STATE_INITIALIZED)
		return -1;

	/* launch filter thread */
	if (pthread_create(&n->thread, NULL, launcher, n) != 0)
		return -1;
	n->net->launch_context->nr_threads++;
	n->state = STATE_LAUNCHED;

	return 0;
}
static int launch_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;

	filternetwork_foreach_node(net, n)
		if (n->ops->launch(n) == -1)
			return -1;
	net->node.state = STATE_LAUNCHED;

	return 0;
}

/* kill threads, close pipes and free pending buffers */
static void postprocess_node(filter_node_t *n)
{
	filter_pipe_t *p;

	if (!n || n->state == STATE_UNDEFINED)
		return;

	if (n->state == STATE_LAUNCHED
	    && pthread_cancel(n->thread) != ESRCH) {
		pthread_join(n->thread, NULL);
		n->state = STATE_INITIALIZED;
	}

	filternode_foreach_input(n, p)
		if (p->source_fd != -1) {
			close(p->source_fd);
			p->source_fd = -1;
		}
	filternode_foreach_output(n, p)
		if (p->dest_fd != -1) {
			close(p->dest_fd);
			p->dest_fd = -1;
		}
	fbuf_free_buffers(&n->buffers);

	n->state = STATE_UNDEFINED;
}
static void postprocess_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;

	filternetwork_foreach_node(net, n)
		n->ops->postprocess(n);
	net->node.state = STATE_UNDEFINED;
}

static int wait_node(filter_node_t *n)
{
	int res, filter_ret;

	DPRINTF("Waiting for %s.\n", n->name);
	while ((res = pthread_join(n->thread, 
				   (void **)&filter_ret)) != 0
	       && (res != ESRCH))
		;

	return filter_ret;
}
static int wait_network(filter_node_t *n)
{
	filter_network_t *net = (filter_network_t *)n;
	int res = 0;

	filternetwork_foreach_node(net, n)
		if (n->ops->wait(n) == -1)
			res = -1;

	return res;
}


struct filter_node_operations filter_node_ops = {
	init_node,
	launch_node,
	postprocess_node,
	wait_node,
};

struct filter_node_operations filter_network_ops = {
	init_network,
	launch_network,
	postprocess_network,
	wait_network,
};
