/*
 * filter_ops.c
 * $Id: filter_ops.c,v 1.15 2000/10/28 13:45:48 richi Exp $
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


/* filter_buffer.c: drain pipe to unblock source. */
void fbuf_drain(filter_pipe_t *p);
/* filter_buffer.c: free pending buffers. */
void fbuf_free_buffers(struct list_head *list);


static int init_node(filter_t *n)
{
	filter_pipe_t *p;
	filter_port_t *port;
	int fds[2];

	if (!n || n->state == STATE_INITIALIZED)
		return 0;
	if (n->state != STATE_UNDEFINED)
		return -1;
	if (filter_has_error(n)) {
		DPRINTF("node %s has error (%s)\n", n->name,
			filter_errstr(n));
	        return -1;
	}

	filterportdb_foreach_port(filter_portdb(n), port) {
		if (!filterport_is_output(port))
			continue;
		filterport_foreach_pipe(port, p) {
			if (pipe(fds) == -1)
				return -1;
			p->source_fd = fds[1];
			p->dest_fd = fds[0];
		}
	}
	n->state = STATE_INITIALIZED;

	return 0;
}
static int init_network(filter_t *n)
{
	filter_t *net = (filter_t *)n;

	if (n->net)
		net->launch_context = n->net->launch_context;

	filter_foreach_node(net, n)
		if (n->ops->init(n) == -1)
			return -1;
	net->state = STATE_INITIALIZED;

	return 0;
}

static void *launcher(void *node)
{
	filter_t *n = (filter_t *)node;
	filter_pipe_t *p;
	filter_port_t *port;
	struct sembuf sop;

	DPRINTF("%s launched (pid %i)\n", n->name, (int)getpid());

	n->glerrno = n->f(n);
	if (n->glerrno == 0) {
		/* either finish or terminate -> drain pipes, send EOFs */
		filterportdb_foreach_port(filter_portdb(n), port) {
			filterport_foreach_pipe(port, p) {
			        if (filterport_is_input(port))
					fbuf_drain(p);
				else
					fbuf_queue(p, NULL);
			}
		}
	        filter_clear_error(n);
		DPRINTF("filter %s completed.\n", n->name);
		pthread_exit(NULL);
	}

	DPRINTF("%s had failure (errstr=\"%s\")\n",
		n->name, n->glerrstr);

	/* FIXME! allow failure of "unused" nodes (without connections)
	   if (n->nr_inputs != 0 || n->nr_outputs != 0) { */
	/* signal net failure */
	atomic_inc(&n->net->launch_context->result);
	/* FIXME! } else {
	   DPRINTF("ignoring failure of %s\n", n->name);
	   } */

	/* increment filter ready semaphore */
	sop.sem_num = 0;
	sop.sem_op = 1;
	sop.sem_flg = IPC_NOWAIT;
	glame_semop(n->net->launch_context->semid, &sop, 1);

	pthread_exit((void *)-1);

	return NULL;
}
static int launch_node(filter_t *n)
{
	if (!n || n->state == STATE_LAUNCHED)
		return 0;
	if (n->state != STATE_INITIALIZED)
		return -1;

	/* launch filter thread */
	DPRINTF("issuing pthread_create for node %s\n", n->name);
	if (pthread_create(&n->thread, NULL, launcher, n) != 0)
		return -1;
	n->net->launch_context->nr_threads++;
	n->state = STATE_LAUNCHED;

	return 0;
}
static int launch_network(filter_t *n)
{
	filter_t *net = (filter_t *)n;

	filter_foreach_node(net, n)
		if (n->ops->launch(n) == -1)
			return -1;
	net->state = STATE_LAUNCHED;

	return 0;
}

/* kill threads, close pipes and free pending buffers */
static void postprocess_node(filter_t *n)
{
	filter_pipe_t *p;
	filter_port_t *port;

	if (!n || n->state == STATE_UNDEFINED)
		return;

	if (n->state == STATE_LAUNCHED
	    && pthread_cancel(n->thread) != ESRCH) {
		pthread_join(n->thread, NULL);
		n->state = STATE_INITIALIZED;
	}

	filterportdb_foreach_port(filter_portdb(n), port) {
		filterport_foreach_pipe(port, p) {
			if (p->source_fd != -1) {
				close(p->source_fd);
				p->source_fd = -1;
			}
			if (p->dest_fd != -1) {
				close(p->dest_fd);
				p->dest_fd = -1;
			}
		}
	}
	fbuf_free_buffers(&n->buffers);

	n->state = STATE_UNDEFINED;
}
static void postprocess_network(filter_t *n)
{
	filter_t *net = (filter_t *)n;

	filter_foreach_node(net, n)
		n->ops->postprocess(n);
	net->state = STATE_UNDEFINED;
}

static int wait_node(filter_t *n)
{
	int res, filter_ret;

	DPRINTF("Waiting for %s.\n", n->name);
	while ((res = pthread_join(n->thread, 
				   (void **)&filter_ret)) != 0
	       && (res != ESRCH))
		;

	return filter_ret;
}
static int wait_network(filter_t *n)
{
	filter_t *net = (filter_t *)n;
	int res = 0;

	filter_foreach_node(net, n)
		if (n->ops->wait(n) == -1)
			res = -1;

	return res;
}


struct filter_operations filter_node_ops = {
	init_node,
	launch_node,
	postprocess_node,
	wait_node,
};

struct filter_operations filter_network_ops = {
	init_network,
	launch_network,
	postprocess_network,
	wait_network,
};
