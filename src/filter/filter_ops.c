/*
 * filter_ops.c
 * $Id: filter_ops.c,v 1.19 2001/03/29 09:04:43 richi Exp $
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
#include <signal.h>
#include "util.h"
#include "filter.h"
#include "filter_ops.h"


struct filter_launchcontext {
	int nr_threads;
	pthread_t waiter;

	int state;

	int semid;
	glame_atomic_t result;
};

struct filter_operations {
	int (*init)(filter_t *n);
	int (*launch)(filter_t *n);
	void (*postprocess)(filter_t *n);
	int (*wait)(filter_t *n);
};

#define STATE_RUNNING 3
#define FILTER_IS_RUNNING(f) ((f)->launch_context && (f)->launch_context->state >= STATE_RUNNING)


/* filter_buffer.c: drain pipe to unblock source. */
void fbuf_drain(filter_pipe_t *p);
/* filter_buffer.c: free pending buffers. */
void fbuf_free_buffers(struct list_head *list);


/* The standard filter operations, for "nodes" and for "networks".
 */

static int init_node(filter_t *n)
{
	filter_pipe_t *p;
	filter_port_t *port;
	int fds[2];

	if (!n || n->state == STATE_INITIALIZED)
		return 0;
	if (n->state != STATE_UNDEFINED)
		return -1;
	filter_clear_error(n);

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
	sigset_t sset;

	DPRINTF("%s launched (pid %i)\n", n->name, (int)getpid());

	sigemptyset(&sset);
	sigaddset(&sset, SIGINT);
	pthread_sigmask(SIG_BLOCK, &sset, NULL);

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
		DPRINTF("filter %s completed.\n", n->name);
		pthread_exit(NULL);
	}

	DPRINTF("%s had failure (errstr=\"%s\")\n",
		n->name, n->glerrstr);

	/* signal net failure */
	atomic_inc(&n->net->launch_context->result);

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



/* The API for operation on/of a filter_t (network).
 */


filter_launchcontext_t *_launchcontext_alloc()
{
	filter_launchcontext_t *c;

	if (!(c = ALLOC(filter_launchcontext_t)))
		return NULL;
	if ((c->semid = glame_semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) == -1) {
		free(c);
		return NULL;
	}
	{
		union semun sun;
		sun.val = 0;
		glame_semctl(c->semid, 0, SETVAL, sun);
	}
	ATOMIC_INIT(c->result, 0);
	c->nr_threads = 0;
	if (c->semid != -1)
		return c;

	free(c);
	return NULL;
}

void _launchcontext_free(filter_launchcontext_t *c)
{
	if (!c)
		return;
	ATOMIC_RELEASE(c->result);
	{
		union semun sun;
		glame_semctl(c->semid, 0, IPC_RMID, sun);
	}
	free(c);
}

static void *waiter(void *network)
{
	filter_t *net = (filter_t *)network;
	int res;
	sigset_t sset;

	sigemptyset(&sset);
	sigaddset(&sset, SIGINT);
	pthread_sigmask(SIG_BLOCK, &sset, NULL);

	res = net->ops->wait(net);

	DPRINTF("starting cleanup\n");
	net->ops->postprocess(net);
	_launchcontext_free(net->launch_context);
	net->launch_context = NULL;
	DPRINTF("finished\n");

	return (void *)res;
}

int filter_launch(filter_t *net)
{
	/* sigset_t sigs; */

	if (!net || !FILTER_IS_NETWORK(net) || FILTER_IS_LAUNCHED(net))
		return -1;

	/* block EPIPE
	sigemptyset(&sigs);
	sigaddset(&sigs, SIG_BLOCK);
	sigprocmask(SIG_BLOCK, &sigs, NULL); */

	/* init state */
	if (!(net->launch_context = _launchcontext_alloc()))
		return -1;

	DPRINTF("initting nodes\n");
	if (net->ops->init(net) == -1)
		goto out;

	DPRINTF("launching nodes\n");
	if (net->ops->launch(net) == -1)
		goto out;

	DPRINTF("launching waiter\n");
	if (pthread_create(&net->launch_context->waiter, NULL,
			   waiter, net) != 0)
		goto out;

	net->launch_context->state = STATE_LAUNCHED;
	DPRINTF("all nodes launched.\n");

	return 0;

 out:
	DPRINTF("error.\n");
	filter_terminate(net);

	return -1;
}

int filter_start(filter_t *net)
{
	struct sembuf sop;

	if (!net || !FILTER_IS_NETWORK(net) || !FILTER_IS_LAUNCHED(net)
	    || FILTER_IS_RUNNING(net))
		return -1;

	DPRINTF("waiting for nodes to complete initialization.\n");
        sop.sem_num = 0;
        sop.sem_op = -net->launch_context->nr_threads;
        sop.sem_flg = 0;
        glame_semop(net->launch_context->semid, &sop, 1);
	if (ATOMIC_VAL(net->launch_context->result) != 0)
		goto out;
	net->launch_context->state = STATE_RUNNING;

	return 0;

out:
	DPRINTF("Network error. Terminating.\n");
	filter_terminate(net);
	return -1;
}

int filter_pause(filter_t *net)
{
	struct sembuf sop;

	if (!net || !FILTER_IS_NETWORK(net) || !FILTER_IS_LAUNCHED(net)
	    || !FILTER_IS_RUNNING(net))
		return -1;

        sop.sem_num = 0;
        sop.sem_op = net->launch_context->nr_threads;
        sop.sem_flg = IPC_NOWAIT;
        glame_semop(net->launch_context->semid, &sop, 1);
	net->launch_context->state = STATE_LAUNCHED;

	return 0;
}

int filter_wait(filter_t *net)
{
	void *res;

	if (!net || !FILTER_IS_NETWORK(net) || !FILTER_IS_LAUNCHED(net)
	    || !FILTER_IS_RUNNING(net))
		return -1;

	DPRINTF("waiting for waiter to complete\n");
	pthread_join(net->launch_context->waiter, &res);

	return (int)res;
}

int filter_is_ready(filter_t *net)
{
	if (!net)
		return -1;
	return !FILTER_IS_RUNNING(net) ? 1 : 0;
}

void filter_terminate(filter_t *net)
{
	if (!net || !FILTER_IS_NETWORK(net) || !FILTER_IS_LAUNCHED(net))
		return;

	atomic_set(&net->launch_context->result, 1);
	{
		union semun sun;
		sun.val = 0;
		glame_semctl(net->launch_context->semid, 0, SETVAL, sun);
	}
	net->launch_context->state = STATE_RUNNING;

	/* "safe" cleanup */
	filter_wait(net);
}



/* Hooks for filter synchronization. Used by FILTER_AFTER_INIT
 * and FILTER_CHECK_STOP macros from filter.h only.
 */

int filter_after_init_hook(filter_t *f)
{
	struct sembuf sop;

	DPRINTF("%s seems ready for signalling\n", f->name);
        filter_clear_error(f);

	/* raise semaphore that counts ready filters */
        sop.sem_num = 0;
        sop.sem_op = 1;
        sop.sem_flg = IPC_NOWAIT;
        glame_semop(f->net->launch_context->semid, &sop, 1);

	/* wait for manager to signal "all filters ready" */
        sop.sem_num = 0;
        sop.sem_op = 0;
        sop.sem_flg = 0;
        glame_semop(f->net->launch_context->semid, &sop, 1);

	/* still check, if any filter had an error */
	return ATOMIC_VAL(f->net->launch_context->result);
}

int filter_check_stop_hook(filter_t *f)
{
        struct sembuf sop;

	/* check if manager said "pause/stop" */
        sop.sem_num = 0;
        sop.sem_op = 0;
        sop.sem_flg = 0;
        glame_semop(f->net->launch_context->semid, &sop, 1);

	/* return if it was "pause" or really "stop" */
	return ATOMIC_VAL(f->net->launch_context->result);
}

