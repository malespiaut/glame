/*
 * filter_ops.c
 * $Id: filter_ops.c,v 1.29 2001/11/05 11:08:59 richi Exp $
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <signal.h>
#include "util.h"
#include "filter.h"
#include "filter_ops.h"



struct filter_operations {
	int (*init)(filter_t *n);
	int (*launch)(filter_t *n);
	void (*postprocess)(filter_t *n);
	int (*wait)(filter_t *n);
};

#define STATE_RUNNING 3
#define FILTER_IS_RUNNING(f) (((f)->launch_context && (f)->launch_context->state >= STATE_RUNNING) || ((f)->net && (f)->net->launch_context && (f)->net->launch_context->state >= STATE_RUNNING))


/* filter_buffer.c: drain pipe to unblock source. */
void fbuf_drain(filter_pipe_t *p);
/* filter_buffer.c: free pending buffers. */
void fbuf_free_buffers(struct glame_list_head *list);


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
	n->launch_context = n->net->launch_context;
	n->state = STATE_INITIALIZED;

	return 0;
}
static int init_network(filter_t *net)
{
	filter_t *n;

	if (net->net)
		net->launch_context = net->net->launch_context;

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
	sigset_t sset;

	DPRINTF("%s launched (pid %i)\n", n->name, (int)getpid());

#ifdef HAVE_PTHREAD_SIGMASK
	sigemptyset(&sset);
	sigaddset(&sset, SIGINT);
	pthread_sigmask(SIG_BLOCK, &sset, NULL);
#endif
#ifndef USE_SYSVSEM
	atomic_inc(&n->net->launch_context->val);
#endif

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

	/* increment filter ready semaphore if still in startup
	 * phase */
	if (n->net->launch_context->state <= STATE_LAUNCHED) {
#ifdef USE_SYSVSEM
		struct sembuf sop;
		sop.sem_num = 0;
		sop.sem_op = 1;
		sop.sem_flg = IPC_NOWAIT;
		glame_semop(n->net->launch_context->semid, &sop, 1);
#else
		pthread_mutex_lock(&n->net->launch_context->cond_mx);
		if (atomic_dec_and_test(&n->net->launch_context->val))
			pthread_cond_broadcast(&n->net->launch_context->cond);
		else
			pthread_cond_wait(&n->net->launch_context->cond,
					  &n->net->launch_context->cond_mx);
		pthread_mutex_unlock(&n->net->launch_context->cond_mx);
#endif
	}

	/* if it was an error during processing, drain the pipes
	 * and send eofs */
	if (n->net->launch_context->state >= STATE_RUNNING) {
		filterportdb_foreach_port(filter_portdb(n), port) {
			filterport_foreach_pipe(port, p) {
			        if (filterport_is_input(port))
					fbuf_drain(p);
				else
					fbuf_queue(p, NULL);
			}
		}
	}

	DPRINTF("%s exiting after error\n", n->name);
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

	n->launch_context = NULL;
	n->state = STATE_UNDEFINED;
}
static void postprocess_network(filter_t *n)
{
	filter_t *net = (filter_t *)n;

	filter_foreach_node(net, n)
		n->ops->postprocess(n);
	net->state = STATE_UNDEFINED;
	net->launch_context = NULL;
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


static filter_launchcontext_t *_launchcontext_alloc(int bufsize)
{
	filter_launchcontext_t *c;

	if (!(c = ALLOC(filter_launchcontext_t)))
		return NULL;
#ifdef USE_SYSVSEM
	if ((c->semid = glame_semget(IPC_PRIVATE, 1, IPC_CREAT|0660)) == -1) {
		free(c);
		return NULL;
	}
	{
		union semun ssun;
		ssun.val = 0;
		glame_semctl(c->semid, 0, SETVAL, ssun);
	}
#else
	pthread_mutex_init(&c->cond_mx, NULL);
	pthread_cond_init(&c->cond, NULL);
	ATOMIC_INIT(c->val, 0);
#endif
	ATOMIC_INIT(c->result, 0);
	c->nr_threads = 0;
	c->bufsize = bufsize;
	return c;
}

static void _launchcontext_free(filter_launchcontext_t *c)
{
	if (!c)
		return;
	ATOMIC_RELEASE(c->result);
#ifdef USE_SYSVSEM
	{
		union semun ssun;
		glame_semctl(c->semid, 0, IPC_RMID, ssun);
	}
#else
	pthread_mutex_destroy(&c->cond_mx);
	pthread_cond_destroy(&c->cond);
	ATOMIC_RELEASE(c->val);
#endif
	free(c);
}

static void *waiter(void *network)
{
	filter_t *net = (filter_t *)network;
	struct filter_launchcontext *c;
	int res;
	sigset_t sset;

#ifdef HAVE_PTHREAD_SIGMASK
	sigemptyset(&sset);
	sigaddset(&sset, SIGINT);
	pthread_sigmask(SIG_BLOCK, &sset, NULL);
#endif

	res = net->ops->wait(net);

	DPRINTF("starting cleanup\n");
	c = net->launch_context;
	net->ops->postprocess(net);
	_launchcontext_free(c);
	DPRINTF("finished\n");

	return (void *)res;
}

int filter_launch(filter_t *net, int bufsize)
{
	/* sigset_t sigs; */

	if (!net || !FILTER_IS_NETWORK(net) || net->net
	    || FILTER_IS_LAUNCHED(net))
		return -1;

	/* block EPIPE
	sigemptyset(&sigs);
	sigaddset(&sigs, SIG_BLOCK);
	sigprocmask(SIG_BLOCK, &sigs, NULL); */

	/* init state */
	if (!(net->launch_context = _launchcontext_alloc(bufsize)))
		return -1;
#ifndef USE_SYSVSEM
	atomic_set(&net->launch_context->val, 1);
#endif

	DPRINTF("initting nodes\n");
	if (net->ops->init(net) == -1)
		goto out;

	DPRINTF("launching nodes\n");
	if (net->ops->launch(net) == -1)
		goto out;

	/* need to wait for all nodes to complete initialization
	 * - FIXME polling is broken
	 * - FIXME SYSVSEM version missing/impossible
	 * - FIXME - doesnt work anyway, racy wrt concurrently
	 *         increasing/decreasing val */
	DPRINTF("waiting for nodes to complete initialization\n");
	while (atomic_read(&net->launch_context->val) != 1)
		usleep(1000);

	DPRINTF("launching waiter\n");
	if (pthread_create(&net->launch_context->waiter, NULL,
			   waiter, net) != 0)
		goto out;

	net->launch_context->state = STATE_LAUNCHED;
	DPRINTF("all nodes launched.\n");

	return 0;

 out:
	DPRINTF("error.\n");
	filter_terminate(net); /* FIXME - this seems to be error prone. */

	return -1;
}

int filter_start(filter_t *net)
{
	if (!net || !FILTER_IS_NETWORK(net) || !FILTER_IS_LAUNCHED(net)
	    || FILTER_IS_RUNNING(net))
		return -1;

	DPRINTF("waiting for nodes to complete initialization.\n");
#ifdef USE_SYSVSEM
	{
		struct sembuf sop;
		sop.sem_num = 0;
		sop.sem_op = -net->launch_context->nr_threads;
		sop.sem_flg = 0;
		glame_semop(net->launch_context->semid, &sop, 1);
	}
#else
	pthread_mutex_lock(&net->launch_context->cond_mx);
	if (atomic_dec_and_test(&net->launch_context->val))
		pthread_cond_broadcast(&net->launch_context->cond);
	else
		pthread_cond_wait(&net->launch_context->cond,
				  &net->launch_context->cond_mx);
	pthread_mutex_unlock(&net->launch_context->cond_mx);
#endif
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
	if (!net || !FILTER_IS_NETWORK(net) || !FILTER_IS_LAUNCHED(net)
	    || !FILTER_IS_RUNNING(net))
		return -1;

#if USE_SYSVSEM
	{
		struct sembuf sop;
		sop.sem_num = 0;
		sop.sem_op = net->launch_context->nr_threads;
		sop.sem_flg = IPC_NOWAIT;
		glame_semop(net->launch_context->semid, &sop, 1);
	}
#else
	atomic_inc(&net->launch_context->val);
#endif
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
	if (!net || !FILTER_IS_NETWORK(net) || !FILTER_IS_LAUNCHED(net)
	    || !net->launch_context)
		return;

	atomic_set(&net->launch_context->result, 1);
#ifdef USE_SYSVSEM
	{
		union semun ssun;
		ssun.val = 0;
		glame_semctl(net->launch_context->semid, 0, SETVAL, ssun);
	}
#else
	atomic_set(&net->launch_context->val, 0);
#endif
	net->launch_context->state = STATE_RUNNING;

	/* "safe" cleanup */
	filter_wait(net);
}



/* Hooks for filter synchronization. Used by FILTER_AFTER_INIT
 * and FILTER_CHECK_STOP macros from filter.h only.
 */

int filter_after_init_hook(filter_t *f)
{
	DPRINTF("%s seems ready for signalling\n", f->name);
        filter_clear_error(f);

#ifdef USE_SYSVSEM
	{
		struct sembuf sop;

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
	}
#else
	pthread_mutex_lock(&f->net->launch_context->cond_mx);
	if (atomic_dec_and_test(&f->net->launch_context->val))
		pthread_cond_broadcast(&f->net->launch_context->cond);
	else
		pthread_cond_wait(&f->net->launch_context->cond,
				  &f->net->launch_context->cond_mx);
	pthread_mutex_unlock(&f->net->launch_context->cond_mx);
#endif

	/* still check, if any filter had an error */
	return ATOMIC_VAL(f->net->launch_context->result);
}

int filter_check_stop_hook(filter_t *f)
{
	/* check if manager said "pause/stop" */
#if USE_SYSVSEM
	{
		struct sembuf sop;
		sop.sem_num = 0;
		sop.sem_op = 0;
		sop.sem_flg = 0;
		glame_semop(f->net->launch_context->semid, &sop, 1);
	}
#else
	if (atomic_read(&f->net->launch_context->val)) {
		pthread_mutex_lock(&f->net->launch_context->cond_mx);
		pthread_cond_wait(&f->net->launch_context->cond,
				  &f->net->launch_context->cond_mx);
		pthread_mutex_unlock(&f->net->launch_context->cond_mx);
	}
#endif

	/* return if it was "pause" or really "stop" */
	return (ATOMIC_VAL(f->net->launch_context->result)
	        || filter_errno(f));
}
