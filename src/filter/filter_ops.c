/*
 * filter_ops.c
 * $Id: filter_ops.c,v 1.41 2004/12/26 20:55:29 richi Exp $
 *
 * Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Richard Guenther
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



#define STATE_RUNNING 3


/* filter_buffer.c: drain pipe to unblock source. */
void fbuf_drain(filter_pipe_t *p);
/* filter_buffer.c: free pending buffers. */
int fbuf_free_buffers(struct glame_list_head *list);


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
	filter_launchcontext_t *c = n->net->launch_context;
	filter_pipe_t *p;
	filter_port_t *port;
	sigset_t sset;

	DPRINTF("%s launched (pid %i)\n", n->name, (int)getpid());

#ifdef HAVE_PTHREAD_SIGMASK
	sigemptyset(&sset);
	sigaddset(&sset, SIGINT);
	sigaddset(&sset, SIGPIPE);
	pthread_sigmask(SIG_BLOCK, &sset, NULL);
#endif
	atomic_inc(&c->val);

	n->glerrno = n->f(n);

	/* First close our input pipe ends 
	 * - note that closing output pipe ends may drop unread
	 *   buffers for adjactant filters, so we may not do that. */
	filterportdb_foreach_port(filter_portdb(n), port) {
		filterport_foreach_pipe(port, p) {
			if (filterport_is_input(port)) {
				close(p->dest_fd);
				p->dest_fd = -1;
			} /* else {
				close(p->source_fd);
				p->source_fd = -1;
			} */ else {
				/* check queueing an EOF... */
				fbuf_queue(p, NULL);
			}
		}
	}

	/* no error? just quit */
	if (n->glerrno == 0) {
		DPRINTF("filter %s completed.\n", n->name);
		pthread_exit(NULL);
	}

	/* failure is more complex */
	DPRINTF("%s had failure (errstr=\"%s\")\n",
		n->name, n->glerrstr);

	/* signal net failure */
	atomic_inc(&c->result);

	/* increment filter ready semaphore if still in startup
	 * phase */
	if (c->state <= STATE_LAUNCHED) {
		pthread_mutex_lock(&c->cond_mx);
		if (atomic_dec_and_test(&c->val))
			pthread_cond_broadcast(&c->cond);
		else
			pthread_cond_wait(&c->cond, &c->cond_mx);
		pthread_mutex_unlock(&c->cond_mx);
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
	int freed;

	if (!n || n->state == STATE_UNDEFINED)
		return;

	/* join thread to cleanup thread resources */
	if (n->state != STATE_INITIALIZED)
		pthread_join(n->thread, NULL);
	n->state = STATE_INITIALIZED;

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
	freed = fbuf_free_buffers(&n->buffers);
	if (freed > 0)
		DPRINTF("freed %i buffers created by %s\n",
			freed, filter_name(n));

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
	int res;
	void *filter_ret;

	DPRINTF("Waiting for %s.\n", n->name);
	while ((res = pthread_join(n->thread, 
				   &filter_ret)) != 0
	       && (res != ESRCH))
		;
	n->state = STATE_INITIALIZED;

	return filter_ret == NULL ? 0 : -1;
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


static filter_launchcontext_t *_launchcontext_alloc(int bufsize, filter_t *net)
{
	filter_launchcontext_t *c;

	if (!(c = ALLOC(filter_launchcontext_t)))
		return NULL;

	ATOMIC_INIT(c->refcnt, 1);

	pthread_mutex_init(&c->cond_mx, NULL);
	pthread_cond_init(&c->cond, NULL);
	ATOMIC_INIT(c->val, 0);

	ATOMIC_INIT(c->result, 0);
	c->net = net;
	c->nr_threads = 0;
	c->bufsize = bufsize;
	return c;
}

static filter_launchcontext_t *_launchcontext_ref(filter_launchcontext_t *c)
{
	atomic_inc(&c->refcnt);
	return c;
}

void filter_launchcontext_ref(filter_launchcontext_t *c)
{
	_launchcontext_ref(c);
}

void filter_launchcontext_unref(filter_launchcontext_t **c_)
{
	filter_launchcontext_t *c = *c_;
	if (!c)
		PANIC("unref'ing freed launchcontext");
	if (!atomic_dec_and_test(&c->refcnt))
		return;
	ATOMIC_RELEASE(c->result);
	pthread_mutex_destroy(&c->cond_mx);
	pthread_cond_destroy(&c->cond);
	ATOMIC_RELEASE(c->val);
	ATOMIC_RELEASE(c->refcnt);
	free(c);
	*c_ = NULL;
	DPRINTF("launchcontext freed.\n");
}

static void *waiter(void *context)
{
	filter_launchcontext_t *c = (filter_launchcontext_t *)context;
	sigset_t sset;

#ifdef HAVE_PTHREAD_SIGMASK
	sigemptyset(&sset);
	sigaddset(&sset, SIGINT);
	pthread_sigmask(SIG_BLOCK, &sset, NULL);
#endif

	DPRINTF("waiting for network to finish\n");
	c->net->ops->wait(c->net);

	DPRINTF("starting cleanup\n");
	c->net->ops->postprocess(c->net);

	/* defer final launchcontext freeing to filter_wait(). */
	c->state = STATE_UNDEFINED;
	filter_launchcontext_unref(&c);
	DPRINTF("finished\n");

	return NULL;
}

filter_launchcontext_t *filter_launch(filter_t *net, int bufsize)
{
	filter_launchcontext_t *c = NULL;
	/* sigset_t sigs; */

	if (!net || !FILTER_IS_NETWORK(net) || net->net
	    || FILTER_IS_LAUNCHED(net))
		return NULL;

	/* block EPIPE
	sigemptyset(&sigs);
	sigaddset(&sigs, SIG_BLOCK);
	sigprocmask(SIG_BLOCK, &sigs, NULL); */

	/* init state, network nodes dont hold references to it (FIXME?) */
	c = _launchcontext_alloc(bufsize, net);
	if (!c)
		return NULL;
	net->launch_context = c;
	atomic_set(&c->val, 1);

	DPRINTF("initting nodes\n");
	if (net->ops->init(net) == -1)
		goto out;

	DPRINTF("launching nodes\n");
	if (net->ops->launch(net) == -1)
		goto out;

	/* need to wait for all nodes to complete initialization
	 * - FIXME polling is broken
	 * - FIXME - doesnt work anyway, racy wrt concurrently
	 *         increasing/decreasing val */
	DPRINTF("waiting for nodes to complete initialization\n");
	while (atomic_read(&c->val) != 1)
		usleep(1000);

	DPRINTF("launching waiter\n");
	_launchcontext_ref(c); /* for waiter */
	if (pthread_create(&c->waiter, NULL, waiter, c) != 0)
		goto out;

	c->state = STATE_LAUNCHED;
	DPRINTF("all nodes launched.\n");

	/* pass reference to caller */
	return c;

 out:
	DPRINTF("error.\n");
	filter_terminate(c); /* FIXME - this seems to be error prone. */
	filter_launchcontext_unref(&c);

	return NULL;
}

int filter_start(filter_launchcontext_t *context)
{
	if (!context || context->state >= STATE_RUNNING)
		return -1;

	DPRINTF("waiting for nodes to complete initialization.\n");

	pthread_mutex_lock(&context->cond_mx);
	if (atomic_dec_and_test(&context->val))
		pthread_cond_broadcast(&context->cond);
	else
		pthread_cond_wait(&context->cond, &context->cond_mx);
	pthread_mutex_unlock(&context->cond_mx);
	context->state = STATE_RUNNING;

	if (ATOMIC_VAL(context->result) != 0)
		goto out;

	return 0;

out:
	DPRINTF("Network error. Terminating with result %i.\n", ATOMIC_VAL(context->result));
	filter_terminate(context);
	return -1;
}

int filter_wait(filter_launchcontext_t *context)
{
	if (!context || context->state < STATE_RUNNING)
		return -1;

	DPRINTF("waiting for waiter to complete\n");
	pthread_join(context->waiter, NULL);

	return ATOMIC_VAL(context->result);
}

int filter_is_ready(filter_launchcontext_t *context)
{
	if (!context)
		return -1;
	return context->state < STATE_LAUNCHED ? 1 : 0;
}

void filter_terminate(filter_launchcontext_t *context)
{
	if (!context)
		return;

	if (context->state == STATE_LAUNCHED) {
		/* signal initialization error and let threads terminate */
		atomic_inc(&context->result);
		pthread_mutex_lock(&context->cond_mx);
		if (atomic_dec_and_test(&context->val))
			pthread_cond_broadcast(&context->cond);
		else
			pthread_cond_wait(&context->cond, &context->cond_mx);
		pthread_mutex_unlock(&context->cond_mx);
		context->state = STATE_RUNNING;
	} else {
		/* signal threads to bail out at next FILTER_CHECK_STOP */
		atomic_inc(&context->result);
	}

	/* "safe" cleanup */
	filter_wait(context);
	atomic_dec(&context->result);
}



/* Hooks for filter synchronization. Used by FILTER_AFTER_INIT
 * and FILTER_CHECK_STOP macros from filter.h only.
 */

int filter_after_init_hook(filter_t *f)
{
	DPRINTF("%s seems ready for signalling\n", f->name);
        filter_clear_error(f);

	pthread_mutex_lock(&f->net->launch_context->cond_mx);
	if (atomic_dec_and_test(&f->net->launch_context->val))
		pthread_cond_broadcast(&f->net->launch_context->cond);
	else
		pthread_cond_wait(&f->net->launch_context->cond,
				  &f->net->launch_context->cond_mx);
	pthread_mutex_unlock(&f->net->launch_context->cond_mx);

	/* still check, if any filter had an error */
	return ATOMIC_VAL(f->net->launch_context->result);
}

