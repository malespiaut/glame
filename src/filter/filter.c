/*
 * filter.c
 * $Id: filter.c,v 1.36 2000/10/28 13:45:48 richi Exp $
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
#include <sys/stat.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <regex.h>
#include <errno.h>
#include "util.h"
#include "glame_sem.h"
#include "filter.h"
#include "filter_methods.h"
#include "filter_mm.h"


/* helper to create automagically unique node names. */
#define hash_unique_name_node(prefix, nt) _hash_unique_name((prefix), (nt), \
        __hash_pos(filter_t, hash, name, net))

/* filter sub-nodes hash/list additions. */
#define hash_add_node(node) _hash_add(&(node)->hash, _hash((node)->name, \
        (node)->net))
#define list_add_node(node) list_add(&(node)->list, &(node)->net->nodes);


/* filter API.
 */

filter_t *filter_alloc_network()
{
	return _filter_alloc(FILTERTYPE_NETWORK);
}

filter_t *filter_alloc_node(int (*func)(filter_t *))
{
	filter_t *f;

	if (!func)
		return NULL;
	if (!(f = _filter_alloc(FILTERTYPE_NODE)))
		return NULL;

	/* fill in main filter function */
	f->f = func;

	return f;
}

filter_t *filter_clone(filter_t *f)
{
	if (!f || FILTER_IS_PART_OF_NETWORK(f))
		return NULL;
	return _filter_instantiate(f);
}

filter_t *filter_instantiate(plugin_t *p)
{
	filter_t *f;

	if (!p || !(f = (filter_t *)plugin_query(p, PLUGIN_FILTER)))
		return NULL;
	return _filter_instantiate(f);
}


void filter_delete(filter_t *f)
{
	if (!f || FILTER_IS_PLUGIN(f) || FILTER_IS_LAUNCHED(f))
		return;

	if (FILTER_IS_PART_OF_NETWORK(f)) {
		list_remove_node(f);
		hash_remove_node(f);
		f->net->nr_nodes--;
	}
	_filter_free(f);
}


void filter_register(filter_t *f, plugin_t *p)
{
	if (FILTER_IS_PLUGIN(f) || FILTER_IS_PART_OF_NETWORK(f))
		return;
	f->plugin = p;
	f->type |= FILTERTYPE_PLUGIN;
	if (FILTER_IS_NETWORK(f)) {
		// FIXME: recurse even more...
		filter_t *n;
		filter_foreach_node(f, n)
			n->type |= FILTERTYPE_PLUGIN;
	}
	plugin_set(p, PLUGIN_FILTER, f);
}


filter_port_t *filter_add_inputport(filter_t *filter, const char *label,
				    const char *description, int type)
{
	filter_port_t *port;

	if (!filter || FILTER_IS_PLUGIN(filter) || !label)
		return NULL;
	port = filterportdb_add_port(filter_portdb(filter), label,
				     type, FILTER_PORTFLAG_INPUT,
				     FILTERPORT_DESCRIPTION, description,
				     FILTERPORT_END);
	if (!port)
		return NULL;
	if (FILTER_IS_PART_OF_NETWORK(filter))
		glsig_emit(&filter->emitter, GLSIG_FILTER_CHANGED, filter);

	return port;
}

filter_port_t *filter_add_outputport(filter_t *filter, const char *label,
				     const char *description, int type)
{
	filter_port_t *port;

	if (!filter || FILTER_IS_PLUGIN(filter) || !label)
		return NULL;
	port = filterportdb_add_port(filter_portdb(filter), label,
				     type, FILTER_PORTFLAG_OUTPUT,
				     FILTERPORT_DESCRIPTION, description,
				     FILTERPORT_END);
	if (!port)
		return NULL;
	if (FILTER_IS_PART_OF_NETWORK(filter))
		glsig_emit(&filter->emitter, GLSIG_FILTER_CHANGED, filter);

	return port;
}

void filter_delete_port(filter_t *filter, filter_port_t *port)
{
	if (!filter || FILTER_IS_PLUGIN(filter)
	    || FILTER_IS_LAUNCHED(filter) || !port)
		return;

	filterport_delete(port);

	if (FILTER_IS_PART_OF_NETWORK(filter))
		glsig_emit(&filter->emitter, GLSIG_FILTER_CHANGED, filter);
}


/* filter "network" API.
 */

static void *waiter(void *network)
{
	filter_t *net = (filter_t *)network;
	int res;

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


int filter_add_node(filter_t *net, filter_t *node, const char *name)
{
	if (!net || !FILTER_IS_NETWORK(net) || !node || !name)
		return -1;
	node->net = net;
	if (filter_get_node(net, name) == NULL)
		node->name = strdup(name);
	else
		node->name = hash_unique_name_node(name, net);
	if (!node->name)
		return -1;
	hash_add_node(node);
	list_add_node(node);
	net->nr_nodes++;

	return 0;
}


/* filternetwork filters / loading / saving
 * BIG FIXME!
 */

filter_param_t *filternetwork_add_param(filter_t *net,
					const char *node, const char *param,
					const char *label, const char *desc)
{
	filter_t *n;
	filter_param_t *od, *d;

	if (!net || !FILTER_IS_NETWORK(net) || FILTER_IS_PLUGIN(net)
	    || FILTER_IS_LAUNCHED(net)
	    || !node || !param || !label)
		return NULL;
	if (!(n = filter_get_node(net, node)))
		return NULL;
	if (!(od = filterparamdb_get_param(filter_paramdb(n), param)))
		return NULL;
	/* Add param to network node. */
	if (!(d = filterparamdb_add_param(filter_paramdb(net),
					  label, od->type, &od->u,
					  FILTERPARAM_DESCRIPTION, desc,
					  FILTERPARAM_MAP_NODE, node,
					  FILTERPARAM_MAP_LABEL, param,
					  FILTERPARAM_END)))
		return NULL;

	return d;
}

filter_port_t *filternetwork_add_input(filter_t *net,
				       const char *node, const char *port,
				       const char *label, const char *desc)
{
	filter_t *n;
	filter_port_t *d;

	if (!net || !FILTER_IS_NETWORK(net) || FILTER_IS_PLUGIN(net)
	    || FILTER_IS_LAUNCHED(net)
	    || !node || !port || !label)
		return NULL;
	if (!(n = filter_get_node(net, node)))
		return NULL;
	if (!(d = filterportdb_get_port(filter_portdb(n), port))
	    || !filterport_is_input(d))
		return NULL;
	d = filterportdb_add_port(filter_portdb(net), label,
				  d->type, FILTER_PORTFLAG_INPUT,
				  FILTERPORT_DESCRIPTION, desc,
				  FILTERPORT_MAP_NODE, node,
				  FILTERPORT_MAP_LABEL, port,
				  FILTERPORT_END);

	return d;
}

filter_port_t *filternetwork_add_output(filter_t *net,
		      const char *node, const char *port,
		      const char *label, const char *desc)
{
	filter_t *n;
	filter_port_t *d;

	if (!net || !FILTER_IS_NETWORK(net) || FILTER_IS_PLUGIN(net)
	    || FILTER_IS_LAUNCHED(net)
	    || !node || !port || !label)
		return NULL;
	if (!(n = filter_get_node(net, node)))
		return NULL;
	if (!(d = filterportdb_get_port(filter_portdb(n), port))
	    || !filterport_is_output(d))
		return NULL;
	d = filterportdb_add_port(filter_portdb(net), label,
				  d->type, FILTER_PORTFLAG_OUTPUT,
				  FILTERPORT_DESCRIPTION, desc,
				  FILTERPORT_MAP_NODE, node,
				  FILTERPORT_MAP_LABEL, port,
				  FILTERPORT_END);

	return d;
}

void filternetwork_delete_port(filter_t *net, filter_port_t *p)
{
	filterport_delete(p);
}




/* I would like to have the following syntax:
 * (let* ((net (filternetwork_new))
 *        (nodename (filternetwork_add_node net "filter" "nodename")))
 *    (filternode_set_param ....)
 *    (filternetwork_add_input ...)
 *    (filternetwork_add_output ...)
 *    (filternetwork_add_param ...)
 *    (filternode_set_param ....) ; wrapped ones
 *    (let ((pipe (filternetwork_add_connection node "port" node "port)))
 *        (filterpipe_set_sourceparam ...)
 *        (filterpipe_set_destparam ...))
 *    net)
 */
char *filter_to_string(filter_t *net)
{
	char *buf, *val;
	int len;
	filter_t *n;
	filter_port_t *portd;
	filter_param_t *param;
	filter_pipe_t *fpipe;

	if (!net || !FILTER_IS_NETWORK(net))
		return NULL;

	/* first alloc a big-enough buffer FIXME - br0ken. */
	if (!(buf = (char *)malloc(64*1024)))
		return NULL;
	len = 0;

	/* generate the network start part */
	len += sprintf(&buf[len], "(let *((net (filternetwork_new))\n");

	/* iterate over all nodes in the network creating
	 * node create commands. */
	filter_foreach_node(net, n) {
		len += sprintf(&buf[len], "\t(%s (filternetwork_add_node net \"%s\" \"%s\"))\n",
			       n->name, plugin_name(n->plugin), n->name);
	}
	/* ((net .. */
	len += sprintf(&buf[len-1], ")\n") - 1;

	/* first create the parameter set commands for the
	 * nodes. */
	filter_foreach_node(net, n) {
		filterparamdb_foreach_param(filter_paramdb(n), param) {
			val = filterparam_to_string(param);
			len += sprintf(&buf[len], "   (filternode_set_param %s \"%s\" %s)\n",
				       n->name, filterparam_label(param), val);
			free(val);
		}
	}

	/* create the port/parameter export commands */
	filter_foreach_node(net, n) {
		/* iterate over all exported inputs/outputs
		 * and params possibly creating export commands. */
		filterportdb_foreach_port(filter_portdb(net), portd) {
			/* check, if exported output is "our" one */
			if (strcmp(n->name, filterport_get_property(portd, FILTERPORT_MAP_NODE)) != 0
			    || !filterportdb_get_port(filter_portdb(n), filterport_get_property(portd, FILTERPORT_MAP_LABEL)))
				continue;
			len += sprintf(&buf[len], "   (filternetwork_add_%s net %s \"%s\" \"%s\" \"%s\")\n",
				       filterport_is_input(portd) ? "input" : "output",
				       n->name, filterport_get_property(portd, FILTERPORT_MAP_LABEL),
				       filterport_label(portd),
				       filterport_get_property(portd, FILTERPORT_DESCRIPTION));
			/* port parameters are magically exported
			 * too - they are set "directly" using the
			 * filterpipe_set_param() call - only querying
			 * those parameter descriptors is little bit
			 * messy ATM.
			 */
		}
		filterparamdb_foreach_param(filter_paramdb(net), param) {
			/* check, if exported param is "our" one */
			if (strcmp(n->name, filterparam_get_property(param, FILTERPARAM_MAP_NODE)) != 0
			    || !filterparamdb_get_param(filter_paramdb(n), filterparam_get_property(param, FILTERPARAM_MAP_LABEL)))
				continue;
			len += sprintf(&buf[len], "   (filternetwork_add_param net %s \"%s\" \"%s\" \"%s\")\n",
				       n->name, filterparam_get_property(param, FILTERPARAM_MAP_LABEL),
				       filterparam_label(param), filterparam_get_property(param, FILTERPARAM_DESCRIPTION));
		}
	}

	/* iterate over all connections and create connect
	 * commands. */
	filter_foreach_node(net, n) {
		filterportdb_foreach_port(filter_portdb(n), portd) {
		    if (filterport_is_input(portd))
			    continue;
		    filterport_foreach_pipe(portd, fpipe) {
			len += sprintf(&buf[len], "   (let ((pipe (filternetwork_add_connection %s \"%s\" %s \"%s\")))\n",
				       filterport_filter(filterpipe_source(fpipe))->name, filterport_label(filterpipe_source(fpipe)),
				       filterport_filter(filterpipe_dest(fpipe))->name, filterport_label(filterpipe_dest(fpipe)));

			/* iterate over all pipe dest parameters creating
			 * parameter set commands. */
			filterparamdb_foreach_param(filterpipe_destparamdb(fpipe), param) {
				val = filterparam_to_string(param);
				len += sprintf(&buf[len], "\t(filterpipe_set_destparam pipe \"%s\" %s)\n",
					       filterparam_label(param), val);
				free(val);
			}

			/* iterate over all pipe source parameters creating
			 * parameter set commands. */
			filterparamdb_foreach_param(filterpipe_sourceparamdb(fpipe), param) {
				val = filterparam_to_string(param);
				len += sprintf(&buf[len], "\t(filterpipe_set_sourceparam pipe \"%s\" %s)\n",
					       filterparam_label(param), val);
				free(val);
			}
			
			/* (let ((pipe... */
			len += sprintf(&buf[len-1], ")\n") - 1;
		    }
		}
	}

	/* (let* ... */
	len += sprintf(&buf[len], "   net)\n");

	return buf;
}
