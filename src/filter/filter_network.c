/*
 * filter_network.c
 * $Id: filter_network.c,v 1.49 2000/05/19 09:27:34 richi Exp $
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
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <regex.h>
#include <errno.h>
#include "util.h"
#include "sem.h"
#include "filter.h"
#include "filter_methods.h"
#include "filter_mm.h"



/* filter network API.
 */

static void *waiter(void *network)
{
	filter_network_t *net = network;
	int res;

	res = net->node.ops->wait(FILTER_NODE(net));

	DPRINTF("starting cleanup\n");
	net->node.ops->postprocess(&net->node);
	_launchcontext_free(net->launch_context);
	net->launch_context = NULL;

	return (void *)res;
}

int filternetwork_launch(filter_network_t *net)
{
	/* sigset_t sigs; */

	if (!net || FILTERNETWORK_IS_LAUNCHED(net))
		return -1;

	/* block EPIPE
	sigemptyset(&sigs);
	sigaddset(&sigs, SIG_BLOCK);
	sigprocmask(SIG_BLOCK, &sigs, NULL); */

	/* init state */
	if (!(net->launch_context = _launchcontext_alloc()))
		return -1;

	DPRINTF("initting nodes\n");
	if (net->node.ops->init(FILTER_NODE(net)) == -1)
		goto out;

	DPRINTF("launching nodes\n");
	if (net->node.ops->launch(FILTER_NODE(net)) == -1)
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
	filternetwork_terminate(net);

	return -1;
}

int filternetwork_start(filter_network_t *net)
{
	if (!net || !FILTERNETWORK_IS_LAUNCHED(net)
	    || FILTERNETWORK_IS_RUNNING(net))
		return -1;

	sem_op(net->launch_context->semid, 0,
	       -net->launch_context->nr_threads);
	if (ATOMIC_VAL(net->launch_context->result) != 0)
		goto out;
	net->launch_context->state = STATE_RUNNING;

	return 0;

out:
	DPRINTF("Network error. Terminating.\n");
	filternetwork_terminate(net);
	return -1;
}

int filternetwork_pause(filter_network_t *net)
{
	if (!net || !FILTERNETWORK_IS_LAUNCHED(net)
	    || !FILTERNETWORK_IS_RUNNING(net))
		return -1;

	sem_op(net->launch_context->semid, 0,
	       +net->launch_context->nr_threads);
	net->launch_context->state = STATE_LAUNCHED;

	return 0;
}

int filternetwork_wait(filter_network_t *net)
{
	void *res;

	if (!net || !FILTERNETWORK_IS_LAUNCHED(net)
	    || !FILTERNETWORK_IS_RUNNING(net))
		return -1;

	DPRINTF("waiting for waiter to complete\n");
	pthread_join(net->launch_context->waiter, &res);

	return (int)res;
}

void filternetwork_terminate(filter_network_t *net)
{
	if (!net || !FILTERNETWORK_IS_LAUNCHED(net))
		return;

	atomic_set(&net->launch_context->result, 1);
	sem_zero(net->launch_context->semid, 0);
	net->launch_context->state = STATE_RUNNING;

	/* "safe" cleanup */
	filternetwork_wait(net);
}


filter_network_t *filternetwork_new()
{
	filter_network_t *net = NULL;
	filter_t *f = NULL;

	if (!(f = _filter_alloc(FILTER_FLAG_NETWORK)))
	        return NULL;
	if (!(net = FILTER_NETWORK(_filter_instantiate(f, "network"))))
		goto err;
	net->node.filter->private = net;

	return net;

 err:
	_filter_free(f);
	return NULL;
}

void filternetwork_delete(filter_network_t *net)
{
	filter_t *f;

	if (FILTERNETWORK_IS_LAUNCHED(net))
		return;

	f = net->node.filter;
	_network_free(net);
	if (!is_hashed_filter(f))
		_filter_free(f);
}

filter_node_t *filternetwork_add_node(filter_network_t *net,
				      const char *filter, const char *name)
{
	filter_t *f;
	plugin_t *p;
	filter_node_t *n;
	const char *nm = name;

	if (!net || !filter)
		return NULL;
	if (!(p = plugin_get(filter))) {
		DPRINTF("no plugin %s\n", filter);
		return NULL;
	}
	if (!(f = (filter_t *)plugin_query(p, PLUGIN_FILTER))) {
		DPRINTF("no filter entry for %s\n", filter);
		return NULL;
	}
	if (!nm)
		nm = hash_unique_name_node(plugin_name(p), net);
	if (!(n = _filter_instantiate(f, nm)))
		return NULL;

	/* Add node to networks node list and to the hash using name/net.
	 */
	n->net = net;
	hash_add_node(n);
	list_add_node(n);
	net->nr_nodes++;

	return n;
}

void filternetwork_delete_node(filter_node_t *node)
{
	if (FILTERNODE_IS_LAUNCHED(node))
		return;

	list_remove_node(node);
	hash_remove_node(node);
	node->net->nr_nodes--;
	_node_free(node);
}

filter_pipe_t *filternetwork_add_connection(filter_node_t *source, const char *source_port,
					    filter_node_t *dest, const char *dest_port)
{
	filter_portdesc_t *out, *in;
	filter_pipe_t *p;

	if (!source || !source_port || !dest || !dest_port
	    || source->net != dest->net)
		return NULL;
	if (FILTERNODE_IS_LAUNCHED(source))
		return NULL;

	/* are there ports with the specified names? */
	if (!(out = filter_get_outputdesc(source->filter, source_port))
	    || !(in = filter_get_inputdesc(dest->filter, dest_port)))
		return NULL;

	/* are there already connections to the ports and do
	 * they not have the AUTOMATIC flag set? */
	if (!(FILTER_PORT_IS_AUTOMATIC(out->type))
	    && filternode_get_output(source, out->label))
	        return NULL;
	if (!(FILTER_PORT_IS_AUTOMATIC(in->type))
	    && filternode_get_input(dest, in->label))
	        return NULL;

	if (!(p = _pipe_alloc(out, in)))
		return NULL;
	p->in_name = in->label;
	p->out_name = out->label;
	p->source = source;
	p->dest = dest;
	if (source->filter->connect_out(source, source_port, p) == -1)
		goto _err;

	/* do we support the requested pipe type? */
	if (!FILTER_PORT_IS_COMPATIBLE(in->type, p->type))
		goto _err;

	if (dest->filter->connect_in(dest, dest_port, p) == -1)
		goto _err;

	/* Now we have source & dest fixed - so we can finally
	 * init the parameter dbs and copy the parameters from
	 * the port descriptors. */
	filterpdb_init(&p->source_params, p->source);
	filterpdb_copy(&p->source_params, &out->params);
	filterpdb_init(&p->dest_params, p->dest);
	filterpdb_copy(&p->dest_params, &in->params);

	/* Also the signal redirector can be installed now. Note
	 * that GLSIG_PIPE_CHANGED is redirected to the destination node
	 * only! This simplifies signal handling a lot as it matches
	 * the semantics of the old fixup_pipe() method.
	 */
	glsig_add_redirector(&p->emitter, ~0, &p->dest->emitter);
	glsig_add_redirector(&p->emitter, ~GLSIG_PIPE_CHANGED, &p->source->emitter);

	/* add the pipe to all port lists/hashes.
	 * connect_out/in may have mucked with p->dest/source, so
	 * we have to use that instead of dest/source directly. */
	list_add_input(p, p->dest);
	hash_add_input(p, p->dest);
	list_add_output(p, p->source);
	hash_add_output(p, p->source);
	p->dest->nr_inputs++;
	p->source->nr_outputs++;

	/* signal pipe changes */
	glsig_emit(&p->emitter, GLSIG_PIPE_CHANGED, p);

	return p;

 _err:
	_pipe_free(p);
	return NULL;
}

void filternetwork_break_connection(filter_pipe_t *p)
{
	if (FILTERNODE_IS_LAUNCHED(p->source))
		return;

	/* disconnect the pipe */
	list_remove_input(p);
	hash_remove_input(p);
	p->source->nr_outputs--;
	list_remove_output(p);
	hash_remove_output(p);
	p->dest->nr_inputs--;

	/* kill the pipe */
	_pipe_free(p);
}



/* filternetwork filters / loading / saving
 * BIG FIXME!
 */

static struct filter_network_mapping *create_map(const char *label,
						 const char *node)
{
	struct filter_network_mapping *map;

	if (!(map = ALLOC(struct filter_network_mapping)))
		return NULL;
	map->label = label;
	map->node = node;

	return map;
}

filter_param_t *filternetwork_add_param(filter_network_t *net,
					const char *node, const char *param,
					const char *label, const char *desc)
{
	filter_node_t *n;
	filter_param_t *od, *d;

	if (!net || !node || !param || !label)
		return NULL;
	if (is_hashed_filter(net->node.filter))
		return NULL;
	if (!(n = filternetwork_get_node(net, node)))
		return NULL;
	if (!(od = filterpdb_get_param(filternode_pdb(n), param)))
		return NULL;
	/* Add param to template (filter_t) and network node. */
	if (!filterpdb_add_param(filter_pdb(net->node.filter),
				 label, od->type, &od->u,
				 FILTERPARAM_DESCRIPTION, desc,
				 FILTERPARAM_MAP_NODE, node,
				 FILTERPARAM_MAP_LABEL, param,
				 FILTERPARAM_END))
		return NULL;
	if (!(d = filterpdb_add_param(filternode_pdb(FILTER_NODE(net)),
				      label, od->type, &od->u,
				      FILTERPARAM_DESCRIPTION, desc,
				      FILTERPARAM_MAP_NODE, node,
				      FILTERPARAM_MAP_LABEL, param,
				      FILTERPARAM_END)))
		return NULL;

	return d;
}

filter_portdesc_t *filternetwork_add_input(filter_network_t *net,
		     const char *node, const char *port,
		     const char *label, const char *desc)
{
	filter_node_t *n;
	filter_portdesc_t *d;

	if (!net || !node || !port || !label)
		return NULL;
	if (is_hashed_filter(net->node.filter))
		return NULL;
	if (!(n = filternetwork_get_node(net, node)))
		return NULL;
	if (!(d = filter_get_inputdesc(n->filter, port)))
		return NULL;
	if (!(d = filter_add_input(net->node.filter, strdup(label),
				   strdup(desc), d->type)))
		return NULL;

	d->private = create_map(strdup(port), strdup(node));

	return d;
}

filter_portdesc_t *filternetwork_add_output(filter_network_t *net,
		      const char *node, const char *port,
		      const char *label, const char *desc)
{
	filter_node_t *n;
	filter_portdesc_t *d;

	if (!net || !node || !port || !label)
		return NULL;
	if (is_hashed_filter(net->node.filter))
		return NULL;
	if (!(n = filternetwork_get_node(net, node)))
		return NULL;
	if (!(d = filter_get_outputdesc(n->filter, port)))
		return NULL;
	if (!(d = filter_add_output(net->node.filter, strdup(label),
				    strdup(desc), d->type)))
		return NULL;

	d->private = create_map(strdup(port), strdup(node));

	return d;
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
char *filternetwork_to_string(filter_network_t *net)
{
	char *buf, *val;
	int len;
	filter_node_t *n;
	filter_portdesc_t *portd;
	filter_param_t *param;
	filter_pipe_t *fpipe;

	if (!net)
		return NULL;

	/* first alloc a big-enough buffer FIXME - br0ken. */
	if (!(buf = (char *)malloc(64*1024)))
		return NULL;
	len = 0;

	/* generate the network start part */
	len += sprintf(&buf[len], "(let *((net (filternetwork_new))\n");

	/* iterate over all nodes in the network creating
	 * node create commands. */
	filternetwork_foreach_node(net, n) {
		len += sprintf(&buf[len], "\t(%s (filternetwork_add_node net \"%s\" \"%s\"))\n",
			       n->name, plugin_name(n->filter->plugin), n->name);
	}
	/* ((net .. */
	len += sprintf(&buf[len-1], ")\n") - 1;

	/* first create the parameter set commands for the
	 * nodes. */
	filternetwork_foreach_node(net, n) {
		filterpdb_foreach_param(filternode_pdb(n), param) {
			val = filterparam_to_string(param);
			len += sprintf(&buf[len], "   (filternode_set_param %s \"%s\" %s)\n",
				       n->name, filterparam_label(param), val);
			free(val);
		}
	}

	/* create the port/parameter export commands */
	filternetwork_foreach_node(net, n) {
		/* iterate over all exported inputs/outputs
		 * and params possibly creating export commands. */
		filter_foreach_inputdesc(net->node.filter, portd) {
			/* check, if exported output is "our" one */
			if (strcmp(n->name, filterdesc_map_node(portd)) != 0
			    || !filter_get_inputdesc(n->filter, filterdesc_map_label(portd)))
				continue;
			len += sprintf(&buf[len], "   (filternetwork_add_input net %s \"%s\" \"%s\" \"%s\")\n",
				       n->name, filterdesc_map_label(portd),
				       portd->label, portd->description);
			/* port parameters are magically exported
			 * too - they are set "directly" using the
			 * filterpipe_set_param() call - only querying
			 * those parameter descriptors is little bit
			 * messy ATM.
			 */
		}
		filter_foreach_outputdesc(net->node.filter, portd) {
			/* check, if exported output is "our" one */
			if (strcmp(n->name, filterdesc_map_node(portd)) != 0
			    || !filter_get_outputdesc(n->filter, filterdesc_map_label(portd)))
				continue;
			len += sprintf(&buf[len], "   (filternetwork_add_output net %s \"%s\" \"%s\" \"%s\")\n",
				       n->name, filterdesc_map_label(portd),
				       portd->label, portd->description);
			/* port parameters - dto. */
		}
		filterpdb_foreach_param(filternode_pdb(&net->node), param) {
			/* check, if exported param is "our" one */
			if (strcmp(n->name, filterparam_get_property(param, FILTERPARAM_MAP_NODE)) != 0
			    || !filterpdb_get_param(filternode_pdb(n), filterparam_get_property(param, FILTERPARAM_MAP_LABEL)))
				continue;
			len += sprintf(&buf[len], "   (filternetwork_add_param net %s \"%s\" \"%s\" \"%s\")\n",
				       n->name, filterparam_get_property(param, FILTERPARAM_MAP_LABEL),
				       filterparam_label(param), filterparam_get_property(param, FILTERPARAM_DESCRIPTION));
		}
	}

	/* iterate over all connections and create connect
	 * commands. */
	filternetwork_foreach_node(net, n) {
		filternode_foreach_input(n, fpipe) {
			len += sprintf(&buf[len], "   (let ((pipe (filternetwork_add_connection %s \"%s\" %s \"%s\")))\n",
				       fpipe->source->name, fpipe->out_name,
				       fpipe->dest->name, fpipe->in_name);

			/* iterate over all pipe dest parameters creating
			 * parameter set commands. */
			filterpdb_foreach_param(filterpipe_destpdb(fpipe), param) {
				val = filterparam_to_string(param);
				len += sprintf(&buf[len], "\t(filterpipe_set_destparam pipe \"%s\" %s)\n",
					       filterparam_label(param), val);
				free(val);
			}

			/* iterate over all pipe source parameters creating
			 * parameter set commands. */
			filterpdb_foreach_param(filterpipe_sourcepdb(fpipe), param) {
				val = filterparam_to_string(param);
				len += sprintf(&buf[len], "\t(filterpipe_set_sourceparam pipe \"%s\" %s)\n",
					       filterparam_label(param), val);
				free(val);
			}
			
			/* (let ((pipe... */
			len += sprintf(&buf[len-1], ")\n") - 1;
		}
	}

	/* (let* ... */
	len += sprintf(&buf[len], "   net)\n");

	return buf;
}
