/*
 * filter.c
 * $Id: filter.c,v 1.39 2000/12/08 14:56:44 richi Exp $
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
#include "filter_ops.h"



/* helper to create automagically unique node names. */
#define hash_unique_name_node(prefix, nt) _hash_unique_name((prefix), (nt), \
        __hash_pos(filter_t, hash, name, net))

/* filter sub-nodes hash/list addition/removal. */
#define hash_add_node(node) _hash_add(&(node)->hash, _hash((node)->name, \
        (node)->net))
#define list_add_node(node) list_add(&(node)->list, &(node)->net->nodes);
#define hash_remove_node(node) _hash_remove(&(node)->hash)
#define list_remove_node(node) list_del(&(node)->list)



/* Helpers for mm of filter_t.
 */

/* "iterator" for filter sub-nodes deletion. */
#define filter_first_node(net) list_gethead(&(net)->nodes, \
        filter_t, list)

/* Allocate pristine filternode/network. */
filter_t *_filter_alloc()
{
	filter_t *f;

	if (!(f = ALLOC(filter_t)))
		return NULL;

	f->type = 0;

	f->net = NULL;
	INIT_HASH_HEAD(&f->hash);
	INIT_LIST_HEAD(&f->list);
	f->name = NULL;

	f->plugin = NULL;
	f->f = NULL;
	f->init = NULL;
	f->connect_out = NULL;
	f->connect_in = NULL;
	f->set_param = NULL;
	filterportdb_init(&f->ports, f);

	f->priv = NULL;

	f->glerrno = 0;
	f->glerrstr = NULL;

	INIT_GLSIG_EMITTER(&f->emitter);

	f->ops = NULL;

	filterparamdb_init(&f->params, f);

	glsdb_init(&f->properties);

	f->state = STATE_UNDEFINED;
	INIT_LIST_HEAD(&f->buffers);

	f->nr_nodes = 0;
	INIT_LIST_HEAD(&f->nodes);
	f->launch_context = NULL;

	return f;
}

void _filter_free(filter_t *f)
{
	filter_t *n;

	if (!f)
		return;

	/* first signal deletion */
	glsig_emit(&f->emitter, GLSIG_FILTER_DELETED, f);

	while ((n = filter_first_node(f))) {
		hash_remove_node(n);
		list_remove_node(n);
		_filter_free(n);
	}

	filterportdb_delete(&f->ports);
	filterparamdb_delete(&f->params);
	glsdb_delete(&f->properties);

	glsig_delete_all(&f->emitter);
	free((char *)f->name);

	free(f);
}


filter_t *_filter_instantiate(filter_t *f)
{
	filter_t *n, *node, *source, *dest;
	filter_pipe_t *pipe, *p;
	filter_port_t *port;

	/* allocate new structure. */
	if (!(n = _filter_alloc()))
		return NULL;

	/* copy all the stuff. */
	n->type = f->type & ~FILTERTYPE_PLUGIN;
	n->name = NULL;
	n->f = f->f;
	n->init = f->init;
	n->connect_out = f->connect_out;
	n->connect_in = f->connect_in;
	n->set_param = f->set_param;

	n->priv = f->priv;

	n->ops = f->ops;

	glsig_copy_handlers(&n->emitter, &f->emitter);
	glsig_copy_redirectors(&n->emitter, &f->emitter);
	filterparamdb_copy(&n->params, &f->params);
	filterportdb_copy(&n->ports, &f->ports);
	glsdb_copy(&n->properties, &f->properties);

	/* copy nodes */
	filter_foreach_node(f, node) {
		if (filter_add_node(n, _filter_instantiate(node), node->name) == -1)
			goto err;
	}

	/* and connections */
	/* second create the connections (loop through all outputs)
	 * and copy pipe parameters */
	filter_foreach_node(f, node) {
	    filterportdb_foreach_port(filter_portdb(node), port) {
		if (!filterport_is_output(port))
			continue;
		filterport_foreach_pipe(port, pipe) {
			source = filter_get_node(n, filterport_filter(filterpipe_source(pipe))->name);
			dest = filter_get_node(n, filterport_filter(filterpipe_dest(pipe))->name);
			if (!(p = filterport_connect(filterportdb_get_port(filter_portdb(source), filterport_label(filterpipe_source(pipe))),
						     filterportdb_get_port(filter_portdb(dest), filterport_label(filterpipe_dest(pipe))))))
				goto err;
			filterparamdb_copy(filterpipe_sourceparamdb(p),
					   filterpipe_sourceparamdb(pipe));
			filterparamdb_copy(filterpipe_destparamdb(p),
					   filterpipe_destparamdb(pipe));
		}
	    }
	}

	if (n->init && n->init(n) == -1)
		goto err;

	return n;

 err:
	_filter_free(n);
	return NULL;
}

int _filter_fixup(filter_t *f)
{
	if (filter_nrnodes(f) == 0 && f->f && !f->ops) {
		/* We are a partially initialized node! */
		f->type |= FILTERTYPE_NODE;
		f->ops = &filter_node_ops;
		if (!f->connect_out)
			f->connect_out = filter_default_connect_out;
		if (!f->connect_in)
			f->connect_in = filter_default_connect_in;
		if (!f->set_param)
			f->set_param = filter_default_set_param;
	} else if (filter_nrnodes(f) > 0 && !f->f && !f->ops) {
		/* We are a partially initialized network! */
		f->type |= FILTERTYPE_NETWORK;
		f->ops = &filter_network_ops;
		f->connect_out = filter_network_connect_out;
		f->connect_in = filter_network_connect_in;
		f->set_param = filter_network_set_param;
	} else if (!f->ops)
		return -1;

	return 0;
}


/* filter API.
 */

filter_t *filter_creat(filter_t *template)
{
	if (template)
		return _filter_instantiate(template);
	return _filter_alloc();
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


int filter_register(filter_t *f, plugin_t *p)
{
	if (FILTER_IS_PLUGIN(f) || FILTER_IS_PART_OF_NETWORK(f))
		return -1;

	/* Try to fixate filter type, provide default methods
	 * and operations. */
	if (_filter_fixup(f) == -1)
		return -1;

	f->plugin = p;
	f->type |= FILTERTYPE_PLUGIN;
	if (FILTER_IS_NETWORK(f)) {
		// FIXME: recurse even more...
		filter_t *n;
		filter_foreach_node(f, n)
			n->type |= FILTERTYPE_PLUGIN;
	}
	plugin_set(p, PLUGIN_FILTER, f);

	return 0;
}


/* filter "network" API.
 */

int filter_add_node(filter_t *net, filter_t *node, const char *name)
{
	if (!net || !node || !name)
		return -1;
	node->net = net;
	if (filter_get_node(net, name) == NULL)
		node->name = strdup(name);
	else
		node->name = hash_unique_name_node(name, net);
	if (!node->name)
		return -1;
	_filter_fixup(node);
	hash_add_node(node);
	list_add_node(node);
	net->nr_nodes++;
	_filter_fixup(net);

	return 0;
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
	sitem_t *pitem;

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

	/* first create the parameter and property set commands for the
	 * nodes. */
	filter_foreach_node(net, n) {
		filterparamdb_foreach_param(filter_paramdb(n), param) {
			val = filterparam_to_string(param);
			len += sprintf(&buf[len], "   (filternode_set_param %s \"%s\" %s)\n",
				       n->name, filterparam_label(param), val);
			free(val);
		}
		glsdb_foreach_item(filter_propertydb(n), pitem) {
			len += sprintf(&buf[len], "   (filter_set_property %s \"%s\" \"%s\")\n",
				       n->name, sitem_label(pitem), sitem_str(pitem));
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

	/* Last, create property set commands for the network. */
	glsdb_foreach_item(filter_propertydb(net), pitem) {
		len += sprintf(&buf[len], "   (filter_set_property net \"%s\" \"%s\")\n",
			       sitem_label(pitem), sitem_str(pitem));
	}

	/* (let* ... */
	len += sprintf(&buf[len], "   net)\n");

	return buf;
}
