/*
 * filter.c
 * $Id: filter.c,v 1.64 2002/08/12 12:54:04 richi Exp $
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
#include "filter.h"
#include "filter_ops.h"


/* Global buffer size hint - can be runtime configured. */
int _GLAME_WBUFSIZE = 1024;


/* helper to create automagically unique node names. */
#define hash_unique_name_node(prefix, nt) _hash_unique_name((prefix), (nt), \
        __hash_pos(filter_t, hash, name, net))

/* filter sub-nodes hash/list addition/removal. */
#define hash_add_node(node) _hash_add(&(node)->hash, _hash((node)->name, \
        (node)->net))
#define glame_list_add_node(node) glame_list_add(&(node)->list, &(node)->net->nodes);
#define hash_remove_node(node) _hash_remove(&(node)->hash)
#define glame_list_remove_node(node) glame_list_del_init(&(node)->list)



/* Helpers for mm of filter_t.
 */

/* "iterator" for filter sub-nodes deletion. */
#define filter_first_node(net) glame_list_gethead(&(net)->nodes, \
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
	GLAME_INIT_LIST_HEAD(&f->list);
	f->name = NULL;

	f->plugin = NULL;
	f->f = NULL;
	f->init = NULL;
	filterportdb_init(&f->ports, f);

	f->priv = NULL;

	f->glerrno = 0;
	f->glerrstr = NULL;

	INIT_GLSIG_EMITTER(&f->emitter);

	f->ops = NULL;

	filterparamdb_init(&f->params, f);

	glsdb_init(&f->properties);

	f->state = STATE_UNDEFINED;
	GLAME_INIT_LIST_HEAD(&f->buffers);

	f->nr_nodes = 0;
	GLAME_INIT_LIST_HEAD(&f->nodes);
	GLAME_INIT_LIST_HEAD(&f->connections);
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
		glame_list_remove_node(n);
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
	filter_pipe_t *p;
	filter_param_t *param;
	filter_pipe_t *c;

	/* allocate new structure. */
	if (!(n = _filter_alloc()))
		return NULL;

	/* copy all the stuff. */
	n->type = f->type & ~FILTERTYPE_PLUGIN;
	n->name = NULL;
	n->f = f->f;
	n->init = f->init;
	
	n->plugin = f->plugin;
	n->priv = f->priv;

	n->ops = f->ops;

	glsig_copy_handlers(&n->emitter, &f->emitter);
	glsig_copy_redirectors(&n->emitter, &f->emitter);
	filterparamdb_copy(&n->params, &f->params);
	filterportdb_copy(&n->ports, &f->ports);
	glsdb_copy(&n->properties, &f->properties);

	/* copy nodes */
	filter_foreach_node(f, node) {
		filter_t *inode = _filter_instantiate(node);
		if (!inode || filter_add_node(n, inode, node->name) == -1)
			goto err;
		inode->plugin = node->plugin;		
	}

	/* and connections */
	/* second create the connections (loop through all outputs)
	 * and copy pipe parameters */
	filter_foreach_node(f, node) {
		glame_list_foreach(&node->connections, filter_pipe_t, list, c) {
			source = filter_get_node(n, filter_name(filterport_filter(c->real_source)));
			dest = filter_get_node(n, filter_name(filterport_filter(c->real_dest)));
			if (!(p = filterport_connect(filterportdb_get_port(filter_portdb(source), filterport_label(c->real_source)),
									   filterportdb_get_port(filter_portdb(dest), filterport_label(c->real_dest)))))
				goto err;
			filterparamdb_foreach_param(filterpipe_destparamdb(c), param) {
				filterparam_set(filterparamdb_get_param(filterpipe_destparamdb(p), filterparam_label(param)), filterparam_val(param));
			}
			filterparamdb_foreach_param(filterpipe_sourceparamdb(c), param) {
				filterparam_set(filterparamdb_get_param(filterpipe_sourceparamdb(p), filterparam_label(param)), filterparam_val(param));
			}
		}
	}

	/* Let the node init itself. */
	if (n->init && n->init(n) == -1)
		goto err;

	/* Re-set all parameters to correctly call the set_param methods. */
	filterparamdb_foreach_param(filter_paramdb(n), param)
		filterparam_set(param, filterparam_val(param));

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
	} else if (filter_nrnodes(f) > 0 && !f->f && !f->ops) {
		/* We are a partially initialized network! */
		f->type |= FILTERTYPE_NETWORK;
		f->ops = &filter_network_ops;
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
	if (!(f = _filter_instantiate(f)))
		return NULL;
	f->plugin = p;
	return f;
}


void filter_delete(filter_t *f)
{
	if (!f || FILTER_IS_PLUGIN(f) || FILTER_IS_LAUNCHED(f))
		return;

	if (FILTER_IS_PART_OF_NETWORK(f)) {
		glame_list_remove_node(f);
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
	if (!net || !node || !name
	    || FILTER_IS_PART_OF_NETWORK(node) || FILTER_IS_LAUNCHED(net))
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
	glame_list_add_node(node);
	net->nr_nodes++;
	_filter_fixup(net);

	return 0;
}

int filter_remove(filter_t *node)
{
	filter_port_t *port;
	filter_pipe_t *pipe;

	if (!node
	    || !FILTER_IS_PART_OF_NETWORK(node)
	    || FILTER_IS_LAUNCHED(node))
		return -1;

	filterportdb_foreach_port(filter_portdb(node), port) {
		filterport_foreach_pipe(port, pipe) {
			filterpipe_delete(pipe);
		}
	}

	node->net->nr_nodes--;
	glame_list_remove_node(node);
	hash_remove_node(node);

	return 0;
}

int filter_expand(filter_t *net)
{
	filter_t *node, *dummy;

	if (!net || !FILTER_IS_PART_OF_NETWORK(net)
	    || FILTER_IS_LAUNCHED(net))
		return -1;

	/* First fixup "real" connection lists. */
	filter_foreach_node(net, node) {
		filter_port_t *port;
		filter_pipe_t *pipe;
		filterportdb_foreach_port(filter_portdb(node), port) {
			char *map_node, *map_port;
			filter_port_t *real_port = port;
			while ((map_node = filterport_get_property(real_port, FILTERPORT_MAP_NODE))
			       && (map_port = filterport_get_property(real_port, FILTERPORT_MAP_LABEL))) {
				filter_t *mnode;
				filter_port_t *mport;
				if (!(mnode = filter_get_node(node, map_node)))
					break;
				if (!(mport = filterportdb_get_port(filter_portdb(mnode), map_port)))
					break;
				real_port = mport;
			}
			filterport_foreach_pipe(real_port, pipe) {
				if (filterport_filter(pipe->real_source) == net) {
					glame_list_del(&pipe->list);
					pipe->real_source = port;
					glame_list_add(&pipe->list, &node->connections);
				} else if (filterport_filter(pipe->real_dest) == net) {
					pipe->real_dest = port;
				} /* else ignore */
			}
		}
	}

	/* Then move all nodes up one level. */
	glame_list_safe_foreach(&(net)->nodes, filter_t, list, dummy, node) {
		/* Remove from old net. */
		node->net->nr_nodes--;
		glame_list_remove_node(node);
		hash_remove_node(node);

		/* Add to new net. */
		node->net = net->net;
		hash_add_node(node);
		glame_list_add_node(node);
		net->net->nr_nodes++;
	}

	return 0;
}

filter_t *filter_collapse(const char *name, filter_t **nodes)
{
	filter_t *net, *parent, **n, **nn;
	filter_port_t *port;
	filter_pipe_t *pipe;

	if (!name || !nodes || !(*nodes))
		return NULL;

	/* Whats the nodes (and new net) parent - is it the same? */
	parent = (*nodes)->net;
	n = nodes;
	while (*n) {
		if (FILTER_IS_LAUNCHED(*n))
			return NULL;
		if ((*n)->net != parent)
			return NULL;
		n++;
	}

	/* Create the new network, add it and specify wrapped
	 * inputs/outputs. */
	net = filter_creat(NULL);
	filter_add_node(parent, net, name);
	n = nodes;
	while (*n) {
		/* Loop through all connections on *n, checking
		 * if they go outwards of nodes, adding appropriate
		 * ports/redirections to net and fixing the pipe's
		 * real connection.
		 */

		/* Scan through ports of nodes exporting them, if
		 * necessary.
		 * -- FIXME: we dont scan pipes from outside to
		 *           inside an inside network.
		 */
		filterportdb_foreach_port(filter_portdb(*n), port) {
			filter_port_t *port3, *real_port;
			char nm[256], *map_node, *map_port;
			int outwards = 0;
			real_port = port;
			do {
				if ((map_node = filterport_get_property(real_port, FILTERPORT_MAP_NODE))
				    && (map_port = filterport_get_property(real_port, FILTERPORT_MAP_LABEL))) {
					filter_t *node;
					filter_port_t *nport;
					if ((node = filter_get_node(*n, map_node))
					    && (nport = filterportdb_get_port(filter_portdb(node), map_port)))
						real_port = nport;
					else
						DPRINTF("Wrong redirection\n");
				}
			} while (filterport_get_property(real_port, FILTERPORT_MAP_NODE));
			filterport_foreach_pipe(real_port, pipe) {
				filter_port_t *port2;
				if (!(pipe->real_source == port
				      || pipe->real_dest == port))
					continue;
				outwards = 1;
				nn = nodes;
				while (*nn) {
					filterportdb_foreach_port(filter_portdb(*nn), port2) {
						if (port == port2)
							continue;
						if (pipe->real_source == port2
						    || pipe->real_dest == port2)
							outwards = 0;
					}
					nn++;
				}
				if (outwards)
					break;
			}
			if (!outwards)
				continue;

			snprintf(nm, 256, "%s::%s", filter_name(*n), filterport_label(port));
			port3 = filterportdb_add_port(filter_portdb(net), nm,
						      filterport_type(port),
						      port->flags, FILTERPORT_END);
			filterport_redirect(port3, port);

			/* Again loop through the pipes, fixing the
			 * connection information.
			 */
			filterport_foreach_pipe(real_port, pipe) {
				filter_port_t *port2;
				if (!(pipe->real_source == port
				      || pipe->real_dest == port))
					continue;
				outwards = 1;
				nn = nodes;
				while (*nn) {
					filterportdb_foreach_port(filter_portdb(*nn), port2) {
						if (port == port2)
							continue;
						if (pipe->real_source == port2
						    || pipe->real_dest == port2)
							outwards = 0;
					}
					nn++;
				}
				if (outwards) {
					if (filterport_is_input(port)) {
						pipe->real_dest = port3;
					} else {
						glame_list_del(&pipe->list);
						pipe->real_source = port3;
						glame_list_add(&pipe->list, &net->connections);
					}
				}
			}
		}
		n++;
	}

	/* Move the nodes. */
	n = nodes;
	while (*n) {
		/* Remove from old net. */
		(*n)->net->nr_nodes--;
		glame_list_remove_node(*n);
		hash_remove_node(*n);

		/* Add to new net. */
		(*n)->net = net;
		hash_add_node(*n);
		glame_list_add_node(*n);
		net->nr_nodes++;

		n++;
	}
	_filter_fixup(net);

	/* FIXME - we need to alter export information of the
	 * parent network.
	 */

	return net;
}


/* I would like to have the following syntax:
 * (let* ((net (filter_creat))
 *        (nodename (filter_add_node net (plugin-get "filter") "nodename")))
 *    (filternode_set_param ....)
 *    (filternetwork_add_input ...)
 *    (filternetwork_add_output ...)
 *    (filternetwork_add_param ...)
 *    (filternode_set_param ....) ; wrapped ones
 *    (let ((pipe (filter_connect node "port" node "port)))
 *        (filterpipe_set_sourceparam ...)
 *        (filterpipe_set_destparam ...))
 *    net)
 */
char *filter_to_string(filter_t *net)
{
	char *buf, *tmp;
	int len;
	filter_t *n;
	filter_port_t *portd;
	filter_param_t *param;
	filter_pipe_t *c;

	if (!net || !FILTER_IS_NETWORK(net))
		return NULL;

	/* first alloc a big-enough buffer FIXME - br0ken. */
	if (!(buf = (char *)malloc(64*1024)))
		return NULL;
	len = 0;

	/* generate the network start part */
	len += sprintf(&buf[len], "(let* ((net (filter-new))\n");

	/* iterate over all nodes in the network creating
	 * node create commands. */
	filter_foreach_node(net, n) {
		if (!n->plugin) {
			char *subnet = filter_to_string(n);
			if (!subnet) {
				DPRINTF("got weird plugin\n");
				return NULL;
			}
			len += sprintf(&buf[len], "\t(%s (filter-add-node net %s \"%s\"))\n",
				       n->name, subnet, n->name);
			free(subnet);
		} else
			len += sprintf(&buf[len], "\t(%s (filter-add-node net (filter-new (plugin-get \"%s\")) \"%s\"))\n",
				       n->name, plugin_name(n->plugin), n->name);
	}
	/* ((net .. */
	len += sprintf(&buf[len-1], ")\n") - 1;

	/* first create the parameter and property set commands for the
	 * nodes. */
	filter_foreach_node(net, n) {
		/* Node params with param properties. */
		if (filterparamdb_nrparams(filter_paramdb(n)) > 0) {
			tmp = filterparamdb_to_string(filter_paramdb(n), 1);
			len += sprintf(&buf[len],
"   (%s (filter-params %s))\n",
			       tmp, n->name);
			free(tmp);
		}

		/* Node properties. */
		if (gldb_nritems(filter_propertydb(n)) > 0) {
			tmp = glsdb_to_list_of_pairs(filter_propertydb(n));
			len += sprintf(&buf[len],
"   (for-each\n"
"     (lambda (p)\n"
"       (set-property! %s (car p) (cdr p)))\n"
"     %s)\n",
			       n->name, tmp);
			free(tmp);
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
		glame_list_foreach(&n->connections, filter_pipe_t, list, c) {
			len += sprintf(&buf[len], "   (let ((pipe (filter-connect %s \"%s\" %s \"%s\")))\n",
				       filter_name(filterport_filter(c->real_source)),
				       filterport_label(c->real_source),
				       filter_name(filterport_filter(c->real_dest)),
				       filterport_label(c->real_dest));

			/* Pipe destination parameters and param properties. */
			if (filterparamdb_nrparams(filterpipe_destparamdb(c)) > 0) {
				tmp = filterparamdb_to_string(filterpipe_destparamdb(c), 1);
				len += sprintf(&buf[len],
				       "       (%s (pipe-dest-params pipe))\n",
				       tmp);
				free(tmp);
			}

			/* Pipe source parameters and param properties. */
			if (filterparamdb_nrparams(filterpipe_sourceparamdb(c)) > 0) {
				tmp = filterparamdb_to_string(filterpipe_sourceparamdb(c), 1);
				len += sprintf(&buf[len],
				       "       (%s (pipe-source-params pipe))\n",
				       tmp);
				free(tmp);
			}
			
			/* (let ((pipe... */
			len += sprintf(&buf[len], "\t#t)\n");
		}
	}

	/* Last, create property set commands for the network. */
	if (gldb_nritems(filter_propertydb(net)) > 0) {
		tmp = glsdb_to_list_of_pairs(filter_propertydb(net));
		len += sprintf(&buf[len],
"   (for-each\n"
"     (lambda (p)\n"
"       (set-property! net (car p) (cdr p)))\n"
"     %s)\n",
		       tmp);
		free(tmp);
	}

	/* (let* ... */
	len += sprintf(&buf[len], "   net)\n");

	return buf;
}

