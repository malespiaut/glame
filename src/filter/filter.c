/*
 * filter.c
 * $Id: filter.c,v 1.53 2001/08/01 15:43:06 nold Exp $
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


/* Global buffer size hint - can be runtime configured. */
int GLAME_WBUFSIZE = 1024;


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
	INIT_LIST_HEAD(&f->connections);
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
	n->connect_out = f->connect_out;
	n->connect_in = f->connect_in;
	n->set_param = f->set_param;
	
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
		list_foreach(&node->connections, filter_pipe_t, list, c) {
			source = filter_get_node(n, c->source_filter);
			dest = filter_get_node(n, c->dest_filter);
			if (!(p = filterport_connect(filterportdb_get_port(filter_portdb(source), c->source_port),
						     filterportdb_get_port(filter_portdb(dest), c->dest_port))))
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
 * (let* ((net (filter_creat))
 *        (nodename (filter_add_node net (plugin_get "filter") "nodename")))
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
	char *buf, *val;
	int len;
	filter_t *n;
	filter_port_t *portd;
	filter_param_t *param;
	sitem_t *pitem;
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
			len += sprintf(&buf[len], "\t(%s (filter-add-node net (filter-new (plugin_get \"%s\")) \"%s\"))\n",
				       n->name, plugin_name(n->plugin), n->name);
	}
	/* ((net .. */
	len += sprintf(&buf[len-1], ")\n") - 1;

	/* first create the parameter and property set commands for the
	 * nodes. */
	filter_foreach_node(net, n) {
		filterparamdb_foreach_param(filter_paramdb(n), param) {
			val = filterparam_to_string(param);
			if (!val)
				continue;
			len += sprintf(&buf[len],
"   (let ((param (call-with-current-continuation\n"
"                  (lambda (return)\n"
"                    (map (lambda (param)\n"
"                           (if (string=? (param-label param) \"%s\")\n"
"	                        (return param)))\n"
"	                  (filter-params %s))))))\n"
"     (param-set! param %s)",
				       filterparam_label(param),
				       n->name, val);
			free(val);
			glsdb_foreach_item(filterparam_propertydb(param), pitem) {
				char *propval, *s, *d;
				propval = malloc(strlen(sitem_str(pitem))+256);
				s = sitem_str(pitem);
				d = propval;
				while (*s) {
					if (*s == '"')
						*d++ = '\\';
					*d++ = *s++;
				}
				*d = '\0';
				len += sprintf(&buf[len],
"\n     (set-property! param \"%s\" \"%s\")",
					       sitem_label(pitem),
					       propval);
				free(propval);
			}
			len += sprintf(&buf[len], ")\n");
		}
		glsdb_foreach_item(filter_propertydb(n), pitem) {
			char *propval, *s, *d;
			propval = malloc(strlen(sitem_str(pitem))+256);
			s = sitem_str(pitem);
			d = propval;
			while (*s) {
				if (*s == '"')
					*d++ = '\\';
				*d++ = *s++;
			}
			*d = '\0';
			len += sprintf(&buf[len], "   (set-property! %s \"%s\" \"%s\")\n",
				       n->name, sitem_label(pitem), propval);
			free(propval);
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
		list_foreach(&n->connections, filter_pipe_t, list, c) {
			len += sprintf(&buf[len], "   (let ((pipe (filter-connect %s \"%s\" %s \"%s\")))\n",
				       c->source_filter, c->source_port,
				       c->dest_filter, c->dest_port);

			/* iterate over all pipe dest parameters creating
			 * parameter set commands. */
			filterparamdb_foreach_param(filterpipe_destparamdb(c), param) {
				val = filterparam_to_string(param);
				if (!val)
					continue;
				len += sprintf(&buf[len], "\t(filterpipe_set_destparam pipe \"%s\" %s)\n",
					       filterparam_label(param), val);
				free(val);
			}

			/* iterate over all pipe source parameters creating
			 * parameter set commands. */
			filterparamdb_foreach_param(filterpipe_sourceparamdb(c), param) {
				val = filterparam_to_string(param);
				if (!val)
					continue;
				len += sprintf(&buf[len], "\t(filterpipe_set_sourceparam pipe \"%s\" %s)\n",
					       filterparam_label(param), val);
				free(val);
			}
			
			/* (let ((pipe... */
			len += sprintf(&buf[len], "\t#t)\n");
		}
	}

	/* Last, create property set commands for the network. */
	glsdb_foreach_item(filter_propertydb(net), pitem) {
		char *propval, *s, *d;
		propval = malloc(strlen(sitem_str(pitem))+256);
		s = sitem_str(pitem);
		d = propval;
		while (*s) {
			if (*s == '"')
				*d++ = '\\';
			*d++ = *s++;
		}
		*d = '\0';
		len += sprintf(&buf[len], "   (set-property! net \"%s\" \"%s\")\n",
			       sitem_label(pitem), propval);
		free(propval);
	}

	/* (let* ... */
	len += sprintf(&buf[len], "   net)\n");

	return buf;
}
