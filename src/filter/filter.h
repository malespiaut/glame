#ifndef _FILTER_H
#define _FILTER_H

/*
 * filter.h
 * $Id: filter.h,v 1.80 2002/02/17 13:51:46 richi Exp $
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
 *
 * For additional Documentation see the texinfo documentation of glame,
 * especially the filter API and the filter tutorial part.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <pthread.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include "glame_types.h"
#include "glame_hash.h"
#include "list.h"
#include "atomic.h"
#include "glplugin.h"
#include "glsignal.h"
#include "filter_types.h"
#include "filter_param.h"
#include "filter_port.h"
#include "filter_pipe.h"
#include "filter_ops.h"



#ifdef __cplusplus
extern "C" {
#endif


#define GLAME_PLUGIN_VERSION 0x00070001


/********************************
 * Filter creating/using User API
 */

/* Signals sent out by the filter subsystem:
 * GLSIG_PARAM_CHANGED  - filter_param_t
 * GLSIG_PARAM_DELETED  - filter_param_t
 * GLSIG_PIPE_CHANGED   - filter_pipe_t
 * GLSIG_PIPE_DELETED   - filter_pipe_t
 * GLSIG_PORT_DELETED   - filter_port_t
 * GLSIG_FILTER_DELETED - filter_t
 * GLSIG_FILTER_CHANGED - filter_t
 */
#define GLSIG_PARAM_CHANGED 1
#define GLSIG_PARAM_DELETED 2
#define GLSIG_PIPE_CHANGED 4
#define GLSIG_PIPE_DELETED 8
#define GLSIG_FILTER_DELETED 16
#define GLSIG_FILTER_CHANGED 32
#define GLSIG_PORT_DELETED 64

#define STATE_UNDEFINED 0
#define STATE_INITIALIZED 1
#define STATE_LAUNCHED 2

#define FILTERTYPE_NODE 0
#define FILTERTYPE_NETWORK 1
#define FILTERTYPE_PLUGIN 2

struct filter {
	/* Whats this actually? Can be |ed together. */
	int type;

	/* connectivity in the net */
	filter_t *net;
        struct hash_head hash;
	struct glame_list_head list;
        const char *name;

	/* plugin that this filter hangs off */
	plugin_t *plugin;

	/* methods for filter setup/operation */
	int (*f)(filter_t *);
	int (*init)(filter_t *);

	void *priv;

	/* Port database. */
	filter_portdb_t ports;

	/* Parameter database. */
	filter_paramdb_t params;

	/* String database for storing random stuff. */
	gldb_t properties;

	/* signal emitter, known signals are
	 * GLSIG_NODE_DELETED */
	glsig_emitter_t emitter;

	/* public state & error string */
	int glerrno;
	const char *glerrstr;

	/* filter node operations for launching, etc. */
	struct filter_operations *ops;

	/* state maintained by launched filters */
	int state;
	pthread_t thread;
	struct glame_list_head buffers;

	/* stuff used if (type & FILTERTYPE_NETWORK) */
	int nr_nodes;
	struct glame_list_head nodes;
	struct glame_list_head connections;
	filter_launchcontext_t *launch_context;
};

/* Macros for publically accessing parts of the filter_t structure.
 * const char *filter_name(filter_t *);
 * filter_paramdb_t *filter_paramdb(filter_t *);
 * filter_portdb_t *filter_portdb(filter_t *);
 * glsig_emitter_t *filter_emitter(filter_t *);
 * gldb_t *filter_propertydb(filter_t *);
 * int filter_nrnodes(filter_t *);
 * int filter_errno(filter_t *);
 * const char *filter_errstr(filter_t *);
 */
#define filter_name(f) ((f)->name)
#define filter_paramdb(f) (&(f)->params)
#define filter_portdb(f) (&(f)->ports)
#define filter_emitter(f) (&(f)->emitter)
#define filter_propertydb(f) (&(f)->properties)
#define filter_nrnodes(f) ((f)->nr_nodes)
#define filter_errno(f) ((f)->glerrno)
#define filter_errstr(f) ((f)->glerrstr)
#define filter_bufsize(f) ((f)->launch_context->bufsize)

/* Convenience for querying/setting the error number/string
 * of filters.
 * int filter_has_error(filter_t *);
 * void filter_set_error(filter_t *, const char *);
 * void filter_clear_error(filter_t *);
 */
#define filter_has_error(f) ((f)->glerrno != 0)
#define filter_set_error(f, errstr) \
do { \
       (f)->glerrno = -1; \
       (f)->glerrstr = (errstr); \
} while (0)
#define filter_clear_error(f) \
do { \
        (f)->glerrno = 0; \
        (f)->glerrstr = NULL; \
} while (0)

/* Filter state/type query.
 */
#define FILTER_IS_PLUGIN(f) ((f)->type & FILTERTYPE_PLUGIN)
#define FILTER_IS_PART_OF_NETWORK(f) ((f)->net != NULL)
#define FILTER_IS_NETWORK(f) ((f)->type & FILTERTYPE_NETWORK)
#define FILTER_IS_NODE(f) (!((f)->type & FILTERTYPE_NETWORK))

#define FILTER_IS_LAUNCHED(f) ((f)->state >= STATE_LAUNCHED)

/* Convenience for setting/querying filter properties.
 * int filter_set_property(filter_t *f, const char *label, const char *value);
 * const char *filter_get_property(filter_t *f, const char *label);
 */
#define filter_set_property(f, l, v) glsdb_set(filter_propertydb(f), (v), (l))
#define filter_get_property(f, l) glsdb_query(filter_propertydb(f), (l))


/* Allocates a new filter structure. You can provide a
 * template to clone from. Returns a new filter_t on
 * success, NULL on error. */
filter_t *filter_creat(filter_t *template);

/* Creates a new filter for use by cloning an existing one
 * specified by a plugin. */
filter_t *filter_instantiate(plugin_t *p);

/* Deletes the specified filter. */
void filter_delete(filter_t *f);


/* Associates the given filter with the plugin. No further
 * modification of this instance is possible after this.
 * Returns 0 on success and -1 on error. */
int filter_register(filter_t *, plugin_t *);


/* FIXME: rename _node to _child? provide _get_parent? */

/* Adds a new instance a filter to the filter network.
 * Returns 0 on success, -1 on error. */
int filter_add_node(filter_t *net, filter_t *node, const char *name);

/* Removes a previously added node from a network thereby deleting
 * existing connections to/from it. Returns 0 on success, -1 on error. */
int filter_remove(filter_t *node);

/* Expands a network into its parent. Returns 0 on success, -1 on error. */
int filter_expand(filter_t *net);

/* Collapses a part of a network (NULL terminated array of filters in
 * nodes) to a subnet which is created, placed and returned.
 * Returns NULL on error, the created subnet on success. */
filter_t *filter_collapse(const char *name, filter_t **nodes);

/* Browse/find a filter node hanging off another filter.
 * filter_foreach_node(filter_t *parent, filter_t *f) {}
 * filter_t *filter_get_node(filter_t *net, const char *name);
 */
#define filter_foreach_node(net, node) glame_list_foreach(&(net)->nodes, filter_t, \
        list, node)
#define filter_get_node(nt, n) __hash_entry(_hash_find((n), (nt), \
        _hash((n), (nt)), __hash_pos(filter_t, hash, name, net)), \
        filter_t, hash)


/* Launches a set of connected filter instances. Does not start
 * processing of the data. Processing will be done with the
 * provided preferred buffersize. */
int filter_launch(filter_t *net, int bufsize);

/* Starts or restarts processing of the data. */
int filter_start(filter_t *net);

/* Suspends a running network. Restart via filter_start(). */
int filter_pause(filter_t *net);

/* Waits for the launched network to finish processing.
 * Returns 0 on successful completion or -1 on error
 * (in waiting or processing). */
int filter_wait(filter_t *net);

/* Queries the network if it is ready processing. This function
 * does not block. Returns 1, if the network is ready, 0 if it
 * is still operating or -1 on error (NULL net). */
int filter_is_ready(filter_t *net);

/* Kills a launched network aborting all processing. */
void filter_terminate(filter_t *net);


/* Filternetwork to scheme code. */
char *filter_to_string(filter_t *net);



/* Macros to fence initialisation from main work and cleanup section
 * inside filters ->f() method.
 * Basic structure of the ->f() method and order of the following
 * macros should be: */
#if 0
int f(filter_t *n)
{
	/* init with error checks like */
	if (/* error? */)
		FILTER_ERROR_{RETURN|CLEANUP}("bla");
	if (/* error? */) {
		filter_set_error("bla");
		/* local cleanup */
		FILTER_DO_CLEANUP;
	}
	/* everything is set up now */
	FILTER_AFTER_INIT;
	do {
		FILTER_CHECK_STOP;
		/* work */
	} while (/* condition */);
	FILTER_BEFORE_STOPCLEANUP;
	/* cleanup in case of goto from FILTER_CHECK_STOP */
	FILTER_BEFORE_CLEANUP;
	/* cleanup */
	FILTER_RETURN;
}
#endif
/* The following use the (internal!) functions */
int filter_after_init_hook(filter_t *f);
int filter_check_stop_hook(filter_t *f);
#define FILTER_AFTER_INIT \
do { \
	if (filter_after_init_hook(n) != 0) \
		goto _glame_filter_cleanup; \
	DPRINTF("%s is ready now.\n", n->name); \
} while (0)

#define FILTER_ERROR_RETURN(msg) \
do { \
        filter_set_error(n, msg); \
        return -1; \
} while (0)

#define FILTER_ERROR_CLEANUP(msg) \
do { \
        filter_set_error(n, msg); \
        goto _glame_filter_cleanup; \
} while (0) 

#define FILTER_ERROR_STOP(msg) \
do { \
        filter_set_error(n, msg); \
        goto _glame_filter_stopcleanup; \
} while (0) 

#define FILTER_DO_CLEANUP goto _glame_filter_cleanup

#define FILTER_CHECK_STOP \
do { \
	if (filter_check_stop_hook(n) != 0) \
		goto _glame_filter_stopcleanup; \
        pthread_testcancel(); \
} while (0)

#define FILTER_BEFORE_STOPCLEANUP _glame_filter_stopcleanup:

#define FILTER_BEFORE_CLEANUP _glame_filter_cleanup:

#define FILTER_RETURN return n->glerrno


/* Basic filter protocol - filter_buffer_t fbuf protocol.
 */
#include "filter_buffer.h"

/* Additional filter protocols (aka fbuf_*, sbuf_*, etc.)
 * included here for convenience.
 */
#include "filter_protocols.h"

/* Additional filter tools (aka *_feedback()) included here
 * for convenience.
 */
#include "filter_tools.h"

/* HACK - FIXME */
#define GLAME_WBUFSIZE (n->launch_context->bufsize)


#ifdef __cplusplus
}
#endif

#endif
