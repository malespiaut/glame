#ifndef _FILTER_H
#define _FILTER_H

/*
 * filter.h
 * $Id: filter.h,v 1.25 2000/02/19 05:23:17 garrison Exp $
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
 *
 * You should only use the publically exploited macros/functions out
 * of this (filter.h) headerfile. Do not use any of the private stuff
 * in filterI.h directly though it is included here by reference!
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <pthread.h>
#include <stdlib.h>
#include <errno.h>
#include "glame_types.h"
#include "swapfile.h"
#include "glame_hash.h"
#include "list.h"
#include "atomic.h"
#include "sem.h"


struct filter;
typedef struct filter filter_t;
struct filter_node;
typedef struct filter_node filter_node_t;
struct filter_network;
typedef struct filter_network filter_network_t;

struct filter_paramdesc;
typedef struct filter_paramdesc filter_paramdesc_t;
struct filter_param;
typedef struct filter_param filter_param_t;

struct filter_portdesc;
typedef struct filter_portdesc filter_portdesc_t;
struct filter_pipe;
typedef struct filter_pipe filter_pipe_t;

struct filter_buffer;
typedef struct filter_buffer filter_buffer_t;


#include "filterI.h"




/*******************************
 * Filter registry API
 */

/* Filter contains the abstract description of a filter and
 * contains a set of methods doing the actual work.
 */
#define FILTER_FLAG_NETWORK 1
struct filter {
	struct list_head list;
	struct hash_head hash;
	void *namespace;

	const char *name;
	const char *description;

	int flags;

	int (*f)(filter_node_t *);

	int (*init)(filter_node_t *);
        void (*cleanup)(filter_node_t *);

	int (*connect_out)(filter_node_t *source, const char *port,
			   filter_pipe_t *p);
	int (*connect_in)(filter_node_t *dest, const char *port,
			  filter_pipe_t *p);

	int (*fixup_param)(filter_node_t *n, const char *name);
	int (*fixup_pipe)(filter_node_t *n, filter_pipe_t *in);
	void (*fixup_break_in)(filter_node_t *n, filter_pipe_t *in);
	void (*fixup_break_out)(filter_node_t *n, filter_pipe_t *out);

	/* parameter specification */
	struct list_head params;

	/* input & output specification */
	struct list_head inputs;
	struct list_head outputs;

	void *private;
};
#define filter_name(f) ((f)->name)
#define filter_description(f) ((f)->description)
#define filter_nrparams(f) (list_count((f)->params))
#define filter_nrinputs(f) (list_count((f)->inputs))
#define filter_nroutputs(f) (list_count((f)->outputs))

/* inits the filter subsystem */
int filter_init();

/* Allocates a new filter structure. You have still to
 * fill it.
 * Can return NULL, if the name is already occupied. */
filter_t *filter_alloc(const char *name, const char *description,
		       int (*f)(filter_node_t *));

filter_t *filter_from_string(const char *name, const char *description,
			     const char *f);

filter_portdesc_t *filter_add_input(filter_t *filter, const char *label,
				    const char *description, int type);
filter_portdesc_t *filter_add_output(filter_t *filter, const char *label,
				     const char *description, int type);
filter_paramdesc_t *filter_add_param(filter_t *filter, const char *label,
				     const char *description, int type);
filter_paramdesc_t *filterport_add_param(filter_portdesc_t *port,
					 const char *label,
					 const char *description, int type);

void filter_delete_port(filter_t *filter, filter_portdesc_t *port);
void filter_delete_param(filter_t *filter, filter_paramdesc_t *param);

/* Filter port declaration. Type is a mask actually which
 * should contain any allowed pipe types.
 */
#define FILTER_PORTTYPE_AUTOMATIC 1
#define FILTER_PORTTYPE_SAMPLE    4
#define FILTER_PORTTYPE_RMS       8
#define FILTER_PORTTYPE_MIDI      16
#define FILTER_PORTTYPE_MISC    128
#define FILTER_PORTTYPE_ANY      -4
#define FILTER_PORT_IS_AUTOMATIC(type) ((type) & FILTER_PORTTYPE_AUTOMATIC)
#define FILTER_PORT_IS_COMPATIBLE(porttype, pipetype) (((porttype) & (pipetype)) == (pipetype))

/* Public access macros for the filter_portdesc_t structure.
 */
#define filterportdesc_label(pd) ((pd)->label)
#define filterportdesc_description(pd) ((pd)->description)
#define filterportdesc_type(pd) ((pd)->type)
#define filterportdesc_nrparams(pd) (list_count((pd)->params))
#define filterportdesc_foreach_paramdesc(pd, d) \
        list_foreach(&(pd)->params, filter_paramdesc_t, list, d)
#define filterportdesc_get_paramdesc(pd, nm) \
        __hash_entry(_hash_find((nm), (pd), _hash((nm), (pd)), \
                     __hash_pos(filter_paramdesc_t, hash, label, namespace)), \
                     filter_paramdesc_t, hash)

/* Parameter declaration/instance types.
 * OUTPUT may be or'ed with the type to make the parameter
 * and output one.
 */
#define FILTER_PARAMTYPE_OUTPUT  1
#define FILTER_PARAMTYPE_INT     (1<<2)
#define FILTER_PARAMTYPE_FLOAT   (2<<2)
#define FILTER_PARAMTYPE_SAMPLE  (3<<2)
#define FILTER_PARAMTYPE_FILE    (4<<2)
#define FILTER_PARAMTYPE_STRING  (5<<2)
#define FILTER_PARAM_IS_OUTPUT(type) ((type) & FILTER_PARAMTYPE_OUTPUT)
#define FILTER_PARAMTYPE(type) ((type) & ~(FILTER_PARAMTYPE_OUTPUT))

/* Public access macros for filter_paramdesc_t - the parameter definition
 * which is stored per filter_t or per filter_portdesc_t.
 */
#define filterparamdesc_label(pd) ((pd)->label)
#define filterparamdesc_description(pd) ((pd)->description)
#define filterparamdesc_type(pd) ((pd)->type)


/* Adds the filter to the filter database. */
int filter_add(filter_t *filter);

/* Browse the list of registered filters. if f is NULL gives first
 * filter. */
filter_t *filter_next(filter_t *f);

/* Find a named filter.
 * filter_t *filter_get(const char *name); */
#define filter_get(n) \
	__hash_entry(_hash_find((n), FILTER_NAMESPACE, _hash((n), \
		     FILTER_NAMESPACE), __hash_pos(filter_t, hash, name, \
                     namespace)), filter_t, hash)

/* Browse/find a input/output port description.
 * filter_portdesc_t *filter_get_inputdesc(filter_t *f, const char *label);
 * filter_foreach_inputdesc(filter_t *f, filter_portdesc_t *d) { }
 * filter_portdesc_t *filter_get_outputdesc(filter_t *f, const char *label);
 * filter_foreach_outputdesc(filter_t *f, filter_portdesc_t *d) { } */
#define filter_get_inputdesc(f, n) \
	__hash_entry(_hash_find((n), &(f)->inputs, _hash((n), &(f)->inputs), \
		     __hash_pos(filter_portdesc_t, hash, label, namespace)), \
		     filter_portdesc_t, hash)
#define filter_foreach_inputdesc(f, d) \
	list_foreach(&(f)->inputs, filter_portdesc_t, list, d)
#define filter_get_outputdesc(f, n) \
	__hash_entry(_hash_find((n), &(f)->outputs, _hash((n), \
		     &(f)->outputs), __hash_pos(filter_portdesc_t, hash, \
		     label, namespace)), filter_portdesc_t, hash)
#define filter_foreach_outputdesc(f, d) \
	list_foreach(&(f)->outputs, filter_portdesc_t, list, d)

/* Browse/find a parameter description.
 * filter_paramdesc_t *filter_get_paramdesc(filter_t *f, filter_t *f);
 * filter_foreach_paramdesc(filter_t *f, filter_paramdesc_t *d) { } */
#define filter_get_paramdesc(f, n) \
	__hash_entry(_hash_find((n), (f), _hash((n), (f)), \
                     __hash_pos(filter_paramdesc_t, hash, label, namespace)), \
                     filter_paramdesc_t, hash)
#define filter_foreach_paramdesc(f, d) \
	list_foreach(&(f)->params, filter_paramdesc_t, list, d)



/*******************************
 * Filter use API
 * aka filter networks
 * aka filter nodes
 */

/* A filter network is a "filter" which
 * contains a set of connected filter instances.
 * Public access macros for filter_network_t
 */
#define filternetwork_nrnodes(net) ((net)->nr_nodes)

/* Browse/find a filter node in a filter network.
 * filternetwork_foreach_node(filter_network_t *net, filter_node_t *n) {}
 * filter_node_t *filternetwork_get_node(filter_network_t *net,
 *                                       const char *name); */
#define filternetwork_foreach_node(net, node) list_foreach(&(net)->nodes, filter_node_t, list, node)
#define filternetwork_get_node(nt, n) __hash_entry(_hash_find((n), (nt), \
        _hash((n), (nt)), __hash_pos(filter_node_t, hash, name, net)), \
        filter_node_t, hash)


/* Allocate a new filter network and initialize it.
 * Returns a filter network identifier or NULL on OOM. */
filter_network_t *filternetwork_new(const char *name);

/* Destroy a filter network. */
void filternetwork_delete(filter_network_t *net);


/* Adds a new instance a filter to the filter network.
 * Returns a filter node identifier or NULL on error. */
filter_node_t *filternetwork_add_node(filter_network_t *net,
				      const char *filter, const char *name);

/* Remove a filter node from a filter network and destroy it. */
void filternetwork_delete_node(filter_node_t *node);


/* Connects the two ports source_port and dest_port of the
 * filter nodes source and dest.
 * Returns -1 if that is not possible. */
filter_pipe_t *filternetwork_add_connection(filter_node_t *source,
					    const char *source_port,
					    filter_node_t *dest,
					    const char *dest_port);

/* Removes and destroys a connection from a filter network */
void filternetwork_break_connection(filter_pipe_t *p);


/* Launches a set of connected filter instances. Does not start
 * processing of the data. */
int filternetwork_launch(filter_network_t *net);

/* Starts or restarts processing of the data. */
int filternetwork_start(filter_network_t *net);

/* Suspends a running network. Restart via filternetwork_start(). */
int filternetwork_pause(filter_network_t *net);

/* Waits for the launched network to finish processing.
 * Returns 0 on successful completion or -1 on error
 * (in waiting or processing). */
int filternetwork_wait(filter_network_t *net);

/* Kills a launched network aborting all processing. */
void filternetwork_terminate(filter_network_t *net);


/* Do wrapping from internal network nodes to external visible
 * ports/parameters */
filter_portdesc_t *filternetwork_add_input(filter_network_t *net,
		     const char *node, const char *port,
		     const char *label, const char *desc);
filter_portdesc_t *filternetwork_add_output(filter_network_t *net,
		      const char *node, const char *port,
		      const char *label, const char *desc);
filter_paramdesc_t *filternetwork_add_param(filter_network_t *net,
		      const char *node, const char *param,
              	      const char *label, const char *desc);

/* Delete wrappers to ports/parameters */
void filternetwork_delete_param(filter_network_t *net, const char *label);
void filternetwork_delete_port(filter_network_t *net, const char *label);


/* Macro filters - conversion between strings and networks. */
filter_network_t *filternetwork_from_string(const char *str);
char *filternetwork_to_string(filter_network_t *net);


/* Filter node is an instance of a filter. A filter node
 * is associated with a filter network.
 * Public access macros for the filter_node_t.
 * const char *filternode_name(filter_node_t *n);
 * int filternode_nrinputs(filter_node_t *n);
 * int filternode_nroutputs(filter_node_t *n); */
#define filternode_name(n) ((n)->name)
#define filternode_nrinputs(n) ((n)->nr_inputs)
#define filternode_nroutputs(n) ((n)->nr_outputs)

/* Filternodes connection query/walk.
 * filter_pipe_t *filternode_get_input(filter_node_t *n, const char *label);
 * filter_pipe_t *filternode_next_input(filter_pipe_t *p);
 * filter_pipe_t *filternode_get_output(filter_node_t *n, const char *label);
 * filter_pipe_t *filternode_next_output(filter_pipe_t *p);
 * filternode_foreach_input(filter_node_t *n, filter_pipe_t *p) { }
 * filternode_foreach_output(filter_node_t *n, filter_pipe_t *p) { } */
#define filternode_get_input(node, n) \
	__hash_entry(_hash_find((n), &(node)->inputs, _hash((n), \
                     &(node)->inputs), __hash_pos(filter_pipe_t, \
                     input_hash, in_name, in_namespace)), filter_pipe_t, \
                     input_hash)
#define filternode_next_input(p) \
	__hash_entry(_hash_find((p)->in_name, (p)->in_namespace, \
                     &(p)->input_hash.next_hash, __hash_pos(filter_pipe_t, \
                     input_hash, in_name, in_namespace)), filter_pipe_t, \
                     input_hash)
#define filternode_get_output(node, n) \
	__hash_entry(_hash_find((n), &(node)->outputs, _hash((n), \
                     &(node)->outputs), __hash_pos(filter_pipe_t, \
                     output_hash, out_name, out_namespace)), filter_pipe_t, \
                     output_hash)
#define filternode_next_output(p) \
        __hash_entry(_hash_find((p)->out_name, (p)->out_namespace, \
                     &(p)->output_hash.next_hash, __hash_pos(filter_pipe_t, \
                     output_hash, out_name, out_namespace)), filter_pipe_t, \
                     output_hash)
#define filternode_foreach_input(n, p) \
	list_foreach(&(n)->inputs, filter_pipe_t, input_list, p)
#define filternode_foreach_output(n, p) \
	list_foreach(&(n)->outputs, filter_pipe_t, output_list, p)

/* Filternodes parameter query.
 * filter_param_t *hash_find_param(filter_node_t *n, const char *label); */
#define filternode_get_param(node, n) \
	__hash_entry(_hash_find((n), (node), _hash((n), (node)), \
		     __hash_pos(filter_param_t, hash, label, namespace)), \
                     filter_param_t, hash)


/* Set the parameter with label label to the value pointed to by
 * val.
 * Returns -1 if that is not possible.
 */
int filternode_set_param(filter_node_t *n, const char *label, void *val);


/* Public access macros for filter_param_t - the parameter instance which
 * is stored per filter_node_t or per filter_pipe_t.
 * Get the type using filterparamdesc_type() and the filter_paramdesc_t
 * structure.
 */
#define filterparam_val_int(fp) ((fp)->val.i)
#define filterparam_val_float(fp) ((fp)->val.f)
#define filterparam_val_file(fp) ((fp)->val.file)
#define filterparam_val_sample(fp) ((fp)->val.sample)
#define filterparam_val_string(fp) ((fp)->val.string)

/* Parameter to/from string conversion routines.
 * Example use:
 *   param = filternode_get_param(node, "gain");
 *   str = filterparam_to_string(param);
 *   val = filterparamval_from_string(filter_get_paramdesc(f, "gain2), str);
 *   filternode_set_param(node2, "gain2", val);
 *   free(val);
 *   free(str);
 */
char *filterparam_to_string(filter_param_t *param);
void *filterparamval_from_string(filter_paramdesc_t *pdesc, const char *val);



/* Filter pipes represent a connection between two
 * instances of a filter. This is per filternode port
 * and depends on both filternode ports filter_portdesc.
 */
#define FILTER_PIPETYPE_SAMPLE  FILTER_PORTTYPE_SAMPLE
#define FILTER_PIPETYPE_RMS     FILTER_PORTTYPE_RMS
#define FILTER_PIPETYPE_MISC    FILTER_PORTTYPE_MISC
#define FILTER_PIPE_IS_COMPATIBLE(pipetype, porttype) (((porttype) & (pipetype)) == (pipetype))
#define FILTER_PIPETYPE_DEFAULT(porttype) ((porttype) & FILTER_PORTTYPE_SAMPLE ? FILTER_PIPETYPE_SAMPLE : ((porttype) & FILTER_PORTTYPE_RMS ? FILTER_PIPETYPE_RMS : FILTER_PIPETYPE_MISC))

/* Common values for hangle value of a filter pipe
 */

#define FILTER_PIPEPOS_LEFT		-M_PI_2
#define FILTER_PIPEPOS_RIGHT		M_PI_2
#define	FILTER_PIPEPOS_CENTRE		0.0	/* Umm, needed at all? [dk] */
#define FILTER_PIPEPOS_DEFAULT		FILTER_PIPEPOS_CENTRE

/* Public access macros for the filter_pipe_t structure.
 */
#define filterpipe_type(fp) ((fp)->type)

#define filterpipe_settype_sample(fp, freq, hangle) do { \
	(fp)->type = FILTER_PIPETYPE_SAMPLE; \
	(fp)->u.sample.rate = (freq); \
	(fp)->u.sample.phi = (hangle); \
} while (0)
#define filterpipe_sample_rate(fp) ((fp)->u.sample.rate)
#define filterpipe_sample_hangle(fp) ((fp)->u.sample.phi)

#define filterpipe_get_sourceparam(fp, n) \
	__hash_entry(_hash_find((n), &(fp)->source_params, _hash((n), \
                     &(fp)->source_params), \
		     __hash_pos(filter_param_t, hash, label, namespace)), \
                     filter_param_t, hash)
#define filterpipe_get_destparam(fp, n) \
	__hash_entry(_hash_find((n), &(fp)->dest_params, _hash((n), \
                     &(fp)->dest_params), \
		     __hash_pos(filter_param_t, hash, label, namespace)), \
                     filter_param_t, hash)

/* Set the source/destination end per-pipe parameter with label label to
 * the value pointed to by val.
 * Returns -1 if that is not possible.
 */
int filterpipe_set_sourceparam(filter_pipe_t *p, const char *label, void *val);
int filterpipe_set_destparam(filter_pipe_t *p, const char *label, void *val);



/* filter buffer stuff
 */
/* public access macros to the filter buffer fields.
 */
#define fbuf_size(fb) ((fb)==NULL ? 0 : (fb)->size)
#define fbuf_buf(fb) (&(fb)->buf[0])

/* fbuf_alloc creates a filter buffer with backing storage for size
 * bytes. The buffer is initially one time referenced.
 * If supplied the buffer is linked into the list (and removed from it
 * at free time) */
filter_buffer_t *fbuf_alloc(int size, struct list_head *list);

/* Get one extra reference (read-only!) of the buffer.
 * If the number of references drops to zero at any point,
 * the buffer may be freed without notice! */
void fbuf_ref(filter_buffer_t *fb);

/* Release one reference of the buffer. */
void fbuf_unref(filter_buffer_t *fb);

/* Make the buffer private so you can read _and_ write.
 * This tries to get exclusive access to the buffer either by
 * copying it or by just doing nothing. */
filter_buffer_t *fbuf_make_private(filter_buffer_t *fb);

/* Get (blocking) the next buffer from the input stream. */
filter_buffer_t *fbuf_get(filter_pipe_t *p);

/* Queue (blocking!) the buffer to the output stream. */
void fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf);



/* Macros to fence initialisation from main work and cleanup section
 * inside filters ->f() method.
 */
#define FILTER_AFTER_INIT \
do { \
	sem_op(n->net->launch_context->semid, 0, 1); \
	sem_op(n->net->launch_context->semid, 0, 0); \
	if (ATOMIC_VAL(n->net->launch_context->result) != 0) \
		goto _glame_filter_cleanup; \
} while (0);

#define FILTER_CHECK_STOP \
do { \
        sem_op(n->net->launch_context->semid, 0, 0); \
	if (ATOMIC_VAL(n->net->launch_context->result) != 0) \
		goto _glame_filter_stopcleanup; \
        pthread_testcancel(); \
} while (0);

#define FILTER_BEFORE_STOPCLEANUP _glame_filter_stopcleanup:

#define FILTER_BEFORE_CLEANUP _glame_filter_cleanup:


/* Additional filter protocols (aka fbuf_*, sbuf_*, etc.)
 * included here for convenience.
 */
#include "filter_protocols.h"

/* Additional filter tools (aka *_feedback()) included here
 * for convenience.
 */
#include "filter_tools.h"


#endif
