#ifndef _FILTER_H
#define _FILTER_H

/*
 * filter.h
 * $Id: filter.h,v 1.52 2000/05/02 07:46:36 richi Exp $
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
#include <math.h>
#include <errno.h>
#include "glame_types.h"
#include "glame_hash.h"
#include "list.h"
#include "atomic.h"
#include "sem.h"
#include "glplugin.h"
#include "glsignal.h"


struct filter;
typedef struct filter filter_t;
struct filter_node;
typedef struct filter_node filter_node_t;
struct filter_network;
typedef struct filter_network filter_network_t;

struct filter_portdesc;
typedef struct filter_portdesc filter_portdesc_t;
struct filter_pipe;
typedef struct filter_pipe filter_pipe_t;

struct filter_buffer;
typedef struct filter_buffer filter_buffer_t;


#include "filter_param.h"
#include "filterI.h"



/*******************************
 * Filter registry API
 */


/* Signals sent out by the filter subsystem:
 * GLSIG_PARAM_CHANGED  - filter_param_t
 * GLSIG_PARAM_DELETED  - filter_param_t
 * GLSIG_PIPE_CHANGED   - filter_pipe_t
 * GLSIG_PIPE_DELETED   - filter_pipe_t
 * GLSIG_NODE_DELETED   - filter_node_t
 */
#define GLSIG_PARAM_CHANGED 1
#define GLSIG_PARAM_DELETED 2
#define GLSIG_PIPE_CHANGED 4
#define GLSIG_PIPE_DELETED 8
#define GLSIG_NODE_DELETED 16


/* Filter contains the abstract description of a filter and
 * contains a set of methods doing the actual work.
 */
#define FILTER_FLAG_NETWORK 1
struct filter {
	plugin_t *plugin;

	int flags;

	int (*f)(filter_node_t *);

	int (*init)(filter_node_t *);

	int (*connect_out)(filter_node_t *source, const char *port,
			   filter_pipe_t *p);
	int (*connect_in)(filter_node_t *dest, const char *port,
			  filter_pipe_t *p);
	int (*set_param)(filter_node_t *n, filter_param_t *param,
			 const void *val);

	/* signal emitter. */
	glsig_emitter_t emitter;

	/* parameter database - default values. */
	filter_pdb_t params;

	/* input & output specification */
	struct list_head inputs;
	struct list_head outputs;

	void *private;
};
#define filter_nrparams(f) (gldb_nritems(&(f)->params))
#define filter_nrinputs(f) (list_count(&(f)->inputs))
#define filter_nroutputs(f) (list_count(&(f)->outputs))

/* Allocates a new filter structure. You have still to
 * fill it (apart from the mandatory f() method). */
filter_t *filter_alloc(int (*f)(filter_node_t *));

filter_t *filter_from_network(filter_network_t *net);

filter_portdesc_t *filter_add_input(filter_t *filter, const char *label,
				    const char *description, int type);
filter_portdesc_t *filter_add_output(filter_t *filter, const char *label,
				     const char *description, int type);

void filter_delete_port(filter_t *filter, filter_portdesc_t *port);

/* Filter port declaration. Type is a mask actually which
 * should contain any allowed pipe types.
 */
#define FILTER_PORTTYPE_AUTOMATIC 1
#define FILTER_PORTTYPE_SAMPLE    4
#define FILTER_PORTTYPE_RMS       8
#define FILTER_PORTTYPE_MIDI      16
#define FILTER_PORTTYPE_CONTROL   32
#define FILTER_PORTTYPE_MISC    128
#define FILTER_PORTTYPE_ANY     (4|8|16|32|128)
#define FILTER_PORT_IS_AUTOMATIC(type) ((type) & FILTER_PORTTYPE_AUTOMATIC)
#define FILTER_PORT_IS_COMPATIBLE(porttype, pipetype) (((porttype) & (pipetype)) == (pipetype))

/* Public access macros for the filter_portdesc_t structure.
 */
#define filterportdesc_label(pd) ((pd)->label)
#define filterportdesc_description(pd) ((pd)->description)
#define filterportdesc_type(pd) ((pd)->type)
#define filterportdesc_pdb(pd) (&(pd)->params)


/* Attach a filter to a plugin.
 */
void filter_attach(filter_t *, plugin_t *);


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

/* Browse/find a parameter description through the parameter database
 * you can find using
 * gldb_t *filter_pdb(filter_t *f); */
#define filter_pdb(f) (&(f)->params)



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
filter_network_t *filternetwork_new();

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
filter_param_t *filternetwork_add_param(filter_network_t *net,
					const char *node, const char *param,
					const char *label, const char *desc);

/* Delete wrappers to ports/parameters */
void filternetwork_delete_param(filter_network_t *net, const char *label);
void filternetwork_delete_port(filter_network_t *net, const char *label);


/* Filternetwork to scheme code. */
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

/* Status quering for filternodes
 * condition -- filternode_has_error(filter_node_t *n)
 * int filternode_errno(filter_node_t *n);
 * const char *filternode_errstr(filter_node_t *n); */
#define filternode_has_error(n) ((n)->glerrno != 0)
#define filternode_errstr(n) ((n)->glerrstr)

/* Status setting (from inside the filter methods)
 * void filternode_set_error(filter_node_t *n, int errno,
 *                           const char *errstr); */
#define filternode_set_error(n, erstr) \
do { \
       (n)->glerrno = -1; \
       (n)->glerrstr = (erstr); \
} while (0)

#define filternode_clear_error(n) \
do { \
        (n)->glerrno = 0; \
        (n)->glerrstr = NULL; \
} while (0)


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

/* Filternodes parameter query. Goes through the parameter database
 * which you can get using
 * gldb_t *filternode_pdb(filter_node_t *node); */
#define filternode_pdb(node) (&(node)->params)



/* Filter pipes represent a connection between two
 * instances of a filter. This is per filternode port
 * and depends on both filternode ports filter_portdesc.
 */
#define FILTER_PIPETYPE_SAMPLE  FILTER_PORTTYPE_SAMPLE
#define FILTER_PIPETYPE_RMS     FILTER_PORTTYPE_RMS
#define FILTER_PIPETYPE_MIDI    FILTER_PORTTYPE_MIDI
#define FILTER_PIPETYPE_CONTROL FILTER_PORTTYPE_CONTROL
#define FILTER_PIPETYPE_MISC    FILTER_PORTTYPE_MISC
#define FILTER_PIPE_IS_COMPATIBLE(pipetype, porttype) (((porttype) & (pipetype)) == (pipetype))

/* Common values for hangle value of a filter pipe
 */
#define FILTER_PIPEPOS_LEFT		-M_PI_2
#define FILTER_PIPEPOS_RIGHT		M_PI_2
#define	FILTER_PIPEPOS_CENTRE		0.0	/* Umm, needed at all? [dk] */
#define FILTER_PIPEPOS_DEFAULT		FILTER_PIPEPOS_CENTRE
#define FILTER_PIPEPOS_IS_LEFT(pos)	((pos)<0.0)
#define FILTER_PIPEPOS_IS_RIGHT(pos)	((pos)>0.0)
#define FILTER_PIPEPOS_IS_CENTRE(pos)	((pos)==0.0)
#define FILTER_SAMPLEPIPE_IS_LEFT(fp)	((fp)->u.sample.phi<0.0)
#define FILTER_SAMPLEPIPE_IS_RIGHT(fp)	((fp)->u.sample.phi>0.0)
#define FILTER_SAMPLEPIPE_IS_CENTRE(fp)	((fp)->u.sample.phi==0.0)
#define FILTER_SAMPLEPIPE_MORE_LEFT(fp1,fp2) ((fp1)->u.sample.phi<(fp2)->u.sample.phi)
#define FILTER_SAMPLEPIPE_MORE_RIGHT(fp1,fp2) ((fp1)->u.sample.phi>(fp2)->u.sample.phi)

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

#define filterpipe_sourcepdb(fp) (&(fp)->source_params)
#define filterpipe_destpdb(fp) (&(fp)->dest_params)


/* filter buffer stuff
 */
/* public access macros to the filter buffer fields.
 */
#ifndef DEBUG
#define fbuf_size(fb) ((fb)==NULL ? 0 : (fb)->size)
#define fbuf_buf(fb) (&(fb)->buf[0])
#else
static inline int fbuf_size(filter_buffer_t *fb)
{
	if (!fb)
		return 0;
	if (ATOMIC_VAL(fb->refcnt) == 0)
		DERROR("no buffer reference for fbuf_size");
	return fb->size;
}
static inline char *fbuf_buf(filter_buffer_t *fb)
{
	if (ATOMIC_VAL(fb->refcnt) == 0)
		DERROR("no buffer reference for fbuf_buf");
	return &fb->buf[0];
}
#endif

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

/* Tries to make the buffer private so you can read _and_ write.
 * Does not do it if it would require copying the buffer. Returns
 * NULL on failure. */
filter_buffer_t *fbuf_try_make_private(filter_buffer_t *fb);

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
        filternode_clear_error(n); \
	sem_op(n->net->launch_context->semid, 0, 1); \
	sem_op(n->net->launch_context->semid, 0, 0); \
	if (ATOMIC_VAL(n->net->launch_context->result) != 0) \
		goto _glame_filter_cleanup; \
} while (0)

#define FILTER_ERROR_RETURN(msg) \
do { \
        filternode_set_error(n, msg); \
        return -1; \
} while (0)

#define FILTER_ERROR_CLEANUP(msg) \
do { \
        filternode_set_error(n, msg); \
        goto _glame_filter_cleanup; \
} while (0) 

#define FILTER_DO_CLEANUP goto _glame_filter_cleanup

#define FILTER_CHECK_STOP \
do { \
        sem_op(n->net->launch_context->semid, 0, 0); \
	if (ATOMIC_VAL(n->net->launch_context->result) != 0) \
		goto _glame_filter_stopcleanup; \
        pthread_testcancel(); \
} while (0)

#define FILTER_BEFORE_STOPCLEANUP _glame_filter_stopcleanup:

#define FILTER_BEFORE_CLEANUP _glame_filter_cleanup:

#define FILTER_RETURN return n->glerrno


/* Additional filter protocols (aka fbuf_*, sbuf_*, etc.)
 * included here for convenience.
 */
#include "filter_protocols.h"

/* Additional filter tools (aka *_feedback()) included here
 * for convenience.
 */
#include "filter_tools.h"



/* FIXME! - temporarily only?
 * Wrappers to old filter API.
 */

#define filternode_get_param(n, l) filterpdb_get_param(filternode_pdb(n), l)
#define filterpipe_get_sourceparam(p, l) filterpdb_get_param(filterpipe_sourcepdb(p), l)
#define filterpipe_get_destparam(p, l) filterpdb_get_param(filterpipe_destpdb(p), l)


#endif
