#ifndef _FILTER_H
#define _FILTER_H

/*
 * filter.h
 * $Id: filter.h,v 1.62 2000/10/28 13:45:48 richi Exp $
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
#include "glame_sem.h"
#include "glplugin.h"
#include "glsignal.h"


struct filter;
typedef struct filter filter_t;

struct filter_pipe;
typedef struct filter_pipe filter_pipe_t;

struct filter_buffer;
typedef struct filter_buffer filter_buffer_t;


#include "filter_param.h"
#include "filter_port.h"
#include "filterI.h"



#ifdef __cplusplus
extern "C" {
#endif

/*******************************
 * Filter registry API
 */

#define STATE_UNDEFINED 0
#define STATE_INITIALIZED 1
#define STATE_LAUNCHED 2
#define STATE_RUNNING 3
struct filter {
	/* Whats this actually? Can be |ed together. */
#define FILTERTYPE_NODE 0
#define FILTERTYPE_NETWORK 1
#define FILTERTYPE_PLUGIN 2
	int type;

	/* connectivity in the net - FIXME: rename to parent */
	filter_t *net;
        struct hash_head hash;
	struct list_head list;
        const char *name;

	/* Filter of which this filter_node is an instance,
	 * together with a private pointer that can be filled
	 * in using the filters init() method. */

	/********************** Work in progess start: filter_t */
	plugin_t *plugin;

	int (*f)(filter_t *);

	int (*init)(filter_t *);

	int (*connect_out)(filter_t *source, filter_port_t *port,
			   filter_pipe_t *p);
	int (*connect_in)(filter_t *dest, filter_port_t *port,
			  filter_pipe_t *p);
	int (*set_param)(filter_t *n, filter_param_t *param,
			 const void *val);

	/* input & output specification */
	filter_portdb_t ports;
	/* work in progess end. *****************************/

	void *priv;

	/* public state & error string */
	int glerrno;
	const char *glerrstr;

	/* signal emitter, known signals are
	 * GLSIG_NODE_DELETED */
	glsig_emitter_t emitter;

	/* filter node operations */
	struct filter_operations *ops;

	/* Parameter database. */
	filter_paramdb_t params;

	/* private - used by launch_filter_network & friends */
	int state;
	pthread_t thread;
	struct list_head buffers;

	/******************** Work in progress: filter_network_t */
	/* Stuff used if (type & FILTERTYPE_NETWORK) only. */
	int nr_nodes;
	struct list_head nodes;
	filter_launchcontext_t *launch_context;
};


/* Signals sent out by the filter subsystem:
 * GLSIG_PARAM_CHANGED  - filter_param_t
 * GLSIG_PARAM_DELETED  - filter_param_t
 * GLSIG_PIPE_CHANGED   - filter_pipe_t
 * GLSIG_PIPE_DELETED   - filter_pipe_t
 * GLSIG_FILTER_DELETED - filter_t
 */
#define GLSIG_PARAM_CHANGED 1
#define GLSIG_PARAM_DELETED 2
#define GLSIG_PIPE_CHANGED 4
#define GLSIG_PIPE_DELETED 8
#define GLSIG_FILTER_DELETED 16
// FIXME: use it on port/param addition to a filter
#define GLSIG_FILTER_CHANGED 32

/* Allocates a new filter structure. You have still to
 * fill it (apart from the mandatory f() method). */
filter_t *filter_alloc_node(int (*func)(filter_t *));
filter_t *filter_alloc_network();
filter_t *filter_clone(filter_t *f);
filter_t *filter_instantiate(plugin_t *p);
void filter_delete(filter_t *f);

filter_port_t *filter_add_inputport(filter_t *filter, const char *label,
				    const char *description, int type);
filter_port_t *filter_add_outputport(filter_t *filter, const char *label,
				     const char *description, int type);

void filter_delete_port(filter_t *filter, filter_port_t *port);


/* Attach a filter to a plugin.
 */
void filter_register(filter_t *, plugin_t *);

/* Browse/find a parameter description through the parameter database
 * you can find using
 * gldb_t *filter_paramdb(filter_t *f); */
#define filter_paramdb(f) (&(f)->params)

#define filter_portdb(f) (&(f)->ports)
#define filter_emitter(f) (&(f)->emitter)


/*******************************
 * Filter use API
 * aka filter networks
 * aka filter nodes
 */

/* A filter network is a "filter" which
 * contains a set of connected filter instances.
 * Public access macros for filter_network_t
 */
#define filter_nrnodes(net) ((net)->nr_nodes)

/* Browse/find a filter node in a filter network.
 * filter_foreach_node(filter_t *net, filter_t *n) {}
 * filter_t *filter_get_node(filter_t *net,
 *                                       const char *name); */
#define filter_foreach_node(net, node) list_foreach(&(net)->nodes, filter_t, list, node)
#define filter_get_node(nt, n) __hash_entry(_hash_find((n), (nt), \
        _hash((n), (nt)), __hash_pos(filter_t, hash, name, net)), \
        filter_t, hash)


/* Adds a new instance a filter to the filter network.
 * Returns a filter node identifier or NULL on error. */
int filter_add_node(filter_t *net, filter_t *node, const char *name);


/* Removes and destroys a connection from a filter network */
void filternetwork_break_connection(filter_pipe_t *p);


/* Launches a set of connected filter instances. Does not start
 * processing of the data. */
int filter_launch(filter_t *net);

/* Starts or restarts processing of the data. */
int filter_start(filter_t *net);

/* Suspends a running network. Restart via filter_start(). */
int filter_pause(filter_t *net);

/* Waits for the launched network to finish processing.
 * Returns 0 on successful completion or -1 on error
 * (in waiting or processing). */
int filter_wait(filter_t *net);

/* Kills a launched network aborting all processing. */
void filter_terminate(filter_t *net);


/* FIXME!!! */
/* Do wrapping from internal network nodes to external visible
 * ports/parameters */
filter_port_t *filternetwork_add_input(filter_t *net,
		     const char *node, const char *port,
		     const char *label, const char *desc);
filter_port_t *filternetwork_add_output(filter_t *net,
		      const char *node, const char *port,
		      const char *label, const char *desc);
filter_param_t *filternetwork_add_param(filter_t *net,
					const char *node, const char *param,
					const char *label, const char *desc);
/* Delete wrappers to ports/parameters */
void filternetwork_delete_port(filter_t *net, filter_port_t *port);


/* Filternetwork to scheme code. */
char *filter_to_string(filter_t *net);


/* Filter node is an instance of a filter. A filter node
 * is associated with a filter network.
 * Public access macros for the filter_t.
 * const char *filter_name(filter_t *n); */
#define filter_name(n) ((n)->name)

/* Status quering for filternodes
 * condition -- filter_has_error(filter_t *n)
 * int filter_errno(filter_t *n);
 * const char *filter_errstr(filter_t *n); */
#define filter_has_error(n) ((n)->glerrno != 0)
#define filter_errstr(n) ((n)->glerrstr)

/* Status setting (from inside the filter methods)
 * void filter_set_error(filter_t *n, int errno,
 *                           const char *errstr); */
#define filter_set_error(n, erstr) \
do { \
       (n)->glerrno = -1; \
       (n)->glerrstr = (erstr); \
} while (0)

#define filter_clear_error(n) \
do { \
        (n)->glerrno = 0; \
        (n)->glerrstr = NULL; \
} while (0)



/* Filter pipes represent a connection between two
 * instances of a filter. This is per filternode port
 * and depends on both filternode ports filter_port.
 */
#define FILTER_PIPETYPE_UNDEFINED FILTER_PORTTYPE_ANY
#define FILTER_PIPETYPE_SAMPLE    FILTER_PORTTYPE_SAMPLE
#define FILTER_PIPETYPE_RMS       FILTER_PORTTYPE_RMS
#define FILTER_PIPETYPE_MIDI      FILTER_PORTTYPE_MIDI
#define FILTER_PIPETYPE_CONTROL   FILTER_PORTTYPE_CONTROL
#define FILTER_PIPETYPE_FFT       FILTER_PORTTYPE_FFT

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

struct filter_pipe {
	/* lists - source_list is the list of the source port,
	 * dest_list is the list of the destination port.
	 * Note that the destination ports are inputs of the
	 * associated filters and vice versa. */
	struct list_head source_list, dest_list;

	/* pipe context - source and destination ports. */
	filter_port_t *source;
	filter_port_t *dest;

	/* pipe specific parameters on the source/destination side */
	filter_paramdb_t source_params;
	filter_paramdb_t dest_params;

	/* fds used for communication */
	int source_fd, dest_fd;

	/* Signal emitter. Know signals are
	 * GLSIG_PIPE_CHANGED
	 * GLSIG_PIPE_DELETED */
	glsig_emitter_t emitter;

	/* data type specification */
	int type;
	union {
		struct {
			int rate; 	/* sample rate, [Hz] */
			float phi; 	/* polar coordinate, [rad] */
		} sample;
		struct{
			int rate;	/* sample rate, [Hz] */
			float phi;	/* polar coordinate, [rad] */
			int bsize;	/* size of single fft-block in half-complex format (see fftw) */
			int osamp;	/* oversampling factor */
		} fft;
		struct {
			int blocksize;
		} rms;
	        struct {
	                int dummy;
	        } midi;
	        struct {
                        int dummy;
	        } control;
	} u;	
};

/* Public access macros for the filter_pipe_t structure.
 */
#define filterpipe_type(fp) ((fp)->type)
#define filterpipe_emitter(fp) (&(fp)->emitter)
#define filterpipe_source(p) ((p)->source)
#define filterpipe_dest(p) ((p)->dest)

#define filterpipe_settype_sample(fp, freq, hangle) do { \
	(fp)->type = FILTER_PIPETYPE_SAMPLE; \
	(fp)->u.sample.rate = (freq); \
	(fp)->u.sample.phi = (hangle); \
} while (0)
#define filterpipe_sample_rate(fp) ((fp)->u.sample.rate)
#define filterpipe_sample_hangle(fp) ((fp)->u.sample.phi)

#define filterpipe_sourceparamdb(fp) (&(fp)->source_params)
#define filterpipe_destparamdb(fp) (&(fp)->dest_params)

#define filterpipe_settype_fft(fp, freq, hangle, bs, os) do { \
	(fp)->type = FILTER_PIPETYPE_FFT; \
	(fp)->u.fft.rate = (freq); \
	(fp)->u.fft.phi = (hangle); \
	(fp)->u.fft.bsize = (bs); \
	(fp)->u.fft.osamp = (os); \
} while (0)
#define filterpipe_fft_rate(fp) ((fp)->u.fft.rate)
#define filterpipe_fft_hangle(fp) ((fp)->u.fft.phi)
#define filterpipe_fft_bsize(fp) ((fp)->u.fft.bsize)
#define filterpipe_fft_osamp(fp) ((fp)->u.fft.osamp)



/* filter buffer stuff
 */
struct filter_buffer {
	struct list_head list;
        glame_atomic_t refcnt;
	int size;              /* size of buffer in bytes */
	char buf[1];
};
/* public access macros to the filter buffer fields. */
#ifndef DEBUG
#define fbuf_size(fb) ((fb)==NULL ? 0 : (fb)->size)
#define fbuf_buf(fb) ((fb)==NULL ? NULL : &(fb)->buf[0])
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
	if (!fb)
		return NULL;
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
        struct sembuf sop; \
	DPRINTF("%s seems ready for signalling\n", n->name); \
        filter_clear_error(n); \
        sop.sem_num = 0; \
        sop.sem_op = 1; \
        sop.sem_flg = IPC_NOWAIT; \
        glame_semop(n->net->launch_context->semid, &sop, 1); \
        sop.sem_num = 0; \
        sop.sem_op = 0; \
        sop.sem_flg = 0; \
        glame_semop(n->net->launch_context->semid, &sop, 1); \
	if (ATOMIC_VAL(n->net->launch_context->result) != 0) \
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

#define FILTER_DO_CLEANUP goto _glame_filter_cleanup

#define FILTER_CHECK_STOP \
do { \
        struct sembuf sop; \
        sop.sem_num = 0; \
        sop.sem_op = 0; \
        sop.sem_flg = 0; \
        glame_semop(n->net->launch_context->semid, &sop, 1); \
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

/* Compatibility #defines for old code.
 */
#ifndef _NO_FILTER_COMPATIBILITY
#define filter_node_t filter_t
#define filter_network_t filter_t
#define filter_portdesc_t filter_port_t

#define GLSIG_NODE_DELETED GLSIG_FILTER_DELETED

#define filter_alloc(f) filter_alloc_node(f)
#define filter_attach(f, p) filter_register(f, p)
#define filter_from_network(n) (n)
#define filter_add_input(f, l, d, t) filter_add_inputport(f, l, d, t)
#define filter_add_output(f, l, d, t) filter_add_outputport(f, l, d, t)

#define filter_pdb(f) filter_paramdb(f)
#define filterpdb_add_param_int filterparamdb_add_param_int
#define filterpdb_add_param_float filterparamdb_add_param_float
#define filterpdb_add_param_string filterparamdb_add_param_string

#define filterportdesc_pdb(pd) filterport_paramdb(pd)

#define filternode_get_input(n, l) filterport_get_pipe(filterportdb_get_port(filter_portdb(n), l))
#define filternode_get_output(n, l) filterport_get_pipe(filterportdb_get_port(filter_portdb(n), l))
#define filternode_next_input(p) list_getnext(&(p)->dest->pipes, p, filter_pipe_t, dest_list)
#define filternode_next_output(p) list_getnext(&(p)->source->pipes, p, filter_pipe_t, source_list)
static inline int filternode_nrinputs(filter_t *n)
{
	filter_port_t *port;
	int nr = 0;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_input(port))
			nr += filterport_nrpipes(port);
	}
	return nr;
}
static inline int filternode_nroutputs(filter_t *n)
{
	filter_port_t *port;
	int nr = 0;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_output(port))
			nr += filterport_nrpipes(port);
	}
	return nr;
}

#define filternode_get_param(n, l) filterparamdb_get_param(filter_paramdb(n), l)
#define filterpipe_get_sourceparam(p, l) filterparamdb_get_param(filterpipe_sourceparamdb(p), l)
#define filterpipe_get_destparam(p, l) filterparamdb_get_param(filterpipe_destparamdb(p), l)

#define filternode_set_error(n, erstr) filter_set_error(n, erstr)
#define filternode_clear_error(n) filter_clear_error(n)

#define filternetwork_new() filter_alloc_network()
static inline filter_t *filternetwork_add_node(filter_t *net, const char *node, const char *name)
{
	filter_t *n;

	if (!(n = filter_instantiate(plugin_get(node)))) {
		DPRINTF("No plugin %s\n", node);
		return NULL;
	}
	if (filter_add_node(net, n, name) == -1) {
		filter_delete(n);
		return NULL;
	}
	return n;
}
#define filternetwork_delete(net) filter_delete(net)
#define filternetwork_delete_node(node) filter_delete(node)
#define filternetwork_launch(net) filter_launch(net)
#define filternetwork_start(net) filter_start(net)
#define filternetwork_pause(net) filter_pause(net)
#define filternetwork_wait(net) filter_wait(net)
#define filternetwork_terminate(net) filter_terminate(net)
#define filternetwork_to_string(net) filter_to_string(net)
#define filternetwork_add_connection(n1, l1, n2, l2) filterport_connect(filterportdb_get_port(filter_portdb(n1), l1), filterportdb_get_port(filter_portdb(n2), l2))
#define filternetwork_break_connection(p) filterpipe_delete(p)
#endif /* _NO_FILTER_COMPATIBILITY */


#ifdef __cplusplus
}
#endif

#endif
