#ifndef _FILTER_H
#define _FILTER_H

/*
 * filter.h
 * $Id: filter.h,v 1.11 2000/02/05 15:59:26 richi Exp $
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
 * For documentation on filters see doc/filter.txt
 */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <pthread.h>
#include "glame_types.h"
#include "swapfile.h"
#include "glame_hash.h"
#include "list.h"
#include "atomic.h"


struct filter;
typedef struct filter filter_t;

struct filter_node;
typedef struct filter_node filter_node_t;

struct filter_network;
typedef struct filter_network filter_network_t;



/* Parameter declaration. This is per filter_t.
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
typedef struct {
	struct list_head list;
	struct hash_head hash;
	void *namespace;

	const char *label;
	int type;

	const char *description;
} filter_paramdesc_t;

/* Parameter instance. This is per filternode_t.
 */
typedef struct {
	struct hash_head hash;
	const char *label;
	void *namespace;

	union {
		int i;
		float f;
		SAMPLE sample;
		fileid_t file;
		char *string;
	} val;
} filter_param_t;



/* Filter port declaration. Type is a mask actually which
 * should contain any allowed pipe types.
 */
#define FILTER_PORTTYPE_AUTOMATIC 1
#define FILTER_PORTTYPE_SAMPLE    4
#define FILTER_PORTTYPE_RMS       8
#define FILTER_PORTTYPE_MISC    128
#define FILTER_PORTTYPE_ANY      -4
#define FILTER_PORT_IS_AUTOMATIC(type) ((type) & FILTER_PORTTYPE_AUTOMATIC)
#define FILTER_PORT_IS_COMPATIBLE(porttype, pipetype) (((porttype) & (pipetype)) == (pipetype))
typedef struct {
	struct list_head list;
	struct hash_head hash;
	void *namespace;

	const char *label;
	int type;

	const char *description;
} filter_portdesc_t;

/* Filter pipes represent a connection between two
 * instances of a filter. This is per filternode port
 * and depends on both filternode ports filter_portdesc.
 */
#define FILTER_PIPETYPE_SAMPLE  FILTER_PORTTYPE_SAMPLE
#define FILTER_PIPETYPE_RMS     FILTER_PORTTYPE_RMS
#define FILTER_PIPETYPE_MISC    FILTER_PORTTYPE_MISC
#define FILTER_PIPE_IS_COMPATIBLE(pipetype, porttype) (((porttype) & (pipetype)) == (pipetype))
#define FILTER_PIPETYPE_DEFAULT(porttype) ((porttype) & FILTER_PORTTYPE_SAMPLE ? FILTER_PIPETYPE_SAMPLE : ((porttype) & FILTER_PORTTYPE_RMS ? FILTER_PIPETYPE_RMS : FILTER_PIPETYPE_MISC))
typedef struct {
	/* lists and hashes */
	struct list_head input_list, output_list;
	struct hash_head input_hash, output_hash;
	const char *in_name, *out_name;
	void *in_namespace, *out_namespace;

	/* source and destination of the pipe, filter node
	 * and outputs[]/inputs[] index */
	filter_node_t *source, *dest;
	int source_fd, dest_fd;

	/* data type specification */
	int type;
	union {
		struct {
			int rate; /* sample rate, [Hz] */
		} sample;
		struct {
			int blocksize;
		} rms;
	} u;	
} filter_pipe_t;




/* Filter contains the abstract description of a filter and
 * contains a set of methods doing the actual work.
 */
struct filter {
	struct list_head list;
	struct hash_head hash;
	void *namespace;

	const char *name;
	const char *description;

	int (*f)(filter_node_t *);

	filter_node_t *(*init)(filter_node_t *);
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
	int nr_params;
	struct list_head params;

	/* input & output specification */
	int nr_inputs;
	struct list_head inputs;
	int nr_outputs;
	struct list_head outputs;

	void *private;
};

/* the global filter hash and list */
#define hash_add_filter(filter) _hash_add(&(filter)->hash, _hash((filter)->name, FILTER_NAMESPACE))
#define hash_remove_filter(filter) _hash_remove(&(filter)->hash)
#define hash_init_filter(filter) do { filter->namespace = FILTER_NAMESPACE; _hash_init(&(filter)->hash); } while (0)
#define is_hashed_filter(filter) _is_hashed(&(filter)->hash)
#define list_add_filter(filter) list_add(&(filter)->list, &filter_list)

/* The filter inputs/outputs description hash and list.
 * Only add and find are needed. */
#define hash_add_inputdesc(d, f) do { (d)->namespace = &(f)->inputs; _hash_add(&(d)->hash, _hash((d)->label, &(f)->inputs)); } while (0)
#define list_add_inputdesc(d, f) list_add(&(d)->list, &(f)->inputs)

#define hash_add_outputdesc(d, f) do { (d)->namespace = &(f)->outputs; _hash_add(&(d)->hash, _hash((d)->label, &(f)->outputs)); } while (0)
#define list_add_outputdesc(d, f) list_add(&(d)->list, &(f)->outputs)

/* The filters parameter description hash and list */
#define hash_add_paramdesc(d, f) do { (d)->namespace = (f); _hash_add(&(d)->hash, _hash((d)->label, (f))); } while (0)
#define list_add_paramdesc(d, f) list_add(&(d)->list, &(f)->params)


/* Filter node is an instance of a filter. A filter node
 * is associated with a filter network.
 */
#define FILTER_NODEFLAG_NETWORK 1
struct filter_node {
        struct hash_head hash;
        const char *name;

	/* connectivity in the net */
	filter_network_t *net;
	struct list_head net_list;
	struct list_head neti_list;

	/* Filter of which this filter_node is an instance,
	 * together with a private pointer that can be filled
	 * in using the filters init() method. */
	filter_t *filter;
	void *private;

	int flags;

	/* parameters */
	int nr_params;
	struct list_head params;

	/* Pipes connected as input/output. All input/output pipes are linked
	 * into the inputs/outputs list and hashed using the input/output slots
	 * name and the &inputs/&outputs pointer as namespace. */
	int nr_inputs;
	struct list_head inputs;
	int nr_outputs;
	struct list_head outputs;

	/* private - used by launch_filter_network & friends */
	int state;
	pthread_t thread;
};

/* filternode hash */
#define hash_find_node(n, nt) __hash_entry(_hash_find((n), (nt), _hash((n), (nt)), __hash_pos(filter_node_t, hash, name, net)), filter_node_t, hash)
#define hash_add_node(node) _hash_add(&(node)->hash, _hash((node)->name, (node)->net))
#define hash_remove_node(node) _hash_remove(&(node)->hash)
#define hash_unique_name_node(prefix, nt) _hash_unique_name((prefix), (nt), __hash_pos(filter_node_t, hash, name, net))

/* inputs & outputs hashes and lists */
#define hash_add_input(p, node) do { (p)->in_namespace = &(node)->inputs; _hash_add(&(p)->input_hash, _hash((p)->in_name, &(node)->inputs)); } while (0)
#define hash_remove_input(p) _hash_remove(&(p)->input_hash)
#define list_add_input(p, node) list_add(&(p)->input_list, &(node)->inputs)
#define list_del_input(p) list_del(&(p)->input_list)
#define list_gethead_input(n) list_gethead(&(n)->inputs, filter_pipe_t, input_list)

#define hash_add_output(p, node) do { (p)->out_namespace = &(node)->outputs; _hash_add(&(p)->output_hash, _hash((p)->out_name, &(node)->outputs)); } while (0)
#define hash_remove_output(p) _hash_remove(&(p)->output_hash)
#define list_add_output(p, node) list_add(&(p)->output_list, &(node)->outputs)
#define list_del_output(p) list_del(&(p)->output_list)
#define list_gethead_output(n) list_gethead(&(n)->outputs, filter_pipe_t, output_list)

#define hash_add_param(p, node) do { (p)->namespace = node; _hash_add(&(p)->hash, _hash((p)->label, node)); } while (0)
#define hash_remove_param(p) _hash_remove(&(p)->hash)



/* A filter network is a "filter" which
 * contains a set of connected filter instances.
 */
struct filter_network {
        filter_node_t node;
	int nr_nodes;
	struct list_head nodes;
	struct list_head inputs;

	int state;

	int semid;
	glame_atomic_t result;

	struct list_head buffers;
};

#define filternetwork_first_node(net) list_gethead(&(net)->nodes, filter_node_t, net_list)
#define filternetwork_foreach_input(net, node) list_foreach(&(net)->inputs, filter_node_t, neti_list, node)
#define filternetwork_foreach_node(net, node) list_foreach(&(net)->nodes, filter_node_t, net_list, node)

#define filternetwork_first_buffer(net) list_gethead(&(net)->buffers, filter_buffer_t, list)

#define FILTER_AFTER_INIT \
do { \
	struct sembuf sop; \
	sop.sem_num = 0; \
	sop.sem_op = 1; \
	sop.sem_flg = 0; \
	semop(n->net->semid, &sop, 1); \
	sop.sem_op = 0; \
	semop(n->net->semid, &sop, 1); \
	if (ATOMIC_VAL(n->net->result) != 0) \
		goto _glame_filter_cleanup; \
} while (0);

#define FILTER_BEFORE_CLEANUP _glame_filter_cleanup:


#ifdef __cplusplus
extern "C" {
#endif


/*******************************
 * Filter registry API
 */

/* inits the filter subsystem */
int filter_init();

/* Allocates a new filter structure. You have still to
 * fill it.
 * Can return NULL, if the name is already occupied. */
filter_t *filter_alloc(const char *name, const char *description,
		       int (*f)(filter_node_t *));

filter_portdesc_t *filter_add_input(filter_t *filter, const char *label,
				    const char *description, int type);
filter_portdesc_t *filter_add_output(filter_t *filter, const char *label,
				     const char *description, int type);
filter_paramdesc_t *filter_add_param(filter_t *filter, const char *label,
				     const char *description, int type);

/* Adds the filter to the filter database. */
int filter_add(filter_t *filter);

/* Browse the list of registered filters. if f is NULL gives first
 * filter. */
filter_t *filter_next(filter_t *f);

/* Find a named filter.
 * filter_t *hash_find_filter(const char *name); */
#define hash_find_filter(n) \
	__hash_entry(_hash_find((n), FILTER_NAMESPACE, _hash((n), \
		     FILTER_NAMESPACE), __hash_pos(filter_t, hash, name, \
                     namespace)), filter_t, hash)

/* Browse/find a input/output port description.
 * filter_portdesc_t *hash_find_inputdesc(const char *label, filter_t *f);
 * list_foreach_inputdesc(filter_t *f, filter_portdesc_t *d) { }
 * filter_portdesc_t *hash_find_outputdesc(const char *label, filter_t *f);
 * list_foreach_outputdesc(filter_t *f, filter_portdesc_t *d) { } */
#define hash_find_inputdesc(n, f) \
	__hash_entry(_hash_find((n), &(f)->inputs, _hash((n), &(f)->inputs), \
		     __hash_pos(filter_portdesc_t, hash, label, namespace)), \
		     filter_portdesc_t, hash)
#define list_foreach_inputdesc(f, d) \
	list_foreach(&(f)->inputs, filter_portdesc_t, list, d)
#define hash_find_outputdesc(n, f) \
	__hash_entry(_hash_find((n), &(f)->outputs, _hash((n), \
		     &(f)->outputs), __hash_pos(filter_portdesc_t, hash, \
		     label, namespace)), filter_portdesc_t, hash)
#define list_foreach_outputdesc(f, d) \
	list_foreach(&(f)->outputs, filter_portdesc_t, list, d)

/* Browse/find a parameter description.
 * filter_paramdesc_t *hash_find_paramdesc(const char *label, filter_t *f);
 * list_foreach_paramdesc(filter_t *f, filter_paramdesc_t *d) { } */
#define hash_find_paramdesc(n, f) \
	__hash_entry(_hash_find((n), (f), _hash((n), (f)), \
                     __hash_pos(filter_paramdesc_t, hash, label, namespace)), \
                     filter_paramdesc_t, hash)
#define list_foreach_paramdesc(f, d) \
	list_foreach(&(f)->params, filter_paramdesc_t, list, d)



/*******************************
 * Filter use API
 * aka filter networks
 * aka filter nodes
 */

/* Allocate a new filter network and initialize it.
 * Returns a filter network identifier or NULL on OOM.
 */
filter_network_t *filternetwork_new(const char *name);

void filternetwork_delete(filter_network_t *net);

/* Adds a new instance a filter to the filter network.
 * Returns a filter node identifier or NULL on error.
 */
filter_node_t *filternetwork_add_node(filter_network_t *net,
				      const char *filter, const char *name);

void filternode_delete(filter_node_t *node);

/* Connects the two ports source_port and dest_port of the
 * filter nodes source and dest.
 * Returns -1 if that is not possible.
 */
filter_pipe_t *filternetwork_add_connection(filter_node_t *source, const char *source_port,
					    filter_node_t *dest, const char *dest_port);

void filternetwork_break_connection(filter_pipe_t *p);

/* Filternodes connection query/walk.
 * filter_pipe_t *hash_find_input(const char *label, filter_node_t *n);
 * filter_pipe_t *hash_next_input(filter_pipe_t *p);
 * filter_pipe_t *hash_find_output(const char *label, filter_node_t *n);
 * filter_pipe_t *hash_next_output(filter_pipe_t *p);
 * list_foreach_input(filter_node_t *n, filter_pipe_t *p) { }
 * list_foreach_output(filter_node_t *n, filter_pipe_t *p) { } */
#define hash_find_input(n, node) \
	__hash_entry(_hash_find((n), &(node)->inputs, _hash((n), \
                     &(node)->inputs), __hash_pos(filter_pipe_t, \
                     input_hash, in_name, in_namespace)), filter_pipe_t, \
                     input_hash)
#define hash_next_input(p) \
	__hash_entry(_hash_find((p)->in_name, (p)->in_namespace, \
                     &(p)->input_hash.next_hash, __hash_pos(filter_pipe_t, \
                     input_hash, in_name, in_namespace)), filter_pipe_t, \
                     input_hash)
#define hash_find_output(n, node) \
	__hash_entry(_hash_find((n), &(node)->outputs, _hash((n), \
                     &(node)->outputs), __hash_pos(filter_pipe_t, \
                     output_hash, out_name, out_namespace)), filter_pipe_t, \
                     output_hash)
#define hash_next_output(p) \
        __hash_entry(_hash_find((p)->out_name, (p)->out_namespace, \
                     &(p)->output_hash.next_hash, __hash_pos(filter_pipe_t, \
                     output_hash, out_name, out_namespace)), filter_pipe_t, \
                     output_hash)
#define list_foreach_input(n, p) \
	list_foreach(&(n)->inputs, filter_pipe_t, input_list, p)
#define list_foreach_output(n, p) \
	list_foreach(&(n)->outputs, filter_pipe_t, output_list, p)

/* Filternodes parameter query.
 * filter_param_t *hash_find_param(const char *label, filter_node_t *n); */
#define hash_find_param(n, node) \
	__hash_entry(_hash_find((n), node, _hash((n), node), \
		     __hash_pos(filter_param_t, hash, label, namespace)), \
                     filter_param_t, hash)


/* Set the parameter with label label to the value pointed to by
 * val.
 * Returns -1 if that is not possible.
 */
int filternode_setparam(filter_node_t *n, const char *label, void *val);


/* Launches a set of connected filter instances and starts
 * processing of the data. */
int filternetwork_launch(filter_network_t *net);

/* Waits for the launched network to finish processing.
 * Returns 0 on successful completion or -1 on error
 * (in waiting or processing).
 */
int filternetwork_wait(filter_network_t *net);

/* Kills a launched network aborting all processing.
 */
void filternetwork_terminate(filter_network_t *net);


/* Macro filters - save/load a network. - FIXME */
int filternetwork_save(filter_network_t *net, const char *filename);

filter_t *filternetwork_load(const char *filename);


#include "filter_buffer.h"


#ifdef __cplusplus
}
#endif


#endif
