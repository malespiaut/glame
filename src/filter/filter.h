#ifndef _FILTER_H
#define _FILTER_H

/*
 * filter.h
 * $Id: filter.h,v 1.3 2000/01/25 16:11:34 richi Exp $
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

#include <pthread.h>
#include "glame_types.h"
#include "swapfile.h"
#include "glame_hash.h"
#include "list.h"


struct filter;
typedef struct filter filter_t;

struct filter_node;
typedef struct filter_node filter_node_t;



#define FILTER_PARAMFLAG_OUTPUT 1
struct filter_param_desc {
	char *label;
	char *type;
	int flags;
};

#define FILTER_PORTFLAG_AUTOMATIC 1
struct filter_port_desc {
	char *label;
	int flags;
};

union filter_param {
	int i;
	float f;
	fileid_t file;
};



/* A filter network contains a set of connected filter instances.
 * Output only and input only instances are special.
 */
typedef struct {
	struct list_head inputs;
	struct list_head outputs;

	int state;
	int result;
	pthread_mutex_t mx;
} filter_network_t;


/* Filter pipes represent a connection between two
 * instances of a filter.
 */
#define FILTER_PIPETYPE_UNINITIALIZED -1
#define FILTER_PIPETYPE_MISC           0
#define FILTER_PIPETYPE_SAMPLES        1
#define FILTER_PIPETYPE_RMS            2
typedef struct {
	/* source and destination of the pipe, filter node
	 * and outputs[]/inputs[] index */
	filter_node_t *source, *dest;
	int source_port, dest_port;
	int source_fd, dest_fd;

	/* data specification */
	int type;
	union {
		struct {
			int rate; /* sample rate, [Hz] */
		} samples;
		struct {
			int blocksize;
		} rms;
	} u;	
} filter_pipe_t;


/* Filter contains the abstract description of a filter and
 * contains a set of methods doing the actual work.
 */
struct filter {
	struct hash_head hash;
	void *namespace;

	char *name;

	/* The filter function. This is mandatory!
	 * Just return -1 if you cant do anything
	 * with the current setup. */
	int (*f)(filter_node_t *);

	/* Init params to default values, fill in the private
	 * field. Called at filternode_add() time.
	 * No default. Not required.
	 */
	void (*init)(filter_node_t *);

	/* Buyers beware! You can do anything with the
	 * following methods! But the semantics in case
	 * of any default method is used is the following:
	 * (1) connections are input[i] -> output[i]
	 * (2) outputs with no input and inputs with no outputs
	 *     are only allowed ON THE LAST PORTS
	 * (3) one automatic input and one automatic output
	 *     is allowed, BUT ONLY AS _LAST_ PORT
	 * => mixing (2) and (3) is IMPOSSIBLE (at least with
	 * the default connect & fixup methods
	 */

	/* These methods are called from filternode_connect()
	 * with a freshly allocated filter pipe.
	 * The methods have to fill their p->*_port.
	 * The out method has to fill in the pipe type
	 * and union, too - if available.
	 * The out method is always called before the in one.
	 * Just return -1 if you dont like the connection.
	 */
	int (*connect_out)(filter_node_t *source, const char *port,
			   filter_pipe_t *p);
	int (*connect_in)(filter_node_t *dest, const char *port,
			  filter_pipe_t *p);

	/* "Parameter or input changed" signal. If input_slot is -1, a
	 * local parameter has changed, else the change was in
	 * the pipe format of the input_slot slot.
	 * Just return -1 if you dont like anything.
	 */
	int (*fixup)(filter_node_t *n, int input_slot);

	/* parameter description */
	int nr_params;
	struct filter_param_desc *params;

	/* input & output specification */
	int nr_inputs;
	struct filter_port_desc *inputs;
	int nr_outputs;
	struct filter_port_desc *outputs;

	void *private;
};

/* the global filter hash */
#define hash_find_filter(n) __hash_entry(_hash_find((n), FILTER_NAMESPACE, (*(_hash((n), FILTER_NAMESPACE))), __hash_pos(filter_t, hash, name, namespace)), filter_t, hash)
#define hash_add_filter(filter) _hash_add(&(filter)->hash, _hash((filter)->name, FILTER_NAMESPACE))
#define hash_remove_filter(filter) _hash_remove(&(filter)->hash)
#define hash_first_filter() __hash_entry(_hash_walk(NULL, FILTER_NAMESPACE, __hash_pos(filter_t, hash, name, namespace)), filter_t, hash)
#define hash_walk_filter(filter) __hash_entry(_hash_walk(&(filter)->hash, FILTER_NAMESPACE, __hash_pos(filter_t, hash, name, namespace)), filter_t, hash)
#define hash_init_filter(filter) do { filter->namespace = FILTER_NAMESPACE; _hash_init(&(filter)->hash); } while (0)
#define is_hashed_filter(filter) _is_hashed(&(filter)->hash)


/* Filter node is an instance of a filter. A filter node
 * is associated with a filter network.
 */
struct filter_node {
	/* connectivity in the net */
	filter_network_t *net;
	struct list_head neti_list;
	struct list_head neto_list;

	/* filter at this node, with private parameters */
	filter_t *filter;
	union filter_param *params;
	void *private;

	/* pipes connected as input */
	int nr_inputs;
	filter_pipe_t **inputs;

	/* pipes connected to output ports */
	int nr_outputs;
	filter_pipe_t **outputs;

	/* private - used by launch_filter_network & friends */
	int state;
	pthread_t thread;
};



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
filter_t *filter_alloc(const char *name,  int (*f)(filter_node_t *),
		       int nr_params, int nr_inputs, int nr_outputs);

/* Adds the filter to the filter database. */
int filter_add(filter_t *filter);



/*******************************
 * Filter use API
 * aka filter networks
 * aka filter nodes
 */

/* Allocate a new filter network and initialize it.
 * Returns a filter network identifier or NULL on OOM.
 */
filter_network_t *filternetwork_new();

/* Adds a new instance a filter to the filter network.
 * Returns a filter node identifier or NULL on error.
 */
filter_node_t *filternode_add(filter_network_t *net, const char *filter);

/* Connects the two ports source_port and dest_port of the
 * filter nodes source and dest.
 * Returns -1 if that is not possible.
 */
int filternode_connect(filter_node_t *source, char *source_port,
		       filter_node_t *dest, char *dest_port);


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



/*******************************
 * Helpers for constructing filters ->connect_*() methods.
 */

int _filterconnect_find_port(struct filter_port_desc *ports, int cnt,
			     const char *port);
int _filterconnect_assign_port(struct filter_port_desc *pdesc, int cnt,
			       const char *port,
			       filter_pipe_t ***slots, int *nr_slots);

/* Better to understand wrappers:
 * - filterconnect_findport_* returns the index to the filters
 *   port description array or -1 if no port with the name can
 *   be found
 * - filterconnect_assignslot_* returns the index to the nodes
 *   pipe array which is used for the port. returns -1 if the
 *   port is already occupied. expands the number of pipes, if
 *   necessary and the port is automatic.
 */
#define filterconnect_findport_input(filter, port) _filterconnect_find_port((filter)->inputs, (filter)->nr_inputs, (port))
#define filterconnect_findport_output(filter, port) _filterconnect_find_port((filter)->outputs, (filter)->nr_outputs, (port))
#define filterconnect_assignslot_input(node, port) _filterconnect_assign_port((node)->filter->inputs, (node)->filter->nr_inputs, (port), &(node)->inputs, &(node)->nr_inputs)
#define filterconnect_assignslot_output(node, port) _filterconnect_assign_port((node)->filter->outputs, (node)->filter->nr_outputs, (port), &(node)->outputs, &(node)->nr_outputs)



/*******************************
 * API for use inside of filters ->f() function.
 */

/* A filter buffer is the internal representation of a data
 * stream piped through the filter network. It is used by
 * the filters ->f() method.
 * Access is only through the fbuf_* functions and macros.
 */
typedef struct {
	int semid;
	int semnum;
	int size;
	SAMPLE *buf;
} filter_buffer_t;

#define fbuf_size(fb) ((fb)->size)
#define fbuf_buf(fb) ((fb)->buf)


/* get (blocking) the next buffer from the input stream
 */
filter_buffer_t *fbuf_get(filter_pipe_t *p);

/* queue (blocking!) the buffer to the output stream
 */
int fbuf_queue(filter_pipe_t *p, filter_buffer_t *fbuf);

/* create a filter buffer with backing storage for size
 * samples (uninitialized!).
 * the buffer is initially one time referenced.
 */
filter_buffer_t *fbuf_alloc(int size);


/* get one extra reference (read-only!) of the buffer.
 * if the number of references drops to zero at any point,
 * the buffer may be freed without notice!
 */
int fbuf_ref(filter_buffer_t *fb);

/* release one reference of the buffer.
 */
int fbuf_unref(filter_buffer_t *fb);


/* lock the buffer for reading _and_ writing, i.e. try to
 * get exclusive access to it. if this does not work, a copy
 * of the buffer is generated (in this case the held reference
 * on fb is released!).
 * the exclusive locked buffer is returned (or NULL on any error)
 */
filter_buffer_t *fbuf_lock(filter_buffer_t *fb);

/* unlock the buffer. this does not release any previous held
 * reference of the buffer! i.e. if you are completely ready
 * with the buffer you still need to do fbuf_unref one time.
 */
int fbuf_unlock(filter_buffer_t *fb);


#ifdef __cplusplus
}
#endif


#endif
