#ifndef _FILTERI_H
#define _FILTERI_H


struct filter_buffer {
	struct list_head list;
        glame_atomic_t refcnt;
	int size;              /* size of buffer in bytes */
	char buf[1];
};
#define list_remove_buffer(fb) list_del(&(fb)->list)

void _buffer_free(filter_buffer_t *fb);


struct filter_paramdesc {
	struct list_head list;
	struct hash_head hash;
	void *namespace; /* NOTE: this is filter_t/portdesc_t */

	const char *label;
	const char *description;

	int type;
        union {
		struct {
			int type;
		} string;
	        struct {
                        int type;
	        } f;
		struct {
			const char **labels;
		} list;
        } u;

	void *private;
};
/* parameter description hash/list addition/removal to filter or
 * port descriptions (the macros are transparent wrt to this).
 */
#define hash_add_paramdesc(d, portfilter) do { (d)->namespace = (portfilter); \
        _hash_add(&(d)->hash, _hash((d)->label, (portfilter))); } while (0)
#define list_add_paramdesc(d, portfilter) list_add(&(d)->list, \
        &(portfilter)->params)
#define hash_remove_paramdesc(d) _hash_remove(&(d)->hash)
#define list_remove_paramdesc(d) list_del(&(d)->list)


struct filter_param {
	struct list_head list;
	struct hash_head hash;
	const char *label;
	void *namespace; /* NOTE: this is node/pipe */

	/* signal emitter, known signals are
	 * GLSIG_PARAM_CHANGED */
	glsig_emitter_t emitter;

	filter_paramdesc_t *desc;
	union {
		int i;
		float f;
		SAMPLE sample;
		char *string;
		int list;
	} val;
};
/* parameter hash/list addition/removal to filter nodes or
 * to filter pipes (the remove macros are transparent wrt to this).
 */
#define hash_add_param(p, node) do { (p)->namespace = node; \
        _hash_add(&(p)->hash, _hash((p)->label, node)); } while (0)
#define hash_add_sourceparam(p, pp) do { \
        (p)->namespace = &(pp)->source_params; _hash_add(&(p)->hash, \
	_hash((p)->label, &(pp)->source_params)); } while (0)
#define hash_add_destparam(p, pp) do { \
        (p)->namespace = &(pp)->dest_params; _hash_add(&(p)->hash, \
	_hash((p)->label, &(pp)->dest_params)); } while (0)
#define list_add_param(p, node) list_add(&(p)->list, &(node)->params)
#define list_add_sourceparam(p, pp) list_add(&(p)->list, &(pp)->source_params)
#define list_add_destparam(p, pp) list_add(&(p)->list, &(pp)->dest_params)
#define hash_remove_param(p) _hash_remove(&(p)->hash)
#define list_remove_param(p) list_del(&(p)->list)


struct filter_portdesc {
	struct list_head list;
	struct hash_head hash;
	void *namespace;

	const char *label;
	int type;

	const char *description;

        filter_t *filter;
	struct list_head params;

	void *private;
};
/* port description hash/list addition/removal to filters qualified
 * by input/output port hash/list (removal is unqualified).
 */
#define hash_add_inputdesc(d, f) do { (d)->namespace = &(f)->inputs; _hash_add(&(d)->hash, _hash((d)->label, &(f)->inputs)); } while (0)
#define hash_add_outputdesc(d, f) do { (d)->namespace = &(f)->outputs; _hash_add(&(d)->hash, _hash((d)->label, &(f)->outputs)); } while (0)
#define list_add_inputdesc(d, f) list_add(&(d)->list, &(f)->inputs)
#define list_add_outputdesc(d, f) list_add(&(d)->list, &(f)->outputs)
#define hash_remove_portdesc(d) _hash_remove(&(d)->hash)
#define list_remove_portdesc(d) list_del(&(d)->list)
/* query parameter descriptions from the port description.
 */
#define filterportdesc_first_paramdesc(pd) list_gethead(&(pd)->params, \
        filter_paramdesc_t, list)


struct filter_pipe {
	/* lists and hashes */
	struct list_head input_list, output_list;
	struct hash_head input_hash, output_hash;
	const char *in_name, *out_name;
	void *in_namespace, *out_namespace;

	/* source and destination of the pipe, filter node
	 * and outputs[]/inputs[] index */
	filter_node_t *source, *dest;
	int source_fd, dest_fd;

	/* pipe specific parameters */
	filter_portdesc_t *source_port;
	struct list_head source_params;
	filter_portdesc_t *dest_port;
	struct list_head dest_params;

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
/* filter pipe hash/list addition/removal to filter nodes qualified by
 * input/output connection.
 */
#define hash_add_input(p, node) do { (p)->in_namespace = &(node)->inputs; \
        _hash_add(&(p)->input_hash, _hash((p)->in_name, &(node)->inputs)); } \
        while (0)
#define list_add_input(p, node) list_add(&(p)->input_list, &(node)->inputs)
#define hash_remove_input(p) _hash_remove(&(p)->input_hash)
#define list_remove_input(p) list_del(&(p)->input_list)
#define hash_add_output(p, node) do { (p)->out_namespace = &(node)->outputs; \
        _hash_add(&(p)->output_hash, _hash((p)->out_name, &(node)->outputs)); \
        } while (0)
#define list_add_output(p, node) list_add(&(p)->output_list, &(node)->outputs)
#define hash_remove_output(p) _hash_remove(&(p)->output_hash)
#define list_remove_output(p) list_del(&(p)->output_list)
/* query parameters from the pipe.
 */
#define filterpipe_first_sourceparam(p) list_gethead(&(p)->source_params, \
	filter_param_t, list)
#define filterpipe_first_destparam(p) list_gethead(&(p)->source_params, \
	filter_param_t, list)
#define filterpipe_foreach_sourceparam(p, parm) \
        list_foreach(&(p)->source_params, filter_param_t, list, parm)
#define filterpipe_foreach_destparam(p, parm) \
        list_foreach(&(p)->dest_params, filter_param_t, list, parm)


/* Global filter registry (via plugin) */
#define is_hashed_filter(filter) ((filter)->plugin != NULL)


/* Query first input/output port descriptions and first parameter
 * description from filter.
 */
#define filter_first_input_portdesc(f) list_gethead(&(f)->inputs, \
        filter_portdesc_t, list)
#define filter_first_output_portdesc(f) list_gethead(&(f)->outputs, \
        filter_portdesc_t, list)
#define filter_first_paramdesc(f) list_gethead(&(f)->params, \
        filter_paramdesc_t, list)


#define STATE_RUNNING 3
typedef struct {
	int nr_threads;
	pthread_t waiter;

	int state;

	int semid;
	glame_atomic_t result;
} filter_launchcontext_t;


struct filter_node_operations {
	int (*init)(filter_node_t *n);
	int (*launch)(filter_node_t *n);
	void (*postprocess)(filter_node_t *n);
	int (*wait)(filter_node_t *n);
};

#define STATE_UNDEFINED 0
#define STATE_INITIALIZED 1
#define STATE_LAUNCHED 2
struct filter_node {
        struct hash_head hash;
	struct list_head list;
        const char *name;

	/* connectivity in the net */
	filter_network_t *net;

	/* Filter of which this filter_node is an instance,
	 * together with a private pointer that can be filled
	 * in using the filters init() method. */
	filter_t *filter;
	void *private;

	/* public state & error string */
	int glerrno;
	const char *glerrstr;

	/* filter node operations */
	struct filter_node_operations *ops;

	/* parameters */
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
	struct list_head buffers;
};
/* filter node hash/list addition/removal to filter networks.
 */
#define hash_add_node(node) _hash_add(&(node)->hash, _hash((node)->name, \
        (node)->net))
#define list_add_node(node) list_add(&(node)->list, &(node)->net->nodes);
#define hash_remove_node(node) _hash_remove(&(node)->hash)
#define list_remove_node(node) list_del(&(node)->list)
/* query first input/output pipe and first parameter from filter node
 */
#define filternode_first_input(node) list_gethead(&(node)->inputs, \
        filter_pipe_t, input_list)
#define filternode_first_output(node) list_gethead(&(node)->outputs, \
        filter_pipe_t, output_list)
#define filternode_first_param(node) list_gethead(&(node)->params, \
        filter_param_t, list)
#define filternode_foreach_param(node, p) \
        list_foreach(&(n)->params, filter_param_t, list, p)

#define filternode_clear_error(n) \
do { \
	(n)->glerrno = 0; \
	(n)->glerrstr = NULL; \
} while (0)


struct filter_network_mapping {
	const char *label;
	const char *node;
};
#define filterdesc_map_label(d) (((struct filter_network_mapping *)(d)->private)->label)
#define filterdesc_map_node(d) (((struct filter_network_mapping *)(d)->private)->node)


struct filter_network {
        filter_node_t node;
	int nr_nodes;
	struct list_head nodes;

	filter_launchcontext_t *launch_context;
};
/* query first filter node in filter network.
 */
#define filternetwork_first_node(net) list_gethead(&(net)->nodes, \
        filter_node_t, list)


/* convenience type converters.
 */
#define FILTER_NETWORK(node) ((filter_network_t *)(node))
#define FILTER_NODE(net) (&(net)->node)

#define FILTERNODE_IS_LAUNCHED(node) ((node)->state >= STATE_LAUNCHED)
#define FILTERNETWORK_IS_LAUNCHED(net) ((net)->node.state >= STATE_LAUNCHED)
#define FILTERNETWORK_IS_RUNNING(net) ((net)->launch_context->state >= STATE_RUNNING)

/* helper to create automagically unique node names.
 */
#define hash_unique_name_node(prefix, nt) _hash_unique_name((prefix), (nt), \
        __hash_pos(filter_node_t, hash, name, net))


/* drain pipe to unblock source.
 */
void fbuf_drain(filter_pipe_t *p);
void fbuf_free_buffers(struct list_head *list);

#endif
