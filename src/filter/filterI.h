#ifndef _FILTERI_H
#define _FILTERI_H


struct filter_buffer {
	struct list_head list;
        glame_atomic_t refcnt;
	int size;              /* size of buffer in bytes */
	char buf[0];
};
#define list_remove_buffer(fb) list_del(&(fb)->list)

void _buffer_free(filter_buffer_t *fb);


struct filter_paramdesc {
	struct list_head list;
	struct hash_head hash;
	void *namespace;

	const char *label;
	int type;

	const char *description;

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
	void *namespace;

	union {
		int i;
		float f;
		SAMPLE sample;
		fileid_t file;
		char *string;
	} val;
};
/* parameter hash/list addition/removal to filter nodes or
 * to filter pipes (the macros are transparent wrt to this).
 */
#define hash_add_param(p, nodepipe) do { (p)->namespace = nodepipe; \
        _hash_add(&(p)->hash, _hash((p)->label, nodepipe)); } while (0)
#define list_add_param(p, nodepipe) list_add(&(p)->list, &(nodepipe)->params)
#define hash_remove_param(p) _hash_remove(&(p)->hash)
#define list_remove_param(p) list_del(&(p)->list)


struct filter_portdesc {
	struct list_head list;
	struct hash_head hash;
	void *namespace;

	const char *label;
	int type;

	const char *description;

	int nr_params;
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
	int nr_params;
	struct list_head params;

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
#define filterpipe_first_param(p) list_gethead(&(p)->params, filter_param_t, \
        list)



/* the global filter hash and list */
#define hash_add_filter(filter) do { (filter)->namespace = FILTER_NAMESPACE; \
        _hash_add(&(filter)->hash, _hash((filter)->name, FILTER_NAMESPACE)); \
        } while (0)
#define list_add_filter(filter) list_add(&(filter)->list, &filter_list)
#define hash_remove_filter(filter) _hash_remove(&(filter)->hash)
#define hash_init_filter(filter) do { (filter)->namespace = FILTER_NAMESPACE; \
        _hash_init(&(filter)->hash); } while (0)
#define is_hashed_filter(filter) _is_hashed(&(filter)->hash)

/* Query first input/output port descriptions and first parameter
 * description from filter.
 */
#define filter_first_input_portdesc(f) list_gethead(&(f)->inputs, \
        filter_portdesc_t, list)
#define filter_first_output_portdesc(f) list_gethead(&(f)->outputs, \
        filter_portdesc_t, list)
#define filter_first_paramdesc(f) list_gethead(&(f)->params, \
        filter_paramdesc_t, list)



typedef struct {
	int nr_threads;

	int state;
	struct list_head buffers;	

	int semid;
	glame_atomic_t result;
} filter_launchcontext_t;
#define filterlaunchcontext_first_buffer(c) list_gethead(&(c)->buffers, \
        filter_buffer_t, list)



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
        const char *name;

	/* connectivity in the net */
	filter_network_t *net;
	struct list_head net_list;

	/* Filter of which this filter_node is an instance,
	 * together with a private pointer that can be filled
	 * in using the filters init() method. */
	filter_t *filter;
	void *private;

	/* filter node operations */
	struct filter_node_operations *ops;

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
/* filter node hash/list addition/removal to filter networks.
 */
#define hash_add_node(node) _hash_add(&(node)->hash, _hash((node)->name, \
        (node)->net))
#define list_add_node(node, nt) list_add(&(node)->net_list, &(nt)->nodes);
#define hash_remove_node(node) _hash_remove(&(node)->hash)
#define list_remove_node(node) list_del(&(node)->net_list)
/* query first input/output pipe and first parameter from filter node
 */
#define filternode_first_input(node) list_gethead(&(node)->inputs, \
        filter_pipe_t, input_list)
#define filternode_first_output(node) list_gethead(&(node)->outputs, \
        filter_pipe_t, output_list)
#define filternode_first_param(node) list_gethead(&(node)->params, \
        filter_param_t, list)



struct filter_network_mapping {
	const char *label;
	const char *node;
};

struct filter_network {
        filter_node_t node;
	int nr_nodes;
	struct list_head nodes;

	filter_launchcontext_t *launch_context;
};
/* query first filter node in filter network.
 */
#define filternetwork_first_node(net) list_gethead(&(net)->nodes, \
        filter_node_t, net_list)


/* convenience type converters.
 */
#define FILTER_NETWORK(node) ((filter_network_t *)(node))
#define FILTER_NODE(net) (&(net)->node)

#define FILTERNODE_IS_RUNNING(node) ((node)->state == STATE_LAUNCHED)
#define FILTERNETWORK_IS_RUNNING(net) ((net)->node.state == STATE_LAUNCHED)


/* helper to create automagically unique node names.
 */
#define hash_unique_name_node(prefix, nt) _hash_unique_name((prefix), (nt), \
        __hash_pos(filter_node_t, hash, name, net))



#endif




