#ifndef _FILTERI_H
#define _FILTERI_H


/* Private filter buffer stuff.
 */


/* Launchcontext and launching operations structure. */
typedef struct {
	int nr_threads;
	pthread_t waiter;

	int state;

	int semid;
	glame_atomic_t result;
} filter_launchcontext_t;

struct filter_operations {
	int (*init)(filter_t *n);
	int (*launch)(filter_t *n);
	void (*postprocess)(filter_t *n);
	int (*wait)(filter_t *n);
};

/* Filter state/type query. */
#define FILTER_IS_PLUGIN(f) ((f)->type & FILTERTYPE_PLUGIN)
#define FILTER_IS_PART_OF_NETWORK(f) ((f)->net != NULL)
#define FILTER_IS_NETWORK(f) ((f)->type & FILTERTYPE_NETWORK)
#define FILTER_IS_NODE(f) (!((f)->type & FILTERTYPE_NETWORK))

#define FILTER_IS_LAUNCHED(node) ((node)->state >= STATE_LAUNCHED)
#define FILTER_IS_RUNNING(net) ((net)->launch_context->state >= STATE_RUNNING)



/* filter node hash/list addition/removal/query to filter networks.
 */
#define hash_remove_node(node) _hash_remove(&(node)->hash)
#define list_remove_node(node) list_del(&(node)->list)


#endif
