#ifndef _FILTER_OPS_H
#define _FILTER_OPS_H

#include "filter_types.h"


extern struct filter_operations filter_node_ops;
extern struct filter_operations filter_network_ops;

struct filter_launchcontext {
	int nr_threads;
	pthread_t waiter;

	int state;

	int semid;
	glame_atomic_t result;
};

#endif
