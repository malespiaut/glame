#ifndef _FILTER_MM_H
#define _FILTER_MM_H


filter_t *_filter_alloc(int flags);
void _filter_free(filter_t *f);

filter_launchcontext_t *_launchcontext_alloc();
void _launchcontext_free(filter_launchcontext_t *c);


filter_pipe_t *_pipe_alloc(filter_portdesc_t *source, filter_portdesc_t *dest);
void _pipe_free(filter_pipe_t *p);

filter_node_t *_filter_instantiate(filter_t *f, const char *name);
void _node_free(filter_node_t *n);
void _network_free(filter_network_t *net);


/* future obsolete stuff */
filter_portdesc_t *_portdesc_alloc(filter_t *filter, const char *label,
				   int type, const char *desc);
void _portdesc_free(filter_portdesc_t *d);


#endif







