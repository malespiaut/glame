#ifndef _FILTER_MM_H
#define _FILTER_MM_H

filter_paramdesc_t *_paramdesc_alloc(const char *label, int type,
				     const char *desc);
void _paramdesc_free(filter_paramdesc_t *d);

filter_param_t *_param_alloc(filter_paramdesc_t *d);
void _param_free(filter_param_t *p);

filter_portdesc_t *_portdesc_alloc(filter_t *filter, const char *label,
				   int type, const char *desc);
void _portdesc_free(filter_portdesc_t *d);

filter_pipe_t *_pipe_alloc();
void _pipe_free(filter_pipe_t *p);

filter_t *_filter_alloc(const char *name, const char *description, int flags);
void _filter_free(filter_t *f);

filter_launchcontext_t *_launchcontext_alloc();
void _launchcontext_free(filter_launchcontext_t *c);

filter_node_t *_filter_instantiate(filter_t *f, const char *name);
void _node_free(filter_node_t *n);
void _network_free(filter_network_t *net);



#endif







