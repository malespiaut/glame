#ifndef _FILTER_METHODS_H
#define _FILTER_METHODS_H

/* default filter methods.
 */
int filter_default_connect_out(filter_node_t *n, const char *port,
				      filter_pipe_t *p);
int filter_default_connect_in(filter_node_t *n, const char *port,
				     filter_pipe_t *p);
int filter_default_fixup_param(filter_node_t *n, filter_pipe_t *p,
			       const char *name, filter_param_t *param);
int filter_default_fixup_pipe(filter_node_t *n, filter_pipe_t *in);
void filter_default_fixup_break_in(filter_node_t *n, filter_pipe_t *in);
void filter_default_fixup_break_out(filter_node_t *n, filter_pipe_t *out);


/* network filter methods.
 */
int filter_network_init(filter_node_t *n);
void filter_network_cleanup(filter_node_t *n);
int filter_network_f(filter_node_t *n);
int filter_network_connect_out(filter_node_t *source, const char *port,
			       filter_pipe_t *p);
int filter_network_connect_in(filter_node_t *dest, const char *port,
			      filter_pipe_t *p);
int filter_network_fixup_param(filter_node_t *node, filter_pipe_t *p,
			       const char *name, filter_param_t *param);


#endif
