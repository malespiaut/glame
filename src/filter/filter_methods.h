#ifndef _FILTER_METHODS_H
#define _FILTER_METHODS_H


/* default filter methods.
 */
int filter_default_connect_out(filter_t *n, filter_port_t *port,
			       filter_pipe_t *p);
int filter_default_connect_in(filter_t *n, filter_port_t *port,
			      filter_pipe_t *p);
int filter_default_set_param(filter_t *n, filter_param_t *param,
			     const void *val);


/* network filter methods.
 */
int filter_network_init(filter_t *n);
int filter_network_f(filter_t *n);
int filter_network_connect_out(filter_t *source, filter_port_t *port,
			       filter_pipe_t *p);
int filter_network_connect_in(filter_t *dest, filter_port_t *port,
			      filter_pipe_t *p);
int filter_network_set_param(filter_t *n, filter_param_t *param,
			     const void *val);


#endif
