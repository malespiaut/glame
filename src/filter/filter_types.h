#ifndef _FILTER_TYPES_H
#define _FILTER_TYPES_H


/* Types that are publically visible - see seperate
 * headerfile for their definitions: filter.h,
 * filter_pipe.h, filter_port.h
 */
struct filter;
typedef struct filter filter_t;

struct filter_pipe;
typedef struct filter_pipe filter_pipe_t;

struct filter_portdb;
typedef struct filter_portdb filter_portdb_t;
struct filter_port;
typedef struct filter_port filter_port_t;

struct filter_buffer;
typedef struct filter_buffer filter_buffer_t;


/* Opaque types used internally only in filter_ops.h
 */
struct filter_operations;

struct filter_launchcontext;
typedef struct filter_launchcontext filter_launchcontext_t;


#endif

