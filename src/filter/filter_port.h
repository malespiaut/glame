#ifndef _FILTER_PORT_H
#define _FILTER_PORT_H

/*
 * filter_port.h
 * $Id: filter_port.h,v 1.7 2001/08/08 09:15:09 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther
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

#include "glame_types.h"
#include "glsignal.h"
#include "gldb.h"
#include "gldb_string.h"
#include "filter_types.h"
#include "filter_param.h"



/* The filter port database type. You should not care
 * about its contents. */
struct filter_portdb {
	gldb_t db;
	filter_t *node;
};


/* Filter port flags. */
#define FILTER_PORTFLAG_INPUT     0
#define FILTER_PORTFLAG_OUTPUT    1

/* Filter port declaration. Type denotes the allowed pipe
 * type with FILTER_PORTTYPE_ANY is special in that it
 * accepts all kind of pipes. */
#define FILTER_PORTTYPE_ANY       0
#define FILTER_PORTTYPE_SAMPLE    1
#define FILTER_PORTTYPE_SSP       2
#define FILTER_PORTTYPE_CONTROL   4
#define FILTER_PORTTYPE_FFT       5
#define FILTER_PORTS_ARE_COMPATIBLE(port1type, port2type) (((port1type) == (port2type)) || ((port1type) == FILTER_PORTTYPE_ANY) || ((port2type) == FILTER_PORTTYPE_ANY))

/* The filter port type. You may want to access the
 * signal emitter directly. */
struct filter_port {
	gldb_item_t entry;

	/* string db for properties like description. */
	gldb_t properties;

	/* signal emitter, for redirection of pipe signals. */
	glsig_emitter_t emitter;

	/* port type and flags. */
	int type;
	int flags;

	/* methods - connect. */
	int (*connect)(filter_port_t *port, filter_pipe_t *pipe);

	/* default parameters for the pipes. */
	filter_paramdb_t params;

	/* list of connected pipes. The associated lists in
	 * the pipe structure are the source_list, if this
	 * is an output port and the dest_list, if this is
	 * an input port. */
	struct list_head pipes;
	int nr_pipes;
};

/* Public access macros for the filter_portdesc_t structure.
 * const char *filterport_label(filter_port_t *);
 * int filterport_type(filter_port_t *);
 * filter_paramdb_t *filterport_paramdb(filter_port_t *);
 * int filterport_is_input(filter_port_t *);
 * int filterport_is_output(filter_port_t *);
 * glsig_emitter_t *filterport_emitter(filter_port_t *);
 * filter_t *filterport_filter(filter_port_t *);
 */
#define filterport_label(pd) ((pd)->entry.label)
#define filterport_type(pd) ((pd)->type)
#define filterport_paramdb(pd) (&(pd)->params)
#define filterport_is_input(pd) ((pd)->flags == FILTER_PORTFLAG_INPUT)
#define filterport_is_output(pd) ((pd)->flags == FILTER_PORTFLAG_OUTPUT)
#define filterport_emitter(pd) (&(pd)->emitter)
#define filterport_filter(pd) (((filter_portdb_t *)((pd)->entry.db))->node)


/* Access to the property database, prototypes are
 * const char *filterport_get_property(filter_param_t *p, const char *label);
 * int filterport_set_property(filter_param_t *p, const char *label,
 *                             const char *value); */
#define filterport_get_property(p, w) (glsdb_query(&(p)->properties, (w)))
#define filterport_set_property(p, w, v) do { glsdb_set(&(p)->properties, \
        (v), (w)); } while (0)

/* Standard property names - MAP_NODE and MAP_LABEL are for internal
 * use only. The END one is used to finish the varargs list to the
 * filterparamdb_add_param*() calls. */
#define FILTERPORT_DESCRIPTION "_desc"
#define FILTERPORT_END NULL
#define FILTERPORT_MAP_NODE "_node"
#define FILTERPORT_MAP_LABEL "_label"


/* Pipe list access.
 * int filterport_nrpipes(filter_port_t *);
 * filter_pipe_t *filterport_get_pipe(filter_port_t *);
 * filter_pipe_t *filterport_next_pipe(filter_port_t *, filter_pipe_t *);
 * filterport_foreach_pipe(filter_port_t *, filter_pipe_t *) {} */
#define filterport_nrpipes(port) ((port)->nr_pipes)
#define filterport_get_pipe(port) (filterport_is_output(port) \
        ? list_gethead(&(port)->pipes, filter_pipe_t, source_list) \
        : list_gethead(&(port)->pipes, filter_pipe_t, dest_list))
#define filterport_next_pipe(port, p) (filterport_is_output(port) \
        ? (((p)->source_list.next == &(p)->source->pipes) \
           ? NULL \
           : list_entry((p)->source_list.next, filter_pipe_t, source_list)) \
        : (((p)->dest_list.next == &(p)->dest->pipes) \
           ? NULL \
           : list_entry((p)->dest_list.next, filter_pipe_t, dest_list)))
#define filterport_foreach_pipe(port, pipe) for ( \
        pipe = filterport_is_input(port) \
             ? list_entry((port)->pipes.next, filter_pipe_t, dest_list) \
             : list_entry((port)->pipes.next, filter_pipe_t, source_list); \
        filterport_is_input(port) \
             ? &pipe->dest_list != &(port)->pipes \
             : &pipe->source_list != &(port)->pipes; \
        pipe = filterport_is_input(port) \
             ? list_entry(pipe->dest_list.next, filter_pipe_t, dest_list) \
             : list_entry(pipe->source_list.next, filter_pipe_t, source_list))


#ifdef __cplusplus
extern "C" {
#endif

/* Redirects connections to this port to another port. Works
 * automagically for network filter ports. Returns -1 on error,
 * 0 on success. */
int filterport_redirect(filter_port_t *source, filter_port_t *dest);

/* Delete a port out of its database. */
void filterport_delete(filter_port_t *port);


/* The API which handles defining/setting/querying ports.
 * All this is done using a filter port database handle,
 * which you can get using filter_portdb().
 */

/* To add a new port (i.e. define it) use the following function
 * through which you specify the port label, its type and flags. Also any
 * number of key/value pairs may be optionally specified and are stored
 * into the ports property database.
 * You have to "finish" the property list by a FILTERPARAM_END argument
 * even if you did not specify any property. */
filter_port_t *filterportdb_add_port(filter_portdb_t *node, const char *label,
				     int type, int flags, ...);

/* filter_port_t *filterportdb_get_port(filter_portdb_t *, const char *label);
 * To query a port out of the filter port database use the
 * following function. If NULL is returned, the port does not exist. */
#define filterportdb_get_port(pdb, label) \
        ((filter_port_t *)gldb_query_item(&(pdb)->db, (label)))

/* void filterportdb_delete_port(filter_portdb_t *, const char *label);
 * To delete a port use the following function. If the paramter
 * does not exist, nothing is done. */
#define filterportdb_delete_port(pdb, label) \
        filterport_delete(filterportdb_get_port(pdb, label))

/* filterportdb_foreach_port(filter_portdb_t *, filter_port_t *) {}
 * You can iterate through all ports of a database using the
 * following iterator (which acts like a for statement with the
 * second parameter as running variable). Note that you may not
 * delete ports in this loop! */
#define filterportdb_foreach_port(pdb, p) list_foreach(&(pdb)->db.items, \
        filter_port_t, entry.list, p)

/* Safe variant - dont use inside f() method. */
#define filterportdb_safe_foreach_port(pdb, dummy, p) list_safe_foreach(&(pdb)->db.items, filter_port_t, entry.list, dummy, p)

/* int filterportdb_nrports(filter_portdb_t *);
 * To just query the number of ports stored in a port
 * database use the following function. */
#define filterportdb_nrports(pdb) gldb_nritems(&(pdb)->db)



/* Internal use API. You will never want to use these.
 */

/* Initialize a filter port database and tell it about
 * the location of the filter methods (via the filter node). */
void filterportdb_init(filter_portdb_t *db, filter_t *f);

/* void filterportdb_delete(filter_portdb_t *);
 * Delete the database, freeing all its ports. */
#define filterportdb_delete(pdb) gldb_delete(&(pdb)->db)

/* int filterportdb_copy(filter_portdb_t *, filter_portdb_t *);
 * Copy all ports from one database to another. Pipes are not
 * copied! Returns 0 on success, -1 on error. */
#define filterportdb_copy(d, s) gldb_copy(&(d)->db, &(s)->db)


#ifdef __cplusplus
}
#endif

#endif
