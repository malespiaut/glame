/*
 * filter_port.c
 * $Id: filter_port.c,v 1.1 2000/10/28 13:51:32 richi Exp $
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

#include <string.h>
#include "filter.h"
#include "filter_mm.h"
#include "filter_port.h"


/* filter pipe addition/removal to filter port pipes list. */
#define list_add_pipe_source(p, port) do { list_add(&(p)->source_list, \
        &(port)->pipes); (port)->nr_pipes++; } while (0)
#define list_add_pipe_dest(p, port) do { list_add(&(p)->dest_list, \
        &(port)->pipes); (port)->nr_pipes++; } while (0)
#define list_remove_pipe_source(p) do { list_del(&(p)->source_list); \
        (p)->source->nr_pipes--; } while (0)
#define list_remove_pipe_dest(p) do { list_del(&(p)->dest_list); \
        (p)->dest->nr_pipes--; } while (0)


static filter_port_t *portdb_alloc_item()
{
	filter_port_t *p;

	if (!(p = (filter_port_t *)malloc(sizeof(filter_port_t))))
		return NULL;
	gldb_init_item(&p->entry);
	glsdb_init(&p->properties);
	INIT_GLSIG_EMITTER(&p->emitter);

	p->type = FILTER_PORTTYPE_ANY;
	p->flags = FILTER_PORTFLAG_INPUT;

	/* Init the parameter database - providing the node is
	 * deferred until add. */
	filterparamdb_init(&p->params, NULL);

	INIT_LIST_HEAD(&p->pipes);
	p->nr_pipes = 0;

	return p;
}

static void portdb_op_delete(gldb_item_t *item)
{
	filter_port_t *p = (filter_port_t *)item;
	filter_pipe_t *pipe;

	/* First remove all connections. */
	while ((pipe = filterport_get_pipe(p)))
		filterpipe_delete(pipe);

	/* Delete all other stuff. But notify the associated
	 * filter about the change before deleting the signal
	 * handlers. */
	glsdb_delete(&p->properties);
	glsig_emit(&p->emitter, GLSIG_FILTER_CHANGED,
		   ((filter_portdb_t *)p->entry.db)->node);
	glsig_delete_all(&p->emitter);
}

static gldb_item_t *portdb_op_copy(gldb_item_t *source)
{
	filter_port_t *d;
	filter_port_t *s = (filter_port_t *)source;

	if (!(d = portdb_alloc_item()))
		return NULL;

	/* Copy property db. */
	glsdb_copy(&d->properties, &s->properties);

	/* Copy signal handlers, not the redirectors. */
	glsig_copy_handlers(&d->emitter, &s->emitter);

	/* Copy type/flags and parameters. */
	d->type = s->type;
	d->flags = s->flags;
	filterparamdb_copy(&d->params, &s->params);

	/* We do NOT copy the pipes! This needs to be done
	 * manually, as both ends (ports) have to exist and
	 * the connect methods need to be called.
	 */

	return &d->entry;
}

static int portdb_op_add(gldb_t *db, gldb_item_t *i)
{
	filter_port_t *p = (filter_port_t *)i;
	filter_t *node = ((filter_portdb_t *)db)->node;

	/* We can and should not try to do anything with
	 * items of a db with no node associated. But we
	 * may consider this case an error. */
	if (!node)
		DPRINTF("Adding to anonymous db!\n"); //return 0;

	/* We need to fix the parameter db's knowledge of
	 * the filter. */
	p->params.node = node;

	/* Add a redirector to the associated node. */
	glsig_add_redirector(&p->emitter, ~0, &node->emitter);

	return 0;
}

static struct gldb_ops ops = { portdb_op_delete,
			       portdb_op_copy,
			       portdb_op_add };



/* API stuff.
 */

void filterportdb_init(filter_portdb_t *db, filter_t *f)
{
	gldb_init(&db->db, &ops);
	db->node = f;
}


/* DB part. */

filter_port_t *filterportdb_add_port(filter_portdb_t *db, const char *label,
				     int type, int flags, ...)
{
	filter_port_t *p;
	va_list va;
	const char *key, *prop;

	if (!db || !label)
		return NULL;

	if (!(p = portdb_alloc_item(db->node)))
		return NULL;
	p->type = type;
	p->flags = flags;

	if (gldb_add_item(&db->db, &p->entry, label) == -1) {
		portdb_op_delete(&p->entry);
		free(p);
		return NULL;
	}

	/* Process the va and add the specified key/value pairs
	 * to the property database. */
	va_start(va, flags);
	while ((key = va_arg(va, const char *)) != FILTERPORT_END) {
		prop = va_arg(va, const char *);
		filterport_set_property(p, key, prop);
	}
	va_end(va);

	/* Signal the associated filter its change. */
	glsig_emit(&p->emitter, GLSIG_FILTER_CHANGED, db->node);

	return p;
}


/* Connection management API.
 */

filter_pipe_t *filterport_connect(filter_port_t *source, filter_port_t *dest)
{
	filter_pipe_t *p;

	if (!source || !dest
	    || !filterport_filter(source) || !filterport_filter(dest)
	    || !filterport_is_output(source)
	    || !filterport_is_input(dest))
		return NULL;
	if (FILTER_IS_LAUNCHED(filterport_filter(source))
	    || FILTER_IS_LAUNCHED(filterport_filter(dest)))
		return NULL;

	/* do we support the out/in port type combination? */
	if (!FILTER_PORTS_ARE_COMPATIBLE(dest->type, source->type))
		goto _err;

	/* Alloc the pipe. */
	if (!(p = _pipe_alloc(source, dest)))
		return NULL;
        /* Yes, this does work. */
	p->type = filterport_type(dest)|filterport_type(source);

	/* Try to establish the connections. */
	if (filterport_filter(source)->connect_out(filterport_filter(source), source, p) == -1)
		goto _err;
	if (filterport_filter(dest)->connect_in(filterport_filter(dest), dest, p) == -1)
		goto _err;

	/* Now we have source & dest fixed - so we can finally
	 * init the parameter dbs and copy the parameters from
	 * the port descriptors. Note that while we associate
	 * the params with the connect_in/out mucked ports, we
	 * copy the (default) parameters from the unmucked ports
	 * - this is a hack introduced for filternetwork parameter
	 * handling that could/should go away for maximum flexibility. */
	filterparamdb_init(&p->source_params, filterport_filter(filterpipe_source(p)));
	filterparamdb_copy(&p->source_params, &source->params);
	filterparamdb_init(&p->dest_params, filterport_filter(filterpipe_dest(p)));
	filterparamdb_copy(&p->dest_params, &dest->params);

	/* Also the signal redirector can be installed now. Note
	 * that GLSIG_PIPE_CHANGED is redirected to the destination node
	 * only! This simplifies signal handling a lot as it matches
	 * the semantics of the old fixup_pipe() method. */
	glsig_add_redirector(filterpipe_emitter(p), ~0, filter_emitter(filterport_filter(filterpipe_dest(p))));
	glsig_add_redirector(filterpipe_emitter(p), ~GLSIG_PIPE_CHANGED, filter_emitter(filterport_filter(filterpipe_source(p))));

	/* add the pipe to all port lists/hashes.
	 * connect_out/in may have mucked with p->dest/source, so
	 * we have to use that instead of int/out directly. */
	list_add_pipe_dest(p, filterpipe_dest(p));
	list_add_pipe_source(p, filterpipe_source(p));

	/* signal pipe changes */
	glsig_emit(filterpipe_emitter(p), GLSIG_PIPE_CHANGED, p);

	return p;

 _err:
	_pipe_free(p);
	return NULL;
}

void filterpipe_delete(filter_pipe_t *p)
{
	if (FILTER_IS_PLUGIN(filterport_filter(filterpipe_source(p)))
	    || FILTER_IS_PLUGIN(filterport_filter(filterpipe_dest(p)))
	    || FILTER_IS_LAUNCHED(filterport_filter(filterpipe_source(p))))
		return;

	/* disconnect the pipe */
	list_remove_pipe_source(p);
	list_remove_pipe_dest(p);

	/* kill the pipe */
	_pipe_free(p);
}
