/*
 * filter.c
 * $Id: filter.c,v 1.30 2000/03/15 16:29:32 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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

#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "filter.h"
#include "filter_mm.h"


/* Global list of registered filters, static initialized. */
static LIST_HEAD(filter_list);



filter_t *filter_alloc(const char *name, const char *description,
		       int (*func)(filter_node_t *))
{
	filter_t *f;

	if (!name || !description || !func)
		return NULL;
	if (filter_get(name))
		return NULL;
	if (!(f = _filter_alloc(name, description, 0)))
		return NULL;

	/* fill in main filter function */
	f->f = func;

	return f;
}

filter_t *filter_from_string(const char *name, const char *description,
			     const char *net)
{
	filter_t *f;

	if (!name || !description || !net)
		return NULL;

	if (!(f = _filter_alloc(name, description, FILTER_FLAG_NETWORK)))
	        return NULL;
	if (!(f->private = (void *)strdup(net)))
		goto err;

	return f;

err:
	_filter_free(f);
	return NULL;
}

int filter_add(filter_t *filter)
{
	if (filter_get(filter->name))
		return -1;

	hash_add_filter(filter);
	list_add_filter(filter);

	return 0;
}


filter_t *filter_next(filter_t *f)
{
	struct list_head *lh;

	if (!f)
		lh = &filter_list;
	else
		lh = &f->list;

	lh = lh->next;
	if (lh == &filter_list)
		return NULL;

	return list_entry(lh, filter_t, list);
}

filter_portdesc_t *filter_add_input(filter_t *filter, const char *label,
				    const char *description, int type)
{
	filter_portdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return NULL;
	if (filter_get_inputdesc(filter, label))
		return NULL;
	if (!(desc = _portdesc_alloc(filter, label, type, description)))
		return NULL;

	hash_add_inputdesc(desc, filter);
	list_add_inputdesc(desc, filter);

	return desc;
}

filter_portdesc_t *filter_add_output(filter_t *filter, const char *label,
				     const char *description, int type)
{
	filter_portdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return NULL;
	if (filter_get_outputdesc(filter, label))
		return NULL;
	if (!(desc = _portdesc_alloc(filter, label, type, description)))
		return NULL;

	hash_add_outputdesc(desc, filter);
	list_add_outputdesc(desc, filter);

	return desc;
}

filter_paramdesc_t *filter_add_param(filter_t *filter, const char *label,
				     const char *description, int type)
{
	filter_paramdesc_t *desc;

	if (!filter || is_hashed_filter(filter) || !label)
		return NULL;
	if (filter_get_paramdesc(filter, label))
		return NULL;
	if (!(desc = _paramdesc_alloc(label, type, description)))
		return NULL;

	hash_add_paramdesc(desc, filter);
	list_add_paramdesc(desc, filter);

	return desc;
}

filter_paramdesc_t *filterport_add_param(filter_portdesc_t *portdesc,
					 const char *label,
					 const char *description, int type)
{
	filter_paramdesc_t *desc;
  
	if (!portdesc || is_hashed_filter(portdesc->filter) || !label)
		return NULL;
	if (filterportdesc_get_paramdesc(portdesc, label))
		return NULL;
	if (!(desc = _paramdesc_alloc(label, type, description)))
		return NULL;
  
	hash_add_paramdesc(desc, portdesc);
	list_add_paramdesc(desc, portdesc);

	return desc;
}




void filter_delete_port(filter_t *filter, filter_portdesc_t *port)
{
	if (!filter || is_hashed_filter(filter) || !port)
		return;
  
	hash_remove_portdesc(port);
	list_remove_portdesc(port);
	_portdesc_free(port);
}
  
void filter_delete_param(filter_t *filter, filter_paramdesc_t *param)
{
	if (!filter || is_hashed_filter(filter) || !param)
		return;

	hash_remove_paramdesc(param);
	list_remove_paramdesc(param);
	_paramdesc_free(param);
}
