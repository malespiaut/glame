/*
 * filter.c
 * $Id: filter.c,v 1.17 2000/02/14 13:23:40 richi Exp $
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


/* Global list of registered filters. */
static struct list_head filter_list;


/* Filter subsystem initialization. Includes registration of
 * compiled in filters.
 */

/* include all static filter headers here */
extern int basic_register();
extern int channel_io_register();
extern int audio_io_register();
extern int debug_register();
extern int volume_adjust(filter_node_t *n);
extern int waveform_register();
extern int echo_register();
extern int echo2_register();
extern int garrison_register();
extern int nold_register();

int filter_init()
{
	filter_t *f;

	INIT_LIST_HEAD(&filter_list);

	/* initialize basic filters */
	if (basic_register() == -1)
		return -1;

	/* initialize channel/file input & output filters */
	if (channel_io_register() == -1)
		return -1;

	/* initialize audio input & output filters */
	if (audio_io_register() == -1)
		return -1;

	/* initialize debug & profile filters */
	if (debug_register() == -1)
		return -1;

	/* initialize waveform filters */
	if (waveform_register() == -1)
		return -1;

	/* initialize echo filter */
	if (echo_register() == -1)
		return -1;

	/* initialize echo2 filter */
	if (echo2_register() == -1)
		return -1;

	if (garrison_register() == -1)
		return -1;

	if (nold_register() == -1)
		return -1;
	
	if (!(f = filter_alloc("volume_adjust", "scale samples",
			       volume_adjust))
	    || !filter_add_param(f, "factor", "scale factor",
				 FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_input(f, "in", "input stream",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, "out", "output stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f) == -1)
		return -1;

	return 0;
}



/* The API.
 */

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
	if (!(desc = _portdesc_alloc(label, type, description)))
		return NULL;

	hash_add_inputdesc(desc, filter);
	list_add_inputdesc(desc, filter);
	filter->nr_inputs++;

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
	if (!(desc = _portdesc_alloc(label, type, description)))
		return NULL;

	hash_add_outputdesc(desc, filter);
	list_add_outputdesc(desc, filter);
	filter->nr_outputs++;

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
	filter->nr_params++;

	return desc;
}

filter_paramdesc_t *filter_add_pipeparam(filter_t *filter, const char *port,
					 const char *label,
					 const char *description, int type)
{
	filter_paramdesc_t *desc;
	filter_portdesc_t *portdesc;
  
	if (!filter || is_hashed_filter(filter) || !port || !label)
		return NULL;
	if (!(portdesc = filter_get_inputdesc(filter, port)))
		return NULL;
	if (filterportdesc_get_paramdesc(portdesc, label))
		return NULL;
	if (!(desc = _paramdesc_alloc(label, type, description)))
		return NULL;
  
	hash_add_paramdesc(desc, portdesc);
	list_add_paramdesc(desc, portdesc);
	portdesc->nr_params++;
  
	return desc;
}




void filter_delete_port(filter_t *filter, filter_portdesc_t *port)
{
	if (!filter || is_hashed_filter(filter) || !port)
		return;
  
	if (filter_get_inputdesc(filter, port->label))
		filter->nr_inputs--;
	else if (filter_get_outputdesc(filter, port->label))
		filter->nr_outputs--;
	else
		return;
  
	hash_remove_portdesc(port);
	list_remove_portdesc(port);
	_portdesc_free(port);
}
  
void filter_delete_param(filter_t *filter, filter_paramdesc_t *param)
{
	filter_portdesc_t *desc;
  
	if (!filter || is_hashed_filter(filter) || !param)
		return;
  
	if (filter_get_paramdesc(filter, param->label)) {
		filter->nr_params--;
	} else {
		filter_foreach_inputdesc(filter, desc)
			if (filterportdesc_get_paramdesc(desc, param->label)) {
				desc->nr_params--;
				goto ok;
			}
		return;
	}
  
ok:
	hash_remove_paramdesc(param);
	list_remove_paramdesc(param);
	_paramdesc_free(param);
}
