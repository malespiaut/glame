/*
 * filter_param.c
 * $Id: filter_param.c,v 1.6 2000/10/28 13:45:48 richi Exp $
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
#include "gldb_string.h"
#include "filter.h"
#include "filter_param.h"


#define ITEM(p) (&(p)->entry)
#define PARAM(i) ((filter_param_t *)(i))

static filter_param_t *paramdb_alloc_item()
{
	filter_param_t *p;

	if (!(p = (filter_param_t *)malloc(sizeof(filter_param_t))))
		return NULL;
	gldb_init_item(&p->entry);
	glsdb_init(&p->properties);
	INIT_GLSIG_EMITTER(&p->emitter);

	return p;
}

static void paramdb_op_delete(gldb_item_t *item)
{
	filter_param_t *p = PARAM(item);

	/* Emit PARAM_DELETED signal. */
	glsig_emit(&p->emitter, GLSIG_PARAM_DELETED, p);

	glsdb_delete(&p->properties);
	if (FILTER_PARAM_IS_STRING(p))
		free(p->u.string);

	/* Notify the associated filter of the change. */
	glsig_emit(&p->emitter, GLSIG_FILTER_CHANGED,
		   ((filter_paramdb_t *)p->entry.db)->node);
	glsig_delete_all(&p->emitter);
}

static gldb_item_t *paramdb_op_copy(gldb_item_t *source)
{
	filter_param_t *d;
	filter_param_t *s = PARAM(source);

	if (!(d = paramdb_alloc_item()))
		return NULL;
	d->type = s->type;

	if (FILTER_PARAM_IS_STRING(s)) {
		d->u.string = s->u.string ? strdup(s->u.string) : NULL;
	} else {
		memcpy(&d->u, &s->u, sizeof(s->u));
	}

	/* copy property db */
	glsdb_copy(&d->properties, &s->properties);

	/* copy signal handlers - not the redirectors. */
	glsig_copy_handlers(&d->emitter, &s->emitter);

	return ITEM(d);
}

static int paramdb_op_add(gldb_t *db, gldb_item_t *i)
{
	filter_param_t *p = PARAM(i);
	glsig_handler_t *h;

	/* We can and should not try to do anything with
	 * items of a db with no node associated. */
	if (!((filter_paramdb_t *)p->entry.db)->node)
		return 0;

	/* The task is to fix the signal redirector which
	 * is probably attached to the item - or just to
	 * add a new "right" one. */
	list_foreach(&p->emitter.handlers, glsig_handler_t, list, h) {
		if (h->priv == (void *)&((filter_paramdb_t *)p->entry.db)->node->emitter)
			return 0;
	}
	glsig_add_redirector(&p->emitter, ~0,
			     &((filter_paramdb_t *)p->entry.db)->node->emitter);
	return 0;
}

static struct gldb_ops ops = { paramdb_op_delete,
			       paramdb_op_copy,
			       paramdb_op_add };


static filter_param_t *_filterparamdb_add_param(filter_paramdb_t *db,
						const char *label, int type,
						const void *val, va_list va)
{
	filter_param_t *p;
	gldb_item_t *i;
	const char *key, *prop;

	if (!db || !label)
		return NULL;

	if ((i = gldb_query_item(&db->db, label)))
		return NULL;
	if (!(p = paramdb_alloc_item()))
		return NULL;

	p->type = type;
	if (FILTER_PARAM_IS_INT(p)) {
		p->u.i = *(int *)val;
	} else if (FILTER_PARAM_IS_FLOAT(p)) {
		p->u.f = *(float *)val;
	} else if (FILTER_PARAM_IS_SAMPLE(p)) {
		p->u.sample = *(SAMPLE *)val;
	} else if (FILTER_PARAM_IS_STRING(p)) {
		p->u.string = val ? strdup((const char *)val) : NULL;
	}

	if (gldb_add_item(&db->db, ITEM(p), label) == -1) {
		paramdb_op_delete(ITEM(p));
		return NULL;
	}

	/* Process the va and add the specified key/value pairs
	 * to the property database. */
	while ((key = va_arg(va, const char *)) != FILTERPARAM_END) {
		prop = va_arg(va, const char *);
		filterparam_set_property(p, key, prop);
	}

	/* Notify the associated filter of the change. */
	glsig_emit(&p->emitter, GLSIG_FILTER_CHANGED,
		   ((filter_paramdb_t *)p->entry.db)->node);

	return p;
}



/* API stuff.
 */

void filterparamdb_init(filter_paramdb_t *db, filter_t *node)
{
	gldb_init(&db->db, &ops);
	db->node = node;
}


/* DB part. */

filter_param_t *filterparamdb_add_param(filter_paramdb_t *db,
					const char *label,
					int type, const void *val, ...)
{
	filter_param_t *p;
	va_list va;

	va_start(va, val);
	p = _filterparamdb_add_param(db, label, type, val, va);
	va_end(va);

	return p;
}
filter_param_t *filterparamdb_add_param_int(filter_paramdb_t *db,
					    const char *label,
					    int type, int val, ...)
{
	filter_param_t *p;
	va_list va;

	va_start(va, val);
	p = _filterparamdb_add_param(db, label, type, &val, va);
	va_end(va);

	return p;
}
filter_param_t *filterparamdb_add_param_float(filter_paramdb_t *db,
					      const char *label,
					      int type, float val, ...)
{
	filter_param_t *p;
	va_list va;

	va_start(va, val);
	p = _filterparamdb_add_param(db, label, type, &val, va);
	va_end(va);

	return p;
}
filter_param_t *filterparamdb_add_param_string(filter_paramdb_t *db,
					       const char *label,
					       int type, const char *val, ...)
{
	filter_param_t *p;
	va_list va;

	va_start(va, val);
	p = _filterparamdb_add_param(db, label, type, val, va);
	va_end(va);

	return p;
}

filter_param_t *filterparamdb_get_param(filter_paramdb_t *db,
					const char *label)
{
	gldb_item_t *i;

	if (!db || !label)
		return NULL;

	if (!(i = gldb_query_item(&db->db, label)))
		return NULL;
	return PARAM(i);
}

void filterparamdb_delete_param(filter_paramdb_t *db, const char *label)
{
	gldb_item_t *i;

	if (!db || !label)
		return;

	if (!(i = gldb_query_item(&db->db, label)))
		return;
	gldb_delete_item(i);
}


/* "Filter" part. */

int filterparam_set(filter_param_t *param, const void *val)
{
	if (!param || !val)
		return -1;

	/* First try to prevent useless parameter changing.
	 */
	if (FILTER_PARAM_IS_INT(param)) {
		if (param->u.i == *(int *)val)
			return 0;
	} else if (FILTER_PARAM_IS_FLOAT(param)) {
		if (param->u.f == *(float *)val)
			return 0;
	} else if (FILTER_PARAM_IS_SAMPLE(param)) {
		if (param->u.sample == *(SAMPLE *)val)
			return 0;
	} else if (FILTER_PARAM_IS_STRING(param)) {
		if (param->u.string && val
		    && strcmp(param->u.string, (const char *)val) == 0)
			return 0;
		free(param->u.string);
		param->u.string = val ? strdup((const char *)val) : NULL;
	}

	/* Then ask the filter about the change.
	 */
	if (filterparam_node(param)->set_param(filterparam_node(param), param, val) == -1)
		return -1;

	/* Finally do the change
	 */
	if (FILTER_PARAM_IS_INT(param))
		param->u.i = *(int *)val;
	else if (FILTER_PARAM_IS_FLOAT(param))
		param->u.f = *(float *)val;
	else if (FILTER_PARAM_IS_SAMPLE(param))
		param->u.sample = *(SAMPLE *)val;
	else if (FILTER_PARAM_IS_STRING(param)) {
		free(param->u.string);
		param->u.string = strdup((const char *)val);
	}

	/* and signal the change.
	 */
	glsig_emit(&param->emitter, GLSIG_PARAM_CHANGED, param);

	return 0;
}

int filterparam_set_string(filter_param_t *param, const char *val)
{
	filter_param_t p;
	char s[512];
	int res;

	if (!param || !val)
		return -1;

	if (FILTER_PARAM_IS_INT(param))
		res = sscanf(val, " %i ", &p.u.i);
	else if (FILTER_PARAM_IS_FLOAT(param))
		res = sscanf(val, " %f ", &p.u.f);
	else if (FILTER_PARAM_IS_SAMPLE(param))
		res = sscanf(val, " %f ", &p.u.sample);
	else if (FILTER_PARAM_IS_STRING(param)) {
		if ((res = sscanf(val, " \"%511[^\"]\" ", s)) != 1)
			res = sscanf(val, " %511[^\"] ", s);
		p.u.string = s;
	} else
		return -1;
	if (res != 1)
		return -1;

	return filterparam_set(param, (void *)&p.u);
}

char *filterparam_to_string(const filter_param_t *param)
{
	char buf[512];

	if (!param)
		return NULL;

	if (FILTER_PARAM_IS_INT(param))
		snprintf(buf, 511, "%i", param->u.i);
	else if (FILTER_PARAM_IS_FLOAT(param))
		snprintf(buf, 511, "%f", param->u.f);
	else if (FILTER_PARAM_IS_SAMPLE(param))
		/* FIXME: this is SAMPLE type specific */
		snprintf(buf, 511, "%f", param->u.sample);
	else if (FILTER_PARAM_IS_STRING(param))
		snprintf(buf, 511, "\"%s\"", param->u.string);
	else
		return NULL;

	return strdup(buf);
}
