/*
 * filter_param.c
 * $Id: filter_param.c,v 1.14 2001/08/08 09:15:09 richi Exp $
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

static int default_set(filter_param_t *p, const void *val)
{
	return 0;
}

static int redirect_set(filter_param_t *param, const void *val)
{
	filter_t *node, *n;
	const char *map_node, *map_label;
	filter_param_t *p;

	/* pipe parameter setting does not go through the wrapped funcs */
	node = filterparam_filter(param);
	map_node = filterparam_get_property(param, FILTERPARAM_MAP_NODE);
	map_label = filterparam_get_property(param, FILTERPARAM_MAP_LABEL);
	if (!node || !map_node || !map_label)
		return -1;

	if (!(n = filter_get_node(node, map_node))) {
		DPRINTF("No such node %s\n", map_node);
		return -1;
	}
	if (!(p = filterparamdb_get_param(filter_paramdb(n), map_label))) {
		DPRINTF("No such param %s\n", map_label);
		return -1;
	}
	return filterparam_set(p, val);
}


static filter_param_t *paramdb_alloc_item()
{
	filter_param_t *p;

	if (!(p = ALLOC(filter_param_t)))
		return NULL;
	gldb_init_item(&p->entry);
	glsdb_init(&p->properties);
	INIT_GLSIG_EMITTER(&p->emitter);
	p->set = default_set;
	p->type = -1;

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
	else if (FILTER_PARAM_IS_BUF(p))
		fbuf_unref(p->u.buf);

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
	d->set = s->set;
	d->type = s->type;

	if (FILTER_PARAM_IS_STRING(s)) {
		d->u.string = s->u.string ? strdup(s->u.string) : NULL;
	} else if (FILTER_PARAM_IS_BUF(s)) {
		fbuf_ref(s->u.buf);
		d->u.buf = s->u.buf;
	} else {
		memcpy(&d->u, &s->u, sizeof(s->u));
	}

	/* copy property db */
	glsdb_copy(&d->properties, &s->properties);

	/* copy signal handlers - not the redirectors. */
	glsig_copy_handlers(&d->emitter, &s->emitter);

	return ITEM(d);
}

static int paramdb_op_add(gldb_t *db, gldb_item_t *i, gldb_item_t *dest)
{
	filter_param_t *p = PARAM(i);
	glsig_handler_t *h;

	/* We can and should not try to do anything with
	 * items of a db with no node associated -- we even fail for this now. */
	if (!((filter_paramdb_t *)p->entry.db)->node)
		DERROR("NULL node in parameters database\n");

	/* The task is to fix the signal redirector which
	 * is probably attached to the item - or just to
	 * add a new "right" one. -- now there should be no such there! [121200] */
	list_foreach(&p->emitter.handlers, glsig_handler_t, list, h) {
		if (h->priv == (void *)&((filter_paramdb_t *)p->entry.db)->node->emitter)
			DERROR("redirector got copied?");
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

	if (!db || FILTER_IS_PLUGIN(db->node) || !label || !val)
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
		p->u.string = *(const char **)val ? strdup(*(const char **)val) : NULL;
	} else if (FILTER_PARAM_IS_POS(p)) {
		p->u.pos = *(long *)val;
	} else if (FILTER_PARAM_IS_BUF(p)) {
		p->u.buf = *(filter_buffer_t **)val;
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
	p = _filterparamdb_add_param(db, label, type, &val, va);
	va_end(va);

	return p;
}
static filter_param_t *_filterparamdb_add_param_pos(filter_paramdb_t *db, ...)
{
	filter_param_t *p;
	va_list va;
	long val = 0;

	va_start(va, db);
	p = _filterparamdb_add_param(db, FILTERPARAM_LABEL_POS,
				     FILTER_PARAMTYPE_POS, &val, va);
	va_end(va);

	return p;
}
filter_param_t *filterparamdb_add_param_pos(filter_paramdb_t *db)
{
	return _filterparamdb_add_param_pos(db, FILTERPARAM_END);
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
	 * -- not a good idea, as parameters are re-set after copydb (121200)
	 */
#if 0
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
		if ((!param->u.string && !*val)
		    || (param->u.string && *val
		        && strcmp(param->u.string, *(const char **)val) == 0))
			return 0;
	} else if (FILTER_PARAM_IS_POS(param)) {
		if (param->u.pos == *(long *)val)
			return 0;
	}
#endif

	/* Then ask the param about the change.
	 */
	if (param->set(param, val) == -1)
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
		/* _first_ copy, then free, in case theyre actually the same... */
		char *vval = *(const char **)val ? strdup(*(const char **)val) : NULL;
		free(param->u.string);
		param->u.string = vval;
	} else if (FILTER_PARAM_IS_POS(param)) {
		param->u.pos = *(long *)val;
	} else if (FILTER_PARAM_IS_BUF(param)) {
		fbuf_ref(*(filter_buffer_t **)val);
		fbuf_unref(param->u.buf);
		param->u.buf = *(filter_buffer_t **)val;
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
	else if (FILTER_PARAM_IS_SAMPLE(param)) {
		float f;
		res = sscanf(val, " %f ", &f);
		p.u.sample = f;
	} else if (FILTER_PARAM_IS_STRING(param)) {
		if ((res = sscanf(val, " \"%511[^\"]\" ", s)) != 1)
			res = sscanf(val, " %511[^\"] ", s);
		p.u.string = s;
	} else if (FILTER_PARAM_IS_POS(param))
		res = sscanf(val, " %li ", &p.u.pos);
	else if (FILTER_PARAM_IS_BUF(param)) {
		filter_buffer_t *buf;
		unsigned char byte, *b;
		const unsigned char *c;
		int i;
		if (*val == '"')
			val++;
		buf = fbuf_alloc(strlen(val)/2, NULL);
		c = val;
		b = fbuf_buf(buf);
		for (i=0; i<fbuf_size(buf); i++) {
			byte = (*c <= '9' ? *c - '0' : *c - 'A' + 10) << 4;
			c++;
			byte += (*c <= '9' ? *c - '0' : *c - 'A' + 10);
			c++;
			*(b++) = byte;
		}
		p.u.buf = buf;
		res = 1;
	} else
		return -1;
	if (res != 1)
		return -1;

	res = filterparam_set(param, (void *)&p.u);

	if (FILTER_PARAM_IS_BUF(param))
		fbuf_unref(p.u.buf);

	return res;
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
		snprintf(buf, 511, "%f", (float)param->u.sample);
	else if (FILTER_PARAM_IS_STRING(param) && param->u.string)
		snprintf(buf, 511, "\"%s\"", param->u.string);
	else if (FILTER_PARAM_IS_POS(param))
		snprintf(buf, 511, "%li", param->u.pos);
	else if (FILTER_PARAM_IS_BUF(param) && param->u.buf) {
		const char CHARS[16] = "0123456789ABCDEF";
		char *str, *chr;
		unsigned char *val;
		int i;
		str = malloc(2*fbuf_size(param->u.buf)+3);
		chr = str;
		val = fbuf_buf(param->u.buf);
		*(chr++) = '"';
		for (i=0; i<fbuf_size(param->u.buf); i++) {
			*(chr++) = CHARS[(*val) >> 4];
			*(chr++) = CHARS[*(val++) & 0x0f];
		}
		*(chr++) = '"';
		*chr = '\0';
		return str;
	} else
		return NULL;

	return strdup(buf);
}

int filterparam_redirect(filter_param_t *source, filter_param_t *dest)
{
	if (!source || !dest || !filterparam_filter(dest)
	    || filterparam_type(source) != filterparam_type(dest))
		return -1;

	filterparam_set_property(source, FILTERPARAM_MAP_NODE,
				 filter_name(filterparam_filter(dest)));
	filterparam_set_property(source, FILTERPARAM_MAP_LABEL,
				 filterparam_label(dest));
	source->set = redirect_set;

	return 0;
}
