#include <string.h>
#include "gldb_string.h"
#include "filter.h"
#include "filter_param.h"


#define ITEM(p) (&(p)->entry)
#define PARAM(i) ((filter_param_t *)(i))

filter_param_t *pdb_alloc_item()
{
	filter_param_t *p;

	if (!(p = (filter_param_t *)malloc(sizeof(filter_param_t))))
		return NULL;
	gldb_init_item(&p->entry);
	glsdb_init(&p->properties);
	INIT_GLSIG_EMITTER(&p->emitter);

	return p;
}

void pdb_op_delete(gldb_item_t *item)
{
	filter_param_t *p = PARAM(item);

	/* Emit PARAM_DELETED signal. */
	glsig_emit(&p->emitter, GLSIG_PARAM_DELETED, p);

	glsdb_delete(&p->properties);
	glsig_delete_all_handlers(&p->emitter);
	if (FILTER_PARAM_IS_STRING(p))
		free(p->u.string);
}

gldb_item_t *pdb_op_copy(gldb_item_t *dest, gldb_item_t *source)
{
	filter_param_t *d = PARAM(dest);
	filter_param_t *s = PARAM(source);
	glsig_handler_t *h;

	if (d)
		pdb_op_delete(&d->entry);
	if (!d && !(d = pdb_alloc_item()))
		return NULL;
	d->type = s->type;

	if (FILTER_PARAM_IS_STRING(s)) {
		d->u.string = s->u.string ? strdup(s->u.string) : NULL;
	} else {
		memcpy(&d->u, &s->u, sizeof(s->u));
	}

	/* copy property db */
	glsdb_copy(&d->properties, &s->properties);

	/* copy signal handlers - we do not
	 * want the source-to-node redirector! */
	glsig_copy_handlers(&d->emitter, &s->emitter);
	list_foreach(&d->emitter.handlers, glsig_handler_t, list, h) {
		if (h->private == (void *)&((filter_pdb_t *)s->entry.db)->node->emitter) {
			glsig_delete_handler(h);
			break;
		}
	}

	return ITEM(d);
}

int pdb_op_add(gldb_t *db, gldb_item_t *i)
{
	filter_param_t *p = PARAM(i);
	glsig_handler_t *h;

	/* We can and should not try to do anything with
	 * items of a db with no node associated. */
	if (!((filter_pdb_t *)p->entry.db)->node)
		return 0;

	/* The task is to fix the signal redirector which
	 * is probably attached to the item - or just to
	 * add a new "right" one. */
	list_foreach(&p->emitter.handlers, glsig_handler_t, list, h) {
		if (h->private == (void *)&((filter_pdb_t *)p->entry.db)->node->emitter)
			return 0;
	}
	glsig_add_redirector(&p->emitter, &((filter_pdb_t *)p->entry.db)->node->emitter);
	return 0;
}

static struct gldb_ops ops = { pdb_op_delete, pdb_op_copy, pdb_op_add };



/* API stuff.
 */

void filterpdb_init(filter_pdb_t *db, filter_node_t *node)
{
	gldb_init(&db->db, &ops);
	db->node = node;
}


/* DB part. */

filter_param_t *filterpdb_add_param(filter_pdb_t *db, const char *label,
				    int type, const void *val)
{
	filter_param_t *p;
	gldb_item_t *i;

	if (!db || !label)
		return NULL;

	if ((i = gldb_query_item(&db->db, label)))
		return NULL;
	if (!(p = pdb_alloc_item()))
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
		pdb_op_delete(ITEM(p));
		return NULL;
	}

	return p;
}

filter_param_t *filterpdb_get_param(filter_pdb_t *db, const char *label)
{
	gldb_item_t *i;

	if (!db || !label)
		return NULL;

	if (!(i = gldb_query_item(&db->db, label)))
		return NULL;
	return PARAM(i);
}

void filterpdb_delete_param(filter_pdb_t *db, const char *label)
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
		    && strcmp(param->u.string, val) == 0)
			return 0;
		free(param->u.string);
		param->u.string = val ? strdup((const char *)val) : NULL;
	}

	/* Then ask the filter about the change.
	 */
	if (filterparam_node(param)->filter->set_param(filterparam_node(param), param, val) == -1)
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
