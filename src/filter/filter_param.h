#ifndef _GLDB_PARAM_H
#define _GLDB_PARAM_H

#include "glame_types.h"
#include "glsignal.h"
#include "gldb.h"
#include "gldb_string.h"

/* How do we like filter parameter registration to be?
 * Example copied from basic_sample.c
 */
#if 0
int mix_register(plugin_t *p)
{
	filter_t *f;
	filter_portdesc_t *in, *out;
	filter_param_t *p;

        if (!(f = filter_alloc(mix_f))
            || !(in = filter_add_input(f, PORTNAME_IN, "input stream",
				       FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE))
	    || !(out = filter_add_output(f, PORTNAME_OUT, "mixed stream",
					 FILTER_PORTTYPE_SAMPLE)))
		return -1;

	/* umm, we should check for !p and property() == -1... */
	p = filterparam_add(filterportdesc_pdb(in), "gain",
			    FILTER_PARAMTYPE_TIME_S);
	filterparam_set_property(p, PARAM_DESCRIPTION, "input gain");

	p = filterparam_add(filterportdesc_pdb(in), "offset",
			    FILTER_PARAMTYPE_FLOAT);
	filterparam_set_property(p, PARAM_DESCRIPTION, "input offset");

	p = filterparam_add(filter_pdb(f), "gain",
			    FILTER_PARAMTYPE_FLOAT);
	filterparam_set_property(p, PARAM_DESCRIPTION, "output gain");

	p = filterparam_add(filter_pdb(f), "position", 
			    FILTER_PARAMTYPE_POSITION);
	filterparam_set_property(p, PARAM_DESCRIPTION,
				 "position of mixed stream");

	f->connect_out = mix_connect_out;
	f->fixup_param = mix_fixup_param;
	f->fixup_pipe = mix_fixup_pipe;

	plugin_set(p, PLUGIN_DESCRIPTION, "mix n streams");
	plugin_set(p, PLUGIN_PIXMAP, "mix1.png");
	filter_attach(f, p);

	return 0;
}
#endif


/* Parameter types.
 * OUTPUT may be or'ed with the type to make the parameter
 * and output one.
 */
#define FILTER_PARAMTYPE_OUTPUT  (1<<30)
#define FILTER_PARAM_IS_OUTPUT(p) ((p)->type & FILTER_PARAMTYPE_OUTPUT)

#define FILTER_PARAMTYPE(p) ((p)->type & ~(FILTER_PARAMTYPE_OUTPUT))

#define FILTER_PARAMTYPE_INT       0
#define FILTER_PARAMTYPE_INT_M     9
#define FILTER_PARAM_IS_INT(p) (FILTER_PARAMTYPE(p) >= FILTER_PARAMTYPE_INT && FILTER_PARAMTYPE(p) <= FILTER_PARAMTYPE_INT_M)

#define FILTER_PARAMTYPE_FLOAT    10
#define FILTER_PARAMTYPE_TIME_MS  11
#define FILTER_PARAMTYPE_TIME_S   12
#define FILTER_PARAMTYPE_POSITION 13
#define FILTER_PARAMTYPE_FLOAT_M  19
#define FILTER_PARAM_IS_FLOAT(p) (FILTER_PARAMTYPE(p) >= FILTER_PARAMTYPE_FLOAT && FILTER_PARAMTYPE(p) <= FILTER_PARAMTYPE_FLOAT_M)

#define FILTER_PARAMTYPE_SAMPLE   20
#define FILTER_PARAMTYPE_SAMPLE_M 29
#define FILTER_PARAM_IS_SAMPLE(p) (FILTER_PARAMTYPE(p) >= FILTER_PARAMTYPE_SAMPLE && FILTER_PARAMTYPE(p) <= FILTER_PARAMTYPE_SAMPLE_M)

#define FILTER_PARAMTYPE_STRING   30
#define FILTER_PARAMTYPE_FILENAME 31
#define FILTER_PARAMTYPE_STRING_M 39
#define FILTER_PARAM_IS_STRING(p) (FILTER_PARAMTYPE(p) >= FILTER_PARAMTYPE_STRING && FILTER_PARAMTYPE(p) <= FILTER_PARAMTYPE_STRING_M)

typedef struct {
	gldb_t db;
	filter_node_t *node;
} filter_pdb_t;

typedef struct {
	gldb_item_t entry;

	/* string db for properties like description.
	 */
	gldb_t properties;

	/* signal emitter, known signals are
	 * GLSIG_PARAM_CHANGED
	 * GLSIG_PARAM_DELETED */
	glsig_emitter_t emitter;

	/* just the old filterparam_t/paramdesc_t fields */
	int type;
	union {
		int i;
		float f;
		SAMPLE sample;
		char *string;
	} u;
} filter_param_t;

#define filterparam_label(p) ((p)->entry.label)
#define filterparam_node(p) (((filter_pdb_t *)((p)->entry.db))->node)
#define filterparam_get_property(p, w) (glsdb_query(&(p)->properties, (w)))
#define filterparam_set_property(p, w, v) do { glsdb_set(&(p)->properties, (v), (w)); } while (0)

/* NOTENOTENOTE! Only works for pipe source/dest params! I.e.
 * you have to know already!! HACK!
 */
#define filterparam_get_sourcepipe(p) (filter_pipe_t *)((char *)&(p)->entry.db - (char *)&((filter_pipe_t *)0)->source_params)
#define filterparam_get_destpipe(p) (filter_pipe_t *)((char *)&(p)->entry.db - (char *)&((filter_pipe_t *)0)->dest_params)


/* Std. property names.
 */
#define FILTERPARAM_DESCRIPTION "_desc"
#define FILTERPARAM_MAP_NODE "_node"
#define FILTERPARAM_MAP_LABEL "_label"

/* Public access macros for the parameter type specific union
 */
#define filterparam_type(p) ((p)->type)
#define filterparam_val_int(p) ((p)->u.i)
#define filterparam_val_string(p) ((p)->u.string)
#define filterparam_val_float(p) ((p)->u.f)
#define filterparam_val_sample(p) ((p)->u.sample)


/* Set already existant parameter. Does not do any checks.
 * Use with care - causes GLSIG_PARAM_CHANGED signal.
 */
#define FILTERPARAM_VAL(foo) ((void *)foo)
int filterparam_set(filter_param_t *param, const void *val);
int filterparam_set_string(filter_param_t *param, const char *val);
char *filterparam_to_string(const filter_param_t *param);

/* Parameter add/delete/query.
 */
filter_param_t *filterpdb_add_param(filter_pdb_t *db, const char *label, int type, const void *val);
filter_param_t *filterpdb_get_param(filter_pdb_t *db, const char *label);
void filterpdb_delete_param(filter_pdb_t *db, const char *label);

/* Parameter addition - wrappers for nice default value specification.
 */
static inline filter_param_t *filterpdb_add_param_int(filter_pdb_t *db, const char *label, int type, int val)
{
	return filterpdb_add_param(db, label, type, &val);
}
static inline filter_param_t *filterpdb_add_param_float(filter_pdb_t *db, const char *label, int type, float val)
{
	return filterpdb_add_param(db, label, type, &val);
}
static inline filter_param_t *filterpdb_add_param_string(filter_pdb_t *db, const char *label, int type, const char *val)
{
	return filterpdb_add_param(db, label, type, val);
}



/* Parameter iterator.
 */
#define filterpdb_foreach_param(pdb, i) list_foreach(&(pdb)->db.items, filter_param_t, entry.list, i)


/* Internal use API.
 */
void filterpdb_init(filter_pdb_t *db, filter_node_t *node);
#define filterpdb_delete(pdb) gldb_delete(&(pdb)->db)
#define filterpdb_copy(d, s) gldb_copy(&(d)->db, &(s)->db)


#endif
