#ifndef _FILTER_PARAM_H
#define _FILTER_PARAM_H

/*
 * filter_param.h
 * $Id: filter_param.h,v 1.8 2000/12/11 13:15:23 richi Exp $
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


/* Parameters are categorized into basic types which are
 * for now int, float, sample and string those types are subtyped
 * by assigning them a type-id in the basic type ids range. So
 * you need to use the type test macros to test for the basic types.
 */
#define FILTER_PARAMTYPE_INT       0
#define FILTER_PARAMTYPE_INT_M     9
#define FILTER_PARAM_IS_INT(p) ((p)->type >= FILTER_PARAMTYPE_INT && (p)->type <= FILTER_PARAMTYPE_INT_M)

#define FILTER_PARAMTYPE_FLOAT    10
#define FILTER_PARAMTYPE_TIME_MS  11
#define FILTER_PARAMTYPE_TIME_S   12
#define FILTER_PARAMTYPE_POSITION 13
#define FILTER_PARAMTYPE_FLOAT_M  19
#define FILTER_PARAM_IS_FLOAT(p) ((p)->type >= FILTER_PARAMTYPE_FLOAT && (p)->type <= FILTER_PARAMTYPE_FLOAT_M)

#define FILTER_PARAMTYPE_SAMPLE   20
#define FILTER_PARAMTYPE_SAMPLE_M 29
#define FILTER_PARAM_IS_SAMPLE(p) ((p)->type >= FILTER_PARAMTYPE_SAMPLE && (p)->type <= FILTER_PARAMTYPE_SAMPLE_M)

#define FILTER_PARAMTYPE_STRING   30
#define FILTER_PARAMTYPE_FILENAME 31
#define FILTER_PARAMTYPE_STRING_M 39
#define FILTER_PARAM_IS_STRING(p) ((p)->type >= FILTER_PARAMTYPE_STRING && (p)->type <= FILTER_PARAMTYPE_STRING_M)


/* The filter parameter database type. You should not care
 * about its contents. */
typedef struct {
	gldb_t db;
	filter_t *node;
} filter_paramdb_t;

/* The filter parameter type. You may want to access the
 * signal emitter directly. */
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

/* Access macros to the various fields of the filter parameter
 * structure.
 * const char *filterparam_label(filter_param_t *);
 * filter_t *filterparam_filter(filter_paramt_t *); */
#define filterparam_label(p) ((p)->entry.label)
#define filterparam_filter(p) (((filter_paramdb_t *)((p)->entry.db))->node)

/* Public access macros for the parameter type and the union
 * int filterparam_type(filter_param_t *);
 * void *filterparam_val(filter_param_t *);
 * int filterparam_val_int(filter_param_t *);
 * const char *filterparam_val_string(filter_param_t *);
 * float filterparam_val_float(filter_param_t *);
 * SAMPLE filterparam_val_sample(filter_param_t *); */
#define filterparam_type(p) ((p)->type)
#define filterparam_val(p) (&(p)->u)
#define filterparam_val_int(p) ((p)->u.i)
#define filterparam_val_string(p) ((p)->u.string)
#define filterparam_val_float(p) ((p)->u.f)
#define filterparam_val_sample(p) ((p)->u.sample)


/* If you know that a particular parameter is stored in a pipes
 * source or destination parameter database, you may query that pipe
 * using the following macros. Use with care, as it will break if
 * your assumptions on the parameters domain are not correct!
 * filter_pipe_t *filterparam_get_sourcepipe(filter_param_t *p);
 * filter_pipe_t *filterparam_get_destpipe(filter_param_t *p);
 */
#define filterparam_get_sourcepipe(p) (filter_pipe_t *)( \
        (char *)&(p)->entry.db - (char *)&((filter_pipe_t *)0)->source_params)
#define filterparam_get_destpipe(p) (filter_pipe_t *)( \
        (char *)&(p)->entry.db - (char *)&((filter_pipe_t *)0)->dest_params)


/* Access to the property database, prototypes are
 * const char *filterparam_get_property(filter_param_t *p, const char *label);
 * int filterparam_set_property(filter_param_t *p, const char *label,
 *                              const char *value); */
#define filterparam_get_property(p, w) (glsdb_query(&(p)->properties, (w)))
#define filterparam_set_property(p, w, v) do { glsdb_set(&(p)->properties, \
        (v), (w)); } while (0)

/* Stdandard property names - MAP_NODE and MAP_LABEL are for internal
 * use only. The END one is used to finish the varargs list to the
 * filterparamdb_add_param*() calls. */
#define FILTERPARAM_DESCRIPTION "_desc"
#define FILTERPARAM_END NULL
#define FILTERPARAM_MAP_NODE "_node"
#define FILTERPARAM_MAP_LABEL "_label"



#ifdef __cplusplus
extern "C" {
#endif

/* Parameter setting/reading stuff API.
 */

/* To change the value of a parameter use the following function.
 * Note that on a successful change 0 is returned and a GLSIG_PARAM_CHANGED
 * signal is emitted. -1 is returned on an error such as memory shortage
 * or a rejected change by the set_param() method of the filter. */
int filterparam_set(filter_param_t *param, const void *val);

/* As filterparam_set() the following function tries to set the
 * parameters value, but this time using the value encoded in the
 * provided string. */
int filterparam_set_string(filter_param_t *param, const char *val);

/* To generate a string representation of the parameters value
 * use the following function. The returned string has to be freed
 * by the caller. NULL is be returned on error. */
char *filterparam_to_string(const filter_param_t *param);


/* void filterparam_delete(filter_param_t *);
 * Delete a parameter out of its database. */
#define filterparam_delete(param) do { if (param) \
        gldb_delete_item(&param->entry); } while (0)

/* Redirects parameter set/query operations (by copy!) to the specified
 * parameter. Returns 0 on success, -1 on error. */
int filterparam_redirect(filter_param_t *source, filter_param_t *dest);



/* The API which handles defining/setting/querying parameters.
 * All this is done using a filter parameter database handle,
 * which you can get using filter_paramdb(), filterport_paramdb(),
 * filterpipe_sourceparamdb() and filterpipe_destparamdb().
 */

/* To add a new parameter (i.e. define it) use the following function
 * through which you specify the parameters label, its type and its
 * default value (see below for some convenience wrappers). Also any
 * number of key/value pairs may be optionally specified and are stored
 * into the parameters property database.
 * You have to "finish" the property list by a FILTERPARAM_END argument
 * even if you did not specify any property. */
filter_param_t *filterparamdb_add_param(filter_paramdb_t *db,
					const char *label,
					int type, const void *val, ...);

/* To ease the use of the filterparamdb_add_param() function with respect to
 * specifying the default parameter value, the following wrappers are
 * provided which take a typed fourth parameter. Nothing else changes. */
filter_param_t *filterparamdb_add_param_int(filter_paramdb_t *db,
					    const char *label,
					    int type, int val, ...);
filter_param_t *filterparamdb_add_param_float(filter_paramdb_t *db,
					      const char *label,
					      int type, float val, ...);
filter_param_t *filterparamdb_add_param_string(filter_paramdb_t *db,
					       const char *label,
					       int type, const char *val, ...);

/* To query a parameter out of the filter parameter database use the
 * following function. If NULL is returned, the parameter does not exist. */
filter_param_t *filterparamdb_get_param(filter_paramdb_t *db,
					const char *label);

/* To delete a parameter use the following function. If the paramter
 * does not exist, nothing is done. */
void filterparamdb_delete_param(filter_paramdb_t *db, const char *label);

/* You can iterate through all parameters of a database using the
 * following iterator (which acts like a for statement with the
 * second parameter as running variable). Note that you may not
 * delete parameters in this loop! 
 * filterparamdb_foreach_param(filter_paramdb_t *, filter_param_t *) {} */
#define filterparamdb_foreach_param(pdb, i) list_foreach(&(pdb)->db.items, \
        filter_param_t, entry.list, i)

/* To just query the number of parameters stored in a parameter
 * database use the following function. 
 * int filterparamdb_nrparams(filter_paramdb_t *); */
#define filterparamdb_nrparams(pdb) gldb_nritems(&(pdb)->db)


/* Internal use API. You will never want to use these.
 */

/* Initialize a filter parameter database and tell it about
 * the location of the filter methods (via the filter node).
 * node may be NULL if you can ensure filterparam_set*() is
 * never called on one of its items. */
void filterparamdb_init(filter_paramdb_t *db, filter_t *node);

/* Delete the database, freeing all its parameters.
 * void filterparamdb_delete(filter_paramdb_t *); */
#define filterparamdb_delete(pdb) gldb_delete(&(pdb)->db)

/* Copy all parameters from one database to another. Returns 0
 * on success and -1 on error.
 * int filterparamdb_copy(filter_paramdb_t *, filter_paramdb_t *); */
#define filterparamdb_copy(d, s) gldb_copy(&(d)->db, &(s)->db)


#ifdef __cplusplus
}
#endif


#endif
