/*
 * glscript_filter.c
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <signal.h>
#include <guile/gh.h>
#include "filter.h"
#include "glplugin.h"
#include "glmid.h"
#include "glscript.h"


/* Hack to allow switching between register(0)/instantiate(1) mode */
int glscript_load_mode;


/* SMOBs for filter_pipe_t, filter_param_t, filter_port_t and plugint_t.
 */

static long pipe_smob_tag;
#define scm2pipe(s) scm2pointer(s, pipe_smob_tag)
#define pipe2scm(p) pointer2scm(p, pipe_smob_tag)
#define scminvalidatepipe(s) scminvalidatepointer(s, pipe_smob_tag)
#define pipe_p(s) (SCM_NIMP(s) && SCM_CAR(s) == pipe_smob_tag)

static long port_smob_tag;
#define scm2port(s) scm2pointer(s, port_smob_tag)
#define port2scm(p) pointer2scm(p, port_smob_tag)
#define scminvalidateport(s) scminvalidatepointer(s, port_smob_tag)
#define port_p(s) (SCM_NIMP(s) && SCM_CAR(s) == port_smob_tag)

static long param_smob_tag;
#define scm2param(s) scm2pointer(s, param_smob_tag)
#define param2scm(p) pointer2scm(p, param_smob_tag)
#define scminvalidateparam(s) scminvalidatepointer(s, param_smob_tag)
#define param_p(s) (SCM_NIMP(s) && SCM_CAR(s) == param_smob_tag)

static int print_param(SCM param_smob, SCM port, scm_print_state *pstate)
{
	struct pointer_smob *param = SCM2POINTERSMOB(param_smob);
	filter_param_t *p;
	char buf[256], *val;

	p = (filter_param_t *)param->pointer;
	snprintf(buf, 255, "#<param %s %s>",
		 filterparam_label(p),
		 val = filterparam_to_string(p));
	scm_puts(buf, port);
	free(val);

	return 1;
}

static long plugin_smob_tag;
#define scm2plugin(s) scm2pointer(s, plugin_smob_tag)
#define plugin2scm(p) pointer2scm(p, plugin_smob_tag)
#define scminvalidateplugin(s) scminvalidatepointer(s, plugin_smob_tag)
#define plugin_p(s) (SCM_NIMP(s) && SCM_CAR(s) == plugin_smob_tag)


/* SMOB for filter_t.
 */

long filter_smob_tag = 0;
struct filter_smob {
	filter_t *filter;
};
#define SCM2FILTERSMOB(s) ((struct filter_smob *)SCM_SMOB_DATA(s))
#define filter_p(s) (SCM_NIMP(s) && SCM_CAR(s) == filter_smob_tag)
static SCM filter2scm(filter_t *filter);
filter_t *scm2filter(SCM filter_smob);

static scm_sizet free_filter(SCM filter_smob)
{
	struct filter_smob *filter = SCM2FILTERSMOB(filter_smob);

	/* Delete the filter, if it is neither plugin, nor part
	 * of a network. */
	if (filter->filter
	    && !FILTER_IS_PART_OF_NETWORK(filter->filter)
	    && !FILTER_IS_PLUGIN(filter->filter))
		filter_delete(filter->filter);

	return sizeof(struct filter_smob);
}

static int print_filter(SCM filter_smob, SCM port, scm_print_state *pstate)
{
	struct filter_smob *filter = SCM2FILTERSMOB(filter_smob);
	filter_t *n;
	char buf[256];

	snprintf(buf, 255, "#<filter %s %s",
		 filter_name(filter->filter)
		 ? filter_name(filter->filter) : "(unnamed)",
		 FILTER_IS_PLUGIN(filter->filter)
		 ? "[RO]" : "");
	scm_puts(buf, port);

	if (FILTER_IS_NETWORK(filter->filter)) {
		scm_puts(" ( ", port);
		filter_foreach_node(filter->filter, n)
			scm_display(filter2scm(n), port);
		scm_puts(") ", port);
	}

	scm_puts("> ", port);

	return 1;
}

static SCM equalp_filter(SCM filter_smob1, SCM filter_smob2)
{
	struct filter_smob *filter1 = SCM2FILTERSMOB(filter_smob1);
	struct filter_smob *filter2 = SCM2FILTERSMOB(filter_smob2);

	if (filter1->filter == filter2->filter)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}


static SCM filter2scm(filter_t *filter)
{
	struct filter_smob *smob;
	SCM filter_smob;

	if (!filter)
		return SCM_BOOL_F;

	smob = (struct filter_smob *)malloc(sizeof(struct filter_smob));
	smob->filter = filter;

	SCM_NEWSMOB(filter_smob, filter_smob_tag, smob);

	return filter_smob;
}

filter_t *scm2filter(SCM filter_smob)
{
	SCM_ASSERT(filter_p(filter_smob),
		   filter_smob, SCM_ARG1, "scm2filter");
	return SCM2FILTERSMOB(filter_smob)->filter;
}

void scminvalidatefilter(SCM filter_smob)
{
	struct filter_smob *filter = SCM2FILTERSMOB(filter_smob);

	SCM_ASSERT(filter_p(filter_smob),
		   filter_smob, SCM_ARG1, "scminvalidatefilter");

	filter->filter = NULL;
}

static SCM filterp(SCM filter_smob)
{
	if (filter_p(filter_smob))
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}




/* The scriptable filter API part.
 */

static SCM gls_filter_creat(SCM s_filter)
{
	filter_t *filter = NULL;
	if (filter_p(s_filter))
		filter = scm2filter(s_filter);
	return filter2scm(filter_creat(filter));
}

static SCM gls_filter_instantiate(SCM s_plugin)
{
	plugin_t *plugin = scm2plugin(s_plugin);
	if (!plugin)
		return SCM_BOOL_F;
	return filter2scm(filter_instantiate(plugin));
}

static SCM gls_create_plugin(SCM s_filter, SCM s_name)
{
	filter_t *filter;
	plugin_t *p;
	char *name;
	int namel;

	filter = scm2filter(s_filter);
	name = gh_scm2newstr(s_name, &namel);
	p = glame_create_plugin(filter, name);
	free(name);
	if (!p)
		return SCM_BOOL_F;
	return plugin2scm(p);
}

static SCM gls_filter_to_string(SCM s_net)
{
	filter_t *net;

	net = scm2filter(s_net);
	return gh_str02scm((char *)filter_to_string(net));
}


static SCM gls_filter_add_node(SCM s_net, SCM s_filter, SCM s_name)
{
	filter_t *net;
	filter_t *filter;
	char *name;
	int namel;
	int res;

	net = scm2filter(s_net);
	filter = scm2filter(s_filter);
	name = gh_scm2newstr(s_name, &namel);
	res = filter_add_node(net, filter, name);
	free(name);
	if (res == -1)
		return SCM_BOOL_F;
	return s_filter;
}

static SCM gls_filter_connect(SCM s_source, SCM s_source_port,
			      SCM s_dest, SCM s_dest_port)
{
	filter_pipe_t *p;
	filter_t *source, *dest;
	char *source_port, *dest_port;
	int source_portl, dest_portl;

	source = scm2filter(s_source);
	dest = scm2filter(s_dest);
	source_port = gh_scm2newstr(s_source_port, &source_portl);
	dest_port = gh_scm2newstr(s_dest_port, &dest_portl);
        p = filterport_connect(filterportdb_get_port(filter_portdb(source),
						     source_port),
			       filterportdb_get_port(filter_portdb(dest),
						     dest_port));
	free(source_port);
	free(dest_port);
	if (!p)
		return SCM_BOOL_F;
	return pipe2scm(p);
}

static SCM gls_filterparam_set(filter_paramdb_t *db, SCM s_label, SCM s_val)
{
	filter_param_t *param;
	char *label, *str;
	int labell, strl, i, res;
	float f;

	label = gh_scm2newstr(s_label, &labell);
	param = filterparamdb_get_param(db, label);
	if (!param) {
		res = -1;
	} else if (FILTER_PARAM_IS_INT(param)) {
		i = gh_scm2long(s_val);
		res = filterparam_set(param, &i);
	} else if (FILTER_PARAM_IS_FLOAT(param)
		   || FILTER_PARAM_IS_SAMPLE(param)) {
		f = gh_scm2double(s_val);
		res = filterparam_set(param, &f);
	} else if (FILTER_PARAM_IS_STRING(param)) {
		str = gh_scm2newstr(s_val, &strl);
		res = filterparam_set(param, &str);
		free(str);
	} else {
		res = -1;
	}
	free(label);
	if (res == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_filternode_set_param(SCM s_n, SCM s_label, SCM s_val)
{
	filter_t *n;

	n = scm2filter(s_n);
	return gls_filterparam_set(filter_paramdb(n), s_label, s_val);
}

static SCM gls_filterpipe_set_sourceparam(SCM s_p, SCM s_label, SCM s_val)
{
	filter_pipe_t *p;

	p = scm2pipe(s_p);
	return gls_filterparam_set(filterpipe_sourceparamdb(p), s_label, s_val);
}

static SCM gls_filterpipe_set_destparam(SCM s_p, SCM s_label, SCM s_val)
{
	filter_pipe_t *p;

	p = scm2pipe(s_p);
	return gls_filterparam_set(filterpipe_destparamdb(p), s_label, s_val);
}


static SCM gls_filter_launch(SCM s_net)
{
	filter_t *net;

	net = scm2filter(s_net);
	if (filter_launch(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_filter_start(SCM s_net)
{
	filter_t *net;

	net = scm2filter(s_net);
	if (filter_start(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_filter_pause(SCM s_net)
{
	filter_t *net;

	net = scm2filter(s_net);
	if (filter_pause(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static filter_t *waitingnet = NULL;
void killnet(int sig)
{
	DPRINTF("got SIGINT - trying to terminate network\n");
	if (waitingnet)
		filter_terminate(waitingnet);
}
static SCM gls_filter_wait(SCM s_net)
{
	filter_t *net;
	struct sigaction sa, oldsa;
	int res;

	net = scm2filter(s_net);

	/* We need to do some tricks to allow for SIGINT to
	 * interrupt the network and return to the guile console. */
	waitingnet = net;
	sa.sa_handler = killnet;
	sa.sa_flags = 0;
	sigaction(SIGINT, &sa, &oldsa);

	/* Do the actual wait. */
	res = filter_wait(net);

	/* Cleanup. */
	sigaction(SIGINT, &oldsa, NULL);

	if (res == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_filter_terminate(SCM s_net)
{
	filter_t *net;

	net = scm2filter(s_net);
	filter_terminate(net);
	return SCM_UNSPECIFIED;
}


static SCM gls_filternetwork_add_input(SCM s_net, SCM s_node, SCM s_port,
				       SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_port_t *d, *destp;
	char *port, *label, *desc;
	int portl, labell, descl;

	net = scm2filter(s_net);
	n = scm2filter(s_node);
	port = gh_scm2newstr(s_port, &portl);
	label = gh_scm2newstr(s_label, &labell);
	desc = gh_scm2newstr(s_desc, &descl);
	destp = filterportdb_get_port(filter_portdb(n), port);
	d = filterportdb_add_port(filter_portdb(net), label,
				  filterport_type(destp),
				  FILTER_PORTFLAG_INPUT,
				  FILTERPORT_DESCRIPTION, desc,
				  FILTERPORT_MAP_NODE, filter_name(n),
				  FILTERPORT_MAP_LABEL, port,
				  FILTERPORT_END);
	free(port);
	free(label);
	free(desc);
	if (!d)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_filternetwork_add_output(SCM s_net, SCM s_node, SCM s_port,
				 SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_port_t *d, *destp;
	char *port, *label, *desc;
	int portl, labell, descl;

	net = scm2filter(s_net);
	n = scm2filter(s_node);
	port = gh_scm2newstr(s_port, &portl);
	label = gh_scm2newstr(s_label, &labell);
	desc = gh_scm2newstr(s_desc, &descl);
	destp = filterportdb_get_port(filter_portdb(n), port);
	d = filterportdb_add_port(filter_portdb(net), label,
				  filterport_type(destp),
				  FILTER_PORTFLAG_OUTPUT,
				  FILTERPORT_DESCRIPTION, desc,
				  FILTERPORT_MAP_NODE, filter_name(n),
				  FILTERPORT_MAP_LABEL, port,
				  FILTERPORT_END);
	free(port);
	free(label);
	free(desc);
	if (!d)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_filternetwork_add_param(SCM s_net, SCM s_node, SCM s_param,
				SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_param_t *p, *destp;
	char *param, *label, *desc;
	int paraml, labell, descl;

	net = scm2filter(s_net);
	n = scm2filter(s_node);
	param = gh_scm2newstr(s_param, &paraml);
	label = gh_scm2newstr(s_label, &labell);
	desc = gh_scm2newstr(s_desc, &descl);
	destp = filterparamdb_get_param(filter_paramdb(n), param);
	p = filterparamdb_add_param(filter_paramdb(net),
				    label, filterparam_type(destp),
				    filterparam_val(destp),
				    FILTERPARAM_DESCRIPTION, desc,
				    FILTERPARAM_MAP_NODE, n->name,
				    FILTERPARAM_MAP_LABEL, param,
				    FILTERPARAM_END);
	free(param);
	free(label);
	free(desc);
	if (!p)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

/* Generic filter*_t methods:
 * - property setting/querying
 * - object deletion
 * - sub-object get (filters, ports, params, pipes)
 */

static SCM gls_get_name(SCM s_obj)
{
	SCM_ASSERT(filter_p(s_obj) || param_p(s_obj) || port_p(s_obj),
		   s_obj, SCM_ARG1, "get_name");
	if (filter_p(s_obj)) {
		filter_t *filter = scm2filter(s_obj);
		return gh_str02scm(filter_name(filter));
	} else if (param_p(s_obj)) {
		filter_param_t *param = scm2param(s_obj);
		return gh_str02scm(filterparam_label(param));
	} else if (port_p(s_obj)) {
		filter_port_t *port = scm2port(s_obj);
		return gh_str02scm(filterport_label(port));
	}
	return SCM_BOOL_F;
}

static SCM gls_set_property(SCM s_obj, SCM s_label, SCM s_value)
{
	char *label, *value;
	int llabel, lvalue;
	SCM_ASSERT(filter_p(s_obj) || param_p(s_obj) || port_p(s_obj),
		   s_obj, SCM_ARG1, "set_property");
	label = gh_scm2newstr(s_label, &llabel);
	value = gh_scm2newstr(s_value, &lvalue);	
	if (filter_p(s_obj)) {
		filter_t *filter = scm2filter(s_obj);
		filter_set_property(filter, label, value);
	} else if (param_p(s_obj)) {
		filter_param_t *param = scm2param(s_obj);
		filterparam_set_property(param, label, value);
	} else if (port_p(s_obj)) {
		filter_port_t *port = scm2port(s_obj);
		filterport_set_property(port, label, value);
	}
	return SCM_BOOL_T;
}

static SCM gls_get_property(SCM s_obj, SCM s_label)
{
	char *label, *value = NULL;
	int llabel;
	SCM_ASSERT(filter_p(s_obj) || param_p(s_obj) || port_p(s_obj),
		   s_obj, SCM_ARG1, "get_property");
	label = gh_scm2newstr(s_label, &llabel);
	if (filter_p(s_obj)) {
		filter_t *filter = scm2filter(s_obj);
		value = filter_get_property(filter, label);
	} else if (param_p(s_obj)) {
		filter_param_t *param = scm2param(s_obj);
		value = filterparam_get_property(param, label);
	} else if (port_p(s_obj)) {
		filter_port_t *port = scm2port(s_obj);
		value = filterport_get_property(port, label);
	}
	if (!value)
		return SCM_BOOL_F;
	return gh_str02scm(value);
}

static SCM gls_delete(SCM s_obj)
{
	SCM_ASSERT(filter_p(s_obj) || param_p(s_obj) || port_p(s_obj)
		   || pipe_p(s_obj),
		   s_obj, SCM_ARG1, "delete");
	if (filter_p(s_obj)) {
		filter_t *filter = scm2filter(s_obj);
		filter_delete(filter);
		scminvalidatefilter(s_obj);
	} else if (param_p(s_obj)) {
		filter_param_t *param = scm2param(s_obj);
		filterparam_delete(param);
		scminvalidateparam(s_obj);
	} else if (port_p(s_obj)) {
		filter_port_t *port = scm2port(s_obj);
		filterport_delete(port);
		scminvalidateport(s_obj);
	} else if (pipe_p(s_obj)) {
		filter_pipe_t *pipe = scm2pipe(s_obj);
		filterpipe_delete(pipe);
		scminvalidatepipe(s_obj);
	}
	return SCM_UNSPECIFIED;
}

static SCM gls_get_nodes(SCM s_obj)
{
	SCM_ASSERT(filter_p(s_obj),
		   s_obj, SCM_ARG1, "get_nodes");
	if (filter_p(s_obj)) {
		filter_t *filter = scm2filter(s_obj);
		filter_t *node;
		SCM s_nodelist = SCM_LIST0;
		filter_foreach_node(filter, node)
			s_nodelist = gh_cons(filter2scm(node), s_nodelist);
		return s_nodelist;
	}
	return SCM_BOOL_F;
}

static SCM gls_get_ports(SCM s_obj)
{
	SCM_ASSERT(filter_p(s_obj),
		   s_obj, SCM_ARG1, "get_ports");
	if (filter_p(s_obj)) {
		filter_t *filter = scm2filter(s_obj);
		filter_port_t *port;
		SCM s_portlist = SCM_LIST0;
		filterportdb_foreach_port(filter_portdb(filter), port)
			s_portlist = gh_cons(port2scm(port), s_portlist);
		return s_portlist;
	}
	return SCM_BOOL_F;
}

static SCM gls_get_params(SCM s_obj)
{
	SCM_ASSERT(filter_p(s_obj) || port_p(s_obj) || pipe_p(s_obj),
		   s_obj, SCM_ARG1, "get_params");
	if (filter_p(s_obj)) {
		filter_t *filter = scm2filter(s_obj);
		filter_param_t *param;
		SCM s_paramlist = SCM_LIST0;
		filterparamdb_foreach_param(filter_paramdb(filter), param)
			s_paramlist = gh_cons(param2scm(param), s_paramlist);
		return s_paramlist;
	} else if (port_p(s_obj)) {
		filter_port_t *port = scm2port(s_obj);
		filter_param_t *param;
		SCM s_paramlist = SCM_LIST0;
		filterparamdb_foreach_param(filterport_paramdb(port), param)
			s_paramlist = gh_cons(param2scm(param), s_paramlist);
		return s_paramlist;
	} else if (pipe_p(s_obj)) {
		filter_pipe_t *pipe = scm2pipe(s_obj);
		filter_param_t *param;
		SCM s_sparamlist = SCM_LIST0;
		SCM s_dparamlist = SCM_LIST0;
		filterparamdb_foreach_param(filterpipe_sourceparamdb(pipe), param)
			s_sparamlist = gh_cons(param2scm(param), s_sparamlist);
		filterparamdb_foreach_param(filterpipe_destparamdb(pipe), param)
			s_dparamlist = gh_cons(param2scm(param), s_dparamlist);
		return gh_cons(s_sparamlist, s_dparamlist);
	}
	return SCM_BOOL_F;
}

static SCM gls_get_pipes(SCM s_obj)
{
	SCM_ASSERT(port_p(s_obj),
		   s_obj, SCM_ARG1, "get_pipes");
	if (port_p(s_obj)) {
		filter_port_t *port = scm2port(s_obj);
		filter_pipe_t *pipe;
		SCM s_pipelist = SCM_LIST0;
		filterport_foreach_pipe(port, pipe)
			s_pipelist = gh_cons(pipe2scm(pipe), s_pipelist);
		return s_pipelist;
	}
	return SCM_BOOL_F;
}


/* The scriptable plugin API part.
 */

static SCM gls_plugin_add_path(SCM s_path)
{
	char *path;
	int pathl, res;

	path = gh_scm2newstr(s_path, &pathl);
	res = plugin_add_path(path);
	free(path);
	if (res == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_plugin_get(SCM s_name)
{
	plugin_t *p;
	char *name;
	int namel;

	name = gh_scm2newstr(s_name, &namel);
	p = plugin_get(name);
	free(name);
	return plugin2scm(p);
}

static SCM gls_plugin_name(SCM s_p)
{
	plugin_t *p;

	p = scm2plugin(s_p);
	return gh_str02scm((char *)plugin_name(p));
}

static SCM gls_plugin_query_string(SCM s_p, SCM s_key)
{
	plugin_t *p;
	char *key;
	int keyl;
	void *val;

	p = scm2plugin(s_p);
	key = gh_scm2newstr(s_key, &keyl);
	val = plugin_query(p, key);
	free(key);
	if (!val)
		return SCM_BOOL_F;
	return gh_str02scm((const char *)val);
}

static SCM gls_plugin_set_string(SCM s_p, SCM s_key, SCM s_val)
{
	plugin_t *p;
	char *key;
	char *val;
	int keyl, vall;

	p = scm2plugin(s_p);
	key = gh_scm2newstr(s_key, &keyl);
	val = gh_scm2newstr(s_val, &vall);
	plugin_set(p, key, val);
	free(key);
	return SCM_BOOL_T;
}


static SCM gls_glame_plugin_define(SCM s_net, SCM s_name)
{
	filter_t *f;
	plugin_t *p;
	char *name;
	int namel;

	f = scm2filter(s_net);
	name = gh_scm2newstr(s_name, &namel);
	if (glscript_load_mode == 0) {
		p = glame_create_plugin(f, name);
		free(name);
		if (!p)
			return SCM_BOOL_F;
		return plugin2scm(p);
	}
	free(name);
	return filter2scm(f);
}


int glscript_init_filter()
{
	/* Register the filter SMOB to guile. */
	filter_smob_tag = scm_make_smob_type("filter",
					     sizeof(struct filter_smob));
	scm_set_smob_free(filter_smob_tag, free_filter);
	scm_set_smob_print(filter_smob_tag, print_filter);
	scm_set_smob_equalp(filter_smob_tag, equalp_filter);
	gh_new_procedure("filter?", (SCM (*)())filterp, 1, 0, 0);

	/* Register the pipe, param, port and plugin SMOB to guile. */
	pipe_smob_tag = scm_make_smob_type("pipe",
					   sizeof(struct pointer_smob));
	scm_set_smob_print(pipe_smob_tag, print_pointer);
	scm_set_smob_equalp(pipe_smob_tag, equalp_pointer);
	port_smob_tag = scm_make_smob_type("port",
					   sizeof(struct pointer_smob));
	scm_set_smob_print(port_smob_tag, print_pointer);
	scm_set_smob_equalp(port_smob_tag, equalp_pointer);
	param_smob_tag = scm_make_smob_type("param",
					    sizeof(struct pointer_smob));
	scm_set_smob_print(param_smob_tag, print_param);
	scm_set_smob_equalp(param_smob_tag, equalp_pointer);
	plugin_smob_tag = scm_make_smob_type("plugin",
					     sizeof(struct pointer_smob));
	scm_set_smob_print(plugin_smob_tag, print_pointer);
	scm_set_smob_equalp(plugin_smob_tag, equalp_pointer);


	/* filter */
	gh_new_procedure1_0("get_name", gls_get_name);
	gh_new_procedure3_0("set_property", gls_set_property);
	gh_new_procedure2_0("get_property", gls_get_property);
	gh_new_procedure1_0("delete", gls_delete);
	gh_new_procedure1_0("get_nodes", gls_get_nodes);
	gh_new_procedure1_0("get_ports", gls_get_ports);
	gh_new_procedure1_0("get_params", gls_get_params);
	gh_new_procedure1_0("get_pipes", gls_get_pipes);

	gh_new_procedure1_0("filter_p", filterp);
	gh_new_procedure0_1("filter_creat", gls_filter_creat);
	gh_new_procedure1_0("filter_instantiate", gls_filter_instantiate);
	gh_new_procedure2_0("glame_create_plugin", gls_create_plugin);
	gh_new_procedure1_0("filter_to_string", gls_filter_to_string);

	gh_new_procedure3_0("filter_add_node", gls_filter_add_node);
	gh_new_procedure4_0("filter_connect", gls_filter_connect);

	gh_new_procedure3_0("filternode_set_param", gls_filternode_set_param);
	gh_new_procedure3_0("filterpipe_set_sourceparam", gls_filterpipe_set_sourceparam);
	gh_new_procedure3_0("filterpipe_set_destparam", gls_filterpipe_set_destparam);

	gh_new_procedure1_0("filter_launch", gls_filter_launch);
	gh_new_procedure1_0("filter_start", gls_filter_start);
	gh_new_procedure1_0("filter_pause", gls_filter_pause);
	gh_new_procedure1_0("filter_wait", gls_filter_wait);
	gh_new_procedure1_0("filter_terminate", gls_filter_terminate);

	gh_new_procedure5_0("filternetwork_add_input", gls_filternetwork_add_input);
	gh_new_procedure5_0("filternetwork_add_output", gls_filternetwork_add_output);
	gh_new_procedure5_0("filternetwork_add_param", gls_filternetwork_add_param);


	/* plugin */
	gh_new_procedure1_0("plugin_add_path", gls_plugin_add_path);
	gh_new_procedure1_0("plugin_get", gls_plugin_get);
	gh_new_procedure1_0("plugin_name", gls_plugin_name);
	gh_new_procedure2_0("plugin_query", gls_plugin_query_string);
	gh_new_procedure3_0("plugin_set", gls_plugin_set_string);

	gh_define("PLUGIN_DESCRIPTION", gh_str02scm(PLUGIN_DESCRIPTION));
	gh_define("PLUGIN_PIXMAP", gh_str02scm(PLUGIN_PIXMAP));
	gh_define("PLUGIN_CATEGORY", gh_str02scm(PLUGIN_CATEGORY));

	/* HACK */
	gh_new_procedure2_0("glame_plugin_define", gls_glame_plugin_define);

	return 0;
}
