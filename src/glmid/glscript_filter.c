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


extern void *scm2pointer(SCM pointer_smob);
extern SCM pointer2scm(void *pointer);


/* SMOB for filter_t.
 */

static long filter_smob_tag = 0;
struct filter_smob {
	filter_t *filter;
};
#define SCM2FILTERSMOB(s) ((struct filter_smob *)SCM_CDR(s))
static SCM filter2scm(filter_t *filter);
static filter_t *scm2filter(SCM filter_smob);

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

static filter_t *scm2filter(SCM filter_smob)
{
	struct filter_smob *filter = SCM2FILTERSMOB(filter_smob);

	SCM_ASSERT((SCM_NIMP(filter_smob)
		    && SCM_CAR(filter_smob) == filter_smob_tag),
		   filter_smob, SCM_ARG1, "scm2filter");

	return filter->filter;
}

void scminvalidatefilter(SCM filter_smob)
{
	struct filter_smob *filter = SCM2FILTERSMOB(filter_smob);

	SCM_ASSERT((SCM_NIMP(filter_smob)
		    && SCM_CAR(filter_smob) == filter_smob_tag),
		   filter_smob, SCM_ARG1, "scminvalidatefilter");

	filter->filter = NULL;
}

static SCM filterp(SCM filter_smob)
{
	if (!SCM_NIMP(filter_smob)
	    || SCM_CAR(filter_smob) != filter_smob_tag)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}




/* The scriptable filter API part.
 */

static SCM gls_filternetwork_new()
{
	return filter2scm(filter_creat(NULL));
}

static SCM gls_filternetwork_add_node(SCM s_net, SCM s_filter, SCM s_name)
{
	filter_t *net;
	filter_t *node;
	char *filter, *name;
	int filterl, namel;
	int res;

	net = scm2filter(s_net);
	name = gh_scm2newstr(s_name, &namel);
	if (namel == 0) {
		free(name);
		return SCM_BOOL_F;
	}
	filter = gh_scm2newstr(s_filter, &filterl);
	node = filter_instantiate(plugin_get(filter));
	free(filter);
	res = filter_add_node(net, node, name);
	free(name);
	if (res == -1)
		return SCM_BOOL_F;
	return filter2scm(node);
}

static SCM gls_filternetwork_delete_node(SCM s_node)
{
	filter_t *node;

	node = (filter_t *)scm2filter(s_node);
	filter_delete(node);
	scminvalidatefilter(s_node);
	return SCM_UNSPECIFIED;
}

static SCM gls_filternetwork_add_connection(SCM s_source, SCM s_source_port,
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
	return pointer2scm(p);
}

static SCM gls_filternetwork_break_connection(SCM s_p)
{
	filter_pipe_t *p;

	p = scm2pointer(s_p);
	filterpipe_delete(p);
	return SCM_UNSPECIFIED;
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
		res = filterparam_set(param, str);
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

	p = scm2pointer(s_p);
	return gls_filterparam_set(filterpipe_sourceparamdb(p), s_label, s_val);
}

static SCM gls_filterpipe_set_destparam(SCM s_p, SCM s_label, SCM s_val)
{
	filter_pipe_t *p;

	p = scm2pointer(s_p);
	return gls_filterparam_set(filterpipe_destparamdb(p), s_label, s_val);
}

static SCM gls_filternetwork_launch(SCM s_net)
{
	filter_t *net;

	net = scm2filter(s_net);
	if (filter_launch(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_filternetwork_start(SCM s_net)
{
	filter_t *net;

	net = scm2filter(s_net);
	if (filter_start(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_filternetwork_pause(SCM s_net)
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
static SCM gls_filternetwork_wait(SCM s_net)
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

static SCM gls_filternetwork_terminate(SCM s_net)
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

static SCM gls_filternetwork_to_filter(SCM s_net, SCM s_name, SCM s_desc)
{
	filter_t *net;
	plugin_t *p;
	char *name, *desc;
	int namel, descl;

	net = scm2filter(s_net);
	name = gh_scm2newstr(s_name, &namel);
	if (!(p = plugin_add(name))) {
		free(name);
		return SCM_BOOL_F;
	}
	filter_register(net, p);
	desc = gh_scm2newstr(s_desc, &descl);
	plugin_set(p, PLUGIN_DESCRIPTION, desc);
	free(name);
	free(desc);
	return SCM_BOOL_T;
}

static SCM gls_filternetwork_to_string(SCM s_net)
{
	filter_t *net;

	net = scm2filter(s_net);
	return gh_str02scm((char *)filter_to_string(net));
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
	return pointer2scm(p);
}

static SCM gls_plugin_name(SCM s_p)
{
	plugin_t *p;

	p = scm2pointer(s_p);
	return gh_str02scm((char *)plugin_name(p));
}

static SCM gls_plugin_query_string(SCM s_p, SCM s_key)
{
	plugin_t *p;
	char *key;
	int keyl;
	void *val;

	p = scm2pointer(s_p);
	key = gh_scm2newstr(s_key, &keyl);
	val = plugin_query(p, key);
	free(key);
	if (!val)
		return SCM_BOOL_F;
	return gh_str02scm((const char *)val);
}



int glscript_init_filter()
{
	/* Register the filter SMOB to guile. */
	filter_smob_tag = scm_make_smob_type("filter", sizeof(struct filter_smob));
	scm_set_smob_free(filter_smob_tag, free_filter);
	scm_set_smob_print(filter_smob_tag, print_filter);
	scm_set_smob_equalp(filter_smob_tag, equalp_filter);
	gh_new_procedure("filter?", (SCM (*)())filterp, 1, 0, 0);

	/* filter */
	gh_new_procedure("filternetwork_new",
			 (SCM (*)())gls_filternetwork_new, 0, 0, 0);
	gh_new_procedure("filternetwork_add_node",
			 (SCM (*)())gls_filternetwork_add_node, 3, 0, 0);
	gh_new_procedure("filternetwork_delete_node",
			 (SCM (*)())gls_filternetwork_delete_node, 1, 0, 0);
	gh_new_procedure("filternetwork_add_connection",
			 (SCM (*)())gls_filternetwork_add_connection, 4, 0, 0);
	gh_new_procedure("filternetwork_break_connection",
			 (SCM (*)())gls_filternetwork_break_connection, 1, 0, 0);
	gh_new_procedure("filternode_set_param",
			 (SCM (*)())gls_filternode_set_param, 3, 0, 0);
	gh_new_procedure("filterpipe_set_sourceparam",
			 (SCM (*)())gls_filterpipe_set_sourceparam, 3, 0, 0);
	gh_new_procedure("filterpipe_set_destparam",
			 (SCM (*)())gls_filterpipe_set_destparam, 3, 0, 0);
	gh_new_procedure("filternetwork_launch",
			 (SCM (*)())gls_filternetwork_launch, 1, 0, 0);
	gh_new_procedure("filternetwork_start",
			 (SCM (*)())gls_filternetwork_start, 1, 0, 0);
	gh_new_procedure("filternetwork_pause",
			 (SCM (*)())gls_filternetwork_pause, 1, 0, 0);
	gh_new_procedure("filternetwork_wait",
			 (SCM (*)())gls_filternetwork_wait, 1, 0, 0);
	gh_new_procedure("filternetwork_terminate",
			 (SCM (*)())gls_filternetwork_terminate, 1, 0, 0);
	gh_new_procedure("filternetwork_to_filter",
			 (SCM (*)())gls_filternetwork_to_filter, 3, 0, 0);
	gh_new_procedure("filternetwork_add_input",
			 (SCM (*)())gls_filternetwork_add_input, 5, 0, 0);
	gh_new_procedure("filternetwork_add_output",
			 (SCM (*)())gls_filternetwork_add_output, 5, 0, 0);
	gh_new_procedure("filternetwork_add_param",
			 (SCM (*)())gls_filternetwork_add_param, 5, 0, 0);
	gh_new_procedure("filternetwork_to_string",
			 (SCM (*)())gls_filternetwork_to_string, 1, 0, 0);

	/* plugin */
	gh_new_procedure("plugin_add_path",
			 (SCM (*)())gls_plugin_add_path, 1, 0, 0);
	gh_new_procedure("plugin_get",
			 (SCM (*)())gls_plugin_get, 1, 0, 0);
	gh_new_procedure("plugin_name",
			 (SCM (*)())gls_plugin_name, 1, 0, 0);
	gh_new_procedure("plugin_query",
			 (SCM (*)())gls_plugin_query_string, 2, 0, 0);

	return 0;
}
