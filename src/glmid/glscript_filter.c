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
filter_t *last_loaded_filter_instance;


/* SMOBs for filter_pipe_t, filter_param_t, filter_port_t and plugint_t.
 */

long pipe_smob_tag;
#define scm2pipe(s) ((filter_pipe_t *)scm2pointer(s, pipe_smob_tag))
#define pipe2scm(p) pointer2scm(p, pipe_smob_tag)
#define scminvalidatepipe(s) scminvalidatepointer(s, pipe_smob_tag)
#define pipe_p(s) (SCM_NIMP(s) && SCM_CAR(s) == pipe_smob_tag)

long port_smob_tag;
#define scm2port(s) ((filter_port_t *)scm2pointer(s, port_smob_tag))
#define port2scm(p) pointer2scm(p, port_smob_tag)
#define scminvalidateport(s) scminvalidatepointer(s, port_smob_tag)
#define port_p(s) (SCM_NIMP(s) && SCM_CAR(s) == port_smob_tag)

long param_smob_tag;
#define scm2param(s) ((filter_param_t *)scm2pointer(s, param_smob_tag))
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

long plugin_smob_tag;
#define scm2plugin(s) ((plugin_t *)scm2pointer(s, plugin_smob_tag))
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
SCM filter2scm(filter_t *filter);
filter_t *scm2filter(SCM filter_smob);

static scm_sizet free_filter(SCM filter_smob)
{
	struct filter_smob *filter = SCM2FILTERSMOB(filter_smob);

	/* Delete the filter, if it is neither plugin, nor part
	 * of a network and not running. */
	if (filter->filter
	    && !FILTER_IS_PART_OF_NETWORK(filter->filter)
	    && !FILTER_IS_PLUGIN(filter->filter)
	    && filter_is_ready(filter->filter) != 0) {
		DPRINTF("GCing %p (%s)\n", filter->filter,
			filter_name(filter->filter));
		filter_delete(filter->filter);
		filter->filter = NULL;
	}

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


SCM filter2scm(filter_t *filter)
{
	struct filter_smob *smob;
	SCM filter_smob;

	if (!filter)
		GLAME_THROW();

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





/* The scriptable filter API part.
 */

static SCM gls_is_filter(SCM s_filter)
{
	if (filter_p(s_filter))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_filter_new(SCM s_object)
{
	if (filter_p(s_object))
		return filter2scm(filter_creat(scm2filter(s_object)));
	else if (plugin_p(s_object))
		return filter2scm(filter_instantiate(scm2plugin(s_object)));
	else if (SCM_UNBNDP(s_object))
		return filter2scm(filter_creat(NULL));
	scm_wrong_type_arg("filter-new", SCM_ARG1, s_object);
}

static SCM gls_filter_delete(SCM s_obj)
{
	SCM_ASSERT(filter_p(s_obj), s_obj, SCM_ARG1, "filter-delete");
	filter_delete(scm2filter(s_obj));
	scminvalidatefilter(s_obj);
	return SCM_UNSPECIFIED;
}

static SCM gls_filter_remove(SCM s_obj)
{
	SCM_ASSERT(filter_p(s_obj), s_obj, SCM_ARG1, "filter-remove");
	filter_remove(scm2filter(s_obj));
	return SCM_UNSPECIFIED;
}

static SCM gls_filter_to_string(SCM s_net)
{
	SCM_ASSERT(filter_p(s_net), s_net, SCM_ARG1, "filter_to_string");
	return gh_str02scm((char *)filter_to_string(scm2filter(s_net)));
}

static SCM gls_filter_name(SCM s_obj)
{
	SCM_ASSERT(filter_p(s_obj), s_obj, SCM_ARG1, "filter-name");
	return gh_str02scm(filter_name(scm2filter(s_obj)));
}

static SCM gls_filter_nodes(SCM s_obj)
{
	filter_t *filter;
	filter_t *node;
	SCM s_nodelist = SCM_LIST0;
	SCM_ASSERT(filter_p(s_obj),
		   s_obj, SCM_ARG1, "filter-nodes");
	filter = scm2filter(s_obj);
	filter_foreach_node(filter, node)
		s_nodelist = gh_cons(filter2scm(node), s_nodelist);
	return s_nodelist;
}

static SCM gls_filter_ports(SCM s_obj)
{
	filter_t *filter;
	filter_port_t *port;
	SCM s_portlist = SCM_LIST0;
	SCM_ASSERT(filter_p(s_obj), s_obj, SCM_ARG1, "filter-ports");
	filter = scm2filter(s_obj);
	filterportdb_foreach_port(filter_portdb(filter), port)
		s_portlist = gh_cons(port2scm(port), s_portlist);
	return s_portlist;
}

static SCM gls_filter_params(SCM s_obj)
{
	filter_t *filter;
	filter_param_t *param;
	SCM s_paramlist = SCM_LIST0;
	SCM_ASSERT(filter_p(s_obj), s_obj, SCM_ARG1, "filter-params");
	filter = scm2filter(s_obj);
	filterparamdb_foreach_param(filter_paramdb(filter), param)
		s_paramlist = gh_cons(param2scm(param), s_paramlist);
	return s_paramlist;
}


static SCM gls_is_port(SCM s_port)
{
	if (port_p(s_port))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_port_delete(SCM s_obj)
{
	SCM_ASSERT(port_p(s_obj), s_obj, SCM_ARG1, "port-delete");
	filterport_delete(scm2port(s_obj));
	scminvalidateport(s_obj);
	return SCM_UNSPECIFIED;
}

static SCM gls_port_label(SCM s_obj)
{
	SCM_ASSERT(port_p(s_obj), s_obj, SCM_ARG1, "port-label");
	return gh_str02scm(filterport_label(scm2port(s_obj)));
}

static SCM gls_port_params(SCM s_obj)
{
	filter_port_t *port;
	filter_param_t *param;
	SCM s_paramlist = SCM_LIST0;
	SCM_ASSERT(port_p(s_obj), s_obj, SCM_ARG1, "port-params");
	port = scm2port(s_obj);
	filterparamdb_foreach_param(filterport_paramdb(port), param)
		s_paramlist = gh_cons(param2scm(param), s_paramlist);
	return s_paramlist;
}

static SCM gls_port_pipes(SCM s_obj)
{
	filter_port_t *port;
	filter_pipe_t *pipe;
	SCM s_pipelist = SCM_LIST0;
	SCM_ASSERT(port_p(s_obj), s_obj, SCM_ARG1, "port-pipes");
	port = scm2port(s_obj);
	filterport_foreach_pipe(port, pipe)
		s_pipelist = gh_cons(pipe2scm(pipe), s_pipelist);
	return s_pipelist;
}


static SCM gls_is_pipe(SCM s_pipe)
{
	if (pipe_p(s_pipe))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_is_pipe_sample(SCM s_pipe)
{
	SCM_ASSERT(pipe_p(s_pipe), s_pipe, SCM_ARG1, "pipe-sample?");
	if (filterpipe_type(scm2pipe(s_pipe)) == FILTER_PIPETYPE_SAMPLE)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_is_pipe_fft(SCM s_pipe)
{
	SCM_ASSERT(pipe_p(s_pipe), s_pipe, SCM_ARG1, "pipe-fft?");
	if (filterpipe_type(scm2pipe(s_pipe)) == FILTER_PIPETYPE_FFT)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_is_pipe_ssp(SCM s_pipe)
{
	SCM_ASSERT(pipe_p(s_pipe), s_pipe, SCM_ARG1, "pipe-ssp?");
	if (filterpipe_type(scm2pipe(s_pipe)) == FILTER_PIPETYPE_SSP)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_pipe_delete(SCM s_obj)
{
	SCM_ASSERT(pipe_p(s_obj), s_obj, SCM_ARG1, "pipe-delete");
	filterpipe_delete(scm2pipe(s_obj));
	scminvalidatepipe(s_obj);
	return SCM_UNSPECIFIED;
}

static SCM gls_pipe_samplerate(SCM s_obj)
{
	filter_pipe_t *pipe;
	SCM_ASSERT(pipe_p(s_obj), s_obj, SCM_ARG1, "pipe-samplerate");
	pipe = scm2pipe(s_obj);
	if (filterpipe_type(pipe) == FILTER_PIPETYPE_SAMPLE)
		return gh_long2scm(filterpipe_sample_rate(pipe));
	else if (filterpipe_type(pipe) == FILTER_PIPETYPE_FFT)
		return gh_long2scm(filterpipe_fft_rate(pipe));
	else if (filterpipe_type(pipe) == FILTER_PIPETYPE_SSP)
		return gh_long2scm(filterpipe_ssp_rate(pipe));
	else
		scm_wrong_type_arg("pipe-samplerate", SCM_ARG1, s_obj);

}

static SCM gls_pipe_position(SCM s_obj)
{
	filter_pipe_t *pipe;
	SCM_ASSERT(pipe_p(s_obj), s_obj, SCM_ARG1, "pipe-position");
	pipe = scm2pipe(s_obj);
	if (filterpipe_type(pipe) == FILTER_PIPETYPE_SAMPLE)
		return gh_long2scm(filterpipe_sample_hangle(pipe));
	else if (filterpipe_type(pipe) == FILTER_PIPETYPE_FFT)
		return gh_long2scm(filterpipe_fft_hangle(pipe));
	else
		scm_wrong_type_arg("pipe-position", SCM_ARG1, s_obj);

}

static SCM gls_pipe_source_params(SCM s_obj)
{
	filter_pipe_t *pipe;
	filter_param_t *param;
	SCM s_paramlist = SCM_LIST0;
	SCM_ASSERT(pipe_p(s_obj), s_obj, SCM_ARG1, "pipe-source-params");
	pipe = scm2pipe(s_obj);
	filterparamdb_foreach_param(filterpipe_sourceparamdb(pipe), param)
		s_paramlist = gh_cons(param2scm(param), s_paramlist);
	return s_paramlist;
}

static SCM gls_pipe_dest_params(SCM s_obj)
{
	filter_pipe_t *pipe;
	filter_param_t *param;
	SCM s_paramlist = SCM_LIST0;
	SCM_ASSERT(pipe_p(s_obj), s_obj, SCM_ARG1, "pipe-dest-params");
	pipe = scm2pipe(s_obj);
	filterparamdb_foreach_param(filterpipe_destparamdb(pipe), param)
		s_paramlist = gh_cons(param2scm(param), s_paramlist);
	return s_paramlist;
}


static SCM gls_is_param(SCM s_param)
{
	if (param_p(s_param))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_param_delete(SCM s_obj)
{
	SCM_ASSERT(param_p(s_obj), s_obj, SCM_ARG1, "param-delete");
	filterparam_delete(scm2param(s_obj));
	scminvalidateparam(s_obj);
	return SCM_UNSPECIFIED;
}

static SCM gls_param_label(SCM s_obj)
{
	SCM_ASSERT(param_p(s_obj), s_obj, SCM_ARG1, "param-label");
	return gh_str02scm(filterparam_label(
		(filter_param_t *)scm2param(s_obj)));
}

static SCM gls_param_to_string(SCM s_param)
{
	SCM_ASSERT(param_p(s_param), s_param, SCM_ARG1, "param->string");
	return gh_str02scm(filterparam_to_string(scm2param(s_param)));
}

static SCM gls_param_value(SCM s_param)
{
	filter_param_t *param;
	SCM_ASSERT(param_p(s_param), s_param, SCM_ARG1, "param-value");
	param = scm2param(s_param);
	if (FILTER_PARAM_IS_LONG(param))
		return gh_long2scm(filterparam_val_long(param));
	else if (FILTER_PARAM_IS_DOUBLE(param))
		return gh_double2scm(filterparam_val_double(param));
	else if (FILTER_PARAM_IS_STRING(param))
		return gh_str02scm(filterparam_val_string(param));
	scm_wrong_type_arg("param-value", SCM_ARG1, s_param);
}

static SCM gls_param_set(SCM s_param, SCM s_val)
{
	filter_param_t *param;
	int res;
	SCM_ASSERT(param_p(s_param), s_param, SCM_ARG1, "param-set!");
	param = scm2param(s_param);
	if (!FILTER_PARAM_IS_STRING(param)
	    && gh_string_p(s_param)) {
		char *str;
		int strl;
		str = gh_scm2newstr(s_val, &strl);
		res = filterparam_set_string(param, str);
		free(str);
	} else if (FILTER_PARAM_IS_LONG(param)) {
		long i;
		SCM_ASSERT(gh_exact_p(s_val), s_val, SCM_ARG2, "param-set!");
		i = gh_scm2long(s_val);
		if (filterparam_type(param) != FILTER_PARAMTYPE_POS)
			res = filterparam_set(param, &i);
	} else if (FILTER_PARAM_IS_DOUBLE(param)) {
		double f;
		SCM_ASSERT(gh_number_p(s_val), s_val, SCM_ARG2, "param-set!");
		f = gh_scm2double(s_val);
		res = filterparam_set(param, &f);
	} else if (FILTER_PARAM_IS_STRING(param)) {
		char *str;
		int strl;
		SCM_ASSERT(gh_string_p(s_val), s_val, SCM_ARG2, "param-set!");
		str = gh_scm2newstr(s_val, &strl);
		res = filterparam_set(param, &str);
		free(str);
	} else if (FILTER_PARAM_IS_BUF(param)) {
		char *str;
		int strl;
		SCM_ASSERT(gh_string_p(s_val), s_val, SCM_ARG2, "param-set!");
		str = gh_scm2newstr(s_val, &strl);
		res = filterparam_set_string(param, str);
		free(str);
	} else
		scm_wrong_type_arg("param-set!", SCM_ARG2, s_val);
	if (res == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}



static SCM gls_set_property(SCM s_obj, SCM s_label, SCM s_value)
{
	char *label, *value;
	int llabel, lvalue;
	SCM_ASSERT(filter_p(s_obj) || param_p(s_obj) || port_p(s_obj),
		   s_obj, SCM_ARG1, "set-property!");
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
		   s_obj, SCM_ARG1, "get-property");
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
		GLAME_THROW();
	return gh_str02scm(value);
}



static SCM gls_filter_launch(SCM s_net, SCM s_bufsize)
{
	SCM_ASSERT(filter_p(s_net), s_net, SCM_ARG1, "filter_launch");
	SCM_ASSERT(SCM_UNBNDP(s_bufsize) || gh_exact_p(s_bufsize),
		   s_bufsize, SCM_ARG2, "filter-launch");
	if (filter_launch(scm2filter(s_net),
			  SCM_UNBNDP(s_bufsize) ? _GLAME_WBUFSIZE : gh_scm2long(s_bufsize)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_filter_start(SCM s_net)
{
	SCM_ASSERT(filter_p(s_net), s_net, SCM_ARG1, "filter_start");
	if (filter_start(scm2filter(s_net)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_filter_pause(SCM s_net)
{
	SCM_ASSERT(filter_p(s_net), s_net, SCM_ARG1, "filter_pause");
	if (filter_pause(scm2filter(s_net)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
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

	SCM_ASSERT(filter_p(s_net), s_net, SCM_ARG1, "filter_wait");
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
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_filter_terminate(SCM s_net)
{
	SCM_ASSERT(filter_p(s_net), s_net, SCM_ARG1, "filter-terminate");
	filter_terminate(scm2filter(s_net));
	return SCM_UNSPECIFIED;
}



static SCM gls_filter_add_node(SCM s_net, SCM s_filter, SCM s_name)
{
	filter_t *net;
	filter_t *filter;
	char *name;
	int namel;
	int res;

	SCM_ASSERT(filter_p(s_net), s_net, SCM_ARG1, "filter-add-node");
	SCM_ASSERT(filter_p(s_filter), s_filter, SCM_ARG2, "filter-add-node");
	SCM_ASSERT(gh_string_p(s_name), s_name, SCM_ARG3, "filter-add-node");
	net = scm2filter(s_net);
	filter = scm2filter(s_filter);
	name = gh_scm2newstr(s_name, &namel);
	res = filter_add_node(net, filter, name);
	free(name);
	if (res == -1)
		GLAME_THROW();
	return s_filter;
}

static SCM gls_filter_expand(SCM s_filter)
{
	SCM_ASSERT(filter_p(s_filter), s_filter, SCM_ARG1, "filter-expand");
	if (filter_expand(scm2filter(s_filter)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_filter_collapse(SCM s_name, SCM s_nodes)
{
	filter_t **nodes, *net;
	SCM s_tail;
	char *name;
	int cnt, i, len;
	SCM_ASSERT(gh_string_p(s_name), s_name, SCM_ARG1, "filter-collapse");
	SCM_ASSERT(gh_list_p(s_nodes), s_nodes, SCM_ARG2, "filter-collapse");

	cnt = gh_length(s_nodes);
	nodes = (filter_t **)alloca((cnt+1)*sizeof(filter_t *));
	s_tail = s_nodes;
	i = 0;
	while (!gh_null_p(s_tail)) {
		SCM_ASSERT(filter_p(gh_car(s_tail)), s_nodes, SCM_ARG2, "filter-collapse");
		nodes[i++] = scm2filter(gh_car(s_tail));
		s_tail = gh_cdr(s_tail);
	}
	nodes[i] = NULL;
	name = gh_scm2newstr(s_name, &len);

	net = filter_collapse(name, nodes);
	free(name);

	if (!net)
		GLAME_THROW();
	return filter2scm(net);
}

static SCM gls_filter_connect(SCM s_source, SCM s_source_port,
			      SCM s_dest, SCM s_dest_port)
{
	filter_pipe_t *p;
	filter_t *source, *dest;
	char *source_port, *dest_port;
	int source_portl, dest_portl;

	SCM_ASSERT(filter_p(s_source), s_source, SCM_ARG1, "filter_connect");
	SCM_ASSERT(gh_string_p(s_source_port), s_source_port,
		   SCM_ARG2, "filter_connect");
	SCM_ASSERT(filter_p(s_dest), s_dest, SCM_ARG3, "filter_connect");
	SCM_ASSERT(gh_string_p(s_dest_port), s_dest_port,
		   SCM_ARG4, "filter_connect");
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
		GLAME_THROW();
	return pipe2scm(p);
}

static SCM gls_filternetwork_add_input(SCM s_net, SCM s_node, SCM s_port,
				       SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_port_t *d, *destp;
	char *port, *label, *desc;
	int portl, labell, descl;

	SCM_ASSERT(filter_p(s_net), s_net,
		   SCM_ARG1, "filternetwork-add-input");
	SCM_ASSERT(filter_p(s_node), s_node,
		   SCM_ARG2, "filternetwork-add-input");
	SCM_ASSERT(gh_string_p(s_port), s_port,
		   SCM_ARG3, "filternetwork-add-input");
	SCM_ASSERT(gh_string_p(s_label), s_label,
		   SCM_ARG4, "filternetwork-add-input");
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
				  FILTERPORT_END);
	filterport_redirect(d, filterportdb_get_port(filter_portdb(n), port));
	free(port);
	free(label);
	free(desc);
	if (!d)
		GLAME_THROW();
	return port2scm(d);
}

static SCM gls_filternetwork_add_output(SCM s_net, SCM s_node, SCM s_port,
				 SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_port_t *d, *destp;
	char *port, *label, *desc;
	int portl, labell, descl;

	SCM_ASSERT(filter_p(s_net), s_net,
		   SCM_ARG1, "filternetwork-add-output");
	SCM_ASSERT(filter_p(s_node), s_node,
		   SCM_ARG2, "filternetwork-add-output");
	SCM_ASSERT(gh_string_p(s_port), s_port,
		   SCM_ARG3, "filternetwork-add-output");
	SCM_ASSERT(gh_string_p(s_label), s_label,
		   SCM_ARG4, "filternetwork-add-output");
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
				  FILTERPORT_END);
	filterport_redirect(d, filterportdb_get_port(filter_portdb(n), port));
	free(port);
	free(label);
	free(desc);
	if (!d)
		GLAME_THROW();
	return port2scm(d);
}

static SCM gls_filternetwork_add_param(SCM s_net, SCM s_node, SCM s_param,
				SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_param_t *p, *destp;
	char *param, *label, *desc;
	int paraml, labell, descl;

	SCM_ASSERT(filter_p(s_net), s_net,
		   SCM_ARG1, "filternetwork-add-param");
	SCM_ASSERT(filter_p(s_node), s_node,
		   SCM_ARG2, "filternetwork-add-param");
	SCM_ASSERT(gh_string_p(s_param), s_param,
		   SCM_ARG3, "filternetwork-add-param");
	SCM_ASSERT(gh_string_p(s_label), s_label,
		   SCM_ARG4, "filternetwork-add-param");
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
				    FILTERPARAM_END);
	filterparam_redirect(p, filterparamdb_get_param(filter_paramdb(n), param));
	free(param);
	free(label);
	free(desc);
	if (!p)
		GLAME_THROW();
	return param2scm(p);
}

static SCM gls_filter_add_param(SCM s_filter, SCM s_label, SCM s_val)
{
	filter_t *filter;
	filter_param_t *param;
	char *label;
	int len;
	SCM_ASSERT(filter_p(s_filter), s_filter,
		   SCM_ARG1, "filter-add-param");
	SCM_ASSERT(gh_string_p(s_label), s_label,
		   SCM_ARG2, "filter-add-param");
	filter = scm2filter(s_filter);
	label = gh_scm2newstr(s_label, &len);
	if (gh_exact_p(s_val)) {
		int val = gh_scm2long(s_val);
		param = filterparamdb_add_param(filter_paramdb(filter),
						label, FILTER_PARAMTYPE_LONG,
						&val, FILTERPARAM_END);
	} else if (gh_number_p(s_val)) {
		float val = gh_scm2double(s_val);
		param = filterparamdb_add_param(filter_paramdb(filter),
						label, FILTER_PARAMTYPE_DOUBLE,
						&val, FILTERPARAM_END);
	} else if (gh_string_p(s_val)) {
		char *val = gh_scm2newstr(s_val, &len);
		param = filterparamdb_add_param(filter_paramdb(filter),
						label, FILTER_PARAMTYPE_STRING,
						&val, FILTERPARAM_END);
		free(val);
	} else
		scm_wrong_type_arg("filter-add-param", SCM_ARG3, s_val);
	free(label);
	if (!param)
		GLAME_THROW();
	return param2scm(param);
}

static SCM gls_create_plugin(SCM s_filter, SCM s_name)
{
	filter_t *filter;
	plugin_t *p;
	char *name;
	int namel;

	SCM_ASSERT(filter_p(s_filter), s_filter, SCM_ARG1, "create_plugin");
	SCM_ASSERT(gh_string_p(s_name), s_name, SCM_ARG2, "create_plugin");
	filter = scm2filter(s_filter);
	name = gh_scm2newstr(s_name, &namel);
	p = glame_create_plugin(filter, name);
	free(name);
	if (!p)
		GLAME_THROW();
	return plugin2scm(p);
}





/* The scriptable plugin API part.
 */

static SCM gls_is_plugin(SCM s_plugin)
{
	if (plugin_p(s_plugin))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_plugin_add_path(SCM s_path)
{
	char *path;
	int pathl, res;

	SCM_ASSERT(gh_string_p(s_path), s_path, SCM_ARG1, "plugin-add-path");
	path = gh_scm2newstr(s_path, &pathl);
	res = plugin_add_path(path);
	free(path);
	if (res == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_plugin_get(SCM s_name)
{
	plugin_t *p;
	char *name;
	int namel;

	SCM_ASSERT(gh_string_p(s_name), s_name, SCM_ARG1, "plugin-get");
	name = gh_scm2newstr(s_name, &namel);
	p = plugin_get(name);
	free(name);
	if (!p)
		GLAME_THROW();
	return plugin2scm(p);
}

static SCM gls_plugin_name(SCM s_p)
{
	plugin_t *p;

	SCM_ASSERT(plugin_p(s_p), s_p, SCM_ARG1, "plugin-name");
	p = scm2plugin(s_p);
	return gh_str02scm((char *)plugin_name(p));
}

static SCM gls_plugin_query_string(SCM s_p, SCM s_key)
{
	plugin_t *p;
	char *key;
	int keyl;
	void *val;

	SCM_ASSERT(plugin_p(s_p), s_p, SCM_ARG1, "plugin-query");
	SCM_ASSERT(gh_string_p(s_key), s_key, SCM_ARG2, "plugin-query");
	p = scm2plugin(s_p);
	key = gh_scm2newstr(s_key, &keyl);
	val = plugin_query(p, key);
	free(key);
	if (!val)
		GLAME_THROW();
	return gh_str02scm((const char *)val);
}

static SCM gls_plugin_set_string(SCM s_p, SCM s_key, SCM s_val)
{
	plugin_t *p;
	char *key;
	char *val;
	int keyl, vall;

	SCM_ASSERT(plugin_p(s_p), s_p, SCM_ARG1, "plugin-set!");
	SCM_ASSERT(gh_string_p(s_key), s_key, SCM_ARG2, "plugin-set!");
	SCM_ASSERT(gh_string_p(s_val), s_val, SCM_ARG3, "plugin-set!");
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

	SCM_ASSERT(filter_p(s_net), s_net, SCM_ARG1, "glame_plugin_define");
	SCM_ASSERT(gh_string_p(s_name), s_name,
		   SCM_ARG2, "glame_plugin_define");
	f = scm2filter(s_net);
	name = gh_scm2newstr(s_name, &namel);
	if (glscript_load_mode == 0) {
		p = glame_create_plugin(f, name);
		free(name);
		if (!p)
			GLAME_THROW();
		return plugin2scm(p);
	}
	free(name);
	last_loaded_filter_instance = filter_creat(f); /* HACK!! */
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

	/* global */
	gh_define("SAMPLE_SIZE", gh_long2scm(SAMPLE_SIZE));

	/* filter */
	gh_new_procedure1_0("filter?", gls_is_filter);
	gh_new_procedure0_1("filter-new", gls_filter_new);
	gh_new_procedure1_0("filter-delete", gls_filter_delete);
	gh_new_procedure1_0("filter-remove", gls_filter_remove);
	gh_new_procedure1_0("filter-name", gls_filter_name);
	gh_new_procedure1_0("filter-nodes", gls_filter_nodes);
	gh_new_procedure1_0("filter-ports", gls_filter_ports);
	gh_new_procedure1_0("filter-params", gls_filter_params);
	gh_new_procedure1_0("filter->string", gls_filter_to_string);

	gh_new_procedure1_1("filter-launch", gls_filter_launch);
	gh_new_procedure1_0("filter-start", gls_filter_start);
	gh_new_procedure1_0("filter-pause", gls_filter_pause);
	gh_new_procedure1_0("filter-wait", gls_filter_wait);
	gh_new_procedure1_0("filter-terminate", gls_filter_terminate);

	gh_new_procedure1_0("port?", gls_is_port);
	gh_new_procedure1_0("port-label", gls_port_label);
	gh_new_procedure1_0("port-delete", gls_port_delete);
	gh_new_procedure1_0("port-pipes", gls_port_pipes);
	gh_new_procedure1_0("port-params", gls_port_params);

	gh_new_procedure1_0("param?", gls_is_param);
	gh_new_procedure1_0("param-label", gls_param_label);
	gh_new_procedure1_0("param-delete", gls_param_delete);
	gh_new_procedure1_0("param->string", gls_param_to_string);
	gh_new_procedure1_0("param-value", gls_param_value);
	gh_new_procedure2_0("param-set!", gls_param_set);

	gh_new_procedure1_0("pipe?", gls_is_pipe);
	gh_new_procedure1_0("pipe-delete", gls_pipe_delete);
	gh_new_procedure1_0("pipe-source-params", gls_pipe_source_params);
	gh_new_procedure1_0("pipe-dest-params", gls_pipe_dest_params);
	gh_new_procedure1_0("pipe-sample?", gls_is_pipe_sample);
	gh_new_procedure1_0("pipe-fft?", gls_is_pipe_fft);
	gh_new_procedure1_0("pipe-ssp?", gls_is_pipe_ssp);
	gh_new_procedure1_0("pipe-samplerate", gls_pipe_samplerate);
	gh_new_procedure1_0("pipe-position", gls_pipe_position);

	gh_new_procedure3_0("set-property!", gls_set_property);
	gh_new_procedure2_0("get-property", gls_get_property);

	gh_define("FILTERPARAM_DESCRIPTION",
		  gh_str02scm(FILTERPARAM_DESCRIPTION));
	gh_define("FILTERPARAM_GLADEXML", gh_str02scm(FILTERPARAM_GLADEXML));
	gh_define("FILTERPARAM_SET_SCM", gh_str02scm(FILTERPARAM_SET_SCM));
	gh_define("FILTERPORT_DESCRIPTION",
		  gh_str02scm(FILTERPORT_DESCRIPTION));


	gh_new_procedure3_0("filter-add-node", gls_filter_add_node);
	gh_new_procedure1_0("filter-expand", gls_filter_expand);
	gh_new_procedure2_0("filter-collapse", gls_filter_collapse);
	gh_new_procedure4_0("filter-connect", gls_filter_connect);
	gh_new_procedure3_0("filter-add-param", gls_filter_add_param);

	gh_new_procedure5_0("filternetwork-add-input",
			    gls_filternetwork_add_input);
	gh_new_procedure5_0("filternetwork-add-output",
			    gls_filternetwork_add_output);
	gh_new_procedure5_0("filternetwork-add-param",
			    gls_filternetwork_add_param);

	gh_new_procedure2_0("glame_create_plugin", gls_create_plugin);



	/* plugin */
	gh_new_procedure1_0("plugin?", gls_is_plugin);
	gh_new_procedure1_0("plugin-add-path", gls_plugin_add_path);
	gh_new_procedure1_0("plugin-get", gls_plugin_get);
	gh_new_procedure1_0("plugin-name", gls_plugin_name);
	gh_new_procedure2_0("plugin-query", gls_plugin_query_string);
	gh_new_procedure3_0("plugin-set!", gls_plugin_set_string);
	/* compatibility */
	gh_new_procedure1_0("plugin_add_path", gls_plugin_add_path);
	gh_new_procedure1_0("plugin_get", gls_plugin_get);
	gh_new_procedure1_0("plugin_name", gls_plugin_name);
	gh_new_procedure2_0("plugin_query", gls_plugin_query_string);
	gh_new_procedure3_0("plugin_set", gls_plugin_set_string);

	gh_define("PLUGIN_DESCRIPTION", gh_str02scm(PLUGIN_DESCRIPTION));
	gh_define("PLUGIN_PIXMAP", gh_str02scm(PLUGIN_PIXMAP));
	gh_define("PLUGIN_CATEGORY", gh_str02scm(PLUGIN_CATEGORY));
	gh_define("PLUGIN_GUI_HELP_PATH", gh_str02scm(PLUGIN_GUI_HELP_PATH));
	gh_define("PLUGIN_LABEL", gh_str02scm(PLUGIN_LABEL));

	/* HACK */
	gh_new_procedure2_0("glame_plugin_define", gls_glame_plugin_define);

	return 0;
}
