/*
 * glscript.c
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

#ifdef HAVE_GUILE

#include <guile/gh.h>
#include <swapfile.h>
#include <filter.h>
#include <glplugin.h>


#define gh_scm2swfd(s) (swfd_t)gh_scm2long(s)
#define gh_scm2txnid(t) (txnid_t)gh_scm2long(t)
#define gh_txnid2scm(t) gh_long2scm((long)(t))
#define gh_scm2off_t gh_scm2long
#define gh_off_t2scm gh_long2scm 


/* The scriptable txn API part.
 */

static SCM gls_txn_start(SCM s_tid)
{
	txnid_t tid;
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	tid = txn_start(gh_scm2txnid(s_tid));
	if (tid == -1)
		return SCM_BOOL_F;
	return gh_txnid2scm(tid);
}

static SCM gls_txn_end(SCM s_tid)
{
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	if (txn_end(gh_scm2txnid(s_tid)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_txn_abort(SCM s_tid)
{
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	if (txn_abort(gh_scm2txnid(s_tid)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_txn_undo(SCM s_tid)
{
	txnid_t tid;
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	tid = txn_undo(gh_scm2txnid(s_tid));
	if (tid == -1)
		return SCM_BOOL_F;
	return gh_txnid2scm(tid);
}

static SCM gls_txn_delete(SCM s_tid)
{
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	if (txn_delete(gh_scm2txnid(s_tid)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}


/* The scriptable swapfile API part.
 * No gls_sw_mmap, gls_sw_munmap.
 */

static SCM gls_swapfile_open(SCM s_name)
{
	char *name;
	int namel, res;

	if (!gh_string_p(s_name))
		return SCM_BOOL_F; /* SCM_ERR? */
	name = gh_scm2newstr(s_name, &namel);
	res = swapfile_open(name, 0);
	free(name);
	if (res == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_swapfile_close()
{
	swapfile_close();
	return SCM_BOOL_T;
}

static SCM gls_swapfile_creat(SCM s_name, SCM s_size)
{
	char *name;
	int namel, res;

	if (!gh_string_p(s_name) || !gh_exact_p(s_size))
		return SCM_BOOL_F; /* SCM_ERR? */
	name = gh_scm2newstr(s_name, &namel);
	res = swapfile_creat(name, gh_scm2long(s_size));
	free(name);
	if (res == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_unlink(SCM s_name)
{
	if (!gh_exact_p(s_name))
		return SCM_BOOL_F; /* SCM_ERR? */
	if (sw_unlink(gh_scm2long(s_name)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_opendir()
{
	return gh_long2scm((long)sw_opendir());
}

static SCM gls_sw_readdir(SCM s_d)
{
	if (!gh_exact_p(s_d))
		return SCM_BOOL_F; /* SCM_ERR */
	return gh_long2scm((long)sw_readdir((SWDIR *)gh_scm2long(s_d)));
}

static SCM gls_sw_closedir(SCM s_d)
{
	if (!gh_exact_p(s_d))
		return SCM_BOOL_F; /* SCM_ERR */
	sw_closedir((SWDIR *)gh_scm2long(s_d));
	return SCM_BOOL_T;
}

static SCM gls_sw_open(SCM s_name, SCM s_flags, SCM s_tid)
{
	swfd_t fd;

	if (!gh_exact_p(s_name) || !gh_exact_p(s_flags)
	    || !gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR */
	fd = sw_open(gh_scm2long(s_name), gh_scm2long(s_flags),
		     gh_scm2long(s_tid));
	if (fd == -1)
		return SCM_BOOL_F;
	return gh_long2scm((long)fd);
}

static SCM gls_sw_close(SCM s_fd)
{
	if (!gh_exact_p(s_fd))
		return SCM_BOOL_F; /* SCM_ERR */
	if (sw_close(gh_scm2swfd(s_fd)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_fstat(SCM s_fd)
{
	struct sw_stat st;

	if (!gh_exact_p(s_fd))
		return SCM_BOOL_F; /* SCM_ERR */
	if (sw_fstat(gh_scm2swfd(s_fd), &st) == -1)
		return SCM_BOOL_F;
	return gh_list(gh_long2scm(st.name), gh_long2scm(st.size),
		       gh_long2scm(st.mode), gh_long2scm(st.offset),
		       gh_long2scm(st.cluster_start),
		       gh_long2scm(st.cluster_end),
		       gh_long2scm(st.cluster_size), SCM_UNDEFINED);
}

static SCM gls_sw_ftruncate(SCM s_fd, SCM s_length)
{
	if (!gh_exact_p(s_fd) || !gh_exact_p(s_length))
		return SCM_BOOL_F; /* SCM_ERR */
	if (sw_ftruncate(gh_scm2swfd(s_fd), gh_scm2long(s_length)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_lseek(SCM s_fd, SCM s_offset, SCM s_whence)
{
	long res;
	if (!gh_exact_p(s_fd) || !gh_exact_p(s_offset)
	    || !gh_exact_p(s_whence))
		return SCM_BOOL_F; /* SCM_ERR */
	res = sw_lseek(gh_scm2swfd(s_fd), gh_scm2long(s_offset),
		       gh_scm2long(s_whence));
	if (res == -1)
		return SCM_BOOL_F;
	return gh_long2scm(res);
}

static SCM gls_sw_sendfile(SCM s_out_fd, SCM s_in_fd, SCM s_count, SCM s_mode)
{
	if (!gh_exact_p(s_out_fd) || !gh_exact_p(s_in_fd)
	    || !gh_exact_p(s_count) || !gh_exact_p(s_mode))
		return SCM_BOOL_F; /* SCM_ERR */
	if (sw_sendfile(gh_scm2swfd(s_out_fd), gh_scm2swfd(s_in_fd),
			gh_scm2long(s_count), gh_scm2long(s_mode)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_read_floatvec(SCM s_fd, SCM s_length)
{
	long length;
	float *m;
	SCM s_vec;

	if (!gh_exact_p(s_fd) || !gh_exact_p(s_length))
		return SCM_BOOL_F; /* SCM_ERR */
	length = gh_scm2long(s_length);
	m = (float *)malloc(length*sizeof(float));
	if (sw_read(gh_scm2swfd(s_fd), m,
		    length*sizeof(float)) != length*sizeof(float)) {
		free(m);
		return SCM_BOOL_F;
	}
	s_vec = gh_floats2fvect(m, length);
	free(m); /* FIXME!?? */
	return s_vec;
}

static SCM gls_sw_read_string(SCM s_fd, SCM s_length)
{
	long length, res;
	char *m;

	if (!gh_exact_p(s_fd) || !gh_exact_p(s_length))
		return SCM_BOOL_F; /* SCM_ERR */
	length = gh_scm2long(s_length);
	m = (char *)malloc(length+1);
	if ((res = sw_read(gh_scm2swfd(s_fd), m, length)) != length) {
		free(m);
		return SCM_BOOL_F;
	}
	m[length] = '\0';
	return gh_str02scm(m);
}

static SCM gls_sw_write(SCM s_fd, SCM s_buf)
{
	long res;

	if (!gh_exact_p(s_fd))
		return SCM_BOOL_F; /* SCM_ERR */
	if (gh_vector_p(s_buf)) {
		float *fvec;
		long length;
		length = gh_vector_length(s_buf);
		fvec = (float *)malloc(length*sizeof(float));
		gh_scm2floats(s_buf, fvec); /* FIXME!? */
		res = sw_write(gh_scm2swfd(s_fd), fvec, length*sizeof(float));
		free(fvec);
		if (res != -1)
			res /= sizeof(float);
	} else if (gh_string_p(s_buf)) {
		char *str;
		int strlen;
		str = gh_scm2newstr(s_buf, &strlen);
		res = sw_write(gh_scm2swfd(s_fd), str, strlen);
		free(str);
	} else
		return SCM_BOOL_F; /* SCM_ERR */
	if (res == -1)
		return SCM_BOOL_F;
	return gh_long2scm(res);
}


/* The scriptable filter API part.
 */

#define gh_scm2pointer (void *)gh_scm2ulong
#define gh_pointer2scm(p) gh_ulong2scm((unsigned long)(p))

SCM gls_filternetwork_new()
{
	filter_t *net;

	net = filter_creat(NULL);
	if (!net)
		return SCM_BOOL_F;
	return gh_pointer2scm(net);
}

SCM gls_filternetwork_delete(SCM s_net)
{
	filter_t *net;

	net = (filter_t *)gh_scm2pointer(s_net);
	filter_delete(net);
	return SCM_UNSPECIFIED;
}

SCM gls_filternetwork_add_node(SCM s_net, SCM s_filter, SCM s_name)
{
	filter_t *net;
	filter_t *node;
	char *filter, *name;
	int filterl, namel;
	int res;

	net = (filter_t *)gh_scm2pointer(s_net);
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
	return gh_pointer2scm(node);
}

SCM gls_filternetwork_delete_node(SCM s_node)
{
	filter_t *node;

	node = (filter_t *)gh_scm2pointer(s_node);
	filter_delete(node);
	return SCM_UNSPECIFIED;
}

SCM gls_filternetwork_add_connection(SCM s_source, SCM s_source_port,
				     SCM s_dest, SCM s_dest_port)
{
	filter_pipe_t *p;
	filter_t *source, *dest;
	char *source_port, *dest_port;
	int source_portl, dest_portl;

	source = (filter_t *)gh_scm2pointer(s_source);
	source_port = gh_scm2newstr(s_source_port, &source_portl);
	dest = (filter_t *)gh_scm2pointer(s_dest);
	dest_port = gh_scm2newstr(s_dest_port, &dest_portl);
        p = filterport_connect(filterportdb_get_port(filter_portdb(source),
						     source_port),
			       filterportdb_get_port(filter_portdb(dest),
						     dest_port));
	free(source_port);
	free(dest_port);
	if (!p)
		return SCM_BOOL_F;
	return gh_pointer2scm(p);
}

SCM gls_filternetwork_break_connection(SCM s_p)
{
	filter_pipe_t *p;

	p = (filter_pipe_t *)gh_scm2pointer(s_p);
	filterpipe_delete(p);
	return SCM_UNSPECIFIED;
}

SCM gls_filterparam_set(filter_paramdb_t *db, SCM s_label, SCM s_val)
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

SCM gls_filternode_set_param(SCM s_n, SCM s_label, SCM s_val)
{
	filter_t *n;

	n = (filter_t *)gh_scm2pointer(s_n);
	return gls_filterparam_set(filter_paramdb(n), s_label, s_val);
}

SCM gls_filterpipe_set_sourceparam(SCM s_p, SCM s_label, SCM s_val)
{
	filter_pipe_t *p;

	p = (filter_pipe_t *)gh_scm2pointer(s_p);
	return gls_filterparam_set(filterpipe_sourceparamdb(p), s_label, s_val);
}

SCM gls_filterpipe_set_destparam(SCM s_p, SCM s_label, SCM s_val)
{
	filter_pipe_t *p;

	p = (filter_pipe_t *)gh_scm2pointer(s_p);
	return gls_filterparam_set(filterpipe_destparamdb(p), s_label, s_val);
}

SCM gls_filternetwork_launch(SCM s_net)
{
	filter_t *net;

	net = (filter_t *)gh_scm2pointer(s_net);
	if (filter_launch(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_start(SCM s_net)
{
	filter_t *net;

	net = (filter_t *)gh_scm2pointer(s_net);
	if (filter_start(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_pause(SCM s_net)
{
	filter_t *net;

	net = (filter_t *)gh_scm2pointer(s_net);
	if (filter_pause(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_wait(SCM s_net)
{
	filter_t *net;

	net = (filter_t *)gh_scm2pointer(s_net);
	if (filter_wait(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_terminate(SCM s_net)
{
	filter_t *net;

	net = (filter_t *)gh_scm2pointer(s_net);
	filter_terminate(net);
	return SCM_UNSPECIFIED;
}

SCM gls_filternetwork_add_input(SCM s_net, SCM s_node, SCM s_port,
				SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_port_t *d, *destp;
	char *port, *label, *desc;
	int portl, labell, descl;

	net = (filter_t *)gh_scm2pointer(s_net);
	n = (filter_t *)gh_scm2pointer(s_node);
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

SCM gls_filternetwork_add_output(SCM s_net, SCM s_node, SCM s_port,
				 SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_port_t *d, *destp;
	char *port, *label, *desc;
	int portl, labell, descl;

	net = (filter_t *)gh_scm2pointer(s_net);
	n = (filter_t *)gh_scm2pointer(s_node);
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

SCM gls_filternetwork_add_param(SCM s_net, SCM s_node, SCM s_param,
				SCM s_label, SCM s_desc)
{
	filter_t *net;
	filter_t *n;
	filter_param_t *p, *destp;
	char *param, *label, *desc;
	int paraml, labell, descl;

	net = (filter_t *)gh_scm2pointer(s_net);
	n = (filter_t *)gh_scm2pointer(s_node);
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

SCM gls_filternetwork_to_filter(SCM s_net, SCM s_name, SCM s_desc)
{
	filter_t *net;
	plugin_t *p;
	char *name, *desc;
	int namel, descl;

	net = (filter_t *)gh_scm2pointer(s_net);
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

SCM gls_filternetwork_to_string(SCM s_net)
{
	filter_t *net;

	net = (filter_t *)gh_scm2pointer(s_net);
	return gh_str02scm((char *)filter_to_string(net));
}


/* The scriptable plugin API part.
 */

SCM gls_plugin_add_path(SCM s_path)
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

SCM gls_plugin_get(SCM s_name)
{
	plugin_t *p;
	char *name;
	int namel;

	name = gh_scm2newstr(s_name, &namel);
	p = plugin_get(name);
	free(name);
	if (!p)
		return SCM_BOOL_F;
	return gh_pointer2scm(p);
}

SCM gls_plugin_name(SCM s_p)
{
	plugin_t *p;

	p = (plugin_t *)gh_scm2pointer(s_p);
	return gh_str02scm((char *)plugin_name(p));
}

SCM gls_plugin_query_string(SCM s_p, SCM s_key)
{
	plugin_t *p;
	char *key;
	int keyl;
	void *val;

	p = (plugin_t *)gh_scm2pointer(s_p);
	key = gh_scm2newstr(s_key, &keyl);
	val = plugin_query(p, key);
	free(key);
	if (!val)
		return SCM_BOOL_F;
	return gh_str02scm((const char *)val);
}


#if 0
/* The scriptable track API part.
 */

SCM gls_track_get(SCM s_group, SCM s_track)
{
	char *group, *track;
	int groupl, trackl;
	track_t *t;

	group = gh_scm2newstr(s_group, &groupl);
	track = gh_scm2newstr(s_track, &trackl);
	t = track_get(group, trackl == 0 ? NULL : track);
	free(group);
	free(track);
	if (!t)
		return SCM_BOOL_F;
	return gh_pointer2scm(t);
}

SCM gls_track_delete(SCM s_track)
{
	track_t *track;

	track = (track_t *)gh_scm2pointer(s_track);
	if (track_delete(track) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}
#endif


int glscript_init()
{
	char str[256];

	/* txn */
	gh_new_procedure("txn_start", (SCM (*)())gls_txn_start, 1, 0, 0);
	gh_new_procedure("txn_end", (SCM (*)())gls_txn_end, 1, 0, 0);
	gh_new_procedure("txn_abort", (SCM (*)())gls_txn_abort, 1, 0, 0);
	gh_new_procedure("txn_undo", (SCM (*)())gls_txn_undo, 1, 0, 0);
	gh_new_procedure("txn_delete", (SCM (*)())gls_txn_delete, 1, 0, 0);
	snprintf(str, 255, "(define TXN_NONE %i)", TXN_NONE); gh_eval_str(str);

	/* swapfile */
	gh_new_procedure("swapfile_open", (SCM (*)())gls_swapfile_open, 1, 0, 0);
	gh_new_procedure("swapfile_close", (SCM (*)())gls_swapfile_close, 0, 0, 0);
	gh_new_procedure("swapfile_creat", (SCM (*)())gls_swapfile_creat, 2, 0, 0);
	gh_new_procedure("sw_unlink", (SCM (*)())gls_sw_unlink, 1, 0, 0);
	gh_new_procedure("sw_opendir", (SCM (*)())gls_sw_opendir, 0, 0, 0);
	gh_new_procedure("sw_readdir", (SCM (*)())gls_sw_readdir, 1, 0, 0);
	gh_new_procedure("sw_closedir", (SCM (*)())gls_sw_closedir, 1, 0, 0);
	gh_new_procedure("sw_open", (SCM (*)())gls_sw_open, 3, 0, 0);
	gh_new_procedure("sw_close", (SCM (*)())gls_sw_close, 1, 0, 0);
	gh_new_procedure("sw_fstat", (SCM (*)())gls_sw_fstat, 1, 0, 0);
	gh_new_procedure("sw_ftruncate", (SCM (*)())gls_sw_ftruncate, 2, 0, 0);
	gh_new_procedure("sw_lseek", (SCM (*)())gls_sw_lseek, 3, 0, 0);
	gh_new_procedure("sw_sendfile", (SCM (*)())gls_sw_sendfile, 4, 0, 0);
	gh_new_procedure("sw_read_floatvec", (SCM (*)())gls_sw_read_floatvec, 2, 0, 0);
	gh_new_procedure("sw_read_string", (SCM (*)())gls_sw_read_string, 2, 0, 0);
	gh_new_procedure("sw_write", (SCM (*)())gls_sw_write, 2, 0, 0);
	sprintf(str, "(define O_CREAT %i)", O_CREAT); gh_eval_str(str);
	sprintf(str, "(define O_EXCL %i)", O_EXCL); gh_eval_str(str);
	sprintf(str, "(define O_TRUNC %i)", O_TRUNC); gh_eval_str(str);
	sprintf(str, "(define O_RDWR %i)", O_RDWR); gh_eval_str(str);
	sprintf(str, "(define O_RDONLY %i)", O_RDONLY); gh_eval_str(str);
	sprintf(str, "(define O_WRONLY %i)", O_WRONLY); gh_eval_str(str);
	sprintf(str, "(define SWSENDFILE_INSERT %i)", SWSENDFILE_INSERT); gh_eval_str(str);
	sprintf(str, "(define SWSENDFILE_CUT %i)", SWSENDFILE_CUT); gh_eval_str(str);
	sprintf(str, "(define SW_NOFILE %li)", (long)SW_NOFILE); gh_eval_str(str);
	sprintf(str, "(define SEEK_SET %i)", SEEK_SET); gh_eval_str(str);
	sprintf(str, "(define SEEK_CUR %i)", SEEK_CUR); gh_eval_str(str);
	sprintf(str, "(define SEEK_END %i)", SEEK_END); gh_eval_str(str);

	/* filter */
	gh_new_procedure("filternetwork_new",
			 (SCM (*)())gls_filternetwork_new, 0, 0, 0);
	gh_new_procedure("filternetwork_delete",
			 (SCM (*)())gls_filternetwork_delete, 1, 0, 0);
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
	gh_new_procedure("plugin_add_path", (SCM (*)())gls_plugin_add_path,
			 1, 0, 0);
	gh_new_procedure("plugin_get", (SCM (*)())gls_plugin_get, 1, 0, 0);
	gh_new_procedure("plugin_name", (SCM (*)())gls_plugin_name, 1, 0, 0);
	gh_new_procedure("plugin_query", (SCM (*)())gls_plugin_query_string, 2, 0, 0);

#if 0
	/* track */
	gh_new_procedure("track_get", (SCM (*)())gls_track_get, 2, 0, 0);
	gh_new_procedure("track_delete", (SCM (*)())gls_track_delete, 1, 0, 0);
#endif


	/* load glame scheme libraries (if existent):
	 * 1. installed glame.scm
	 * 2. glmid/glame.scm
	 * 3. ~/.glame.scm
	 */
	gh_eval_str("(map (lambda (file) (if (file-exists? file) (begin "
#ifdef DEBUG
"(display (string-append \"loading \" file)) (newline) "
#endif
"(load file)))) "
"`(\"" PKGDATADIR "/glame.scm\" \"glmid/glame.scm\" ,(string-append (getenv \"HOME\") \"/.glame.scm\")))");

	return 0;
}

#else /* !HAVE_GUILE */

int glscript_init()
{
        return 0;
}

#endif /* HAVE_GUILE */
