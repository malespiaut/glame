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
#include <gltrack.h>


/* The scriptable swapfile API part.
 */

#define gh_scm2fileid_t gh_scm2long
#define gh_fileid_t2scm gh_long2scm
#define gh_scm2off_t gh_scm2long
#define gh_off_t2scm gh_long2scm 

static SCM gls_file_alloc(SCM s_size)
{
	fileid_t fid;
	off_t size;

	size = gh_scm2off_t(s_size);
	if ((fid = file_alloc(size)) == -1)
		return SCM_BOOL_F;
	return gh_fileid_t2scm(fid);
}

static SCM gls_file_unref(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	file_unref(fid);
	return SCM_UNSPECIFIED;
}

static SCM gls_file_size(SCM s_fid)
{
	fileid_t fid;
	off_t size;

	fid = gh_scm2fileid_t(s_fid);
	if ((size = file_size(fid)) == -1)
		return SCM_BOOL_F;
	return gh_off_t2scm(size);
}

static SCM gls_file_truncate(SCM s_fid, SCM s_size)
{
	fileid_t fid;
	off_t size;

	fid = gh_scm2fileid_t(s_fid);
	size = gh_scm2off_t(s_size);
	if (file_truncate(fid, size) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_copy(SCM s_fid, SCM s_pos, SCM s_size)
{
	fileid_t fid, c;
	off_t size, pos;

	fid = gh_scm2fileid_t(s_fid);
	pos = gh_scm2off_t(s_pos);
	size = gh_scm2off_t(s_size);
	if ((c = file_copy(fid, pos, size)) == -1)
		return SCM_BOOL_F;
	return gh_fileid_t2scm(c);
}

static SCM gls_file_op_insert(SCM s_fid, SCM s_pos, SCM s_file)
{
	fileid_t fid, file;
	off_t pos;

	fid = gh_scm2fileid_t(s_fid);
	pos = gh_scm2off_t(s_pos);
        file = gh_scm2fileid_t(s_file);
	if (file_op_insert(fid, pos, file) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_op_cut(SCM s_fid, SCM s_pos, SCM s_size)
{
	fileid_t fid;
	off_t size, pos;

	fid = gh_scm2fileid_t(s_fid);
	pos = gh_scm2off_t(s_pos);
	size = gh_scm2off_t(s_size);
	if (file_op_cut(fid, pos, size) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_transaction_begin(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	if (file_transaction_begin(fid) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_transaction_end(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	if (file_transaction_begin(fid) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_transaction_undo(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	if (file_transaction_begin(fid) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_transaction_redo(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	if (file_transaction_begin(fid) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}



/* The scriptable filter API part.
 */

#define gh_scm2pointer (void *)gh_scm2ulong
#define gh_pointer2scm(p) gh_ulong2scm((unsigned long)(p))

SCM gls_filternetwork_new()
{
	filter_network_t *net;

	net = filternetwork_new();
	if (!net)
		return SCM_BOOL_F;
	return gh_pointer2scm(net);
}

SCM gls_filternetwork_delete(SCM s_net)
{
	filter_network_t *net;

	net = gh_scm2pointer(s_net);
	filternetwork_delete(net);
	return SCM_UNSPECIFIED;
}

SCM gls_filternetwork_add_node(SCM s_net, SCM s_filter, SCM s_name)
{
	filter_network_t *net;
	filter_node_t *node;
	char *filter, *name;
	int filterl, namel;

	net = gh_scm2pointer(s_net);
	name = gh_scm2newstr(s_name, &namel);
	filter = gh_scm2newstr(s_filter, &filterl);
	node = filternetwork_add_node(net, filter, namel == 0 ? NULL : name);
	free(name);
	free(filter);
	if (!node)
		return SCM_BOOL_F;
	return gh_pointer2scm(node);	
}

SCM gls_filternetwork_delete_node(SCM s_node)
{
	filter_node_t *node;

	node = gh_scm2pointer(s_node);
	filternetwork_delete_node(node);
	return SCM_UNSPECIFIED;
}

SCM gls_filternetwork_add_connection(SCM s_source, SCM s_source_port,
				     SCM s_dest, SCM s_dest_port)
{
	filter_pipe_t *p;
	filter_node_t *source, *dest;
	char *source_port, *dest_port;
	int source_portl, dest_portl;

	source = gh_scm2pointer(s_source);
	source_port = gh_scm2newstr(s_source_port, &source_portl);
	dest = gh_scm2pointer(s_dest);
	dest_port = gh_scm2newstr(s_dest_port, &dest_portl);
        p = filternetwork_add_connection(source, source_port, dest, dest_port);
	free(source_port);
	free(dest_port);
	if (!p)
		return SCM_BOOL_F;
	return gh_pointer2scm(p);
}

SCM gls_filternetwork_break_connection(SCM s_p)
{
	filter_pipe_t *p;

	p = gh_scm2pointer(s_p);
	filternetwork_break_connection(p);
	return SCM_UNSPECIFIED;
}

SCM gls_filterparam_set(filter_pdb_t *db, SCM s_label, SCM s_val)
{
	filter_param_t *param;
	char *label, *str;
	int labell, strl, i, res;
	float f;

	label = gh_scm2newstr(s_label, &labell);
	param = filterpdb_get_param(db, label);
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
	filter_node_t *n;

	n = gh_scm2pointer(s_n);
	return gls_filterparam_set(filternode_pdb(n), s_label, s_val);
}

SCM gls_filterpipe_set_sourceparam(SCM s_p, SCM s_label, SCM s_val)
{
	filter_pipe_t *p;

	p = gh_scm2pointer(s_p);
	return gls_filterparam_set(filterpipe_sourcepdb(p), s_label, s_val);
}

SCM gls_filterpipe_set_destparam(SCM s_p, SCM s_label, SCM s_val)
{
	filter_pipe_t *p;

	p = gh_scm2pointer(s_p);
	return gls_filterparam_set(filterpipe_destpdb(p), s_label, s_val);
}

SCM gls_filternetwork_launch(SCM s_net)
{
	filter_network_t *net;

	net = gh_scm2pointer(s_net);
	if (filternetwork_launch(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_start(SCM s_net)
{
	filter_network_t *net;

	net = gh_scm2pointer(s_net);
	if (filternetwork_start(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_pause(SCM s_net)
{
	filter_network_t *net;

	net = gh_scm2pointer(s_net);
	if (filternetwork_pause(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_wait(SCM s_net)
{
	filter_network_t *net;

	net = gh_scm2pointer(s_net);
	if (filternetwork_wait(net) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_terminate(SCM s_net)
{
	filter_network_t *net;

	net = gh_scm2pointer(s_net);
	filternetwork_terminate(net);
	return SCM_UNSPECIFIED;
}

SCM gls_filternetwork_add_input(SCM s_net, SCM s_node, SCM s_port,
				SCM s_label, SCM s_desc)
{
	filter_network_t *net;
	filter_node_t *n;
	filter_portdesc_t *d;
	char *port, *label, *desc;
	int portl, labell, descl;

	net = gh_scm2pointer(s_net);
	n = gh_scm2pointer(s_node);
	port = gh_scm2newstr(s_port, &portl);
	label = gh_scm2newstr(s_label, &labell);
	desc = gh_scm2newstr(s_desc, &descl);
	d = filternetwork_add_input(net, n->name, port, label, desc);
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
	filter_network_t *net;
	filter_node_t *n;
	filter_portdesc_t *d;
	char *port, *label, *desc;
	int portl, labell, descl;

	net = gh_scm2pointer(s_net);
	n = gh_scm2pointer(s_node);
	port = gh_scm2newstr(s_port, &portl);
	label = gh_scm2newstr(s_label, &labell);
	desc = gh_scm2newstr(s_desc, &descl);
	d = filternetwork_add_output(net, n->name, port, label, desc);
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
	filter_network_t *net;
	filter_node_t *n;
	filter_param_t *p;
	char *param, *label, *desc;
	int paraml, labell, descl;

	net = gh_scm2pointer(s_net);
	n = gh_scm2pointer(s_node);
	param = gh_scm2newstr(s_param, &paraml);
	label = gh_scm2newstr(s_label, &labell);
	desc = gh_scm2newstr(s_desc, &descl);
	p = filternetwork_add_param(net, n->name, param, label, desc);
	free(param);
	free(label);
	free(desc);
	if (!p)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

SCM gls_filternetwork_to_filter(SCM s_net, SCM s_name, SCM s_desc)
{
	filter_network_t *net;
	filter_t *f;
	plugin_t *p;
	char *name, *desc;
	int namel, descl;

	net = gh_scm2pointer(s_net);
	if (!(f = filter_from_network(net)))
		return SCM_BOOL_F;
	name = gh_scm2newstr(s_name, &namel);
	if (!(p = plugin_add(name))) {
		free(name);
		return SCM_BOOL_F;
	}
	filter_attach(f, p);
	desc = gh_scm2newstr(s_desc, &descl);
	plugin_set(p, PLUGIN_DESCRIPTION, desc);
	free(name);
	free(desc);
	return SCM_BOOL_T;
}

SCM gls_filternetwork_to_string(SCM s_net)
{
	filter_network_t *net;

	net = gh_scm2pointer(s_net);
	return gh_str02scm((char *)filternetwork_to_string(net));	
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

	p = gh_scm2pointer(s_p);
	return gh_str02scm((char *)plugin_name(p));
}


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

	track = gh_scm2pointer(s_track);
	if (track_delete(track) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}


int glscript_init()
{
	/* swapfile */
	gh_new_procedure("file_alloc", gls_file_alloc, 1, 0, 0);
	gh_new_procedure("file_unref", gls_file_unref, 1, 0, 0);
	gh_new_procedure("file_size", gls_file_size, 1, 0, 0);
	gh_new_procedure("file_truncate", gls_file_truncate, 2, 0, 0);
	gh_new_procedure("file_copy", gls_file_copy, 3, 0, 0);
	gh_new_procedure("file_op_insert", gls_file_op_insert, 3, 0, 0);
	gh_new_procedure("file_op_cut", gls_file_op_cut, 3, 0, 0);
	gh_new_procedure("file_transaction_begin", gls_file_transaction_begin,
			 1, 0, 0);
	gh_new_procedure("file_transaction_end", gls_file_transaction_end,
			 1, 0, 0);
	gh_new_procedure("file_transaction_undo", gls_file_transaction_undo,
			 1, 0, 0);
	gh_new_procedure("file_transaction_redo", gls_file_transaction_redo,
			 1, 0, 0);

	/* filter */
	gh_new_procedure("filternetwork_new", gls_filternetwork_new, 0, 0, 0);
	gh_new_procedure("filternetwork_delete", gls_filternetwork_delete,
			 1, 0, 0);
	gh_new_procedure("filternetwork_add_node", gls_filternetwork_add_node,
			 3, 0, 0);
	gh_new_procedure("filternetwork_delete_node",
			 gls_filternetwork_delete_node, 1, 0, 0);
	gh_new_procedure("filternetwork_add_connection",
			 gls_filternetwork_add_connection, 4, 0, 0);
	gh_new_procedure("filternetwork_break_connection",
			 gls_filternetwork_break_connection, 1, 0, 0);
	gh_new_procedure("filternode_set_param",
			 gls_filternode_set_param, 3, 0, 0);
	gh_new_procedure("filterpipe_set_sourceparam",
			 gls_filterpipe_set_sourceparam, 3, 0, 0);
	gh_new_procedure("filterpipe_set_destparam",
			 gls_filterpipe_set_destparam, 3, 0, 0);
	gh_new_procedure("filternetwork_launch",
			 gls_filternetwork_launch, 1, 0, 0);
	gh_new_procedure("filternetwork_start",
			 gls_filternetwork_start, 1, 0, 0);
	gh_new_procedure("filternetwork_pause",
			 gls_filternetwork_pause, 1, 0, 0);
	gh_new_procedure("filternetwork_wait",
			 gls_filternetwork_wait, 1, 0, 0);
	gh_new_procedure("filternetwork_terminate",
			 gls_filternetwork_terminate, 1, 0, 0);
	gh_new_procedure("filternetwork_to_filter",
			 gls_filternetwork_to_filter, 3, 0, 0);
	gh_new_procedure("filternetwork_add_input",
			 gls_filternetwork_add_input, 5, 0, 0);
	gh_new_procedure("filternetwork_add_output",
			 gls_filternetwork_add_output, 5, 0, 0);
	gh_new_procedure("filternetwork_add_param",
			 gls_filternetwork_add_param, 5, 0, 0);
	gh_new_procedure("filternetwork_to_string",
			 gls_filternetwork_to_string, 1, 0, 0);

	/* plugin */
	gh_new_procedure("plugin_add_path", gls_plugin_add_path,
			 1, 0, 0);
	gh_new_procedure("plugin_get", gls_plugin_get, 1, 0, 0);
	gh_new_procedure("plugin_name", gls_plugin_name, 1, 0, 0);

	/* track */
	gh_new_procedure("track_get", gls_track_get, 2, 0, 0);
	gh_new_procedure("track_delete", gls_track_delete, 1, 0, 0);


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
