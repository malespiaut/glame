/*
 * swapfile_io.c
 * $Id: swapfile_io.c,v 1.2 2000/10/28 13:45:48 richi Exp $
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

#include <glmid.h>


PLUGIN_SET(swapfile_io, "swapfile_in swapfile_out")


/* Read a swapfile file into the filter network. */
static int swapfile_in_f(filter_node_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	long fname;
	off_t size, pos;
	int cnt;
	swfd_t fd;
	struct sw_stat st;

	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");
	fname = filterparam_val_int(filternode_get_param(n, "filename"));
	if (fname == -1)
		FILTER_ERROR_RETURN("no input filename specified");
	if (!(fd = sw_open(fname, O_RDONLY, TXN_NONE)))
		FILTER_ERROR_RETURN("cannot open swapfile file");

	FILTER_AFTER_INIT;

	/* Get file/cluster info. */
	sw_fstat(fd, &st);
	size = st.size;

	pos = 0;
	while (size > 0) {
		FILTER_CHECK_STOP;

		/* Alloc a buffer of default size or
		 * a tail buffer. */
		cnt = MIN(size/SAMPLE_SIZE, GLAME_WBUFSIZE);
		buf = sbuf_make_private(sbuf_alloc(cnt, n));
		size -= cnt*SAMPLE_SIZE;

		/* read data into the buffer - stop on error. */
		if (sw_read(fd, sbuf_buf(buf), cnt*SAMPLE_SIZE)
		    != cnt*SAMPLE_SIZE)
			size = 0;

		/* queue the buffer */
		sbuf_queue(out, buf);
	}
	/* send an EOF */
	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	sw_close(fd);

	return 0;
}
static void swapfile_in_fixup_param(glsig_handler_t *h, long sig, va_list va)
{
	filter_param_t *param;
	filter_node_t *n;
	filter_pipe_t *out;
	long fname;
	swfd_t fd;

	GLSIGH_GETARGS1(va, param);
	n = filterparam_node(param);
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return;
	fname = filterparam_val_int(filternode_get_param(n, "filename"));
	if (!(fd = sw_open(fname, O_RDONLY, TXN_NONE)))
		return;
	sw_close(fd);

	/* fix the output pipe stream information */
	filternode_clear_error(n);
	filterpipe_settype_sample(out, filterparam_val_int(filternode_get_param(n, "rate")),
				  filterparam_val_float(filternode_get_param(n, "position")));
	glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	return;
}
static int swapfile_in_connect_out(filter_node_t *n, filter_port_t *outp,
				   filter_pipe_t *p)
{
	long fname;
	swfd_t fd;

	if (filterport_nrpipes(outp) > 0)
		return -1;
	fname = filterparam_val_int(filternode_get_param(n, "filename"));
	if (fname != -1) {
		if (!(fd = sw_open(fname, O_RDONLY, TXN_NONE)))
			return -1;
		sw_close(fd);
	}

	/* fix the output pipe stream information */
	filterpipe_settype_sample(p, filterparam_val_int(filternode_get_param(n, "rate")),
				  filterparam_val_float(filternode_get_param(n, "position")));
	return 0;
}

int swapfile_in_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_alloc(swapfile_in_f)))
		return -1;

	filter_add_output(f, PORTNAME_OUT, "output stream",
			  FILTER_PORTTYPE_SAMPLE);

	filterpdb_add_param_int(filter_pdb(f), "filename",
				FILTER_PARAMTYPE_INT, -1,
				FILTERPARAM_END);
	filterpdb_add_param_int(filter_pdb(f), "rate",
				FILTER_PARAMTYPE_INT, GLAME_DEFAULT_SAMPLERATE,
				FILTERPARAM_END);
	filterpdb_add_param_float(filter_pdb(f), "position",
				  FILTER_PARAMTYPE_POSITION, FILTER_PIPEPOS_DEFAULT,
				  FILTERPARAM_END);

	f->connect_out = swapfile_in_connect_out;

	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED,
			  swapfile_in_fixup_param, NULL);

	plugin_set(p, PLUGIN_DESCRIPTION, "swapfile file to audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "default.xpm");
	filter_attach(f, p);

	return 0;
}





/* Store an audio stream into a track (swapfile file with associated
 * information). */
static int swapfile_out_f(filter_node_t *n)
{
	filter_pipe_t *in;
	filter_buffer_t *buf;
	long fname;
	swfd_t fd;

	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	fname = filterparam_val_int(filternode_get_param(n, "filename"));
	if (fname == -1)
		FILTER_ERROR_RETURN("no filename");
	if (!(fd = sw_open(fname, O_RDWR|O_CREAT|O_TRUNC, TXN_NONE)))
		FILTER_ERROR_RETURN("cannot create file");

	FILTER_AFTER_INIT;

	while ((buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;

		/* Write the buffers data to the file. */
		if (sw_write(fd, sbuf_buf(buf), sbuf_size(buf)*SAMPLE_SIZE)
		    != sbuf_size(buf)*SAMPLE_SIZE)
			DPRINTF("Did not write the whole buffer!?");

		/* free the buffer */
		sbuf_unref(buf);
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	sw_close(fd);

	return 0;
}

int swapfile_out_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_alloc(swapfile_out_f)))
		return -1;

	filter_add_input(f, PORTNAME_IN, "input stream",
			 FILTER_PORTTYPE_SAMPLE);

	filterpdb_add_param_int(filter_pdb(f), "filename",
				FILTER_PARAMTYPE_INT, -1,
				FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "audio stream to swapfile file");
	plugin_set(p, PLUGIN_PIXMAP, "default.xpm");
	filter_attach(f, p);

	return 0;
}
