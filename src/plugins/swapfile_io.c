/*
 * swapfile_io.c
 * $Id: swapfile_io.c,v 1.11 2001/04/17 17:54:52 richi Exp $
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

#include "filter.h"
#include "swapfile.h"
#include "glplugin.h"


PLUGIN_SET(swapfile_io, "swapfile_in swapfile_out")


/* Read a swapfile file into the filter network. */
static int swapfile_in_f(filter_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	long fname, offset;
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
	offset = filterparam_val_int(filternode_get_param(n, "offset"));
	offset *= SAMPLE_SIZE;
	if (sw_lseek(fd, offset, SEEK_SET) != offset) {
		sw_close(fd);
		FILTER_ERROR_RETURN("cannot seek to offset");
	}
	size = filterparam_val_int(filternode_get_param(n, "size"));
	sw_fstat(fd, &st);
	if (size == -1)
		size = st.size - offset;
	else {
		size *= SAMPLE_SIZE;
		if (size+offset > st.size) {
			sw_close(fd);
			FILTER_ERROR_RETURN("offset + size does not match file");
		}
	}

	FILTER_AFTER_INIT;

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
	filter_t *n;
	filter_pipe_t *out;
	long fname;
	swfd_t fd;

	GLSIGH_GETARGS1(va, param);
	n = filterparam_filter(param);
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return;
	fname = filterparam_val_int(filternode_get_param(n, "filename"));
	if (!(fd = sw_open(fname, O_RDONLY, TXN_NONE)))
		return;
	sw_close(fd);

	/* fix the output pipe stream information */
	filter_clear_error(n);
	filterpipe_settype_sample(out, filterparam_val_int(filternode_get_param(n, "rate")),
				  filterparam_val_float(filternode_get_param(n, "position")));
	glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	return;
}
static int swapfile_in_connect_out(filter_t *n, filter_port_t *outp,
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

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = swapfile_in_f;

	filter_add_output(f, PORTNAME_OUT, "output stream",
			  FILTER_PORTTYPE_SAMPLE);

	filterparamdb_add_param_int(filter_paramdb(f), "filename",
				FILTER_PARAMTYPE_INT, -1,
				FILTERPARAM_END);
	filterparamdb_add_param_int(filter_paramdb(f), "rate",
				FILTER_PARAMTYPE_INT, GLAME_DEFAULT_SAMPLERATE,
				FILTERPARAM_END);
	filterparamdb_add_param_float(filter_paramdb(f), "position",
				  FILTER_PARAMTYPE_POSITION, FILTER_PIPEPOS_DEFAULT,
				  FILTERPARAM_END);
	filterparamdb_add_param_int(filter_paramdb(f), "offset",
				FILTER_PARAMTYPE_INT, 0,
				FILTERPARAM_END);
	filterparamdb_add_param_int(filter_paramdb(f), "size",
				FILTER_PARAMTYPE_INT, -1,
				FILTERPARAM_DESCRIPTION, "size to stream or -1 for the full file",
				FILTERPARAM_END);

	f->connect_out = swapfile_in_connect_out;

	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED,
			  swapfile_in_fixup_param, NULL);

	plugin_set(p, PLUGIN_DESCRIPTION, "swapfile file to audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "input.png");
	plugin_set(p, PLUGIN_CATEGORY, "Input");

	filter_register(f, p);

	return 0;
}





/* Store an audio stream into a track (swapfile file with associated
 * information). */
static int swapfile_out_f(filter_t *n)
{
	filter_pipe_t *in;
	filter_buffer_t *buf;
	long fname, offset, size, cnt;
	swfd_t fd;

	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	fname = filterparam_val_int(filternode_get_param(n, "filename"));
	if (fname == -1)
		FILTER_ERROR_RETURN("no filename");
	offset = filterparam_val_int(filternode_get_param(n, "offset"));
	if (offset == -1) {
		if (!(fd = sw_open(fname, O_RDWR|O_CREAT|O_TRUNC, TXN_NONE)))
			FILTER_ERROR_RETURN("cannot create file");
		sw_lseek(fd, 0, SEEK_SET);
	} else {
		if (!(fd = sw_open(fname, O_RDWR, TXN_NONE)))
			FILTER_ERROR_RETURN("cannot open file");
		if (sw_lseek(fd, offset*SAMPLE_SIZE, SEEK_SET) != offset*SAMPLE_SIZE) {
			FILTER_ERROR_RETURN("cannot seek to offset");
			sw_close(fd);
		}
	}
	size = filterparam_val_int(filternode_get_param(n, "size"));

	FILTER_AFTER_INIT;

	while ((size != 0) && (buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;

		/* Check, if we are allowed to write the full buffer. */
		cnt = sbuf_size(buf);
		if (size != -1)
			cnt = MIN(cnt, size);

		/* Write the buffers data to the file. */
		if (sw_write(fd, sbuf_buf(buf), cnt*SAMPLE_SIZE)
		    != cnt*SAMPLE_SIZE)
			DPRINTF("Did not write the whole buffer!?");

		/* Update size, if necessary. */
		if (size != -1)
			size -= cnt;

		/* free the buffer */
		sbuf_unref(buf);
	}

	/* Rest goes to the bitbucket. */
	while (buf) {
		FILTER_CHECK_STOP;

		buf = sbuf_get(in);
		if (buf)
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

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = swapfile_out_f;

	filter_add_input(f, PORTNAME_IN, "input stream",
			 FILTER_PORTTYPE_SAMPLE);

	filterparamdb_add_param_int(filter_paramdb(f), "filename",
				FILTER_PARAMTYPE_INT, -1,
				FILTERPARAM_END);
	filterparamdb_add_param_int(filter_paramdb(f), "offset",
				FILTER_PARAMTYPE_INT, -1,
				FILTERPARAM_DESCRIPTION, "offset to start writing or -1 for new file",
				FILTERPARAM_END);
	filterparamdb_add_param_int(filter_paramdb(f), "size",
				FILTER_PARAMTYPE_INT, -1,
				FILTERPARAM_DESCRIPTION, "max size to record or -1 for the full stream",
				FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "audio stream to swapfile file");
	plugin_set(p, PLUGIN_PIXMAP, "output.png");
	plugin_set(p, PLUGIN_CATEGORY, "Output");
	filter_register(f, p);

	return 0;
}
