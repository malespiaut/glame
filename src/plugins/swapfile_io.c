/*
 * swapfile_io.c
 * $Id: swapfile_io.c,v 1.31 2004/12/26 20:58:12 richi Exp $
 *
 * Copyright (C) 2000, 2001, 2002, 2003, 2004 Richard Guenther
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

#include "filter.h"
#include "swapfile.h"
#include "glplugin.h"


PLUGIN_SET(swapfile_io, "swapfile_in swapfile_out")


/* Read a swapfile file into the filter network. */
static int swapfile_in_f(filter_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	long fname, offset, cnt, o_offset;
	off_t size, pos, o_size;
	int res;
	swfd_t fd;
	struct sw_stat st;
	filter_param_t *pos_param;
	int loop;

	if (!(out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		FILTER_ERROR_RETURN("no output");
	fname = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "filename"));
	if (fname == -1)
		FILTER_ERROR_RETURN("no input filename specified");
	if (!(fd = sw_open(fname, O_RDONLY)))
		FILTER_ERROR_RETURN("cannot open swapfile file");
	offset = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "offset"));
	offset *= SAMPLE_SIZE;
	if (sw_lseek(fd, MAX(0, offset), SEEK_SET) != MAX(0, offset)) {
		DPRINTF("Cannot seek to %li\n", MAX(0, offset));
		sw_close(fd);
		FILTER_ERROR_RETURN("cannot seek to offset");
	}
	size = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "size"));
	sw_fstat(fd, &st);
	if (size == -1)
		size = (((long)st.size - offset)/SAMPLE_SIZE)*SAMPLE_SIZE;
	else
		size *= SAMPLE_SIZE;
	o_offset = offset;
	o_size = size;
	loop = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "flags"));
	DPRINTF("from %li size %li%s\n", offset, size, loop ? ", looping" : "");

	FILTER_AFTER_INIT;
	pos_param = filterparamdb_get_param(
		filter_paramdb(n), FILTERPARAM_LABEL_POS);
	filterparam_val_set_pos(pos_param, 0);

	pos = 0;
	while (loop || size > 0) {
		FILTER_CHECK_STOP;

		if (loop && size == 0) {
			size = o_size;
			offset = o_offset;
			pos = 0;
			if (sw_lseek(fd, MAX(0, offset), SEEK_SET) != MAX(0, offset))
				FILTER_ERROR_STOP("Cannot seek");
		}
		/* Alloc a buffer of default size or
		 * a tail buffer. */
		cnt = MIN(size/SAMPLE_SIZE, GLAME_WBUFSIZE);
		buf = sbuf_make_private(sbuf_alloc(cnt, n));
		size -= cnt*SAMPLE_SIZE;

		/* prefill with zeroes, if necessary. */
		if (offset < 0)
			memset(sbuf_buf(buf), 0, MIN(cnt*SAMPLE_SIZE, -offset));
		/* read data into the buffer - stop on error. */
		if (-offset < (long)(cnt*SAMPLE_SIZE)) {
			res = sw_read(fd,
				      sbuf_buf(buf) + (offset < 0 ? -offset/SAMPLE_SIZE : 0),
				      cnt*SAMPLE_SIZE - (offset < 0 ? -offset : 0));
			if (res == -1)
				FILTER_ERROR_STOP("Error reading");
			if (res < cnt*SAMPLE_SIZE - (offset < 0 ? -offset : 0))
				memset(sbuf_buf(buf) + res/SAMPLE_SIZE, 0,
				       cnt*SAMPLE_SIZE - (offset < 0 ? -offset : 0) - res);
		}

		offset += cnt*SAMPLE_SIZE;
		pos += cnt;
		filterparam_val_set_pos(pos_param, pos);

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
	if (!(out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT))))
		return;
	fname = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "filename"));
	if (!(fd = sw_open(fname, O_RDONLY)))
		return;
	sw_close(fd);

	/* fix the output pipe stream information */
	filter_clear_error(n);
	filterpipe_settype_sample(out, filterparam_val_long(filterparamdb_get_param(filter_paramdb(n), "rate")),
				  filterparam_val_double(filterparamdb_get_param(filter_paramdb(n), "position")));
	glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
	return;
}
static int swapfile_in_connect_out(filter_port_t *outp, filter_pipe_t *p)
{
	filter_t *n;
	long fname;
	swfd_t fd;

	if (filterport_nrpipes(outp) > 0)
		return -1;
	n = filterport_filter(outp);
	fname = filterparam_val_long(filterparamdb_get_param(filter_paramdb(n), "filename"));
	if (fname != -1) {
		if (!(fd = sw_open(fname, O_RDONLY)))
			return -1;
		sw_close(fd);
	}

	/* fix the output pipe stream information */
	filterpipe_settype_sample(p, filterparam_val_long(filterparamdb_get_param(filter_paramdb(n), "rate")),
				  filterparam_val_double(filterparamdb_get_param(filter_paramdb(n), "position")));
	return 0;
}

int swapfile_in_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *out;

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = swapfile_in_f;

	out = filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
				    FILTER_PORTTYPE_SAMPLE,
				    FILTER_PORTFLAG_OUTPUT,
				    FILTERPORT_DESCRIPTION, "output stream",
				    FILTERPORT_END);

	filterparamdb_add_param_long(filter_paramdb(f), "filename",
				FILTER_PARAMTYPE_LONG, -1,
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "rate",
				FILTER_PARAMTYPE_LONG, GLAME_DEFAULT_SAMPLERATE,
				FILTERPARAM_END);
	filterparamdb_add_param_double(filter_paramdb(f), "position",
				  FILTER_PARAMTYPE_POSITION, FILTER_PIPEPOS_DEFAULT,
				  FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "offset",
				FILTER_PARAMTYPE_LONG, 0,
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "size",
				FILTER_PARAMTYPE_LONG, -1,
				FILTERPARAM_DESCRIPTION, "size to stream or -1 for the full file",
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "flags",
				FILTER_PARAMTYPE_LONG, 0,
				FILTERPARAM_DESCRIPTION, "1: loop",
				FILTERPARAM_END);
	filterparamdb_add_param_pos(filter_paramdb(f));

	out->connect = swapfile_in_connect_out;

	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED,
			  swapfile_in_fixup_param, NULL);

	plugin_set(p, PLUGIN_DESCRIPTION, "swapfile file to audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "input.png");
	plugin_set(p, PLUGIN_CATEGORY, "Input");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Swapfile_I_O");
	plugin_set(p, PLUGIN_LABEL, "Swapfile input");

	filter_register(f, p);

	return 0;
}





/* Store an audio stream into a track (swapfile file with associated
 * information). */
static int swapfile_out_f(filter_t *n)
{
	filter_pipe_t *in;
	filter_buffer_t *buf = NULL;
	filter_param_t *pos_param;
	long fname, offset, drop, size, cnt, pos, res, flags, changed_start;
	swfd_t fd;

	if (!(in = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN))))
		FILTER_ERROR_RETURN("no input");
	fname = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "filename"));
	if (fname == -1)
		FILTER_ERROR_RETURN("no filename");
	flags = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "flags"));
	offset = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "offset"));
	drop = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "drop"));

	if (!(fd = sw_open(fname, O_RDWR | (flags & 1 ? O_CREAT : 0) | (flags & 2 ? O_TRUNC : 0))))
		FILTER_ERROR_RETURN("cannot open file");

	/* backward compatible offset/drop */
	if (drop == 0 && offset < 0) {
		drop = -offset;
		offset = 0;
	}
	if (sw_lseek(fd, offset*SAMPLE_SIZE, SEEK_SET) != offset*SAMPLE_SIZE) {
		DPRINTF("Cannot seek to %li\n", offset*SAMPLE_SIZE);
		sw_close(fd);
		FILTER_ERROR_RETURN("cannot seek to offset");
	}

	size = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "size"));

	pos_param = filterparamdb_get_param(
		filter_paramdb(n), FILTERPARAM_LABEL_POS);
	filterparam_val_set_pos(pos_param, 0);
	pos = 0;
	changed_start = MAX(0, offset);
	offset = drop;

	FILTER_AFTER_INIT;

	while ((size != 0) && (buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;

		/* Check, if we are allowed to write the full buffer. */
		cnt = sbuf_size(buf);
		if (size != -1)
			cnt = MIN(cnt, size);

		/* Write the buffers data to the file (drop until offset >= 0). */
		if (-offset < cnt) {
			res = sw_write(fd,
				       sbuf_buf(buf) + (offset < 0 ? -offset : 0),
				       (cnt - (offset < 0 ? -offset : 0))*SAMPLE_SIZE);
			if (res == -1) {
				sbuf_unref(buf);
				FILTER_ERROR_STOP("Error writing to swapfile");
			}
			if (res != cnt*SAMPLE_SIZE) {
				sbuf_unref(buf);
				FILTER_ERROR_STOP("No longer can write (disk full?)");
			}
		}
		pos += cnt;
		filterparam_val_set_pos(pos_param, pos);

		/* Update size, if necessary. */
		if (size != -1)
			size -= cnt;
		offset += cnt;

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

	/* Report changed range to "output" parameters. */
	filterparam_val_long(filterparamdb_get_param(filter_paramdb(n), "changed_start")) = changed_start;
	filterparam_val_long(filterparamdb_get_param(filter_paramdb(n), "changed_end")) = changed_start + MAX(0, offset) - 1;

	return 0;
}

int swapfile_out_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = swapfile_out_f;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream",
			      FILTERPORT_END);

	filterparamdb_add_param_long(filter_paramdb(f), "filename",
				FILTER_PARAMTYPE_LONG, -1,
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "offset",
				FILTER_PARAMTYPE_LONG, 0,
				FILTERPARAM_DESCRIPTION, "file offset to start writing",
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "drop",
				FILTER_PARAMTYPE_LONG, 0,
				FILTERPARAM_DESCRIPTION, "number of samples to drop before start writing",
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "size",
				FILTER_PARAMTYPE_LONG, -1,
				FILTERPARAM_DESCRIPTION, "max size to record or -1 for the full stream (allow extend)",
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "flags",
				FILTER_PARAMTYPE_LONG, 0,
				FILTERPARAM_DESCRIPTION, "1: create file 2: truncate file",
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "changed_start",
				FILTER_PARAMTYPE_LONG, -1,
				FILTERPARAM_DESCRIPTION, "file change report",
				FILTERPARAM_END);
	filterparamdb_add_param_long(filter_paramdb(f), "changed_end",
				FILTER_PARAMTYPE_LONG, -1,
				FILTERPARAM_DESCRIPTION, "file change report",
				FILTERPARAM_END);
	filterparamdb_add_param_pos(filter_paramdb(f));

	plugin_set(p, PLUGIN_DESCRIPTION, "audio stream to swapfile file");
	plugin_set(p, PLUGIN_PIXMAP, "output.png");
	plugin_set(p, PLUGIN_CATEGORY, "Output");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Swapfile_I_O");
	plugin_set(p, PLUGIN_LABEL, "Swapfile output");
	filter_register(f, p);

	return 0;
}
