/*
 * track_io.c
 * $Id: track_io.c,v 1.3 2000/03/25 15:03:21 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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


PLUGIN_SET(track_io, "track_in track_out")


/* Read a track (swapfile file with associated information) into
 * the filter network. */
static int track_in_f(filter_node_t *n)
{
	filter_pipe_t *out;
	filter_param_t *chan, *group;
	filecluster_t *fc;
	filter_buffer_t *buf;
	track_t *c;
	fileid_t f;
	char *mem;
	off_t size, pos;
	int cnt, i;
	SAMPLE *s;

	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");
	if (!(chan = filternode_get_param(n, "track"))
	    || !(group = filternode_get_param(n, "group")))
		FILTER_ERROR_RETURN("no input track specified");
	if (!(c = track_get(filterparam_val_string(chan), 
			    filterparam_val_string(group))))
		FILTER_ERROR_RETURN("input track not found");

	FILTER_AFTER_INIT;

	/* get the first filecluster */
	f = track_fid(c);
	size = file_size(f);

	pos = 0;
	while (size > 0) {
		FILTER_CHECK_STOP;

		/* Alloc a buffer of default size or
		 * a tail buffer. */
		cnt = MIN(size/SAMPLE_SIZE, GLAME_WBUFSIZE);
		buf = sbuf_make_private(sbuf_alloc(cnt, n));
		s = sbuf_buf(buf);
		size -= cnt*SAMPLE_SIZE;

		/* map and copy filecluster data */
		fc = filecluster_get(f, pos);
		do {
			mem = filecluster_mmap(fc);
			mem += pos-filecluster_start(fc);
			i = MIN(SAMPLE_SIZE*cnt,
				filecluster_size(fc) - pos + filecluster_start(fc));
			memcpy(s, mem, i);
			cnt -= i/SAMPLE_SIZE;
			s += i/SAMPLE_SIZE;
			pos += i;
			filecluster_munmap(fc);
			if (filecluster_end(fc) < pos)
				fc = filecluster_next(fc);
		} while (cnt>0);

		/* queue the buffer */
		sbuf_queue(out, buf);
	}
	/* send an EOF */
	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}
static int track_in_fixup_param(filter_node_t *n, filter_pipe_t *p,
				  const char *name, filter_param_t *param)
{
	filter_param_t *chan, *group;
	filter_pipe_t *out;
	track_t *c;

	if (!(chan = filternode_get_param(n, "track"))
	    || !(group = filternode_get_param(n, "group")))
		return 0;
	if (!(c = track_get(filterparam_val_string(chan),
			    filterparam_val_string(group)))) {
		filternode_set_error(n, "track not found");
		return -1;
	}
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return 0;

	/* fix the output pipe stream information */
	filterpipe_settype_sample(out, track_rate(c), 
				  track_hangle(c));
	out->dest->filter->fixup_pipe(out->dest, out);
	return 0;
}
static int track_in_connect_out(filter_node_t *n, const char *port,
				 filter_pipe_t *p)
{
	filter_param_t *chan, *group;
	track_t *c;

	if (!(chan = filternode_get_param(n, "track"))
	    || !(group = filternode_get_param(n, "group")))
		return 0;
	if (!(c = track_get(filterparam_val_string(chan),
			    filterparam_val_string(group))))
		return 0;

	/* fix the output pipe stream information */
	filterpipe_settype_sample(p, track_rate(c), 
				  track_hangle(c));
	return 0;
}

PLUGIN_DESCRIPTION(track_in, "track to stream")
PLUGIN_PIXMAP(track_in, "default.xpm")
int track_in_register()
{
	filter_t *f;

	if (!(f = filter_alloc(track_in_f)))
		return -1;
	f->fixup_param = track_in_fixup_param;
	f->connect_out = track_in_connect_out;
	if (!filter_add_param(f, "track", "input track",
			      FILTER_PARAMTYPE_STRING)
	    || !filter_add_param(f, "group", "input group",
				 FILTER_PARAMTYPE_STRING)
	    || !filter_add_output(f, PORTNAME_OUT, "output stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f, "track_in", "stream a track") == -1)
		return -1;
	return 0;
}





/* Store an audio stream into a track (swapfile file with associated
 * information). */
static int track_out_f(filter_node_t *n)
{
	filter_pipe_t *in;
	filecluster_t *fc;
	filter_buffer_t *buf;
	fileid_t file;
	filter_param_t *chan, *group;
	char *mem;
	int pos, p, res = 0;

	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	if (!(chan = filternode_get_param(n, "track"))
	    || !(group = filternode_get_param(n, "group")))
		FILTER_ERROR_RETURN("no output track specified");

	FILTER_AFTER_INIT;

	file = file_alloc(0);
	pos = 0;

	while ((buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;
		/* fix size of swapfile file wrt to input buffer */
		file_truncate(file, file_size(file)
			      + sbuf_size(buf)*SAMPLE_SIZE);

		/* copy the buffer */
		p = 0;
		while (p < sbuf_size(buf)*SAMPLE_SIZE) {
			fc = filecluster_get(file, pos);
			mem = filecluster_mmap(fc);
			memcpy(mem, sbuf_buf(buf)+p/SAMPLE_SIZE, filecluster_size(fc));
			p += filecluster_size(fc);
			pos += filecluster_size(fc);
			filecluster_munmap(fc);
		}

		/* free the buffer */
		sbuf_unref(buf);
	}
	FILTER_BEFORE_STOPCLEANUP;

	/* store the file into the submitted track */
	res = track_add(filterparam_val_string(chan),
			filterparam_val_string(group),
			file, filterpipe_sample_rate(in),
			filterpipe_sample_hangle(in), 0);

	FILTER_BEFORE_CLEANUP;

	return res;
}

PLUGIN_DESCRIPTION(track_out, "stream to track")
PLUGIN_PIXMAP(track_out, "default.xpm")
int track_out_register()
{
	filter_t *f;

	if (!(f = filter_alloc(track_out_f))
	    || !filter_add_param(f, "track", "output track",
				 FILTER_PARAMTYPE_STRING)
	    || !filter_add_param(f, "group", "output group",
				 FILTER_PARAMTYPE_STRING)
	    || !filter_add_input(f, PORTNAME_IN, "input stream",
				 FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f, "track_out", "store a stream into a track") == -1)
		return -1;
	return 0;
}
