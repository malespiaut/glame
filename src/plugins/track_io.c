/*
 * track_io.c
 * $Id: track_io.c,v 1.2 2000/03/20 09:51:53 richi Exp $
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

	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");
	if (!(chan = filternode_get_param(n, "track"))
	    || !(group = filternode_get_param(n, "group")))
		FILTER_ERROR_RETURN("no input track specified");
	if (!(c = get_track(filterparam_val_string(chan), 
	                      filterparam_val_string(group))))
		FILTER_ERROR_RETURN("input track not found");

	FILTER_AFTER_INIT;

	/* get the first filecluster */
	f = track_fid(c);
	fc = filecluster_get(f, 0);

	while (fc != NULL) {
		FILTER_CHECK_STOP;
		/* map the filecluster data */
		mem = filecluster_mmap(fc);

		/* alloc a new stream buffer and copy the data
		 * FIXME: split the buffer into parts not bigger
		 *        than f.i. GLAME_BUFSIZE */
		buf = sbuf_alloc(filecluster_size(fc)/SAMPLE_SIZE, n);
		memcpy(sbuf_buf(buf), mem, filecluster_size(fc));

		/* queue the buffer */
		sbuf_queue(out, buf);

		/* unmap the filecluster and get the next cluster */
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
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
	if (!(c = get_track(filterparam_val_string(chan),
	                      filterparam_val_string(group)))) {
		filternode_set_error(n, "track not found");
		return -1;
	}
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return 0;

	/* fix the output pipe stream information */
	filterpipe_settype_sample(out, track_freq(c), 
				  FILTER_PIPEPOS_DEFAULT);
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
	if (!(c = get_track(filterparam_val_string(chan),
	                      filterparam_val_string(group))))
		return 0;

	/* fix the output pipe stream information */
	filterpipe_settype_sample(p, track_freq(c), 
				  FILTER_PIPEPOS_DEFAULT);
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
	filter_param_t *chan, *group, *type;
	char *mem;
	int pos, p;
	int ctype, res = 0;

	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	if (!(chan = filternode_get_param(n, "track"))
	    || !(group = filternode_get_param(n, "group")))
		FILTER_ERROR_RETURN("no output track specified");
	/* FIXME! */
	if ((type = filternode_get_param(n, "type")))
		ctype = filterparam_val_int(type);
	else
		ctype = TRACK_NUM_MISC;

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
	res = add_track(filterparam_val_string(chan),
	                  filterparam_val_string(group),
			  file, ctype, filterpipe_sample_rate(in));

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
	    || !filter_add_param(f, "type", "output type",
				 FILTER_PARAMTYPE_INT)
	    || !filter_add_input(f, PORTNAME_IN, "input stream",
				 FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f, "track_out", "store a stream into a track") == -1)
		return -1;
	return 0;
}
