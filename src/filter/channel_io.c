/*
 * channel_io.c
 * $Id: channel_io.c,v 1.2 2000/02/03 18:21:21 richi Exp $
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

#include "filter.h"
#include "swapfile.h"
#include "channel.h"


/* first the inner loops of all file/channel_in/out filters
 */
static void do_file_in(fileid_t f, filter_pipe_t *out)
{
	filecluster_t *fc;
	filter_buffer_t *buf;
	char *mem;

	/* get the first filecluster */
	fc = filecluster_get(f, 0);

	while (pthread_testcancel(), fc != NULL) {
		/* map the filecluster data */
		mem = filecluster_mmap(fc);

		/* alloc a new stream buffer and copy the data
		 * FIXME: split the buffer into parts not bigger
		 *        than f.i. GLAME_BUFSIZE */
		buf = fbuf_alloc(filecluster_size(fc)/sizeof(SAMPLE));
		memcpy(fbuf_buf(buf), mem, filecluster_size(fc));

		/* queue the buffer */
		fbuf_queue(out, buf);

		/* unmap the filecluster and get the next cluster */
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
	}

	/* send an EOF */
	fbuf_queue(out, NULL);
}

static int do_file_out(filter_pipe_t *in)
{
	filecluster_t *fc;
	filter_buffer_t *buf;
	fileid_t file;
	char *mem;
	int pos, p;

	file = file_alloc(0);
	pos = 0;

	while (pthread_testcancel(),
	       (buf = fbuf_get(in))) {
		/* fix size of swapfile file wrt to input buffer */
		file_truncate(file, file_size(file)
			      + fbuf_size(buf)*sizeof(SAMPLE));

		/* copy the buffer */
		p = 0;
		while (p < fbuf_size(buf)) {
			fc = filecluster_get(file, pos);
			mem = filecluster_mmap(fc);
			memcpy(mem, fbuf_buf(buf)+p, filecluster_size(fc));
			p += filecluster_size(fc)/sizeof(SAMPLE);
			pos += filecluster_size(fc);
			filecluster_munmap(fc);
		}

		/* free the buffer */
		fbuf_unref(buf);
	}

	return file;
}


/* transform a swapfile file into a stream */
static int file_in_f(filter_node_t *n)
{
	filter_pipe_t *out;
	filter_param_t *fname;

	if (!(out = hash_find_output("out", n))
	    || !(fname = hash_find_param("file", n)))
		return -1;

	do_file_in(fname->val.file, out);

	return 0;
}


/* transform a stream into a swapfile file.
 * the parameter is actually an output value, i.e.
 * the file gets allocated by the filter and you can
 * get it by looking into params[0].file after completion. */
static int file_out_f(filter_node_t *n)
{
	filter_pipe_t *in;
	fileid_t file;

	if (!(in = hash_find_input("in", n)))
		return -1;

	file = do_file_out(in);

	/* save the allocated file as "parameter" */
	filternode_setparam(n, "file", &file);

	return 0;
}


static int channel_in_f(filter_node_t *n)
{
	filter_pipe_t *out;
	filter_param_t *chan, *group;
	channel_t *c;

	if (!(out = hash_find_output("out", n))
	    || !(chan = hash_find_param("channel", n))
	    || !(group = hash_find_param("group", n)))
		return -1;

	if (!(c = get_channel(chan->val.string, group->val.string)))
		return -1;
	do_file_in(channel_fid(c), out);

	return 0;
}
static int channel_in_fixup_param(filter_node_t *n, filter_pipe_t *p)
{
	filter_param_t *chan, *group;
	filter_pipe_t *out;
	channel_t *c;

	if (!(chan = hash_find_param("channel", n))
	    || !(group = hash_find_param("group", n)))
		return 0;
	if (!(out = hash_find_output("out", n)))
		return 0;

	if (!(c = get_channel(chan->val.string, group->val.string)))
		return 0;

	/* fix the output pipe stream information */
	out->type = FILTER_PIPETYPE_SAMPLE;
	out->u.sample.rate = channel_freq(c);

	return 0;
}


static int channel_out_f(filter_node_t *n)
{
	filter_pipe_t *in;
	fileid_t file;
	filter_param_t *chan, *group, *type;
	int ctype;

	if (!(in = hash_find_input("in", n))
	    || !(chan = hash_find_param("channel", n))
	    || !(group = hash_find_param("group", n)))
		return -1;

	if ((type = hash_find_param("type", n)))
		ctype = type->val.i;
	else
		ctype = CHANNEL_NUM_MISC;

	file = do_file_out(in);

	/* store the file into the submitted channel */
	return add_channel(group->val.string, chan->val.string,
			   file, ctype, in->u.sample.rate);
}


int channel_io_register()
{
	filter_t *f;

	if (!(f = filter_alloc("file_in", "stream a swapfile file", file_in_f))
	    || filter_add_param(f, "file", "input file",
				FILTER_PARAMTYPE_FILE) == -1
	    || filter_add_output(f, "out", "output stream",
				 FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("file_out", "file a stream", file_out_f))
	    || filter_add_param(f, "file", "output file",
				FILTER_PARAMTYPE_OUTPUT|FILTER_PARAMTYPE_FILE) == -1
	    || filter_add_input(f, "in", "input stream",
				FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("channel_in", "stream a channel", channel_in_f)))
		return -1;
	f->fixup_param = channel_in_fixup_param;
	if (filter_add_param(f, "channel", "input channel",
			     FILTER_PARAMTYPE_STRING) == -1
	    || filter_add_param(f, "group", "input group",
				FILTER_PARAMTYPE_STRING) == -1
	    || filter_add_output(f, "out", "output stream",
				 FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("channel_out", "store a stream into a channel", channel_out_f))
	    || filter_add_param(f, "channel", "output channel",
				FILTER_PARAMTYPE_STRING) == -1
	    || filter_add_param(f, "group", "output group",
				FILTER_PARAMTYPE_STRING) == -1
	    || filter_add_param(f, "type", "output type",
				FILTER_PARAMTYPE_INT) == -1
	    || filter_add_input(f, "in", "input stream",
				FILTER_PORTTYPE_SAMPLE) == -1
	    || filter_add(f) == -1)
		return -1;


	return 0;
}
