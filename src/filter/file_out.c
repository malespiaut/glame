/*
 * file_out.c
 * $Id: file_out.c,v 1.1 2000/01/20 14:54:19 richi Exp $
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


/* transform a stream into a swapfile file.
 * the parameter is actually an output value, i.e.
 * the file gets allocated by the filter and you can
 * get it by looking into params[0].file after completion. */
int file_out(filter_node_t *n)
{
	filter_buffer_t *in;
	filecluster_t *fc;
	fileid_t file;
	char *mem;
	int pos, p;

	file = file_alloc(0);
	pos = 0;

	while (pthread_testcancel(),
	       (in = fbuf_get(n->inputs[0]))) {
		/* fix size of swapfile file wrt to input buffer */
		file_truncate(file, file_size(file)
			      + fbuf_size(in)*sizeof(SAMPLE));

		/* copy the buffer */
		p = 0;
		while (p < fbuf_size(in)) {
			fc = filecluster_get(file, pos);
			mem = filecluster_mmap(fc);
			memcpy(mem, fbuf_buf(in)+p, filecluster_size(fc));
			p += filecluster_size(fc)/sizeof(SAMPLE);
			pos += filecluster_size(fc);
			filecluster_munmap(fc);
		}

		/* free the buffer */
		fbuf_unref(in);
	}

	/* save the allocated file as "parameter" */
	n->params[0].file = file;

	return 0;
}

