/*
 * file_in.c
 * $Id: file_in.c,v 1.3 2000/01/27 10:30:30 richi Exp $
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


/* transform a swapfile file into a stream */
int file_in(filter_node_t *n)
{
	filter_buffer_t *buf;
	filter_pipe_t *out;
	filter_param_t *fname;
	filecluster_t *fc;
	char *mem;

	if (!(out = hash_find_output("out", n))
	    || !(fname = hash_find_param("file", n)))
		return -1;

	/* get the first filecluster */
	fc = filecluster_get(fname->val.file, 0);

	while (pthread_testcancel(), fc != NULL) {
		/* map the filecluster data */
		mem = filecluster_mmap(fc);

		/* alloc a new stream buffer and copy the data */
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

	return 0;
}

