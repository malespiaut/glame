/*
 * file_in.c
 * $Id: file_in.c,v 1.1 2000/01/20 14:54:19 richi Exp $
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
	filter_buffer_t *out;
	filecluster_t *fc;
	char *mem;

	/* get the first filecluster */
	fc = filecluster_get(n->params[0].file, 0);

	while (pthread_testcancel(), fc != NULL) {
		/* map the filecluster data */
		mem = filecluster_mmap(fc);

		/* alloc a new stream buffer and copy the data */
		out = fbuf_alloc(filecluster_size(fc)/sizeof(SAMPLE));
		memcpy(fbuf_buf(out), mem, filecluster_size(fc));

		/* queue the buffer */
		fbuf_queue(n->outputs[0], out);

		/* unmap the filecluster and get the next cluster */
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
	}

	/* send an EOF */
	fbuf_queue(n->outputs[0], NULL);

	return 0;
}

