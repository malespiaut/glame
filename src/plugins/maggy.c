/*
 * maggy.c
 * $Id: maggy.c,v 1.16 2000/11/06 09:48:08 richi Exp $
 *
 * Copyright (C) 2000 Alexander Ehlert
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
 *
 * This is maggy's filter collection!
 * Please refrain from commiting any changes to cvs, just send me a patch!
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


static int resample_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;

	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!in || !out)
		return -1;

	FILTER_AFTER_INIT;

	/* The loop condition is at the end to get and
	 * forward the EOF mark. */
	do {
		FILTER_CHECK_STOP;
		/* get an input buffer */
		buf = sbuf_get(in);

		sbuf_queue(out, buf);
	} while (buf);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}

int maggy_register()
{
	filter_t *f;

	if (!(f = filter_creat(NULL))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_ANY)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				 FILTER_PORTTYPE_ANY)
	    || filter_add(f,"resample","not that it would do really anything..") == -1)
		return -1;
	f->f = resample_f;

	return 0;
}
