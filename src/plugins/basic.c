/*
 * basic.c
 * $Id: basic.c,v 1.12 2000/11/06 09:48:08 richi Exp $
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
 *
 * This file contains basic filters which do not depend on the actual
 * used protocol. Contained are
 * - drop
 * - one2n
 */

#define _NO_FILTER_COMPATIBILITY
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


PLUGIN_SET(basic, "drop one2n")


/* Drop is a wastebucket for stream data. it does throw away
 * any number of input channels, producing nothing. It does
 * this asynchronly to avoid deadlocks due to blocking reads
 * and writes (so its a nice example of multiple asynchronous
 * inputs - but still rather simple).
 */
static int drop_f(filter_t *n)
{
	filter_buffer_t *in;
	filter_pipe_t **inputs, *p;
	filter_port_t *inp;
	int active_channels, i, maxfd;
	fd_set channels;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	if (!(inputs = ALLOCN(filterport_nrpipes(inp), filter_pipe_t *)))
		FILTER_ERROR_RETURN("no memory");

	/* put all input connections into an easy accessable
	 * array - we can use it for active connection
	 * tracking, too. */
	active_channels = 0;
	filterport_foreach_pipe(inp, p)
		inputs[active_channels++] = p;


	FILTER_AFTER_INIT;

	while (active_channels>0) {
	        FILTER_CHECK_STOP;

		/* wait for pipe activity */
		FD_ZERO(&channels);
		maxfd = 0;
		for (i=0; i<filterport_nrpipes(inp); i++)
			if (inputs[i]) {
				FD_SET(inputs[i]->dest_fd, &channels);
				if (inputs[i]->dest_fd > maxfd)
					maxfd = inputs[i]->dest_fd;
			}
		if (select(maxfd+1, &channels, NULL, NULL, NULL) <= 0)
			continue;

		/* just unref all pending buffers */
		for (i=0; i<filterport_nrpipes(inp); i++)
			if (inputs[i]
			    && FD_ISSET(inputs[i]->dest_fd, &channels)) {
				if (!(in = fbuf_get(inputs[i]))) {
					inputs[i] = NULL;
					active_channels--;
				}
				fbuf_unref(in);
			}
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(inputs);

	FILTER_RETURN;
}

static int drop_connect_in(filter_t *n, filter_port_t *port,
			   filter_pipe_t *p)
{
	/* We accept n connections. */
	return 0;
}

int drop_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL))
	    || !filterportdb_add_port(filter_portdb(f),
				      PORTNAME_IN, FILTER_PORTTYPE_ANY,
				      FILTER_PORTFLAG_INPUT,
				      FILTERPORT_DESCRIPTION, "input",
				      FILTERPORT_END))
		return -1;
	f->f = drop_f;
	f->connect_in = drop_connect_in;
	plugin_set(p, PLUGIN_DESCRIPTION, "drops n streams");
	plugin_set(p, PLUGIN_PIXMAP, "dumpster.xpm");

	return filter_register(f, p);
}





/* one2n is another channel routing filter, it does n - times duplication
 * of one input channel. To avoid deadlocks, this has again be done totally
 * asynchron, both from the input and the output ends. So inbetween buffering
 * in a queue is done per output. */
static int one2n_f(filter_t *n)
{
	typedef struct {
		filter_pipe_t *out;
		feedback_fifo_t fifo;
		int fifo_size;
	} one2n_param_t;
	filter_pipe_t *in, *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf;
	one2n_param_t *p;
	int i, res;
	int maxfd, nr, eof, empty, oneempty, maxfifosize;
	fd_set rset, wset;
	int nrin = 0, nrsel = 0;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
        outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	nr = filterport_nrpipes(outp);
	if (!(in = filterport_get_pipe(inp)))
		FILTER_ERROR_RETURN("no input");
	if (!(p = ALLOCN(nr, one2n_param_t)))
	        FILTER_ERROR_RETURN("no memory");

	/* init struct */
	i = 0;
	filterport_foreach_pipe(outp, out) {
		INIT_FEEDBACK_FIFO(p[i].fifo);
		p[i].fifo_size = 0;
		p[i++].out = out;
	}

	FILTER_AFTER_INIT;

	/* In the following loop you may miss the EOF send - but its
	 * there, EOFs just get queued like regular buffers. Loop termination
	 * is at input EOF and all send queues empty time.
	 */
	eof = 0;
	do {
	        FILTER_CHECK_STOP;

		/* set up fd sets for select */
		maxfd = 0;
		maxfifosize = 0;
		empty = 1; oneempty = 0;
		FD_ZERO(&wset);
		for (i=0; i<nr; i++) {
			if (!has_feedback(&p[i].fifo)) {  /* empty? */
				oneempty = 1;
				continue;
			}
			empty = 0;
			if (p[i].fifo_size > maxfifosize)
				maxfifosize = p[i].fifo_size;
			FD_SET(p[i].out->source_fd, &wset);
			if (p[i].out->source_fd > maxfd)
				maxfd = p[i].out->source_fd;
		}
		FD_ZERO(&rset);
		if (!eof && oneempty && !(maxfifosize > GLAME_WBUFSIZE)) {
			FD_SET(in->dest_fd, &rset);
			if (in->dest_fd > maxfd)
				maxfd = in->dest_fd;
		}
		if (eof && empty)
			break;
		nrsel++;
		res = select(maxfd+1,
			     eof || !oneempty || (maxfifosize > GLAME_WBUFSIZE) ? NULL : &rset,
			     empty ? NULL : &wset, NULL, NULL);
		if (res == -1)
			perror("select");
		if (res <= 0)
			continue;

		/* do we have input? - queue in each feedback buffer,
		 * be clever with the references, too. */
		if (FD_ISSET(in->dest_fd, &rset)) {
			nrin++;
			if (!(buf = fbuf_get(in)))
			        eof = 1;
			for (i=0; i<nr-1; i++) {
				if (p[i].fifo_size > GLAME_WBUFSIZE)
					DPRINTF("fifo size is %i\n", p[i].fifo_size);
				fbuf_ref(buf);
				add_feedback(&p[i].fifo, buf);
				p[i].fifo_size += fbuf_size(buf);
			}
			if (nr >= 1) {
				add_feedback(&p[nr-1].fifo, buf);
				p[nr-1].fifo_size += fbuf_size(buf);
			} else
				fbuf_unref(buf);
		}

		/* foreach output check, if we are ready to queue
		 * a buffer. */
		for (i=0; i<nr; i++) {
			if (!has_feedback(&p[i].fifo)
			    || !FD_ISSET(p[i].out->source_fd, &wset))
				continue;
			buf = get_feedback(&p[i].fifo);
			p[i].fifo_size -= fbuf_size(buf);
			fbuf_queue(p[i].out, buf);
		}
	} while (1);

	DPRINTF("%i input buffers, %i times select()\n", nrin, nrsel);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

static int one2n_connect_out(filter_t *n, filter_port_t *inp,
			     filter_pipe_t *p)
{
	filter_pipe_t *in;

	/* We accept any number of outputs. */
	if ((in = filterport_get_pipe(inp))) {
		p->type = in->type;
		p->u = in->u;
	}
	return 0;
}

int one2n_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL))
	    || !filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				      FILTER_PORTTYPE_ANY,
				      FILTER_PORTFLAG_INPUT,
				      FILTERPORT_DESCRIPTION, "input",
				      FILTERPORT_END)
	    || !filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
				      FILTER_PORTTYPE_ANY,
				      FILTER_PORTFLAG_OUTPUT,
				      FILTERPORT_DESCRIPTION, "output",
				      FILTERPORT_END))
		return -1;
	f->f = one2n_f;
	f->connect_out = one2n_connect_out;

	plugin_set(p, PLUGIN_DESCRIPTION, "replicates one input n times");
	plugin_set(p, PLUGIN_PIXMAP, "default.png");

	return filter_register(f, p);
}
