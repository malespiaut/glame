/*
 * basic.c
 * $Id: basic.c,v 1.37 2006/09/19 20:59:48 richi Exp $
 *
 * Copyright (C) 1999, 2000, 2001, 2002, 2003 Richard Guenther
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


PLUGIN_SET(basic, "drop one2n buffer")


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
	if (!(inputs = ALLOCN(filterport_nrpipes(inp)+1, filter_pipe_t *)))
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

static int drop_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	/* We accept n connections. */
	return 0;
}

int drop_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *in;

	if (!(f = filter_creat(NULL))
	    || !(in = filterportdb_add_port(filter_portdb(f),
					    PORTNAME_IN, FILTER_PORTTYPE_ANY,
					    FILTER_PORTFLAG_INPUT,
					    FILTERPORT_DESCRIPTION, "input",
					    FILTERPORT_END)))
		return -1;
	f->f = drop_f;
	in->connect = drop_connect_in;
	plugin_set(p, PLUGIN_DESCRIPTION, "drops n streams");
	plugin_set(p, PLUGIN_PIXMAP, "drop.png");
	plugin_set(p, PLUGIN_CATEGORY, "Routing");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Junctions_and_Dead_Ends");
	
	return filter_register(f, p);
}





#define FIFO_AUTOADJUST

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
		int feedback;
	} one2n_param_t;
	filter_pipe_t *in, *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf;
	one2n_param_t *p;
	int i, res, have_feedback, do_timeout;
	int maxfd, nr, eof, empty, oneempty, maxfifosize, maxallowedfifo;
	fd_set rset, wset;
	int nrin = 0, nrsel = 0;
	struct timeval timeout;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
        outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	nr = filterport_nrpipes(outp);
	if (!(in = filterport_get_pipe(inp)))
		FILTER_ERROR_RETURN("no input");
	if (!(p = ALLOCN(nr, one2n_param_t)))
	        FILTER_ERROR_RETURN("no memory");

	/* init struct */
	i = 0;
	have_feedback = 0;
	filterport_foreach_pipe(outp, out) {
		INIT_FEEDBACK_FIFO(p[i].fifo);
		p[i].fifo_size = 0;
		p[i].feedback = filterpipe_is_feedback(out);
		if (p[i].feedback)
			have_feedback = 1;
		p[i++].out = out;
	}

	FILTER_AFTER_INIT;

	/* In the following loop you may miss the EOF send - but its
	 * there, EOFs just get queued like regular buffers. Loop termination
	 * is at input EOF and all send queues empty time.
	 */
	eof = 0;
	maxallowedfifo = GLAME_MAX_BUFSIZE*SAMPLE_SIZE;
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
		if (!eof && /*oneempty &&*/ !(maxfifosize >= maxallowedfifo)) {
			FD_SET(in->dest_fd, &rset);
			if (in->dest_fd > maxfd)
				maxfd = in->dest_fd;
		}
		if (eof && empty)
			break;
#ifdef FIFO_AUTOADJUST
		do_timeout = (have_feedback && !eof
			      && maxfifosize >= maxallowedfifo
			      && maxallowedfifo < 1024*1024 /* hard limit */
			      && !empty && oneempty);
		if (do_timeout) {
			timeout.tv_sec = GLAME_WBUFSIZE/44100;
			timeout.tv_usec = (long)((1000000.0*(float)(GLAME_WBUFSIZE%44100))/44100);
		} else {
			/* We still need a timeout to catch configuration
			 * errors. */
			do_timeout = 1;
			timeout.tv_sec = 5;
			timeout.tv_usec = 0;
		}
#else
		do_timeout = 1;
		timeout.tv_sec = 5;
		timeout.tv_usec = 0;
#endif
		nrsel++;
		res = select(maxfd+1,
			     eof || /*!oneempty ||*/ (maxfifosize >= maxallowedfifo) ? NULL : &rset,
			     empty ? NULL : &wset, NULL,
			     do_timeout ? &timeout : NULL);
		if (res == -1 && errno == EPIPE)
			break;
		if (res == -1 && errno != EINTR)
			perror("select");
		if (res < 0)
			continue;
#ifdef FIFO_AUTOADJUST
		if (res == 0) {
			/* Can only happen after timeout -> adjust fifo size,
			 * but only if we can read from the input and write
			 * to at least one empty fifo output that is not
			 * feedback and we can _not_ write to all full feedback
			 * pipes (implicitly guaranteed by res == 0). */
			fd_set inset, outset;
			/* Network paused? */
			if (filter_has_error(n->launch_context->net))
				FILTER_ERROR_STOP("Network error");
			if (filter_is_ready(n->launch_context))
				continue;
			FD_ZERO(&inset);
			FD_SET(in->dest_fd, &inset);
			maxfd = in->dest_fd;
			FD_ZERO(&outset);
			for (i=0; i<nr; i++) {
				if (has_feedback(&p[i].fifo)
				    || p[i].feedback)
					continue;
				FD_SET(p[i].out->source_fd, &outset);
				if (p[i].out->source_fd > maxfd)
					maxfd = p[i].out->source_fd;
			}
			timeout.tv_sec = timeout.tv_usec = 0;
			if (select(maxfd+1, &inset, &outset, NULL, &timeout) <= 0)
				continue;
			if (!FD_ISSET(in->dest_fd, &inset))
				continue;
			for (i=0; i<nr; i++) {
				if (has_feedback(&p[i].fifo)
				    || p[i].feedback)
					continue;
				if (FD_ISSET(p[i].out->source_fd, &outset)) {
					maxallowedfifo *= 2;
					DPRINTF("Adjusting fifo size, fifo now %li (%.3fs at 44.1kHz)\n", (long)(maxallowedfifo/SAMPLE_SIZE), ((float)maxallowedfifo)/(44100*SAMPLE_SIZE));
					break;
				}
			}
			continue;
		}
#else
		if (res == 0) {
			/* Network paused? */
			if (filter_is_ready(n->launch_context))
				continue;
			FILTER_ERROR_STOP("Deadlock");
		}
#endif

		/* do we have input? - queue in each feedback buffer,
		 * be clever with the references, too. */
		if (FD_ISSET(in->dest_fd, &rset)) {
			nrin++;
			if (!(buf = fbuf_get(in)))
			        eof = 1;
			for (i=0; i<nr-1; i++) {
				if (p[i].fifo_size > maxallowedfifo)
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

	/* empty fifos. */
	for (i=0; i<nr; i++) {
		while (has_feedback(&p[i].fifo))
			fbuf_unref(get_feedback(&p[i].fifo));
	}

	FILTER_BEFORE_CLEANUP;

	free(p);

	FILTER_RETURN;
}

static int one2n_connect_out(filter_port_t *outp, filter_pipe_t *p)
{
	filter_pipe_t *in;
	filter_port_t *inp;
	filter_t *n;
	
	/* We accept any number of outputs. */
	n = filterport_filter(outp);
	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	if ((in = filterport_get_pipe(inp))) {
		p->type = in->type;
		p->u = in->u;
	};
	return 0;
}

int one2n_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *out;

	if (!(f = filter_creat(NULL))
	    || !filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				      FILTER_PORTTYPE_ANY,
				      FILTER_PORTFLAG_INPUT,
				      FILTERPORT_DESCRIPTION, "input",
				      FILTERPORT_END)
	    || !(out = filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
					     FILTER_PORTTYPE_ANY,
					     FILTER_PORTFLAG_OUTPUT,
					     FILTERPORT_DESCRIPTION, "output",
					     FILTERPORT_END)))
		return -1;
	f->f = one2n_f;
	out->connect = one2n_connect_out;

	plugin_set(p, PLUGIN_DESCRIPTION, "replicates one input n times");
	plugin_set(p, PLUGIN_PIXMAP, "one2n.png");
	plugin_set(p, PLUGIN_CATEGORY, "Routing");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Junctions_and_Dead_Ends");

	return filter_register(f, p);
}



/* Buffer provides an asynchron operating fifo with configurable
 * size. It can be used to prevent deadlocks in feed-forward or
 * feed-backward networks.
 */
static int buffer_f(filter_t *n)
{
	feedback_fifo_t fifo;
	filter_buffer_t *buf;
	filter_pipe_t *in, *out;
	fd_set infd, outfd;
	int maxfd, size, fifo_size, res;

	in = filterport_get_pipe(filterportdb_get_port(
		filter_portdb(n), PORTNAME_IN));
	out = filterport_get_pipe(filterportdb_get_port(
		filter_portdb(n), PORTNAME_OUT));
	if (!in || !out)
		FILTER_ERROR_RETURN("Insufficient inputs/outputs");

	size = filterparam_val_long(
		filterparamdb_get_param(filter_paramdb(n), "size"));
	size &= ~(SAMPLE_SIZE-1);
	size = MAX(size, GLAME_WBUFSIZE);

	INIT_FEEDBACK_FIFO(fifo);

	FILTER_AFTER_INIT;

	fifo_size = 0;
	while (in || has_feedback(&fifo)) {
	        FILTER_CHECK_STOP;

		/* Wait for pipe activity */
		maxfd = 0;
		FD_ZERO(&infd);
		if (in && fifo_size <= size) {
			FD_SET(in->dest_fd, &infd);
			maxfd = in->dest_fd;
		}
		FD_ZERO(&outfd);
		if (has_feedback(&fifo)) {
			FD_SET(out->source_fd, &outfd);
			maxfd = MAX(maxfd, out->source_fd);
		}
		res = select(maxfd+1,
			     (in && fifo_size <= size) ? &infd : NULL,
			     has_feedback(&fifo) ? &outfd : NULL,
			     NULL, NULL);
		if (res == -1 && errno != EINTR)
			FILTER_ERROR_STOP("Error in select");
		if (res <= 0)
			continue;

		/* First handle writes. */
		if (FD_ISSET(out->source_fd, &outfd)) {
			buf = get_feedback(&fifo);
			fifo_size -= fbuf_size(buf);
			fbuf_queue(out, buf);
		}

		/* Then handle reads. */
		if (FD_ISSET(in->dest_fd, &infd)) {
			buf = fbuf_get(in);
			if (!buf) {
				/* EOF. */
				in = NULL;
				continue;
			}
			/* TODO: shortcut on empty fifo, if possible. */
			add_feedback(&fifo, buf);
			fifo_size += fbuf_size(buf);
		}
	}

	FILTER_BEFORE_STOPCLEANUP;

	/* Queue EOF. */
	fbuf_queue(out, NULL);

	/* empty fifo. */
	while (has_feedback(&fifo))
		fbuf_unref(get_feedback(&fifo));

	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

static int buffer_set_size(filter_param_t *param, const void *val)
{
	filter_t *f;
	filter_pipe_t *pipe;
	filter_param_t *time;
	double t;

	/* check if we have a SAMPLE pipe as input. */
	f = filterparam_filter(param);
	pipe = filterport_get_pipe(
		filterportdb_get_port(filter_portdb(f), PORTNAME_IN));
	if (!pipe || filterpipe_type(pipe) != FILTER_PIPETYPE_SAMPLE)
		return 0;

	t = (double)filterparam_val_long(param)/SAMPLE_SIZE/filterpipe_sample_rate(pipe);
	time = filterparamdb_get_param(filter_paramdb(f), "time");
	if (!f->priv) {
		f->priv = (void *)1;
		filterparam_set(time, &t);
		f->priv = NULL;
	}

	return 0;
}

static int buffer_set_time(filter_param_t *param, const void *val)
{
	filter_t *f;
	filter_pipe_t *pipe;
	filter_param_t *size;
	long s;

	/* check if we have a SAMPLE pipe as input. */
	f = filterparam_filter(param);
	pipe = filterport_get_pipe(
		filterportdb_get_port(filter_portdb(f), PORTNAME_IN));
	if (!pipe || filterpipe_type(pipe) != FILTER_PIPETYPE_SAMPLE)
		return 0; /* FIXME - network load! return -1; */

	s = filterparam_val_double(param)*filterpipe_sample_rate(pipe)*SAMPLE_SIZE;
	size = filterparamdb_get_param(filter_paramdb(f), "size");
	if (!f->priv) {
		f->priv = (void *)1;
		filterparam_set(size, &s);
		f->priv = NULL;
	}

	return 0;
}

int buffer_register(plugin_t *p)
{
	filter_t *f;
	filter_param_t *param;

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = buffer_f;

	filterportdb_add_port(filter_portdb(f),
			      PORTNAME_IN, FILTER_PORTTYPE_ANY,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f),
			      PORTNAME_OUT, FILTER_PORTTYPE_ANY,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "output",
			      FILTERPORT_END);
	param = filterparamdb_add_param_long(filter_paramdb(f), "size",
				    FILTER_PARAMTYPE_LONG, 0,
				    FILTERPARAM_DESCRIPTION, "fifo size [bytes]",
				    FILTERPARAM_END);
	param->set = buffer_set_size;
	param = filterparamdb_add_param_double(filter_paramdb(f), "time",
				      FILTER_PARAMTYPE_TIME_S, 0.0,
				      FILTERPARAM_DESCRIPTION, "fifo time",
				      FILTERPARAM_END);
	param->set = buffer_set_time;

	plugin_set(p, PLUGIN_DESCRIPTION, "buffers a stream");
	plugin_set(p, PLUGIN_PIXMAP, "buffer.png");
	plugin_set(p, PLUGIN_CATEGORY, "Routing");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Junctions_and_Dead_Ends");
	
	return filter_register(f, p);
}
