/*
 * basic_sample.c
 * $Id: basic_sample.c,v 1.13 2000/04/25 09:05:23 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther
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
 * This file contains basic filters that operate using the sample
 * filter protocol. Contained are
 * - mix, mix2
 * - volume-adjust
 * - delay
 * - extend
 * - repeat
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


PLUGIN_SET(basic_sample, "mix mix2 volume_adjust delay extend repeat")



/* The mix filter mixes any number of input channels with an optional
 * gain parameter per input into one output channel which position
 * can be specified using the phi parameter.
 * - FIXME - positional information is ignored for now
 * Required for correct operation is at least one connected input
 * channel, and a connected output channel. Mixing is stopped if
 * one input is EOF, other inputs are subsequently dropped.
 * mix uses the following methods:
 * - connect_out to fill in the destinations rate and phi,
 * - fixup_param to correct the output phi,
 * - fixup_pipe to check for changed rate/phi, this also handles
 *   connects.
 * Failure is at launch time if samplerates of inputs dont match.
 * Mix is completely asynchron wrt input and output. This is to allow
 * buffer merging and to avoid deadlocks with feedback.
 */
static int mix(filter_node_t *n, int drop)
{
	typedef struct {
		filter_pipe_t *in;
		feedback_fifo_t fifo;
		int fifo_size, fifo_pos, out_pos;

		filter_buffer_t *buf;
		SAMPLE *s;
		int pos;

		int done;
		float factor;
	} mix_param_t;
	mix_param_t *p = NULL;
	filter_pipe_t  *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	int i, res, cnt, icnt;
	int rate, maxfd, out_pos, eof_pos, fifo_full, output_ready;
	int *j, jcnt;
	fd_set rset, wset;
	SAMPLE *s;
	float factor, gain;
	int nr, nr_done;

	/* We require at least one connected input and
	 * a connected output.
	 */
	nr = filternode_nrinputs(n);
	if (nr == 0)
		FILTER_ERROR_RETURN("no inputs");
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	/* init the structure, compute the needed factors */
	factor = 0.0;
	if (!(p = ALLOCN(nr, mix_param_t))
	    || !(j = ALLOCN(nr, int)))
		FILTER_ERROR_RETURN("no memory");

	gain = 1.0;
	if ((param = filternode_get_param(n, "gain")))
		gain = filterparam_val_float(param);

	rate = -1;
	i = 0;
	cnt = 1<<30;
	nr_done = 0;
	filternode_foreach_input(n, in) {
		p[i].in = in;
		INIT_FEEDBACK_FIFO(p[i].fifo);
		p[i].fifo_size = 0;
		p[i].fifo_pos = 0;
		p[i].buf = NULL;

		/* check rate */
		if (rate == -1)
			rate = filterpipe_sample_rate(in);
		else if (rate != filterpipe_sample_rate(in))
			FILTER_ERROR_CLEANUP("non matching samplerates");

		/* position factor - normalized afterwards, FIXME? */
	        p[i].factor = fabs(cos((filterpipe_sample_hangle(p[i].in)
					- filterpipe_sample_hangle(out))*0.5));
		factor += p[i].factor;

		/* external factor */
		if ((param = filterpipe_get_destparam(in, "gain")))
			p[i].factor *= filterparam_val_float(param);

		/* "offset" */
		p[i].out_pos = 0;
		if ((param = filterpipe_get_destparam(in, "offset"))) {
			p[i].out_pos = rate*filterparam_val_float(param);
			p[i].fifo_pos = p[i].out_pos;
		}
		cnt = MIN(cnt, p[i].out_pos);
		p[i].done = 0;
		i++;
	}
	/* normalize (position) factors, "move" delays */
	for (i=0; i<nr; i++) {
		p[i].factor = (p[i].factor*gain)/factor;
		p[i].out_pos -= cnt;
	}

	FILTER_AFTER_INIT;

	/* p[].in and out is used as done/EOF flag if it is NULL
	 */

	/* mix only until one input has eof */
	eof_pos = 1<<30;
	out_pos = 0;
	output_ready = 0;
	while (nr_done < nr) {
		FILTER_CHECK_STOP;

		/* set up fd sets for select */
		maxfd = 0;
		fifo_full = 1;
		FD_ZERO(&rset);
		for (i=0; i<nr; i++) {
			/* EOF or fifo full? */
			if (!p[i].in
			    || p[i].fifo_size >= GLAME_WBUFSIZE)
				continue;
			fifo_full = 0;
			FD_SET(p[i].in->dest_fd, &rset);
			if (p[i].in->dest_fd > maxfd)
				maxfd = p[i].in->dest_fd;
		}
		FD_ZERO(&wset);
		/* Queued a chunk but FIFO still full? -> Must keep going! */
		output_ready |= fifo_full;
		if (out && output_ready) {
			FD_SET(out->source_fd, &wset);
			if (out->source_fd > maxfd)
				maxfd = out->source_fd;
		}
		res = select(maxfd+1, fifo_full ? NULL : &rset,
			     !out || !output_ready ? NULL : &wset, NULL, NULL);
		if (res == -1) {
			if (errno != EINTR)
				perror("select");
			continue;
		}

		/* Check the inputs for buffers. */
		for (i=0; i<nr; i++) {
			if (!p[i].in || !FD_ISSET(p[i].in->dest_fd, &rset))
				continue;
			buf = sbuf_get(p[i].in);

			/* EOF from input? */
			if (!buf) {
				/* mark EOF */
				p[i].in = NULL;
				/* correct "drop after" pos */
				if (drop && p[i].fifo_pos < eof_pos)
					eof_pos = p[i].fifo_pos;
				/* input inactive now? */
				if (p[i].fifo_size == 0) {
					p[i].done = 1;
					nr_done++;
				}
				continue;
			}

			/* output finished? -> drop the buffer. */
			if (!out) {
				sbuf_unref(buf);
				continue;
			}

			/* add the buffer to the inputs feedback. */
			add_feedback(&p[i].fifo, buf);
			p[i].fifo_size += sbuf_size(buf);
			p[i].fifo_pos += sbuf_size(buf);
		}


		/* Send enough already? - queue EOF & stop sending. */
		if (out_pos == eof_pos) {
			sbuf_queue(out, NULL);
			out = NULL;
			output_ready = 0;

			/* Drop buffers pending in the fifos, further
			 * buffers will get dropped directly at receive. */
			for (i=0; i<nr; i++) {
				sbuf_unref(p[i].buf);
				while (has_feedback(&p[i].fifo))
					sbuf_unref(get_feedback(&p[i].fifo));
				p[i].buf = NULL;
				p[i].fifo_size = 0;
				if (!p[i].in) {
					p[i].done = 1;
					nr_done++;
				}
			}
		}


		/* The send code - rather complex as we try to group all
		 * pending buffers into the largest possible buffer. */

		/* Find # of samples we can send, if there is nothing
		 * (at least one input is missing) continue with receive.
		 * The following rules are used:
		 * - all buffers to be used have to be in fifos already
		 * - maximum # is GLAME_WBUFSIZE
		 * - do not write beyond eof_pos */
		cnt = MIN(GLAME_WBUFSIZE, eof_pos - out_pos);
		for (i=0; i<nr; i++) {
			/* fix cnt wrt available buffers */
			if (!p[i].done && p[i].fifo_pos - out_pos < cnt)
				cnt = p[i].fifo_pos - out_pos;
		}

		/* If there is nothing to write, give a hint to avoid
		 * select()ing on the output fd the next iteration. */
		if (cnt == 0) {
			output_ready = 0;
			continue;
		}
		output_ready = 1;


		/* If we are not ready to send anything just skip the
		 * send code. */
		if (!out || !FD_ISSET(out->source_fd, &wset))
			continue;

		/* alloc output buffer */
		buf = sbuf_make_private(sbuf_alloc(cnt, n));
		s = sbuf_buf(buf);

		/* loop through the input buffers, chunk by chunk
		 * - "inner loop" */
		do {
			/* get a buffer from the fifo for each input, by
			 * the way find the max. number of samples that can
			 * be processed in one turn. */
			jcnt = 0;
			icnt = cnt;
			for (i=0; i<nr; i++) {
				/* ignore finished (EOF) inputs. */
				if (p[i].done)
					continue;
				/* if input is inactive check we dont get
				 * active during this chunk. */
				if (!(p[i].out_pos == out_pos)) {
					icnt = MIN(icnt, p[i].out_pos - out_pos);
					continue;
				}
				/* input is active. Do we need to get a
				 * new buffer from the fifo? */
				if (!p[i].buf) {
					p[i].buf = get_feedback(&p[i].fifo);
					p[i].s = sbuf_buf(p[i].buf);
					p[i].pos = 0;
					p[i].fifo_size -= sbuf_size(p[i].buf);
				}
				/* check size */
				if (sbuf_size(p[i].buf) - p[i].pos < icnt)
					icnt = sbuf_size(p[i].buf) - p[i].pos;
				j[jcnt++] = i;
			}

			/* fix the resulting positions */
			for (i=0; i<nr; i++)
				if (p[i].out_pos == out_pos) {
					p[i].pos += icnt;
					p[i].out_pos += icnt;
				}
			cnt -= icnt;
			out_pos += icnt;

			/* to do really fast processing special-code
			 * a number of input channel counts */
			switch (jcnt) {
			case 0:
				/* Aieee, mummy told us but did we listen? */
				memset(s, 0, SAMPLE_SIZE*icnt);
				s += icnt;
				break;
			case 1:
				for (; (icnt & 3)>0; icnt--) {
					SCALARPROD_1D_1(s, p[j[0]].s, p[j[0]].factor);
				}
				for (; icnt>0; icnt-=4) {
					SCALARPROD_1D_4(s, p[j[0]].s, p[j[0]].factor);
				}
				break;
     			case 2:
				for (; (icnt & 3)>0; icnt--) {
					SCALARPROD_2D_1(s, p[j[0]].s, p[j[1]].s, p[j[0]].factor, p[j[1]].factor);
				}
				for (; icnt>0; icnt-=4) {
					SCALARPROD_2D_4(s, p[j[0]].s, p[j[1]].s, p[j[0]].factor, p[j[1]].factor);
				}
				break;
			case 3:
				for (; (icnt & 3)>0; icnt--) {
					SCALARPROD_3D_1(s, p[j[0]].s, p[j[1]].s, p[j[2]].s, p[j[0]].factor, p[j[1]].factor, p[j[2]].factor);
				}
				for (; icnt>0; icnt-=4) {
					SCALARPROD_3D_4(s, p[j[0]].s, p[j[1]].s, p[j[2]].s, p[j[0]].factor, p[j[1]].factor, p[j[2]].factor);
				}
				break;
			default:
				for (; icnt>0; icnt--) {
					*s = 0.0;
					for (i=0; i<jcnt; i++) {
						*s += *(p[j[i]].s++)*p[j[i]].factor;
					}
					s++;
				}
			}

			/* check for completed buffers and for
			 * inputs which become active. */
			for (i=0; i<nr; i++) {
				if (p[i].done)
					continue;
				if (p[i].buf
				    && sbuf_size(p[i].buf) == p[i].pos) {
					sbuf_unref(p[i].buf);
					p[i].buf = NULL;
					p[i].pos = 0;
					if (!p[i].in && p[i].fifo_size == 0) {
						p[i].done = 1;
						nr_done++;
					}
				}
			}
		} while (cnt>0);

		/* queue buffer */
		sbuf_queue(out, buf);
		/* Careful! There may still be plenty of samples in the FIFO
		 * we'll fixup later. */
		output_ready = 0;
	};

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(p);
	free(j);

	FILTER_RETURN;
}

static int mix_f(filter_node_t *n)
{
	return mix(n, 1);
}

static int mix2_f(filter_node_t *n)
{
	return mix(n, 0);
}

/* shared destination pipe property fixup code (rate & phi) */
static int mix_fixup(filter_node_t *n, filter_pipe_t *out)
{
	filter_param_t *param;
	filter_pipe_t *in;
	float phi = FILTER_PIPEPOS_DEFAULT;
	int rate = GLAME_DEFAULT_SAMPLERATE;

	if (!out && !(out = filternode_get_output(n, PORTNAME_OUT)))
		return 0;

	/* get samplerate & destination phi from inputs
	 * - rate to max rate of inputs (to allow feedback!)
	 * - phi either to the common input phi or to
	 *   the default phi */
	if ((in = filternode_get_input(n, PORTNAME_IN))) {
		rate = filterpipe_sample_rate(in);
		phi = filterpipe_sample_hangle(in);
		filternode_foreach_input(n, in) {
			if (filterpipe_sample_rate(in) > rate)
				rate = filterpipe_sample_rate(in);
			if (filterpipe_sample_hangle(in) != phi)
				phi = FILTER_PIPEPOS_DEFAULT;
		}
	}

	/* phi can be overridden by parameter */
	if ((param = filternode_get_param(n, "position")))
		phi = filterparam_val_float(param);

	if (rate != filterpipe_sample_rate(out)
	    || phi != filterpipe_sample_hangle(out)) {
		filterpipe_settype_sample(out, rate, phi);
		return 1;
	}
	return 0;
}
static void mix_fixup_param(filter_node_t *n, filter_pipe_t *p,
			    const char *name, filter_param_t *param)
{
	filter_pipe_t *out;

	if ((out = filternode_get_output(n, PORTNAME_OUT))
	    && mix_fixup(n, out))
		out->dest->filter->fixup_pipe(out->dest, out);
}
static int mix_connect_out(filter_node_t *n, const char *port,
			   filter_pipe_t *p)
{
	mix_fixup(n, p);

	return 0;
}
static void mix_fixup_pipe(filter_node_t *n, filter_pipe_t *in)
{
	filter_pipe_t *out;

	if ((out = filternode_get_output(n, PORTNAME_OUT))
	    && mix_fixup(n, out))
		out->dest->filter->fixup_pipe(out->dest, out);
}

int mix_register(plugin_t *p)
{
	filter_t *f;
	filter_portdesc_t *port;
	filter_paramdesc_t *param;

        if (!(f = filter_alloc(mix_f))
            || !(port = filter_add_input(f, PORTNAME_IN, "input stream",
				      FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE))
	    || !filterport_add_param(port, "gain", "input gain",
				     FILTER_PARAMTYPE_FLOAT)
	    || !(param = filterport_add_param(port, "offset", "input offset",
					      FILTER_PARAMTYPE_FLOAT)))
		return -1;
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_TIME_S);
	if (!filter_add_output(f, PORTNAME_OUT, "mixed stream",
			       FILTER_PORTTYPE_SAMPLE)
	    || !(filter_add_param(f, "gain", "output gain",
				  FILTER_PARAMTYPE_FLOAT))
	    || !(param = filter_add_param(f, "position", 
	                                  "position of mixed stream",
					  FILTER_PARAMTYPE_FLOAT)))
		return -1;
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_POSITION);
	f->connect_out = mix_connect_out;
	f->fixup_param = mix_fixup_param;
	f->fixup_pipe = mix_fixup_pipe;

	plugin_set(p, PLUGIN_DESCRIPTION, "mix n streams");
	plugin_set(p, PLUGIN_PIXMAP, "mix1.png");
	filter_attach(f, p);

	return 0;
}

int mix2_register(plugin_t *p)
{
	filter_t *f;
	filter_portdesc_t *port;
	filter_paramdesc_t *param;

        if (!(f = filter_alloc(mix2_f))
            || !(port = filter_add_input(f, PORTNAME_IN, "input stream",
				      FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE))
	    || !filterport_add_param(port, "gain", "input gain",
				     FILTER_PARAMTYPE_FLOAT)
	    || !(param = filterport_add_param(port, "offset", "input offset",
					      FILTER_PARAMTYPE_FLOAT)))
		return -1;
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_TIME_MS);
	if (!filter_add_output(f, PORTNAME_OUT, "mixed stream",
			       FILTER_PORTTYPE_SAMPLE)
	    || !(filter_add_param(f, "gain", "output gain",
				  FILTER_PARAMTYPE_FLOAT))
	    || !(param = filter_add_param(f, "position", 
	                                  "position of mixed stream",
					  FILTER_PARAMTYPE_FLOAT)))
		return -1;
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_POSITION);
	f->connect_out = mix_connect_out;
	f->fixup_param = mix_fixup_param;
	f->fixup_pipe = mix_fixup_pipe;

	plugin_set(p, PLUGIN_DESCRIPTION, "mix n streams");
	plugin_set(p, PLUGIN_PIXMAP, "mix2.png");
	filter_attach(f, p);

	return 0;
}





/* this is simple, it does work with one channel
 * only. */
static int volume_adjust_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *b;
	filter_param_t *scaleparam;
	float scale;
	SAMPLE *buf;
	int cnt;

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no input or no output");
	scale = 1.0;
	if ((scaleparam = filternode_get_param(n, "factor")))
		scale = filterparam_val_float(scaleparam);

	FILTER_AFTER_INIT;

	/* do the actual work */
	while ((b = sbuf_get(in))) {
		FILTER_CHECK_STOP;

		/* get working copy, if necessary,
		 * prepare for streamed loops. */
		b = sbuf_make_private(b);
		buf = sbuf_buf(b);
		cnt = sbuf_size(b);

		/* alignment loop */
		for (; (cnt & 3)>0; cnt--) {
			SCALARPROD_1D_1(buf, buf, scale);
		}

		/* streamed loop */
		for (; cnt>0; cnt-=4) {
			SCALARPROD_1D_4(buf, buf, scale);
		}

		/* queue the modified buffer */
		sbuf_queue(out, b);
	};

	/* forward the EOF mark */
	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int volume_adjust_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_alloc(volume_adjust_f))
	    || !filter_add_param(f, "factor", "scale factor",
				 FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_input(f, PORTNAME_IN, "input stream",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "scaled stream",
				  FILTER_PORTTYPE_SAMPLE))
		return -1;

	plugin_set(p, PLUGIN_DESCRIPTION, "adjust the volume of a stream");
	plugin_set(p, PLUGIN_PIXMAP, "default.xpm");
	filter_attach(f, p);

	return 0;
}





static int delay_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	int delay, chunksize;

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no input or no output");

	delay = 0;
	if ((param = filternode_get_param(n, "delay")))
	        delay = (int)(filterpipe_sample_rate(in)
			        * filterparam_val_float(param)/1000.0);
	if (delay < 0)
		FILTER_ERROR_RETURN("weird delay time");

	FILTER_AFTER_INIT;

	/* send "delay" zero samples, GLAME_WBUFSIZE samples per buffer */
	chunksize = GLAME_WBUFSIZE; // filterpipe_sample_rate(in)/10;
	buf = sbuf_alloc(chunksize, n);
	memset(sbuf_buf(buf), 0, SAMPLE_SIZE*chunksize);
	for (; delay/chunksize > 0; delay -= chunksize) {
		sbuf_ref(buf);
		sbuf_queue(out, buf);
	}
	sbuf_unref(buf);

	/* send the rest in one buffer */
	if (delay > 0) {
		buf = sbuf_alloc(delay, n);
		memset(sbuf_buf(buf), 0, SAMPLE_SIZE*delay);
		sbuf_queue(out, buf);
	}

	/* just forward all incoming buffers now */
	while ((buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;
		sbuf_queue(out, buf);
	}
	sbuf_queue(out, buf);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int delay_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdesc_t *param;

	if (!(f = filter_alloc(delay_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input stream to delay",
				  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_output(f, PORTNAME_OUT, "delayed output stream",
				   FILTER_PORTTYPE_SAMPLE))
	    || !(param = filter_add_param(f, "delay", "delay in ms",
					  FILTER_PARAMTYPE_FLOAT)))
		return -1;
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_TIME_MS);

	plugin_set(p, PLUGIN_DESCRIPTION, "delay an audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "delay.xpm");
	filter_attach(f, p);

	return 0;
}





static int extend_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	int time, chunksize;

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no input or no output");

	time = 0;
	if ((param = filternode_get_param(n, "time")))
	        time = (int)(filterpipe_sample_rate(in)
			        * filterparam_val_float(param)/1000.0);
	if (time < 0)
		FILTER_ERROR_RETURN("weird extend time");

	FILTER_AFTER_INIT;

	/* just forward all incoming buffers now */
	while ((buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;
		sbuf_queue(out, buf);
	}

	/* send "time" zero samples, GLAME_WBUFSIZE samples per buffer */
	chunksize = GLAME_WBUFSIZE; // filterpipe_sample_rate(in)/10;
	buf = sbuf_alloc(chunksize, n);
	memset(sbuf_buf(buf), 0, SAMPLE_SIZE*chunksize);
	for (; time/chunksize > 0; time -= chunksize) {
		FILTER_CHECK_STOP;
		sbuf_ref(buf);
		sbuf_queue(out, buf);
	}
	sbuf_unref(buf);

	/* send the rest in one buffer */
	if (time > 0) {
		buf = sbuf_alloc(time, n);
		memset(sbuf_buf(buf), 0, SAMPLE_SIZE*time);
		sbuf_queue(out, buf);
	}

	/* send EOF */
	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int extend_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdesc_t *param;

	if (!(f = filter_alloc(extend_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input stream to extend",
				  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_output(f, PORTNAME_OUT, "extended output stream",
				   FILTER_PORTTYPE_SAMPLE))
	    || !(param = filter_add_param(f, "time", "extend time in ms",
					  FILTER_PARAMTYPE_FLOAT)))
		return -1;
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_TIME_MS);

	plugin_set(p, PLUGIN_DESCRIPTION, "extend an audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "extend.xpm");
	filter_attach(f, p);

	return 0;
}





static int repeat_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf, *buf2;
	feedback_fifo_t fifo;
	filter_param_t *param;
	int duration;

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no input or no output");

	duration = 0;
	if ((param = filternode_get_param(n, "duration")))
	        duration = filterpipe_sample_rate(in)*filterparam_val_float(param);
	if (duration < 0)
		FILTER_ERROR_RETURN("weird time");
	INIT_FEEDBACK_FIFO(fifo);

	FILTER_AFTER_INIT;

	/* read whole input stream into internal fifo
	 * but queue stuff once already. */
	goto entry1;
	do {
		FILTER_CHECK_STOP;
		sbuf_ref(buf);
		add_feedback(&fifo, buf);
		duration -= sbuf_size(buf);
		sbuf_queue(out, buf);
	entry1:
		buf = sbuf_get(in);
	} while (buf && duration >= sbuf_size(buf));

	/* too many buffers from input? */
	if (buf) {
		/* part of a buffer still to be send? */
		if (duration > 0) {
			buf2 = sbuf_alloc(duration, n);
			buf2 = sbuf_make_private(buf2);
			memcpy(sbuf_buf(buf2), sbuf_buf(buf), duration*SAMPLE_SIZE);
			duration = 0;
			sbuf_queue(out, buf2);
		}
		/* drop mode */
		do {
			sbuf_unref(buf);
		} while ((buf = sbuf_get(in)));
	}

	/* all input is processed, continue to output from fifo
	 * until duration number of samples were sent. */
	goto entry2;
	do {
		FILTER_CHECK_STOP;
		sbuf_ref(buf);
		add_feedback(&fifo, buf);
		duration -= sbuf_size(buf);
		sbuf_queue(out, buf);
	entry2:
		buf = get_feedback(&fifo);
	} while (buf && duration > sbuf_size(buf));

	/* part left to be sent? */
	if (buf && duration > 0) {
		buf2 = sbuf_alloc(duration, n);
		buf2 = sbuf_make_private(buf2);
		memcpy(sbuf_buf(buf2), sbuf_buf(buf), duration*SAMPLE_SIZE);
		sbuf_unref(buf);
		sbuf_queue(out, buf2);
	}

	FILTER_BEFORE_STOPCLEANUP;

	/* free buffers in the fifo and send final EOF */
	while ((buf = get_feedback(&fifo)))
		sbuf_unref(buf);
	sbuf_queue(out, NULL);

	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int repeat_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdesc_t *param;

	if (!(f = filter_alloc(repeat_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input stream to repeat",
				  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_output(f, PORTNAME_OUT, "repeated stream",
				   FILTER_PORTTYPE_SAMPLE))
	    || !(param = filter_add_param(f, "duration",
					  "total duration in ms",
					  FILTER_PARAMTYPE_FLOAT)))
		return -1;
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_TIME_S);

	plugin_set(p, PLUGIN_DESCRIPTION, "repeat an audio stream for the specified time");
	plugin_set(p, PLUGIN_PIXMAP, "repeat.xpm");
	filter_attach(f, p);

	return 0;
}
