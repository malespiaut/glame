/*
 * basic_sample.c
 * $Id: basic_sample.c,v 1.49 2001/07/31 09:22:30 richi Exp $
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
 * - mix
 * - render
 * - volume-adjust
 * - delay
 * - extend
 * - repeat
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "filter_methods.h"

PLUGIN_SET(basic_sample, "mix render volume_adjust delay extend repeat")



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
 * The mix "state machine" has the following states with associated
 * actions:
 * - at least one fifo is not full, there is enough data for an output
 *   try to read from not full fifos, try to output
 * - at least one fifo is not full, there is not enough data for an output
 *   try to read from not full fifo
 * - all fifos are full, there is enough data for an output
 *   try to output
 * - (all fifos are full, there is not enough data for an output)
 *   not possible
 */
static int mix_f(filter_t *n)
{
	typedef struct {
		filter_pipe_t *in;
		feedback_fifo_t fifo;
		int fifo_size, fifo_pos, out_pos;

		filter_buffer_t *buf;
		SAMPLE *s;
		int pos;

		int done, feedback;
		float factor;
	} mix_param_t;
	mix_param_t *p = NULL;
	filter_pipe_t *in, *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf;
	filter_param_t *param;
	struct timeval timeout;
	int i, res, cnt, icnt;
	int rate, maxfd, out_pos, eof_pos, have_feedback;
	int fifo_full, output_ready, nr_ready_to_send, feedback_fifo_full;
	int *j, jcnt;
	fd_set rset, wset;
	SAMPLE *s, **js;
	float factor, gain, *jf;
	int nr, nr_done, nr_eof, drop;
	int max_fifo_size;

	/* We require at least one connected input and
	 * a connected output.
	 */
	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	nr = filterport_nrpipes(inp);
	if (nr == 0)
		FILTER_ERROR_RETURN("no inputs");
	if (!(out = filterport_get_pipe(outp)))
		FILTER_ERROR_RETURN("no output");

	/* init the structure, compute the needed factors */
	factor = 0.0;
	if (!(p = ALLOCN(nr, mix_param_t))
	    || !(j = ALLOCN(nr, int))
	    || !(js = ALLOCN(nr, SAMPLE *))
	    || !(jf = ALLOCN(nr, float)))
		FILTER_ERROR_RETURN("no memory");

	gain = filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "gain"));

	rate = -1;
	i = 0;
	cnt = 1<<30;
	nr_done = nr_eof = 0;
	have_feedback = 0;
	filterport_foreach_pipe(inp, in) {
		p[i].in = in;
		INIT_FEEDBACK_FIFO(p[i].fifo);
		p[i].fifo_size = 0;
		p[i].fifo_pos = 0;
		p[i].buf = NULL;

		/* check rate */
		if (rate == -1)
			rate = filterpipe_sample_rate(in);
		else if (rate != filterpipe_sample_rate(in)) {
			DPRINTF("rate = %d, other rate = %d\n",rate, filterpipe_sample_rate(in));
			FILTER_ERROR_CLEANUP("non matching samplerates");
		}
		/* position factor - normalized afterwards, FIXME? */
	        p[i].factor = fabs(cos((filterpipe_sample_hangle(p[i].in)
					- filterpipe_sample_hangle(out))*0.5));
		factor += p[i].factor;

		/* FIXME! external factor */
		param = filterparamdb_get_param(filterpipe_destparamdb(in), "gain");
		p[i].factor *= filterparam_val_float(param);

		/* FIXME! "offset" */
		p[i].out_pos = 0;
		param = filterparamdb_get_param(filterpipe_destparamdb(in), "offset");
		p[i].out_pos = rate*filterparam_val_float(param);
		p[i].fifo_pos = p[i].out_pos;

		cnt = MIN(cnt, p[i].out_pos);
		p[i].done = 0;
		p[i].feedback = filterpipe_is_feedback(in);
		if (p[i].feedback)
			have_feedback = 1;
		i++;
	}
	/* normalize (position) factors, "move" delays */
	for (i=0; i<nr; i++) {
		p[i].factor = (p[i].factor*gain)/factor;
		p[i].out_pos -= cnt;
	}

	/* FIXME: drop only after all non-feedback channels have
	 * completed. */
	if (have_feedback)
		drop = 1;
	else
		drop = 0;


	FILTER_AFTER_INIT;

	/* p[].in and out is used as done/EOF flag if it is NULL
	 */

	/* mix only until one input has eof (if drop parameter is true) */
	max_fifo_size = GLAME_MAX_BUFSIZE;
	eof_pos = 1<<30;
	out_pos = 0;
	output_ready = 0;
	while (nr_done < nr) {
		FILTER_CHECK_STOP;

		/* Set up fd sets for select. */
		maxfd = 0;
		/* The read part - only select those fds who do
		 * not have full fifos / have EOF. */
		fifo_full = 1;
		feedback_fifo_full = 0;
		FD_ZERO(&rset);
		for (i=0; i<nr; i++) {
			/* EOF or fifo full? */
			if (!p[i].in
			    || p[i].fifo_size >= max_fifo_size) {
				if (p[i].feedback)
					feedback_fifo_full = 1;
				continue;
			}
			fifo_full = 0;
			FD_SET(p[i].in->dest_fd, &rset);
			if (p[i].in->dest_fd > maxfd)
				maxfd = p[i].in->dest_fd;
		}
		/* Queued a chunk but FIFO still full? -> Must keep going!
		   output_ready |= fifo_full; */
		/* Write part - only select, if we're not in dropping
		 * mode and have data ready to send. */
		FD_ZERO(&wset);
		if ((out && output_ready) || fifo_full) {
			FD_SET(out->source_fd, &wset);
			if (out->source_fd > maxfd)
				maxfd = out->source_fd;
		}
		/* Do the actual select - the timeout is to detect
		 * deadlocks in the network or a huge amount of
		 * feedback for which we should adjust our maximum
		 * fifo size (this timeout - at full fifo - is adjusted
		 * to meet the RT criteria). */
		if (feedback_fifo_full && out && output_ready
		    && max_fifo_size < 1024*1024) {
			timeout.tv_sec = GLAME_WBUFSIZE/rate;
			timeout.tv_usec = (long)((1000000.0*(float)(GLAME_WBUFSIZE%rate))/rate);
		} else {
			timeout.tv_sec = 5;
			timeout.tv_usec = 0;
		}
		res = select(maxfd+1,
			     fifo_full ? NULL : &rset,
			     (!out || !output_ready) && !fifo_full ? NULL : &wset,
			     NULL, &timeout);
		if (res == -1) {
			if (errno != EINTR)
				perror("select");
			continue;
		}
		if (res == 0) {
			/* Network paused? */
			if (filter_is_ready(n))
				continue;
			/* If we have a ready-to-read (or full fifo?) feedback
			 * input and a not-ready-to-write output we have to
			 * enlarge our fifo. */
			if (out && output_ready && feedback_fifo_full) {
				if (!FD_ISSET(out->source_fd, &wset)) {
					max_fifo_size *= 2;
					DPRINTF("Adjusting fifo size, fifo now %i (%.3fs)\n",
						max_fifo_size, ((float)max_fifo_size)/rate);
				}
				continue;
			}
			/* Uh, deadlock... */
			FILTER_ERROR_STOP("Deadlock");
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
				nr_eof++;
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
		 * - do not write beyond eof_pos */
		cnt = eof_pos - out_pos;
		for (i=0; i<nr; i++) {
			/* fix cnt wrt available buffers */
			if (!p[i].done && p[i].fifo_pos - out_pos < cnt)
				cnt = p[i].fifo_pos - out_pos;
		}
		/* Dont send more than GLAME_WBUFSIZE in one buffer,
		 * remember the # of buffers we could send. */
		nr_ready_to_send = cnt / GLAME_WBUFSIZE;
		cnt = MIN(GLAME_WBUFSIZE, cnt);

		/* If there is nothing to write, give a hint to avoid
		 * select()ing on the output fd the next iteration.
		 * Try to coalesce at least GLAME_WBUFSIZE size buffers,
		 * only allow smaller buffers, if all inputs are EOF. */
		if (cnt == 0
		    || (cnt < GLAME_WBUFSIZE
			&& nr_eof != nr && (!drop || nr_eof == 0))) {
			output_ready = 0;
			continue;
		}
		output_ready = 1;

		/* If we are not ready to send anything just skip the
		 * send code. */
		if (!out || !FD_ISSET(out->source_fd, &wset))
			continue;

		/* Alloc output buffer. */
		buf = sbuf_make_private(sbuf_alloc(cnt, n));
		s = sbuf_buf(buf);

		/* Loop through the input buffers, chunk by chunk
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

			/* Do SIMD optimized mixing. We need to setup
			 * a sample pointer array and a factor array
			 * and cleanup afterwards (correcting the position
			 * pointers). */
			for (i=0; i<jcnt; i++) {
				js[i] = p[j[i]].s;
				jf[i] = p[j[i]].factor;
			}
			glsimd.scalar_product_Nd(s, icnt, js, jf, jcnt);
			s += icnt;
			for (i=0; i<jcnt; i++)
				p[j[i]].s += icnt;

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

		/* We remembered the number of buffers we could have send,
		 * so just update output_read with this information. */
		output_ready = nr_ready_to_send - 1 > 0 ? 1 : 0;
	};

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(p);
	free(j);
	free(js);
	free(jf);

	FILTER_RETURN;
}

/* shared destination pipe property fixup code (rate & phi) */
static int mix_fixup(filter_t *n, filter_pipe_t *out)
{
	filter_pipe_t *in;
	filter_port_t *inp, *outp;
	float phi = FILTER_PIPEPOS_DEFAULT;
	int rate = GLAME_DEFAULT_SAMPLERATE;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if (!out && !(out = filterport_get_pipe(outp)))
		return 0;

	/* get samplerate & destination phi from inputs
	 * - rate to max rate of inputs (to allow feedback!)
	 * - phi either to the common input phi or to
	 *   the default phi */
	if ((in = filterport_get_pipe(inp))) {
		rate = filterpipe_sample_rate(in);
		phi = filterpipe_sample_hangle(in);
		filterport_foreach_pipe(inp, in) {
			if (filterpipe_sample_rate(in) > rate)
				rate = filterpipe_sample_rate(in);
			if (filterpipe_sample_hangle(in) != phi)
				phi = FILTER_PIPEPOS_DEFAULT;
		}
	}

	/* FIXME! phi can be overridden by parameter */
	phi = filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "position"));

	if (rate != filterpipe_sample_rate(out)
	    || phi != filterpipe_sample_hangle(out)) {
		filterpipe_settype_sample(out, rate, phi);
		glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
		return 1;
	}
	return 0;
}
static int mix_connect_in(filter_t *n, filter_port_t *port,
			  filter_pipe_t *p)
{
	/* We accept any number of inputs. */
	return 0;
}
static int mix_connect_out(filter_t *n, filter_port_t *port,
			   filter_pipe_t *p)
{
	/* We accept only one output. */
	if (filterport_get_pipe(port))
		return -1;

	/* Fixup wrt new pipe. */
	mix_fixup(n, p);

	return 0;
}
static void mix_handler(glsig_handler_t *h, long sig, va_list va)
{
	filter_t *n;
	filter_param_t *param;
	filter_pipe_t *in;
	filter_pipe_t *out;
	filter_port_t *outp;

	if (sig == GLSIG_PIPE_CHANGED) {
		GLSIGH_GETARGS1(va, in);
		n = filterport_filter(filterpipe_dest(in));
	} else if (sig == GLSIG_PARAM_CHANGED) {
		GLSIGH_GETARGS1(va, param);
		n = filterparam_filter(param);
	} else
		return;

	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if ((out = filterport_get_pipe(outp)))
		mix_fixup(n, out);
}

int mix_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *port;

        if (!(f = filter_creat(NULL)))
		return -1;

	port = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				     FILTER_PORTTYPE_SAMPLE,
				     FILTER_PORTFLAG_INPUT,
				     FILTERPORT_DESCRIPTION, "input stream",
				     FILTERPORT_END);
	filterparamdb_add_param_float(filterport_paramdb(port), "gain",
				      FILTER_PARAMTYPE_FLOAT, 1.0,
				      FILTERPARAM_END);
	filterparamdb_add_param_float(filterport_paramdb(port), "offset",
				      FILTER_PARAMTYPE_TIME_MS, 0.0,
				      FILTERPARAM_END);

	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "mixed stream",
			      FILTERPORT_END);

	filterparamdb_add_param_float(filter_paramdb(f), "gain",
				      FILTER_PARAMTYPE_FLOAT, 1.0,
				      FILTERPARAM_END);
	filterparamdb_add_param_float(filter_paramdb(f), "position",
				      FILTER_PARAMTYPE_POSITION, FILTER_PIPEPOS_DEFAULT,
				      FILTERPARAM_END);

	f->f = mix_f;
	f->connect_in = mix_connect_in;
	f->connect_out = mix_connect_out;

	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED|GLSIG_PIPE_CHANGED,
			  mix_handler, NULL);

	plugin_set(p, PLUGIN_DESCRIPTION, "mix n streams until all non-feedback inputs end allowing feedback");
	plugin_set(p, PLUGIN_PIXMAP, "mix1.png");
	plugin_set(p, PLUGIN_CATEGORY, "Routing");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Junctions_and_Dead_Ends");
	return filter_register(f, p);
}





static int render_f(filter_t *n)
{
	filter_port_t *in, *out;
	filter_pipe_t *p;
	nto1_state_t *I;
	SAMPLE **facts;
	filter_buffer_t *buf;
	SAMPLE *s, **simd_buf, *simd_fact;
	int nr_in, nr_out, nr_active;
	int cnt, i, j, k;

	in = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	out = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if ((nr_out = filterport_nrpipes(out)) == 0)
		FILTER_ERROR_RETURN("no outputs");
	if ((nr_in = nto1_init(&I, in)) == -1)
		FILTER_ERROR_RETURN("no inputs");

	/* Generate nr_out arrays of factors for the inputs. */
	i = 0;
	facts = ALLOCN(nr_out, float *);
	filterport_foreach_pipe(out, p) {
		facts[i] = ALLOCN(nr_in, float);
		for (j=0; j<nr_in; j++) {
			/* The factor is the difference in hangle divided
			 * by PI and subtracted from 1.0 - this way we
			 * get linear scaling between 0.0 and PI/2. */
			float ang_diff, min_ang, max_ang;
			min_ang = MIN(filterpipe_sample_hangle(I[j].in),
				      filterpipe_sample_hangle(p));
			max_ang = MAX(filterpipe_sample_hangle(I[j].in),
				      filterpipe_sample_hangle(p));
			ang_diff = max_ang - min_ang;
			if (ang_diff > M_PI)
				ang_diff = 2.0*M_PI - ang_diff;
			if (ang_diff < 0.0 || ang_diff > M_PI)
				DPRINTF("FUCK!\n");
			facts[i][j] = 1.0 - ang_diff/M_PI;
			DPRINTF("For output %i [%.3f] and input %i [%.3f] out of ang_diff %.3f factor is %.3f\n",
				i, filterpipe_sample_hangle(p),
				j, filterpipe_sample_hangle(I[j].in),
				ang_diff, facts[i][j]);
		}
		i++;
	}

	simd_buf = ALLOCN(nr_in, float *);
	simd_fact = ALLOCN(nr_in, float);

	FILTER_AFTER_INIT;

	nr_active = nr_in;
	goto entry;
	do {
		FILTER_CHECK_STOP;

		/* Head. Find maximum number of processable samples,
		 * fix destination position. */
		cnt = nto1_head(I, nr_in);

		/* Prepare active buffers for simd. */
		i = 0;
		for (j=0; j<nr_in; j++)
			if (I[j].buf)
				simd_buf[i++] = I[j].s;
		/* now i == nr_active */

		i = 0;
		filterport_foreach_pipe(out, p) {
			/* Allocate the output buffer. */
			buf = sbuf_make_private(sbuf_alloc(cnt, n));
			s = sbuf_buf(buf);

			/* prepare factors for simd */
			k = 0;
			for (j=0; j<nr_in; j++)
				if (I[j].buf)
					simd_fact[k++] = facts[i][j];

			/* do simd operation */
			glsimd.scalar_product_Nd(s, cnt,
						 simd_buf, simd_fact,
						 nr_active);

			sbuf_queue(p, buf);
			i++;
		}

		for (i=0; i<nr_in; i++)
			if (I[i].buf)
				I[i].s += cnt;

	entry:
		/* Tail & entry. Check if we need to get additional
		 * buffers, recognize EOF's. */
		nr_active -= nto1_tail(I, nr_in);
	} while (nr_active>0);

	/* Queue EOFs. */
	filterport_foreach_pipe(out, p)
		sbuf_queue(p, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	for (i=0; i<nr_out; i++)
		free(facts[i]);
	free(facts);
	free(simd_buf);
	free(simd_fact);
	nto1_cleanup(I);

	FILTER_RETURN;
}
static void render_fixup_pipe(glsig_handler_t *h, long sig, va_list va)
{
	filter_port_t *port;
	filter_pipe_t *pipe;
	int rate;

	/* We dont care about a special pipe - just re-scan all pipes
	 * looking for matching samplerates and possibly fixing all
	 * outgoing pipes. */
	GLSIGH_GETARGS1(va, pipe);
	port = filterpipe_source(pipe);
	rate = filterpipe_sample_rate(pipe);
	filterport_foreach_pipe(port, pipe)
		if (filterpipe_sample_rate(pipe) != rate) {
			filter_set_error(filterport_filter(port),
					 "not matching samplerates");
			return;
		}
	filter_clear_error(filterport_filter(port));

	port = filterportdb_get_port(filter_portdb(filterport_filter(port)),
				     PORTNAME_OUT);
	filterport_foreach_pipe(port, pipe) {
		if (filterpipe_sample_rate(pipe) == rate)
			continue;
		filterpipe_settype_sample(pipe, rate,
					  filterpipe_sample_hangle(pipe));
		glsig_emit(filterpipe_emitter(pipe), GLSIG_PIPE_CHANGED, pipe);
	}
}
static void render_fixup_param(glsig_handler_t *h, long sig, va_list va)
{
	filter_param_t *param;
	filter_pipe_t *pipe;

	GLSIGH_GETARGS1(va, param);
	pipe = filterparam_get_sourcepipe(param);
	filterpipe_settype_sample(pipe,
				  filterpipe_sample_rate(pipe),
				  filterparam_val_float(param));
	glsig_emit(filterpipe_emitter(pipe), GLSIG_PIPE_CHANGED, pipe);
}
static int render_connect_in(filter_t *n, filter_port_t *port,
			     filter_pipe_t *p)
{
	/* We accept any number of inputs. Fixup is done through
	 * raised GLSIG_PIPE_CHANGED. */
	return 0;
}
static int render_connect_out(filter_t *n, filter_port_t *port,
			      filter_pipe_t *p)
{
	filter_pipe_t *pipe;
	int rate = GLAME_DEFAULT_SAMPLERATE;

	/* We accept any number of outputs. Do basic setup - i.e.
	 * set rate and position. */
	port = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	pipe = filterport_get_pipe(port);
	if (pipe)
		rate = filterpipe_sample_rate(pipe);
	filterpipe_settype_sample(p, rate,
				  FILTER_PIPEPOS_DEFAULT);
	return 0;
}
int render_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *in, *out;

        if (!(f = filter_creat(NULL)))
		return -1;

	in = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				   FILTER_PORTTYPE_SAMPLE,
				   FILTER_PORTFLAG_INPUT,
				   FILTERPORT_DESCRIPTION, "input stream",
				   FILTERPORT_END);
	out = filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
				    FILTER_PORTTYPE_SAMPLE,
				    FILTER_PORTFLAG_OUTPUT,
				    FILTERPORT_DESCRIPTION, "mixed stream",
				    FILTERPORT_END);
	filterparamdb_add_param_float(filterport_paramdb(out), "position",
				      FILTER_PARAMTYPE_POSITION,
				      FILTER_PIPEPOS_DEFAULT,
				      FILTERPARAM_END);

	f->f = render_f;
	f->connect_in = render_connect_in;
	f->connect_out = render_connect_out;

	glsig_add_handler(filterport_emitter(in), GLSIG_PIPE_CHANGED,
			  render_fixup_pipe, NULL);
	glsig_add_handler(filter_emitter(f), GLSIG_PARAM_CHANGED,
			  render_fixup_param, NULL);

	plugin_set(p, PLUGIN_DESCRIPTION, "re-render soundfield");
	plugin_set(p, PLUGIN_PIXMAP, "render.png");
	plugin_set(p, PLUGIN_CATEGORY, "Filter");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Junctions_and_Dead_Ends");

	return filter_register(f, p);
}




/* this is simple, it does work with one channel
 * only. */

/* use set_param to link factor and dbgain parameters */

static int vadjust_set_param(filter_t *n, filter_param_t *param, const void *val) {
	float ngain, gain;
	filter_param_t *anyscale;

	if (n->priv)
		return 0;

	gain = *((float*)val);
	
	if (strcmp("factor", filterparam_label(param))==0) {
		anyscale = filterparamdb_get_param(filter_paramdb(n), "dbgain");
		ngain = GAIN2DB(gain);
		n->priv = param;
		filterparam_set(anyscale, &ngain);
		n->priv = NULL;
	}
	
	if (strcmp("dbgain", filterparam_label(param))==0) {
		anyscale = filterparamdb_get_param(filter_paramdb(n), "factor");
		ngain = DB2GAIN(gain);
		n->priv = param;
		filterparam_set(anyscale, &ngain);
		n->priv = NULL;
	}
	return 0;
}
		
static int volume_adjust_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_port_t *inp, *outp;
	filter_param_t *scale;
	filter_buffer_t *b;
	SAMPLE *buf;
	int cnt;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if (!(in = filterport_get_pipe(inp))
	    || !(out = filterport_get_pipe(outp)))
		FILTER_ERROR_RETURN("no input or no output");
	scale = filterparamdb_get_param(filter_paramdb(n), "factor");
	
	FILTER_AFTER_INIT;

	/* do the actual work */
	while ((b = sbuf_get(in))) {
		FILTER_CHECK_STOP;

		/* get working copy, if necessary,
		 * prepare for streamed loops. */
		b = sbuf_make_private(b);
		buf = sbuf_buf(b);
		cnt = sbuf_size(b);

		/* adjust the amplitude by scale */
		glsimd.scalar_product_1dI(buf, cnt,
					  filterparam_val_float(scale));

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

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = volume_adjust_f;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "scaled stream",
			      FILTERPORT_END);

	filterparamdb_add_param_float(filter_paramdb(f), "factor",
				      FILTER_PARAMTYPE_FLOAT, 1.0,
				      FILTERPARAM_END);
	
	filterparamdb_add_param_float(filter_paramdb(f), "dbgain",
				      FILTER_PARAMTYPE_FLOAT, 0.0,
				      FILTERPARAM_END);
	
	f->set_param = vadjust_set_param;
	
	plugin_set(p, PLUGIN_DESCRIPTION, "adjust the volume of a stream");
	plugin_set(p, PLUGIN_PIXMAP, "volume_adjust.png");
	plugin_set(p, PLUGIN_CATEGORY, "Volume");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Mangling_Data_Streams");
	plugin_set(p, PLUGIN_LABEL, "Volume Adjust");
  
	return filter_register(f, p);
}





static int delay_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf;
	int delay, chunksize;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if (!(in = filterport_get_pipe(inp))
	    || !(out = filterport_get_pipe(outp)))
		FILTER_ERROR_RETURN("no input or no output");

	delay = (int)(filterpipe_sample_rate(in)
		      * filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "delay"))/1000.0);
	if (delay < 0)
		FILTER_ERROR_RETURN("weird delay time");

	FILTER_AFTER_INIT;

	/* send "delay" zero samples, GLAME_WBUFSIZE samples per buffer */
	chunksize = GLAME_WBUFSIZE;
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
	filter_param_t *param;

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = delay_f;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream to delay",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "delayed output stream",
			      FILTERPORT_END);

	param = filterparamdb_add_param_float(filter_paramdb(f), "delay",
					      FILTER_PARAMTYPE_TIME_MS, 0.0,
					      FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "delay an audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "delay.png");
	plugin_set(p, PLUGIN_CATEGORY, "Time");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Mangling_Data_Streams");
  
	return filter_register(f, p);
}





static int extend_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf;
	int time, chunksize;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if (!(in = filterport_get_pipe(inp))
	    || !(out = filterport_get_pipe(outp)))
		FILTER_ERROR_RETURN("no input or no output");

	time = (int)(filterpipe_sample_rate(in)
		     * filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "time"))/1000.0);
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

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = extend_f;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream to extend",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "extended output stream",
			      FILTERPORT_END);

	filterparamdb_add_param_float(filter_paramdb(f), "time",
				      FILTER_PARAMTYPE_TIME_MS, 0.0,
				      FILTERPARAM_DESCRIPTION, "extend time in ms",
				      FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "extend an audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "extend.png");
	plugin_set(p, PLUGIN_CATEGORY, "Time");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Mangling_Data_Streams");
  
	return filter_register(f, p);
}





static int repeat_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf, *buf2;
	feedback_fifo_t fifo;
	int duration;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if (!(in = filterport_get_pipe(inp))
	    || !(out = filterport_get_pipe(outp)))
		FILTER_ERROR_RETURN("no input or no output");

	duration = filterpipe_sample_rate(in)*filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "duration"));
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

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = repeat_f;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream to repeat",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "repeated stream",
			      FILTERPORT_END);
	filterparamdb_add_param_float(filter_paramdb(f), "duration",
				  FILTER_PARAMTYPE_TIME_S, 1.0,
				  FILTERPARAM_DESCRIPTION, "total duration in seconds",
				  FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION,
		   "repeat an audio stream for the specified time");
	plugin_set(p, PLUGIN_PIXMAP, "repeat.png");
	plugin_set(p, PLUGIN_CATEGORY, "Time");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Mangling_Data_Streams");
  
	return filter_register(f, p);
}
