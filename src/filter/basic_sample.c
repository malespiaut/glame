/*
 * basic_sample.c
 * $Id: basic_sample.c,v 1.2 2000/02/21 13:03:48 richi Exp $
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
 * - volume-adjust
 * - phase-invert
 * I like to have
 * - delay
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include "filter.h"
#include "util.h"



/* The mix filter mixes any number of input channels with an optional
 * gain parameter per input into one output channel which position
 * can be specified using the phi parameter.
 * Required for correct operation is at least one connected input
 * channel, and a connected output channel.
 * mix uses the following methods:
 * - connect_in to ensure all inputs have the same sample rate
 * - connect_out to fill in the destinations rate and phi
 * - fixup_param to correct the outputs phi
 * - fixup_pipe to check for changed rate and break the connection,
 *   if a conflict arises
 */
static int mix_f(filter_node_t *n)
{
	typedef struct {
		filter_pipe_t *in;
		filter_buffer_t *buf;
		SAMPLE *s;
		int pos;
		float factor;
	} mix_param_t;
	mix_param_t *inputs = NULL;
	filter_pipe_t  *p, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	int i, cnt, nrinputs, eofs;
	SAMPLE *s;
	float factor;

	/* We require at least one connected input and
	 * a connected output.
	 */
	nrinputs = filternode_nrinputs(n);
	if (nrinputs == 0)
		return -1;
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return -1;

	/* init the structure, compute the needed factors */
	eofs = 0;
	factor = 0.0;
	i = 0;
	if (!(inputs = ALLOCN(nrinputs, mix_param_t)))
		return -1;

	filternode_foreach_input(n, p) {
		inputs[i].in = p;
		inputs[i].factor = 1.0;
		if ((param = filterpipe_get_destparam(p, "gain")))
			inputs[i].factor = filterparam_val_float(param);
		factor += inputs[i].factor;
		i++;
	}
	for (i=0; i<nrinputs; i++) {
		inputs[i].factor /= factor;
		inputs[i].factor *= cosf(filterpipe_sample_hangle(inputs[i].in) - filterpipe_sample_hangle(out));
	}

	FILTER_AFTER_INIT;


	/* get first input buffers from all channels and init the
	 * structure. */
	for (i=0; i<nrinputs; i++) {
		if (!(inputs[i].buf = sbuf_get(inputs[i].in)))
			eofs++;
		inputs[i].s = sbuf_buf(inputs[i].buf);
		inputs[i].pos = 0;
	}

	while (eofs != nrinputs) {
		FILTER_CHECK_STOP;

		/* find the maximum number of samples we can process
		 * without getting an additional buffer. */
		cnt = 1<<30;
		for (i=0; i<nrinputs; i++) {
			if (!inputs[i].buf
			    || sbuf_size(inputs[i].buf) - inputs[i].pos >= cnt)
				continue;
			cnt = sbuf_size(inputs[i].buf) - inputs[i].pos;
		}

		/* fix the end positions */
		for (i=0; i<nrinputs; i++)
			if (inputs[i].buf)
				inputs[i].pos += cnt;

		/* in one run process cnt number of samples. alloc
		 * a new buffer for them. */
		buf = sbuf_alloc(cnt, n);
		s = sbuf_buf(buf);
		for (; cnt>0; cnt--) {
			*s = 0.0;
			for (i=0; i<nrinputs; i++)
				if (inputs[i].buf) {
					*s += *(inputs[i].s)*inputs[i].factor;
					inputs[i].s++;
				}
			s++;
		}

		/* queue buffer */
		sbuf_queue(out, buf);

		/* check which input buffer had the underflow. */
		for (i=0; i<nrinputs; i++) {
			if (!inputs[i].buf
			    || inputs[i].pos != sbuf_size(inputs[i].buf))
				continue;
			sbuf_unref(inputs[i].buf);
			if (!(inputs[i].buf = sbuf_get(inputs[i].in)))
				eofs++;
			inputs[i].s = sbuf_buf(inputs[i].buf);
			inputs[i].pos = 0;
		}
	}

	/* queue an EOF */
	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(inputs);

	return 0;
}

/* shared destination pipe property fixup code */
static int mix_fixup(filter_node_t *n, filter_pipe_t *out)
{
	filter_param_t *param;
	filter_pipe_t *in;
	float phi;
	int rate;

	if (!out)
		return 0;

	rate = 44100;
	if ((in = filternode_get_input(n, PORTNAME_IN)))
		rate = filterpipe_sample_rate(in);
	phi = FILTER_PIPEPOS_DEFAULT;
	if ((param = filternode_get_param(n, "phi")))
		phi = filterparam_val_float(param);
	filterpipe_settype_sample(out, rate, phi);

	/* tell destination filter about possible pipe change */
	return out->dest->filter->fixup_pipe(out->dest, out);
}
static int mix_fixup_param(filter_node_t *n, filter_pipe_t *p,
			   const char *name, filter_param_t *param)
{
	filter_pipe_t *out;

	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return 0;

	return mix_fixup(n, out);
}
static int mix_connect_out(filter_node_t *n, const char *port,
			   filter_pipe_t *p)
{
	return mix_fixup(n, p);
}
/* check for same sample rate of new connection as the already
 * connected ones. */
static int mix_connect_in(filter_node_t *n, const char *port,
			  filter_pipe_t *p)
{
	filter_pipe_t *in;

	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		return 0;
	if (filterpipe_sample_rate(p) != filterpipe_sample_rate(in))
		return -1;
	return 0;
}
/* check if the changed pipe has still the same sample rate.
 * if not, just disconnect it. fix the output properties in case
 * the changed pipe was the only connection. */
static int mix_fixup_pipe(filter_node_t *n, filter_pipe_t *in)
{
	filter_pipe_t *p;

	filternode_foreach_input(n, p)
		if (filterpipe_sample_rate(p) != filterpipe_sample_rate(in)) {
			filternetwork_break_connection(in);
			return -1;
		}
	return mix_fixup_param(n, NULL, NULL, NULL);
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
		return -1;
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
			SCALARPROD1_1(buf, scale);
		}

		/* streamed loop */
		for (; cnt>0; cnt-=4) {
			SCALARPROD4_1(buf, scale);
		}

		/* queue the modified buffer */
		sbuf_queue(out, b);
	};

	/* forward the EOF mark */
	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}


/* This effect inverts the phase of a signal.  It can be used to correct
 * phase problems. */
static int invert_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	SAMPLE *s;
	int cnt;

	in = filternode_get_input(n, PORTNAME_IN);	
	out = filternode_get_output(n, PORTNAME_OUT);	
	if (!in || !out)
		return -1;

	FILTER_AFTER_INIT;

	while ((buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;
		buf = sbuf_make_private(buf);
		s = sbuf_buf(buf);
		cnt = sbuf_size(buf);
		for (; (cnt&3)>0; cnt--)
			INVERT1(s);
		for (; cnt>0; cnt-=4)
			INVERT4(s);
		sbuf_queue(out, buf);
	}

	sbuf_queue(out, buf);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}



/* Registry setup of all contained filters
 */
int basic_sample_register()
{
	filter_t *f;
	filter_portdesc_t *d;

        if (!(f = filter_alloc("mix", "mix n channels", mix_f))
            || !(d = filter_add_input(f, PORTNAME_IN, "input stream",
				      FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE))
	    || !filterport_add_param(d, "gain", "input gain",
				     FILTER_PARAMTYPE_FLOAT)
            || !filter_add_output(f, PORTNAME_OUT, "mixed stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f, "phi", "position of mixed stream",
				 FILTER_PARAMTYPE_FLOAT))
		return -1;
	f->connect_in = mix_connect_in;
	f->connect_out = mix_connect_out;
	f->fixup_param = mix_fixup_param;
	f->fixup_pipe = mix_fixup_pipe;
        if (filter_add(f) == -1)
                return -1;

	if (!(f = filter_alloc("volume-adjust", "scale samples",
			       volume_adjust_f))
	    || !filter_add_param(f, "factor", "scale factor",
				 FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_input(f, PORTNAME_IN, "input stream",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "scaled stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("phase-invert",
			       "Inverses the phase of the audio signal",
			       invert_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input stream to invert",
				  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_output(f, PORTNAME_OUT, "inverted output stream",
				   FILTER_PORTTYPE_SAMPLE))
	    || filter_add(f) == -1)
		return -1;


	return 0;
}
