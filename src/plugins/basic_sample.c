/*
 * basic_sample.c
 * $Id: basic_sample.c,v 1.4 2000/03/21 09:40:09 richi Exp $
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
 * - mix [broken?]
 * - volume-adjust [broken?]
 * - phase-invert
 * - delay
 * - extend
 * - repeat
 * - add [broken? should add n streams instead?]
 * I like to see
 * - sub, mul
 * do you really want to see sub, when we have add ? [mag]
 * mul is just the same like volume-adjust! [mag]
 * I wanted add, sub, mul to operate on whole streams, not
 * just one stream an one constant! (see const filter in
 * waveform.c) - needs discussion [richi]
 * 
 * I propose arithmetic filters which additionally take
 * a const offset (to add) and a const factor (to multiply):
 * - add
 * - mul (with fft we get convolute for free)
 * - max, min, invert(1/x), clip (just brainstorming)
 * these would go into a seperate filter group.
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


PLUGIN_SET(basic_sample, "mix volume_adjust invert delay extend repeat add")



/* FIXME! mix needs to be asynchronly wrt reads & writes! */
/* The mix filter mixes any number of input channels with an optional
 * gain parameter per input into one output channel which position
 * can be specified using the phi parameter.
 * Required for correct operation is at least one connected input
 * channel, and a connected output channel. Mixing is stopped if
 * one input is EOF, other inputs are subsequently dropped.
 * mix uses the following methods:
 * - connect_out to fill in the destinations rate and phi,
 * - fixup_param to correct the output phi,
 * - fixup_pipe to check for changed rate/phi, this also handles
 *   connects.
 * Failure is at launch time if samplerates of inputs dont match.
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
	int i, cnt, nrinputs, eofs, rate;
	SAMPLE *s;
	float factor;

	/* We require at least one connected input and
	 * a connected output.
	 */
	nrinputs = filternode_nrinputs(n);
	if (nrinputs == 0)
		FILTER_ERROR_RETURN("no inputs");
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");

	/* init the structure, compute the needed factors */
	eofs = 0;
	factor = 0.0;
	i = 0;
	if (!(inputs = ALLOCN(nrinputs, mix_param_t)))
		FILTER_ERROR_RETURN("no memory");

	rate = -1;
	filternode_foreach_input(n, p) {
		if (rate == -1)
			rate = filterpipe_sample_rate(p);
		else if (rate != filterpipe_sample_rate(p))
			FILTER_ERROR_CLEANUP("non matching samplerates");
		inputs[i].in = p;
		inputs[i].factor = 1.0;
		if ((param = filterpipe_get_destparam(p, "gain")))
			inputs[i].factor = filterparam_val_float(param);
		factor += inputs[i].factor;
		i++;
	}
	/* FIXME: on linux (glibc) we want this to be cosf() - i.e.
	 * operate on floats w/o casting to double. On non glibc
	 * platforms like solaris cosf does not exist. ugh. */
	for (i=0; i<nrinputs; i++) {
		inputs[i].factor /= factor;
		// inputs[i].factor *= cos(filterpipe_sample_hangle(inputs[i].in) - filterpipe_sample_hangle(out));
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

	/* mix only until one input has eof */
	while (eofs == 0) {
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

		/* to do really fast processing special-code
		 * a number of input channel counts */
		switch (nrinputs) {
		case 2:
			for (; (cnt & 3)>0; cnt--) {
				SCALARPROD1_2_d(s, inputs[0].s, inputs[1].s, inputs[0].factor, inputs[1].factor);
			}
			for (; cnt>0; cnt-=4) {
				SCALARPROD4_2_d(s, inputs[0].s, inputs[1].s, inputs[0].factor, inputs[1].factor);
			}
			break;
		case 3:
			for (; (cnt & 3)>0; cnt--) {
				SCALARPROD1_3_d(s, inputs[0].s, inputs[1].s, inputs[2].s, inputs[0].factor, inputs[1].factor, inputs[2].factor);
			}
			for (; cnt>0; cnt-=4) {
				SCALARPROD4_3_d(s, inputs[0].s, inputs[1].s, inputs[2].s, inputs[0].factor, inputs[1].factor, inputs[2].factor);
			}
			break;
		default:
			for (; cnt>0; cnt--) {
				*s = 0.0;
				for (i=0; i<nrinputs; i++)
					if (inputs[i].buf) {
						*s += *(inputs[i].s)*inputs[i].factor;
						inputs[i].s++;
					}
				s++;
			}
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

	/* drop the rest of the input */
	while (eofs != nrinputs) {
		FILTER_CHECK_STOP;

		for (i=0; i<nrinputs; i++) {
			if (!inputs[i].buf)
				continue;
			sbuf_unref(inputs[i].buf);
			if (!(inputs[i].buf = sbuf_get(inputs[i].in)))
				eofs++;
		}
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(inputs);

	FILTER_RETURN;
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
	if ((param = filternode_get_param(n, "phi")))
		phi = filterparam_val_float(param);

	if (rate != filterpipe_sample_rate(out)
	    || phi != filterpipe_sample_hangle(out)) {
		filterpipe_settype_sample(out, rate, phi);
		return 1;
	}
	return 0;
}
static int mix_fixup_param(filter_node_t *n, filter_pipe_t *p,
			   const char *name, filter_param_t *param)
{
	filter_pipe_t *out;

	if ((out = filternode_get_output(n, PORTNAME_OUT))
	    && mix_fixup(n, out))
		out->dest->filter->fixup_pipe(out->dest, out);

	return 0;
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

PLUGIN_DESCRIPTION(mix, "mix streams")
PLUGIN_PIXMAP(mix, "mix.xpm")
int mix_register()
{
	filter_t *f;
	filter_portdesc_t *port;
	filter_paramdesc_t *param;

        if (!(f = filter_alloc(mix_f))
            || !(port = filter_add_input(f, PORTNAME_IN, "input stream",
				      FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE))
	    || !filterport_add_param(port, "gain", "input gain",
				     FILTER_PARAMTYPE_FLOAT)
            || !filter_add_output(f, PORTNAME_OUT, "mixed stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || !(param = filter_add_param(f, "phi", "position of mixed stream",
				      FILTER_PARAMTYPE_FLOAT)))
		return -1;
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_POSITION);
	f->connect_out = mix_connect_out;
	f->fixup_param = mix_fixup_param;
	f->fixup_pipe = mix_fixup_pipe;
        if (filter_add(f, "mix", "mix n channels") == -1)
                return -1;
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

	FILTER_RETURN;
}

PLUGIN_DESCRIPTION(volume_adjust, "adjust the volume of a stream")
PLUGIN_PIXMAP(volume_adjust, "default.xpm")
int volume_adjust_register()
{
	filter_t *f;

	if (!(f = filter_alloc(volume_adjust_f))
	    || !filter_add_param(f, "factor", "scale factor",
				 FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_input(f, PORTNAME_IN, "input stream",
				 FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "scaled stream",
				  FILTER_PORTTYPE_SAMPLE)
	    || filter_add(f, "volume-adjust", "scale samples") == -1)
		return -1;
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

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no input or no output");

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

	FILTER_RETURN;
}

PLUGIN_DESCRIPTION(invert, "invert a stream")
PLUGIN_PIXMAP(invert, "invert.xpm")
int invert_register()
{
	filter_t *f;

	if (!(f = filter_alloc(invert_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input stream to invert",
				  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_output(f, PORTNAME_OUT, "inverted output stream",
				   FILTER_PORTTYPE_SAMPLE))
	    || filter_add(f, "phase-invert",
			  "Inverses the phase of the audio signal") == -1)
		return -1;
	return 0;
}





/* This filter can be used to correct for DC offsets */
static int add_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	SAMPLE *s,sum;
	int cnt;

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no input or no output");

	if ((param = filternode_get_param(n,"offset")))
		sum = filterparam_val_float(param);
	else 
		sum=0.0;
	
	FILTER_AFTER_INIT;

	while ((buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;
		buf = sbuf_make_private(buf);
		s = sbuf_buf(buf);
		cnt = sbuf_size(buf);
		for (; (cnt&3)>0; cnt--)
			ADD1(s,sum);
		for (; cnt>0; cnt-=4)
			ADD4(s,sum);
		sbuf_queue(out, buf);
	}

	sbuf_queue(out, buf);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
		
	FILTER_RETURN;	
}

PLUGIN_DESCRIPTION(add, "add a constant to a stream")
PLUGIN_PIXMAP(add, "add.xpm")
int add_register()
{
	filter_t *f;

	if (!(f = filter_alloc(add_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input stream",
			    	  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_output(f, PORTNAME_OUT, "output stream with offset",
			    	  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_param(f, "offset", "offset",
				  FILTER_PARAMTYPE_FLOAT)))
		return -1;
	if (filter_add(f, "add",
		       "Add an offset to every sample value") == -1)
	        return -1;
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

PLUGIN_DESCRIPTION(delay, "delay a stream")
PLUGIN_PIXMAP(delay, "delay.xpm")
int delay_register()
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
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_TIME);
	if (filter_add(f, "delay", "Delay's an audio signal") == -1)
	        return -1;
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

PLUGIN_DESCRIPTION(extend, "extend a stream")
PLUGIN_PIXMAP(extend, "extend.xpm")
int extend_register()
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
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_TIME);
	if (filter_add(f, "extend", "Extend an audio signal") == -1)
	        return -1;
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
	        duration = filterpipe_sample_rate(in)*filterparam_val_float(param)/1000.0;
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

PLUGIN_DESCRIPTION(repeat, "repeat a stream")
PLUGIN_PIXMAP(repeat, "repeat.xpm")
int repeat_register()
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
	filterparamdesc_float_settype(param, FILTER_PARAM_FLOATTYPE_TIME);
	if (filter_add(f, "repeat",
		       "Repeat an audio signal for the specified time") == -1)
		return -1;
	return 0;
}
