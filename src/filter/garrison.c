/*
 * garrison.c
 *
 * Copyright (C) 2000 Jim Garrison
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
#define	SIZE	1024

/* This effect inverts the phase of a signal.  It can be used to correct
   phase problems.  Hopefully this will eventually make it into basic.c
   since it is an extremely simple effect and commonly used. */

static int invert_f(filter_node_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	int i;

	in = filternode_get_input(n, PORTNAME_IN);	
	out = filternode_get_output(n, PORTNAME_OUT);	
	if (!in || !out)
		return -1;

	FILTER_AFTER_INIT;

	while (pthread_testcancel(), buf = sbuf_get(in)) {
		buf = sbuf_make_private(buf);
		for (i = 0; i < sbuf_size(buf); i++) {
			sbuf_buf(buf)[i] = -(sbuf_buf(buf)[i]);
		}
		sbuf_queue(out, buf);
	}

	sbuf_queue(out, buf);

	FILTER_BEFORE_CLEANUP;

	return 0;
}

/* This filter positions a mono sound in the stereo field */

static int pan_f(filter_node_t *n)
{
	filter_param_t *pan;	/* between -1 and 1 */
	filter_pipe_t *in, *l_out, *r_out;
	filter_buffer_t *l_buf, *r_buf;
	int i;

	if (!(pan = filternode_get_param(n, "pan")))
		return -1;
	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		return -1;
	if (!(l_out = filternode_get_output(n, PORTNAME_LEFT_OUT)))
		return -1;
	if (!(r_out = filternode_get_output(n, PORTNAME_RIGHT_OUT)))
		return -1;

	FILTER_AFTER_INIT;

	while (pthread_testcancel(), l_buf = sbuf_get(in)) {
		l_buf = sbuf_make_private(l_buf);
		r_buf = sbuf_alloc(sbuf_size(l_buf), n);
		for (i = 0; i < sbuf_size(l_buf); i++) {
			sbuf_buf(r_buf)[i] = sbuf_buf(l_buf)[i] 
				* (filterparam_val_float(pan) + 1.0) / 2;
			sbuf_buf(l_buf)[i] *= 
				(-(filterparam_val_float(pan)) + 1.0) / 2;
		}
		sbuf_queue(l_out, l_buf);
		sbuf_queue(r_out, r_buf);
	}

	sbuf_queue(l_out, l_buf);
	sbuf_queue(r_out, l_buf);

	FILTER_BEFORE_CLEANUP;

	return 0;
}

#if 0

/* Mixes and balances effected and uneffected signals */

static int bal_f(filter_node_t *n)
{
	return 0;
}

/* Another shot at the infamous echo filter */

static int echo3_f(filter_node_t *n)
{
	filter_param_t *feedback, *delay;
	filter_pipe_t *in, *out;
	feedback_fifo_t fifo;
	filter_buffer_t *buf, *dbuf = NULL;
	int i;

	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	feedback = filternode_get_param(n, "feedback");
	delay = filternode_get_param(n, "delay");
	INIT_FEEDBACK_FIFO(fifo);

	FILTER_AFTER_INIT;

	while (pthread_testcancel(), 
	    ((buf = sbuf_make_private(sbuf_get(in))) || (dbuf))) {
		
		sbuf_queue(out, dbuf);
		
	}

	FILTER_BEFORE_CLEANUP;

	return 0;
}

#endif

/* This takes a parameter and turns it into a stream. Will be useful
   when combined with my new echo and tremolo effects (soon to come) */

static int p2s_f (filter_node_t *n)
{
	filter_pipe_t *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	SAMPLE val;
	int i;

	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		return -1;

	param = filternode_get_param(n, "val");
	val = filterparam_val_sample(param);

	buf = sbuf_make_private(sbuf_alloc(SIZE, n));
	for (i = 0; i < SIZE; i++)
		sbuf_buf(buf)[i] = val;

	FILTER_AFTER_INIT;

	while (1) {		/**** FIXME ****/
		if (filterparam_val_sample(param) != val) {
			sbuf_unref(buf);
			val = filterparam_val_sample(param);
			buf = sbuf_make_private(sbuf_alloc(SIZE, n));
			for (i = 0; i < SIZE; i++)
				sbuf_buf(buf)[i] = val;
		}
		sbuf_ref(buf);
		sbuf_queue(out, buf);
	}

	FILTER_BEFORE_CLEANUP;

	sbuf_unref(buf);

	return 0;
}

/* 
 * This is the mixer from basic.c, modified to multiply sounds instead of
 * adding them. 
 */

static int ringmod_f (filter_node_t *n)
{
	filter_pipe_t  *p, **pin=NULL, *pout;
	filter_buffer_t **in=NULL, *out, *lastout;
	int i, eofs, *pos=NULL, opos, res=-1, rate;
	SAMPLE s;

	/* We require at least one connected input and
	 * a connected output. Also all input pipes have
	 * to match in the sample rate!
	 */
	if (filternode_nrinputs(n) == 0)
		return -1;
	if (!(pout = filternode_get_output(n, PORTNAME_OUT)))
		return -1;
	rate = filterpipe_sample_rate(filternode_get_input(n, PORTNAME_IN));
	filternode_foreach_input(n, p)
		if (filterpipe_sample_rate(p) != rate)
			return -1;

	/* init */
	eofs = 0;
	if (!(in = ALLOCN(filternode_nrinputs(n), filter_buffer_t *)))
			return -1;
	if (!(pos = ALLOCN(filternode_nrinputs(n), int)))
		goto _cleanup;
	if (!(pin = ALLOCN(filternode_nrinputs(n), filter_pipe_t *)))
		goto _cleanup;

	FILTER_AFTER_INIT;
	
	/* get first input buffers from all channels */
	i=0;
	filternode_foreach_input(n, p) {
		pin[i] = p;
		if (!(in[i] = sbuf_get(pin[i])))
			eofs++;
		pos[i] = 0;
		i++;
	}

	/* get first output buffer */
	out = sbuf_alloc(GLAME_WBUFSIZE, n);
	opos = 0;

	while (pthread_testcancel(), (eofs != n->nr_inputs)) {
		s = 0;
		for (i=0; i<n->nr_inputs; i++) {
			/* check, if we need a new input buffer */
			if (in[i] && (sbuf_size(in[i]) == pos[i])) {
				/* unref the processed buffer */
			        sbuf_unref(in[i]);
				/* get new buffer */
				if (!(in[i] = sbuf_get(pin[i])))
					eofs++;
				pos[i] = 0;
			}
			/* sum the channels */
			if(in[i]){
				s *= sbuf_buf(in[i])[pos[i]];
				pos[i]++;
			}
		}

		/* check, if we need a new output buffer */
		if (sbuf_size(out) == opos) {
			/* submit the (full) output buffer.
			 * we have already a reference to out (ours! - and
			 * we wont release it */
			sbuf_queue(pout, out);
			/* alloc new buffer */
			out = sbuf_alloc(GLAME_WBUFSIZE, n);
			opos = 0;
		}

		/* write the sample  */
		sbuf_buf(out)[opos++] = s/(n->nr_inputs);
	}

	/* submit the last pending output buffer - but truncate
	 * it to the actual necessary length */
	lastout = sbuf_alloc(opos, n);
	memcpy(sbuf_buf(lastout), sbuf_buf(out), sizeof(SAMPLE)*opos);
	sbuf_unref(out);
	sbuf_queue(pout, lastout);

	FILTER_BEFORE_CLEANUP;
	/* cleanup - FIXME, we need to do this with a
	 * pthread_cleanup_pop() - and previous
	 * pthread_cleanup_push() - ugh! */
	res=0;
_cleanup:
	if (in) free(in);
	if (pos) free(pos);
        if (pin) free(pin);

	return res;
}

int garrison_register()
{
	filter_t *f;

	/***** invert filter *****/
	if ((f = filter_alloc("phase", "Inverses the phase of the audio signal", invert_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, PORTNAME_IN, "input stream to invert", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, PORTNAME_OUT, "inverted output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (filter_add(f))
		return -1;

	/***** pan filter *****/
	if ((f = filter_alloc("pan", "Positions a mono audio stream in the stereo field", pan_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, PORTNAME_IN, "input stream to pan", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, PORTNAME_LEFT_OUT, "left output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, PORTNAME_RIGHT_OUT, "right output stream", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_param(f, "pan", "position in stereo field", FILTER_PARAMTYPE_FLOAT)))
		return -1;
	if (filter_add(f))
		return -1;

#if 0
	/***** balance filter *****/
	if ((f = filter_alloc("Balance", "Mixes wet/dry signals", bal_f)) == NULL)
		return -1;
	if (filter_add(f))
		return -1;

	/***** echo3 filter *****/
	if ((f = filter_alloc("Echo 3", "echo", echo3_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, "delay", "delay time (in samples)", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_param(f, "feedback", "feedback ratio", FILTER_PARAMTYPE_FLOAT)))
		return -1;
	if (filter_add(f))
		return -1;
#endif

	/***** parameter to stream *****/
	if ((f = filter_alloc("p2s", "Converts parameter to stream", p2s_f)) == NULL)
		return -1;
	if (!(filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_param(f, "val", "value to output", FILTER_PARAMTYPE_FLOAT)))
		return -1;
	if (filter_add(f))
		return -1;

	/***** ring modulator filter *****/
	if ((f = filter_alloc("Ring Modulator", "", ringmod_f)) == NULL)
		return -1;
	if (!(filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_AUTOMATIC|FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (!(filter_add_output(f, PORTNAME_OUT, "output", FILTER_PORTTYPE_SAMPLE)))
		return -1;
	if (filter_add(f))
		return -1;

	return 0;
}
