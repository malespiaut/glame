/*
 * arithmetic.c
 * $Id: arithmetic.c,v 1.3 2000/04/25 08:58:00 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther, Alexander Ehlert, Jim Garrison
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
 * This file contains arithmeric filters that operate using the sample
 * filter protocol. Contained are
 * - phase-invert
 * - mul
 * - add
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


PLUGIN_SET(arithmetic, "add mul invert")


/* Note, for optimisation we want a one input only filter
 * which can do constant addition and multiplication, i.e.
 * a volume-adjust with more features.
 */



/* This filter can be used to multiply inputs after adding a
 * constant and multiply a constant factor:
 * $O_i = c_1 \prod_n{{I_n}_i + c_2}$
 */
static int mul_f(filter_node_t *n)
{
	nto1_state_t *I;
	filter_pipe_t *p, *out;
	filter_param_t *pmul, *padd;
	filter_buffer_t *buf;
	float cmul, cadd;
	SAMPLE *s;
	int nr, nr_active, cnt, i;

	if ((nr = filternode_nrinputs(n)) == 0)
		FILTER_ERROR_RETURN("no inputs");
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");
	cmul = 1.0;
	if ((pmul = filternode_get_param(n, "factor")))
		cmul = filterparam_val_float(pmul);
	cadd = 0.0;
	if ((padd = filternode_get_param(n, "add")))
		cadd = filterparam_val_float(padd);

	if (!(I = ALLOCN(nr, nto1_state_t)))
		FILTER_ERROR_RETURN("no memory");
	i = 0;
	filternode_foreach_input(n, p) {
		I[i++].in = p;
	}

	FILTER_AFTER_INIT;

	nr_active = nr;
	goto entry;
	do {
		FILTER_CHECK_STOP;

		/* Head. Find maximum number of processable samples,
		 * fix destination position. */
		cnt = nto1_head(I, nr);

		/* Allocate the output buffer. */
		buf = sbuf_make_private(sbuf_alloc(cnt, n));
		s = sbuf_buf(buf);

		/* Do the actual work. Should be optimized by
		 * special casing for !padd or 2 inputs. */
		while (cnt--) {
			*s = cmul;
			for (i=0; i<nr; i++)
				if (I[i].buf)
					*s *= *I[i].s++ + cadd;
			s++;
		}

		sbuf_queue(out, buf);

	entry:
		/* Tail & entry. Check if we need to get additional
		 * buffers, recognize EOF's. */
		nr_active -= nto1_tail(I, nr);
	} while (nr_active>0);

	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(I);

	FILTER_RETURN;
}

int mul_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_alloc(mul_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input streams",
			    	  FILTER_PORTTYPE_SAMPLE
				  |FILTER_PORTTYPE_AUTOMATIC))
	    || !(filter_add_output(f, PORTNAME_OUT, "output stream",
			    	  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_param(f, "add", "to be added constant",
				  FILTER_PARAMTYPE_FLOAT))
	    || !(filter_add_param(f, "factor", "to be multiplied constant",
				  FILTER_PARAMTYPE_FLOAT)))
		return -1;

	plugin_set(p, PLUGIN_DESCRIPTION, "multiply audio streams");
	plugin_set(p, PLUGIN_PIXMAP, "mul.png");
	filter_attach(f, p);

	return 0;
}





/* This filter can be used to add inputs and a constant and
 * multiply a constant factor:
 * $O_i = c_1 (\sum_n{{I_n}_i} + c_2)$
 */
static int add_f(filter_node_t *n)
{
	nto1_state_t *I;
	filter_pipe_t *p, *out;
	filter_param_t *pmul, *padd;
	filter_buffer_t *buf;
	float cmul, cadd;
	SAMPLE *s;
	int nr, nr_active, cnt, i;

	if ((nr = filternode_nrinputs(n)) == 0)
		FILTER_ERROR_RETURN("no inputs");
	if (!(out = filternode_get_output(n, PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no output");
	cmul = 1.0;
	if ((pmul = filternode_get_param(n, "factor")))
		cmul = filterparam_val_float(pmul);
	cadd = 0.0;
	if ((padd = filternode_get_param(n, "add")))
		cadd = filterparam_val_float(padd);

	if (!(I = ALLOCN(nr, nto1_state_t)))
		FILTER_ERROR_RETURN("no memory");
	i = 0;
	filternode_foreach_input(n, p) {
		I[i++].in = p;
	}

	FILTER_AFTER_INIT;

	nr_active = nr;
	goto entry;
	do {
		FILTER_CHECK_STOP;

		/* Head. Find maximum number of processable samples,
		 * fix destination position. */
		cnt = nto1_head(I, nr);

		/* Allocate the output buffer. */
		buf = sbuf_make_private(sbuf_alloc(cnt, n));
		s = sbuf_buf(buf);

		/* Do the actual work. Should be optimized by
		 * special casing for !pmul or 2 inputs. */
		while (cnt--) {
			*s = cadd;
			for (i=0; i<nr; i++)
				if (I[i].buf)
					*s += *I[i].s++;
			*s++ *= cmul;
		}

		sbuf_queue(out, buf);

	entry:
		/* Tail & entry. Check if we need to get additional
		 * buffers, recognize EOF's. */
		nr_active -= nto1_tail(I, nr);
	} while (nr_active>0);
	sbuf_queue(out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	free(I);

	FILTER_RETURN;
}

int add_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_alloc(add_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input streams",
			    	  FILTER_PORTTYPE_SAMPLE
				  |FILTER_PORTTYPE_AUTOMATIC))
	    || !(filter_add_output(f, PORTNAME_OUT, "output stream",
			    	  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_param(f, "add", "to be added constant",
				  FILTER_PARAMTYPE_FLOAT))
	    || !(filter_add_param(f, "factor", "to be multiplied constant",
				  FILTER_PARAMTYPE_FLOAT)))
		return -1;

	plugin_set(p, PLUGIN_DESCRIPTION, "addition filter");
	plugin_set(p, PLUGIN_PIXMAP, "add.png");
	filter_attach(f, p);

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

int invert_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_alloc(invert_f))
	    || !(filter_add_input(f, PORTNAME_IN, "input stream to invert",
				  FILTER_PORTTYPE_SAMPLE))
	    || !(filter_add_output(f, PORTNAME_OUT, "inverted output stream",
				   FILTER_PORTTYPE_SAMPLE)))
		return -1;

	plugin_set(p, PLUGIN_DESCRIPTION, "inverse the phase of an audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "invert.xpm");
	filter_attach(f, p);

	return 0;
}
