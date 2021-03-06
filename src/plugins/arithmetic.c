/*
 * arithmetic.c
 * $Id: arithmetic.c,v 1.21 2004/10/23 13:09:27 richi Exp $
 *
 * Copyright (C) 2000, 2001, 2002 Richard Guenther, Alexander Ehlert,
 * 	Jim Garrison
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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


static int arithmetic_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	/* We support any number of inputs. */
	return 0;
}

static void arithmetic_fixup_pipe(glsig_handler_t *h, long sig, va_list va)
{
	filter_pipe_t *p, *out;

	GLSIGH_GETARGS1(va, p);
	if (!(out = filterport_get_pipe(filterportdb_get_port(filter_portdb(filterport_filter(filterpipe_dest(p))), PORTNAME_OUT))))
		return;
	filterpipe_settype_sample(out, filterpipe_sample_rate(p),
				  filterpipe_sample_hangle(p));
	glsig_emit(filterpipe_emitter(out), GLSIG_PIPE_CHANGED, out);
}


/* This filter can be used to multiply inputs after adding a
 * constant and multiply a constant factor:
 * $O_i = c_1 \prod_n{{I_n}_i + c_2}$
 */
static int mul_f(filter_t *n)
{
	nto1_state_t *I;
	filter_pipe_t *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf;
	float cmul, cadd;
	SAMPLE *s;
	int nr, nr_active, cnt, i;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if (!(out = filterport_get_pipe(outp)))
		FILTER_ERROR_RETURN("no output");
	cmul = filterparam_val_double(filterparamdb_get_param(filter_paramdb(n), "factor"));
	cadd = filterparam_val_double(filterparamdb_get_param(filter_paramdb(n), "add"));

	if ((nr = nto1_init(&I, inp)) == -1)
		FILTER_ERROR_RETURN("no inputs");

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

	nto1_cleanup(I);

	FILTER_RETURN;
}

int mul_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *in;

	if (!(f = filter_creat(NULL)))
		return -1;

	in = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				   FILTER_PORTTYPE_SAMPLE,
				   FILTER_PORTFLAG_INPUT,
				   FILTERPORT_DESCRIPTION, "input streams",
				   FILTERPORT_END);
	in->connect = arithmetic_connect_in;
	glsig_add_handler(filterport_emitter(in), GLSIG_PIPE_CHANGED,
			  arithmetic_fixup_pipe, NULL);

	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "output stream",
			      FILTERPORT_END);

	f->f = mul_f;

	filterparamdb_add_param_double(filter_paramdb(f), "add",
				       FILTER_PARAMTYPE_DOUBLE, 0.0,
				       FILTERPARAM_END);
	filterparamdb_add_param_double(filter_paramdb(f), "factor",
				       FILTER_PARAMTYPE_DOUBLE, 1.0,
				       FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "multiply audio streams");
	plugin_set(p, PLUGIN_PIXMAP, "prod.png");
	plugin_set(p, PLUGIN_CATEGORY, "Filter");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Arithmetics");
	
	return filter_register(f, p);
}





/* This filter can be used to add inputs and a constant and
 * multiply a constant factor:
 * $O_i = c_1 (\sum_n{{I_n}_i} + c_2)$
 */
static int add_f(filter_t *n)
{
	nto1_state_t *I;
	filter_pipe_t *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf;
	float cmul, cadd;
	SAMPLE *s;
	int nr, nr_active, cnt, i;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if (!(out = filterport_get_pipe(outp)))
		FILTER_ERROR_RETURN("no output");
	cmul = filterparam_val_double(filterparamdb_get_param(filter_paramdb(n), "factor"));
	cadd = filterparam_val_double(filterparamdb_get_param(filter_paramdb(n), "add"));

	if ((nr = nto1_init(&I, inp)) == -1)
		FILTER_ERROR_RETURN("no inputs");

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

	nto1_cleanup(I);

	FILTER_RETURN;
}

int add_register(plugin_t *p)
{
	filter_t *f;
	filter_port_t *in;

	if (!(f = filter_creat(NULL)))
		return -1;

	in = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				   FILTER_PORTTYPE_SAMPLE,
				   FILTER_PORTFLAG_INPUT,
				   FILTERPORT_DESCRIPTION, "input streams",
				   FILTERPORT_END);
	glsig_add_handler(filterport_emitter(in), GLSIG_PIPE_CHANGED,
			  arithmetic_fixup_pipe, NULL);
	in->connect = arithmetic_connect_in;

	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "output stream",
			      FILTERPORT_END);

	f->f = add_f;

	filterparamdb_add_param_double(filter_paramdb(f), "add",
				       FILTER_PARAMTYPE_DOUBLE, 0.0,
				       FILTERPARAM_END);
	filterparamdb_add_param_double(filter_paramdb(f), "factor",
				       FILTER_PARAMTYPE_DOUBLE, 1.0,
				       FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "addition filter");
	plugin_set(p, PLUGIN_PIXMAP, "sum.png");
	plugin_set(p, PLUGIN_CATEGORY, "Filter");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Arithmetics");

	return filter_register(f, p);
}





/* This effect inverts the phase of a signal.  It can be used to correct
 * phase problems. */
static int invert_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_port_t *inp, *outp;
	filter_buffer_t *buf;
	SAMPLE *s;
	int cnt;

	inp = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	if (!(in = filterport_get_pipe(inp))
	    || !(out = filterport_get_pipe(outp)))
		FILTER_ERROR_RETURN("no input or no output");

	FILTER_AFTER_INIT;

	while ((buf = sbuf_get(in))) {
		FILTER_CHECK_STOP;
		buf = sbuf_make_private(buf);
		s = sbuf_buf(buf);
		cnt = sbuf_size(buf);
		for (; (cnt&3)>0; cnt--) {
			*s = - *s;
			s++;
		}
		for (; cnt>0; cnt-=4) {
			*s = - *s;
			s++;
			*s = - *s;
			s++;
			*s = - *s;
			s++;
			*s = - *s;
			s++;
		}
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

	if (!(f = filter_creat(NULL)))
		return -1;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream to invert",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "inverted output stream",
			      FILTERPORT_END);

	f->f = invert_f;

	plugin_set(p, PLUGIN_DESCRIPTION, "inverse the phase of an audio stream");
	plugin_set(p, PLUGIN_PIXMAP, "invert.png");
	plugin_set(p, PLUGIN_CATEGORY, "Filter");
	
	return filter_register(f, p);
}
