/*
 * stretch.c
 * $Id: stretch.c,v 1.4 2002/05/23 22:27:23 mag Exp $
 *
 * Copyright (C) 2002 Alexander Ehlert
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "filter_tools.h"
#include "math.h"

static SAMPLE *hanning(int n)
{
	SAMPLE *win;
	int i;
	
	if (!(win=ALLOCN(n,SAMPLE)))
		return NULL;
	for(i=0;i<n;i++)
		win[i]=0.5-0.5*cos((SAMPLE)i/(SAMPLE)(n-1)*2.0*M_PI);
	return win;
}

static SAMPLE window_gain(SAMPLE *win, int n, int osamp)
{
	SAMPLE *s;
	double max;
	int i, j, off;
	
	off = n / osamp;
	s = ALLOCN(n, SAMPLE);

	memcpy(s, win, n*SAMPLE_SIZE);
	
	for(i=1; i<osamp; i++)
		for(j=0; j<n; j++)
			s[j] += win[(j+i*off)%n];

	max = 0;
	for(i=0; i<n; i++)
		max += s[i]*s[i];

	max /= n;
	max = sqrt(max);

	free(s);
	return (SAMPLE)max;
}

static int stretch_f(filter_t *n)
{
	filter_port_t *in_port, *out_port;
	filter_pipe_t *in, *out;
	filter_param_t *param, *sparam;
	float factor, gain, pos, dpos, dfak;
	in_queue_t in_queue;
	out_queue_t out_queue;
	SAMPLE *win, *buffer;
	int bsize, osamp, inshift, outshift, i;

	in_port = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	out_port = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	in = filterport_get_pipe(in_port);
	out = filterport_get_pipe(out_port);
	if (!in || !out)
		FILTER_ERROR_RETURN("no input or no output");

	sparam = filterparamdb_get_param(filter_paramdb(n), "stretchfactor");


	param = filterparamdb_get_param(filter_paramdb(n), "buffersize");
	bsize = (int)filterparam_val_long(param);

	param = filterparamdb_get_param(filter_paramdb(n), "oversampling");
	osamp = (int)filterparam_val_long(param);

	inshift = (int)((float)bsize/(float)osamp);

	if (bsize<1)
		bsize=1;

	if (!(win=hanning(bsize)))
		FILTER_ERROR_RETURN("couldn't allocate window buffer");

	gain = 1.0f/window_gain(win, bsize, osamp);

	for(i=0; i<bsize; i++)
		win[i]*=gain;

	if (!(buffer=ALLOCN(bsize, SAMPLE)))
		FILTER_ERROR_RETURN("couldn't allocate synthesis buffer");

	init_in_queue(&in_queue, in, n);
	init_out_queue(&out_queue, out, n);
	pos = 0.0f;
	dfak = (float)bsize/(float)osamp;

	FILTER_AFTER_INIT;

	do {
		FILTER_CHECK_STOP;
		factor = filterparam_val_double(sparam);
		dpos = dfak*factor;
		if (dpos<=0.0f)
			dpos = 0.1f;


		in_queue_copy_pad(&in_queue, buffer, bsize);
		in_queue_shift(&in_queue, inshift);

		for (i=0; i<bsize; i++)
			buffer[i]*=win[i];
		
		pos += dpos;
		outshift = (int)floor(pos);
		pos -= (float)outshift;
		
		out_queue_add(&out_queue, buffer, bsize);
		out_queue_shift(&out_queue, outshift);

	} while (!in_queue.done);

	out_queue_drain(&out_queue);
	sbuf_queue(out, NULL);
	in_queue_drain(&in_queue);

	free(win);
	free(buffer);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int stretch_register(plugin_t *p)
{
	filter_t *f;
	filter_param_t *param;
	filter_port_t *port;

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = stretch_f;

	port = filterportdb_add_port(
		filter_portdb(f), PORTNAME_IN,
		FILTER_PORTTYPE_SAMPLE,
		FILTER_PORTFLAG_INPUT,
		FILTERPORT_DESCRIPTION, "input stream to stretch",
		FILTERPORT_END);

	port = filterportdb_add_port(
		filter_portdb(f), PORTNAME_OUT,
		FILTER_PORTTYPE_SAMPLE,
		FILTER_PORTFLAG_OUTPUT,
		FILTERPORT_DESCRIPTION, "stretched stream",
		FILTERPORT_END);

	param = filterparamdb_add_param_double(
		filter_paramdb(f), "stretchfactor",
		FILTER_PARAMTYPE_DOUBLE, 1.0,
		FILTERPARAM_LABEL, "stretch factor",
		FILTERPARAM_DESCRIPTION, "stretch the duration of the stream by this factor",
		FILTERPARAM_END);

	param = filterparamdb_add_param_long(
		filter_paramdb(f), "buffersize",
		FILTER_PARAMTYPE_LONG, 64,
		FILTERPARAM_LABEL, "buffersize",
		FILTERPARAM_DESCRIPTION, "size of the synthesis window",
		FILTERPARAM_END);

	param = filterparamdb_add_param_long(
		filter_paramdb(f), "oversampling",
		FILTER_PARAMTYPE_LONG, 4,
		FILTERPARAM_LABEL, "oversampling",
		FILTERPARAM_DESCRIPTION, "determines the overlap of synthesis windows by 1/oversamp*buffersize",
		FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION,
		   "stretch a stream by a given factor");
	plugin_set(p, PLUGIN_PIXMAP, "stretch.png");
	plugin_set(p, PLUGIN_CATEGORY, "Time");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Mangling_Data_Streams");
  
	return filter_register(f, p);
}
