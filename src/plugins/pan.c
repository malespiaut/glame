/*
 * pan.c
 *
 * Copyright (C) 2000 Jim Garrison, Daniel Kobras
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

#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include "util.h"
#include "filter.h"
#include "glplugin.h"

/* This filter positions a mono sound in the stereo field, based 
 * upon the following algorithm:
 * 
 * - First distribute the incoming stream so that overall intensity is
 *   preserved:
 *   	Left1  = In*cos(th),
 *   	Right1 = In*sin(th).
 * - Then we rescale so that intensity in the louder of both output
 *   streams is equal to the incoming intensity. Without loss of
 *   generality let Left1 be the the louder one:
 *   	Left  = Left1/cos(th)  = In,
 *   	Right = Right1/cos(th) = In*tan(th).
 *   Alas, we pass the louder stream unmodified. The other stream is a
 *   duplicate scaled with tan(th).
 * - So what is th? Obviously, for a pan setting of 0, th should be
 *   pi/4, i.e. equal distribution to left and right. For pan equal to
 *   +/-pi/2, th should be 0, i.e. all data goes solely to left or
 *   right respectively. So let th = pi/4 - pan/2 and swap roles of
 *   left and right if th < 0.
 */

static int pan_f(filter_t *n)
{
	filter_pipe_t *in, *mod, *pass;
	filter_buffer_t *m_buf, *p_buf;
	int size;
	float scale;
	SAMPLE *m, *p;

	if (!(in = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN))))
		FILTER_ERROR_RETURN("No input.");
	
	/* Use pipe position by default, pan param may override. */	
	scale = filterpipe_sample_hangle(in);
	if (fabs(scale) > M_PI_2)
		scale = -scale + M_PI*(scale > 0 ? 1.0 : -1.0);
	/* doh - FIXME - this does nolonger work. Always overriden
	 * at least by the default value of the parameter.
	 */
	scale = filterparam_val_float(filterparamdb_get_param(filter_paramdb(n), "pan"));
	
	if (!(mod = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), "left-out")))
	    || !(pass = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), "right-out"))))
		FILTER_ERROR_RETURN("Must connect all output ports.");

	 /* FIXME: Move to fixup_pipe - too lazy now. */
	filterpipe_sample_hangle(mod) = FILTER_PIPEPOS_LEFT;
	filterpipe_sample_hangle(pass) = FILTER_PIPEPOS_RIGHT;

	if (scale < 0.0) {
		filter_pipe_t *tmp = mod;
		mod = pass;
		pass = tmp;
		scale *= -1.0;
	}
	
	scale = tan(M_PI_4-scale/2.0);
	
	FILTER_AFTER_INIT;

	while ((p_buf = sbuf_get(in))) {
		size = sbuf_size(p_buf);
		m_buf = sbuf_make_private(sbuf_alloc(size, n));
		p = sbuf_buf(p_buf);
		m = sbuf_buf(m_buf);
		while (size--) 
			*m++ = *p++ * scale;
		sbuf_queue(pass, p_buf);
		sbuf_queue(mod, m_buf);

		FILTER_CHECK_STOP;
	}

	sbuf_queue(pass, NULL);
	sbuf_queue(mod, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

/* XXX: Never ever again shalt thou forget those words for they art written 
 * in the blood of many good and pious men:
 * Wanderer, if thou cometh to Sparta, tell people not to compare float
 * values against double constants, or they shall bring hatred and 
 * misery among their breed! Cast thy M_PI_2 to float or use a float variable 
 * to compare against instead, and thou shalt live long and prosperous!
 */
static int pan_set_param(filter_param_t *param, const void *val)
{
	const float min = -M_PI_2;
	const float max = M_PI_2;
	float pan;

	pan = *((float *)val);
	
	if (pan < min)
		return -1;
	else if (pan > max)
		return -1;

	return 0;
}
	

/* Most other filters try to avoid explicit reference to 'left' and 'right'
 * ports. Not so with pan, as it is tailor-made for stereo needs.
 */
int pan_register(plugin_t *p)
{
	filter_t *f;
	filter_param_t *pan;

	if (!(f = filter_creat(NULL)))
		return -1;
	f->f = pan_f;
	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "input stream to pan",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), "left-out",
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "left output stream",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), "right-out",
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "right output stream",
			      FILTERPORT_END);

	pan = filterparamdb_add_param_float(filter_paramdb(f), "pan",
				  FILTER_PARAMTYPE_POSITION, 0.0/* FIXME - use magic (invalid) default value to mark "unset"? */,
				  FILTERPARAM_DESCRIPTION,
				  "position in stereo field [-pi/2, pi/2]", 
				  FILTERPARAM_END);
	pan->set = pan_set_param;

	plugin_set(p, PLUGIN_DESCRIPTION, "Positions a mono audio stream in the stereo field");
	plugin_set(p, PLUGIN_PIXMAP, "pan.png");
	plugin_set(p, PLUGIN_CATEGORY, "Filter");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Orientation");

	filter_register(f, p);

	return 0;
}
