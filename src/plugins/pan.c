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

static int pan_f(filter_node_t *n)
{
	filter_param_t *pan;	/* [-pi/2, pi/2] */
	filter_pipe_t *in, *mod, *pass;
	filter_buffer_t *m_buf, *p_buf;
	int size;
	float scale;
	SAMPLE *m, *p;

	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("No input.");
	
	/* Use pipe position by default, pan param may override. */	
	scale = filterpipe_sample_hangle(in);
	if (fabs(scale) > M_PI_2)
		scale = -scale + M_PI*(scale > 0 ? 1.0 : -1.0);
	if ((pan = filternode_get_param(n, "pan")))
		scale = filterparam_val_float(pan);
	
	if (!(mod = filternode_get_output(n, "left-out"))
	    || !(pass = filternode_get_output(n, "right-out")))
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

/* BUGBUGBUG! According to docs, we need to fixup a failed param change 
 * ourselves via filternode_set_param(). Now guess what 
 * filternode_set_param() calls as its fixup method... Here's a quick and
 * dirty band aid that is definitely not threadsafe. richi needs to fix
 * properly. (WTH is going on here anyway. For all I know it shouldn't 
 * recurse as min and max are valid param settings!)
 */
static int pan_fixup_param(filter_node_t *src, filter_pipe_t *pipe,
                           const char *name, filter_param_t *param)
{
	float min = -M_PI_2;
	float max = M_PI_2;
	float pan;
	static int norecurse = 0;

	if (norecurse) {
		norecurse = 0;
		return 0;
	}
	
	pan = filterparam_val_float(param);
	if (pan < -M_PI_2) {
		norecurse = 1;
		filternode_set_param(src, "pan", &min);
		return -1;
	} else if (pan > M_PI_2) {
		norecurse = 1;
		filternode_set_param(src, "pan", &max);
		return -1;
	}
	return 0;
}
	
PLUGIN_DESCRIPTION(pan, "Place mono stream in stereo field")
PLUGIN_PIXMAP(pan, "default.xpm")

/* Most other filters try to avoid explicit reference to 'left' and 'right'
 * ports. Not so with pan, as it is tailor-made for stereo needs.
 */
int pan_register()
{
	filter_t *f;

	if (((f = filter_alloc(pan_f)) == NULL)
	    || !filter_add_input(f, PORTNAME_IN, "input stream to pan", 
	                         FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, "left-out", "left output stream", 
	                          FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, "right-out", "right output stream",
	                          FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f, "pan", 
	                         "position in stereo field [-pi/2, pi/2]", 
				 FILTER_PARAMTYPE_FLOAT)
	    || filter_add(f, "pan", 
	                  "Positions a mono audio stream in the stereo field"))
		return -1;

	f->fixup_param = pan_fixup_param;
	
	return 0;
}
