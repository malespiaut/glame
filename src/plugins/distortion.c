/*
 * distortion.c
 * $Id: distortion.c,v 1.2 2001/05/03 12:34:47 mag Exp $ 
 *
 * Copyright (C) 2001 Alexander Ehlert
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

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "math.h"

PLUGIN_SET(reverbs,"distortion")

static int distortion_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf;
	filter_param_t *param;
	
	SAMPLE  *s, w;
	int	i, mode;
	float	pregain, pos_clip, neg_clip, asym, clip, sign, gain, fxgain;
	
	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	if (!in || !out)
		FILTER_ERROR_RETURN("no in/output connected");
	
	if ((param=filternode_get_param(n,"pregain")))
			pregain=filterparam_val_float(param);
	
	if ((param=filternode_get_param(n,"fxgain")))
			fxgain=filterparam_val_float(param);
	
	if ((param=filternode_get_param(n,"clip")))
			clip=filterparam_val_float(param);
	
	if ((param=filternode_get_param(n,"asym")))
			asym=filterparam_val_float(param);

	if ((param=filternode_get_param(n,"mode")))
			mode = filterparam_val_int(param);
	
	pos_clip = asym + clip;
	neg_clip = asym - clip;

	gain = 1.0 / (1.0 + fxgain);
	
	FILTER_AFTER_INIT;
	
	switch (mode)
	{
		case 0:
			goto entry;
			break;
		case 1:
			goto entry1;
			break;
		default:
			goto exitus;
	}

	while (buf) {
		FILTER_CHECK_STOP;
		
		/* got an input buffer */
		s = &sbuf_buf(buf)[0];
		for(i=0; i < sbuf_size(buf); i++) {
			w = *s;
			sign = (w >= 0.0 ? 1.0 : -1.0);
			w *= pregain;
			w *= w * sign;
			w = (w > pos_clip ? pos_clip :
				( w < neg_clip ? neg_clip : w));
			*s += w * fxgain;
			*s++ *= gain;
		}

		sbuf_queue(out, buf);
entry:
		buf = sbuf_make_private(sbuf_get(in));
	};
	
	goto exitus;

	/* Here starts the full wave rectifier distortion */
	while (buf) {
		FILTER_CHECK_STOP;
		
		/* got an input buffer */
		s = &sbuf_buf(buf)[0];
		for(i=0; i < sbuf_size(buf); i++) {
			w = *s;
			w *= w * pregain;
			w = (w > clip ? clip : w);
			*s += w * fxgain;
			*s++ *= gain;
		}

		sbuf_queue(out, buf);
entry1:
		buf = sbuf_make_private(sbuf_get(in));
	};

exitus:
	sbuf_queue(out, buf);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int distortion_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdb_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "audio stream in", FILTER_PORTTYPE_SAMPLE);
	filter_add_output(f, PORTNAME_OUT, "audio stream out", FILTER_PORTTYPE_SAMPLE);
	
	f->f = distortion_f;
	
	param = filter_paramdb(f);

	filterparamdb_add_param_float(param, "pregain", FILTER_PARAMTYPE_FLOAT, 10.0,
				    FILTERPARAM_DESCRIPTION, "gain before distortion",
				    FILTERPARAM_END);
	
	filterparamdb_add_param_float(param, "fxgain", FILTER_PARAMTYPE_FLOAT, 1.0,
				    FILTERPARAM_DESCRIPTION, "distortion effect gain",
				    FILTERPARAM_END);
	
	filterparamdb_add_param_float(param, "clip", FILTER_PARAMTYPE_FLOAT, 0.8,
				    FILTERPARAM_DESCRIPTION, "clip range",
				    FILTERPARAM_END);

	filterparamdb_add_param_float(param, "asym", FILTER_PARAMTYPE_FLOAT, 0.0,
				    FILTERPARAM_DESCRIPTION, 
				    "midpoint for asymmetrical clipping",
				    FILTERPARAM_END);
	
	filterparamdb_add_param_int(param, "mode", FILTER_PARAMTYPE_INT, 0,
				    FILTERPARAM_DESCRIPTION, 
				    "(0) halfwave/asymmetrical (1) fullwave rectifier",
				    FILTERPARAM_END);

	plugin_set(p, PLUGIN_DESCRIPTION, "distortion effect");
	/* plugin_set(p, PLUGIN_PIXMAP, "flanger.xpm"); */
	plugin_set(p, PLUGIN_CATEGORY, "Effects");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Distortion");

	return filter_register(f, p);
}
