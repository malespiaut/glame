/*
 * distortion.c
 * $Id: distortion.c,v 1.8 2001/07/07 12:28:07 mag Exp $ 
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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
	filter_param_t *param, *preg_param, *fxg_param, *clip_param, *asym_param;
	
	SAMPLE  *s, w;
	int	i;
	float	pregain, pos_clip, neg_clip, asym, clip, sign, gain, fxgain;
	
	in = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN));
	out = filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT));
	if (!in || !out)
		FILTER_ERROR_RETURN("no in/output connected");
	
	preg_param = filterparamdb_get_param(filter_paramdb(n), "pregain");
	
	fxg_param = filterparamdb_get_param(filter_paramdb(n), "fxgain");
	
	clip_param = filterparamdb_get_param(filter_paramdb(n), "clip");
	
	asym_param = filterparamdb_get_param(filter_paramdb(n), "asym");

	param=filterparamdb_get_param(filter_paramdb(n), "mode");
	
	pos_clip = asym + clip;
	neg_clip = asym - clip;

	gain = 1.0 / (1.0 + fxgain);
	
	FILTER_AFTER_INIT;
	
	switch (filterparam_val_int(param))
	{
		case 0:
			goto entry;
			break;
		case 1:
			goto entry1;
			break;
		case 2: 
			goto entry2;
			break;
		default:
			goto exitus;
	}
	
	while (buf) {
		FILTER_CHECK_STOP;
		
		asym = filterparam_val_float(asym_param);
		clip = filterparam_val_float(clip_param);

		pregain = filterparam_val_float(preg_param);
		
		pos_clip = asym + clip;
		neg_clip = asym - clip;
		fxgain = filterparam_val_float(fxg_param);
		gain = 1.0 / (1.0 + fxgain);
		
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

		switch (filterparam_val_int(param))
		{
			case 0:
				goto entry;
				break;
			case 1:
				goto entry1;
				break;
			case 2: 
				goto entry2;
				break;
			default:
				goto exitus;
		}
entry:
		buf = sbuf_make_private(sbuf_get(in));
	};
	
	goto exitus;

	/* Here starts the full wave rectifier distortion */
	while (buf) {
		FILTER_CHECK_STOP;
		
		asym = filterparam_val_float(asym_param);
		clip = filterparam_val_float(clip_param);

		pregain = filterparam_val_float(preg_param);
		
		pos_clip = asym + clip;
		neg_clip = asym - clip;
		fxgain = filterparam_val_float(fxg_param);
		gain = 1.0 / (1.0 + fxgain);
		
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
		
		switch (filterparam_val_int(param))
		{
			case 0:
				goto entry;
				break;
			case 1:
				goto entry1;
				break;
			case 2: 
				goto entry2;
				break;
			default:
				goto exitus;
		}
entry1:
		buf = sbuf_make_private(sbuf_get(in));
	};

	goto exitus;
	
	/* Simple sinusoider */


	while (buf) {
		FILTER_CHECK_STOP;

		pregain = filterparam_val_float(preg_param);
		fxgain  = filterparam_val_float(fxg_param);
		clip	= filterparam_val_float(clip_param);
		asym = filterparam_val_float(asym_param);

		pos_clip = asym + clip;
		neg_clip = asym - clip;
		
		pregain *= M_PI;
		if (pregain < M_PI*0.5)
			fxgain *= 1.0/sinf(clip);
	
		gain = 1.0 / (1.0 + fxgain);
		
		/* got an input buffer */
		s = &sbuf_buf(buf)[0];
		for(i=0; i < sbuf_size(buf); i++) {
			*s = gain * (*s + fxgain * sinf(*s * pregain));
			if (*s > pos_clip)
				*s = pos_clip;
			else if (*s < neg_clip)
				*s = neg_clip;
			s++;
		}

		sbuf_queue(out, buf);
		
		switch (filterparam_val_int(param))
		{
			case 0:
				goto entry;
				break;
			case 1:
				goto entry1;
				break;
			case 2: 
				goto entry2;
				break;
			default:
				goto exitus;
		}
entry2:
		buf = sbuf_make_private(sbuf_get(in));
	}
	
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
	f->f = distortion_f;

	filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_INPUT,
			      FILTERPORT_DESCRIPTION, "audio stream in",
			      FILTERPORT_END);
	filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
			      FILTER_PORTTYPE_SAMPLE,
			      FILTER_PORTFLAG_OUTPUT,
			      FILTERPORT_DESCRIPTION, "audio stream out",
			      FILTERPORT_END);
	
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
	
        filterparamdb_add_param_int(param,"mode", 
				    FILTER_PARAMTYPE_INT, 0, 
				    FILTERPARAM_DESCRIPTION,
				    "(0) halfwave/asymmetrical" 
				    "(1) fullwave rectifier"
				    "(2) sinusoider",
				    FILTERPARAM_GLADEXML,
"<?xml version=\"1.0\"?><GTK-Interface><widget> 
	<class>GtkOptionMenu</class> 
	<name>widget</name> 
	<can_focus>True</can_focus> 
	<items>Half wave rectifier 
Full Wave rectifier
Sinusoider</items> 
	<initial_choice>0</initial_choice> 
</widget></GTK-Interface>", 
	FILTERPARAM_END);


	plugin_set(p, PLUGIN_DESCRIPTION, "distortion effect");
	plugin_set(p, PLUGIN_PIXMAP, "distortion.png"); 
	plugin_set(p, PLUGIN_CATEGORY, "Effects");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Distortion");

	return filter_register(f, p);
}
