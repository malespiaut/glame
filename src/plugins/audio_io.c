/*
 * audio_io.c
 * $Id: audio_io.c,v 1.32 2001/04/24 14:08:06 xwolf Exp $
 *
 * Copyright (C) 1999-2001 Richard Guenther, Alexander Ehlert, Daniel Kobras
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
#include <string.h>
#include <errno.h>
#include <limits.h>
#include "glame_types.h"
#include "filter.h"
#include "util.h"
#include "glplugin.h"

/* TODO: (post 0.2)
 *       * alsa, oss and sgi audio in. Implement when we have a proper
 *         control interface from UI to filters.
 *       * The whole file needs a file_io'ish re-write to further
 *         centralize common code. This is post-0.2 work however. Let's
 *         stick with a little code duplication for now.
 */

PLUGIN_SET(audio_io, "audio_out audio_in")

/* Generic versions of filter methods. Called from various low-level
 * implementations.
 */

/* Generic connect_in method for audio output filters. We assume a
 * single (automatic) input port.
 */

static int aio_generic_connect_in(filter_t *dest, filter_port_t *inp,
				  filter_pipe_t *pipe)
{
	filter_pipe_t *portishead;
	
	/* Limit to stereo output */
	if (filterport_nrpipes(inp) > 1)
		return -1;
	
	/* Assert a common sample rate between all input pipes */
	if ((portishead = filterport_get_pipe(inp)))
		if (filterpipe_sample_rate(portishead) !=
		    filterpipe_sample_rate(pipe))
			return -1;
	return 0;
}

/* Generic connect_out method for audio input filters. We assume there
 * are no input ports and a single (automatic) output port.
 */

static int aio_generic_connect_out(filter_t *src, filter_port_t *outp,
				   filter_pipe_t *pipe)
{
	filter_param_t *ratep;
	filter_pipe_t *prev;
	float phi = FILTER_PIPEPOS_CENTRE;
	int rate = GLAME_DEFAULT_SAMPLERATE;

	/* Limit to stereo capture */
	if (filterport_nrpipes(outp) > 1)
		return -1;
	
	/* Check for default rate parameter */
	if ((ratep=filternode_get_param(src, "rate")))
		rate = filterparam_val_int(ratep);

	/* That's a bit messy. If there are two pipes and the user
	 * didn't specify explicit directional information, we set 
	 * this port to stereo right and the other port to left. We
	 * default to centre, if we are the only port.
	 */
	if ((prev = filterport_get_pipe(outp))) {
		if (!filterpipe_get_sourceparam(prev, "hangle"))
			filterpipe_sample_hangle(prev) = FILTER_PIPEPOS_LEFT;
		phi = FILTER_PIPEPOS_RIGHT;
	}
	
	filterpipe_settype_sample(pipe, rate, phi);

	return 0;
}						

/* Generic fixup_pipe method for audio output filters. Assert a common
 * sample rate between all input pipes.
 */

static void aio_generic_fixup_pipe(glsig_handler_t *h, long sig, va_list va)
{
	filter_t *src;
	filter_pipe_t *pipe;
	filter_port_t *inp;
	int rate;

	GLSIGH_GETARGS1(va, pipe);
	src = filterport_filter(filterpipe_dest(pipe));
	rate = filterpipe_sample_rate(pipe);

	filterportdb_foreach_port(filter_portdb(src), inp) {
		if (!filterport_is_input(inp))
			continue;
		filterport_foreach_pipe(inp, pipe) {
			if (rate != filterpipe_sample_rate(pipe)) {
				filter_set_error(src, "mismatching input samplerates");
				return;
			}
		}
	}
	filter_clear_error(src);
	/* No output pipes. */
}

/* Generic fixup_param method for audio input filters. */

static void aio_generic_fixup_param(glsig_handler_t *h, long sig, va_list va)
{
	filter_param_t *param;
	filter_t *n;
	filter_pipe_t *out;
	filter_port_t *outp;
	int rate;

	GLSIGH_GETARGS1(va, param);
	n = filterparam_filter(param);
	out = filterparam_get_sourcepipe(param);

	if (out && strcmp("position", filterparam_label(param)) == 0) {
		/* FIXME: param setting checks belong to set_param()
		 * now. fixup_param is only fixup. */
		float hangle = filterparam_val_float(param);
		if (hangle <= -M_PI || hangle > M_PI)
			return;
		filterpipe_sample_hangle(out) = hangle;
		glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);

	} else if (strcmp("rate", filterparam_label(param)) == 0) {
		rate = filterparam_val_int(param);
		/* Make sure to update rate param on all pipes
		 */
		outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
		filterport_foreach_pipe(outp, out) {
			if (filterpipe_sample_rate(out) == rate)
				continue;
			filterpipe_sample_rate(out) = rate;
			glsig_emit(&out->emitter, GLSIG_PIPE_CHANGED, out);
		}
	}
}

/* Clumsy try to clean up the register mess a bit. */

int aio_generic_register_input(plugin_t *pl, char *name,
			       int (*f)(filter_t *), const char *defaultdevice)
{
	filter_t *filter;
	filter_port_t *p;

	if (!f)
		return -1;

	if (!pl && !(pl = plugin_add(name)))
		return -1;

	if (!(filter = filter_creat(NULL)))
		return -1;

	p = filter_add_output(filter, PORTNAME_OUT, "output port",
			      FILTER_PORTTYPE_SAMPLE);
	filterparamdb_add_param_float(filterport_paramdb(p), "position",
				  FILTER_PARAMTYPE_POSITION,
				  FILTER_PIPEPOS_DEFAULT, FILTERPARAM_END);

	filterparamdb_add_param_int(filter_paramdb(filter), "rate",
				FILTER_PARAMTYPE_INT, GLAME_DEFAULT_SAMPLERATE,
				FILTERPARAM_END);
	filterparamdb_add_param_float(filter_paramdb(filter), "duration", 
				  FILTER_PARAMTYPE_TIME_S, 0.0,
				  FILTERPARAM_END);
	filterparamdb_add_param_string(filter_paramdb(filter), "device",
				   FILTER_PARAMTYPE_STRING, defaultdevice,
				   FILTERPARAM_END);

	filter->f = f;
	filter->connect_out = aio_generic_connect_out;
	glsig_add_handler(&filter->emitter, GLSIG_PARAM_CHANGED,
			  aio_generic_fixup_param, NULL);

	plugin_set(pl, PLUGIN_DESCRIPTION, "record stream");
	plugin_set(pl, PLUGIN_PIXMAP, "input.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Input");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "Audio_I_O");	
	filter_register(filter, pl);

	return 0;
}
			
int aio_generic_register_output(plugin_t *pl, char *name, int (*f)(filter_t *),
				const char *defaultdevice) 
{
	filter_t *filter;
	
	if (!f)
		return -1;
	if (!pl && !(pl = plugin_add(name)))
		return -1;
	
	if (!(filter=filter_creat(NULL)))
		return -1;

	filter_add_input(filter, PORTNAME_IN, "input port",
			 FILTER_PORTTYPE_SAMPLE);
	filterparamdb_add_param_string(filter_paramdb(filter), "device",
				   FILTER_PARAMTYPE_STRING, defaultdevice,
				   FILTERPARAM_END);

	filter->f = f;
	filter->connect_in = aio_generic_connect_in;
	glsig_add_handler(&filter->emitter, GLSIG_PIPE_CHANGED,
			  aio_generic_fixup_pipe, NULL);

	plugin_set(pl, PLUGIN_DESCRIPTION, "playback stream");
	plugin_set(pl, PLUGIN_PIXMAP, "output.png");
        plugin_set(pl, PLUGIN_CATEGORY, "Output");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "Audio_I_O");	
	
	filter_register(filter, pl);

	return 0;
}




/* The registration functions.
 * Only two "real" plugins will appear - the generic audio_out
 * and the audio_in plugin/filter. Others are only accessible
 * directly.
 */

int audio_out_register(plugin_t *p)
{
	plugin_t *aout, *audio_out = NULL;
	filter_t *audio_out_f;

	plugin_get("audio_io_oss");
	plugin_get("audio_io_alsa");
	plugin_get("audio_io_esd");
	plugin_get("audio_io_irix");
	if ((aout = plugin_get("oss-audio-out")))
		audio_out = aout;
	if ((aout = plugin_get("alsa-audio-out")))
		audio_out = aout;
	if ((aout = plugin_get("esd-audio-out")))
		audio_out = aout;
	if ((aout = plugin_get("sgi-audio-out")))
		audio_out = aout;
	if (!audio_out)
		return -1;

	plugin_set(p, PLUGIN_DESCRIPTION, "playback stream");
	plugin_set(p, PLUGIN_PIXMAP, "output.png");
        plugin_set(p, PLUGIN_CATEGORY, "Output");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Audio_I_O");	

	audio_out_f = filter_instantiate(audio_out);
	return filter_register(audio_out_f, p);
}

int audio_in_register(plugin_t *p)
{
	plugin_t *ain, *audio_in = NULL;
	filter_t *audio_in_f;

	plugin_get("audio_io_oss");
	plugin_get("audio_io_alsa");
	plugin_get("audio_io_esd");
	plugin_get("audio_io_irix");
	if ((ain = plugin_get("oss-audio-in")))
		audio_in = ain;
	if ((ain = plugin_get("alsa-audio-in")))
		audio_in = ain;
	if ((ain = plugin_get("esd-audio-in")))
		audio_in = ain;
	if ((ain = plugin_get("sgi-audio-in")))
		audio_in = ain;
	if (!audio_in)
		return -1;

	plugin_set(p, PLUGIN_DESCRIPTION, "record stream");
	plugin_set(p, PLUGIN_PIXMAP, "input.png");
        plugin_set(p, PLUGIN_CATEGORY, "Input");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Audio_I_O");	

	audio_in_f = filter_instantiate(audio_in);
	return filter_register(audio_in_f, p);
}
