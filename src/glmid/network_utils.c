/*
 * network_utils.c
 *
 * $Id: network_utils.c,v 1.6 2001/07/13 08:57:38 richi Exp $
 *
 * Copyright (C) 2001 Richard Guenther
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

#include "network_utils.h"


filter_t *net_add_plugin_by_name(filter_t *net, const char *plugin)
{
	plugin_t *p;
	filter_t *f;

	if (!net || !plugin)
		return NULL;
	if (!(p = plugin_get(plugin))) {
		DPRINTF("No such plugin %s\n", plugin);
		return NULL;
	}
	if (!(f = filter_instantiate(p))) {
		DPRINTF("Cannot instantiate %s\n", plugin);
		return NULL;
	}
	if (filter_add_node(net, f, plugin) == -1) {
		DPRINTF("Cannot add %s to network\n", plugin);
		filter_delete(f);
		return NULL;
	}
	return f;
}

filter_t *net_add_gpsm_input(filter_t *net, gpsm_swfile_t *swfile,
			     long start, long length)
{
	filter_t *f;
	long swname, swrate;
	float swpos;

	if (!net || !swfile)
		return NULL;

	start = start - gpsm_item_hposition(swfile);
	if (length < -1)
		return NULL;
	swname = gpsm_swfile_filename(swfile);
	swrate = gpsm_swfile_samplerate(swfile);
	swpos = gpsm_swfile_position(swfile);

	if (!(f = filter_instantiate(plugin_get("swapfile_in"))))
		return NULL;
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "filename"), &swname);
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "rate"), &swrate);
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "position"), &swpos);
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "offset"), &start);
	if (length != -1)
		filterparam_set(filterparamdb_get_param(filter_paramdb(f), "size"), &length);

	if (filter_add_node(net, f, gpsm_item_label(swfile)) == -1) {
		filter_delete(f);
		return NULL;
	}

	return f;
}

filter_t *net_add_gpsm_output(filter_t *net, gpsm_swfile_t *swfile,
			      long start, long length, long flags)
{
	filter_t *f;
	long swname;

	if (!net || !swfile)
		return NULL;

	start = start - gpsm_item_hposition(swfile);
	if (length < -1)
		return NULL;
	swname = gpsm_swfile_filename(swfile);

	if (!(f = filter_instantiate(plugin_get("swapfile_out"))))
		return NULL;
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "filename"), &swname);
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "offset"), &start);
	if (length != -1)
		filterparam_set(filterparamdb_get_param(filter_paramdb(f), "size"), &length);
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "flags"), &flags);

	if (filter_add_node(net, f, gpsm_item_label(swfile)) == -1) {
		filter_delete(f);
		return NULL;
	}

	return f;
}

int net_apply_effect(filter_t *net, filter_t *effect)
{
	filter_port_t *out, *in;
	filter_t *f, *e;

	if (!net || !effect)
		return -1;

	filter_foreach_node(net, f) {
		/* Use the first "matching" node output. */
		filterportdb_foreach_port(filter_portdb(f), out) {
			if (!filterport_is_output(out)
			    || !FILTER_PORTS_ARE_COMPATIBLE(filterport_type(out), FILTER_PORTTYPE_SAMPLE)
			    || filterport_get_pipe(out))
				continue;

			/* This is ok only because nodes get inserted at
			 * the head. */
			if (!(e = filter_creat(effect))
			    || filter_add_node(net, e, "effect") == -1)
				return -1;

			/* Use the first "matching" effect input. */
			filterportdb_foreach_port(filter_portdb(e), in) {
				if (filterport_is_input(in)
				    && FILTER_PORTS_ARE_COMPATIBLE(filterport_type(in), FILTER_PORTTYPE_SAMPLE))
					break;
			}
			if (!in || !filterport_connect(out, in)) {
				filter_delete(e);
				return -1;
			}
			DPRINTF("connected effect to %s::%s\n", filter_name(f), filterport_label(out));
		}
	}

	return 0;
}

int net_apply_node(filter_t *net, filter_t *node)
{
	filter_port_t *out, *in;
	filter_t *f;

	if (!net || !node)
		return -1;

	filterportdb_foreach_port(filter_portdb(node), in)
		if (filterport_is_input(in))
			break;
	if (!in)
		return -1;

	filter_foreach_node(net, f) {
		if (f == node)
			continue;
		filterportdb_foreach_port(filter_portdb(f), out) {
			if (!filterport_is_output(out)
			    || !FILTER_PORTS_ARE_COMPATIBLE(filterport_type(out), filterport_type(in))
			    || filterport_get_pipe(out))
				continue;
			if (!filterport_connect(out, in))
				return -1;
			DPRINTF("connected node to %s::%s\n", filter_name(f), filterport_label(out));
		}
	}

	return 0;
}

filter_t *net_apply_audio_out(filter_t *net)
{
	filter_port_t *out, *in;
	filter_pipe_t *pipe;
	float pos;
	filter_t *aout = NULL, *render = NULL;

	if (!net)
		return NULL;

	/* create the render node and apply it to the network */
	if (!(render = filter_instantiate(plugin_get("render")))
	    || filter_add_node(net, render, "render") == -1
	    || net_apply_node(net, render) == -1)
		goto err;

	/* create the audio out node */
	if (!(aout = filter_instantiate(plugin_get("audio_out")))
	    || filter_add_node(net, aout, "audio-out") == -1)
		goto err;
	in = filterportdb_get_port(filter_portdb(aout), PORTNAME_IN);

	/* connect render and audio out - FIXME */
	out = filterportdb_get_port(filter_portdb(render), PORTNAME_OUT);
	if (!(pipe = filterport_connect(out, in)))
		goto err;
	pos = FILTER_PIPEPOS_LEFT;
	filterparam_set(filterparamdb_get_param(
		filterpipe_sourceparamdb(pipe), "position"), &pos);
	if (!(pipe = filterport_connect(out, in)))
		goto err;
	pos = FILTER_PIPEPOS_RIGHT;
	filterparam_set(filterparamdb_get_param(
		filterpipe_sourceparamdb(pipe), "position"), &pos);

	return aout;

 err:
	if (render)
		filter_delete(render);
	if (aout)
		filter_delete(aout);
	return NULL;
}

static int saved_wbufsize = -1;
void net_prepare_bulk()
{
	if (saved_wbufsize == -1) {
		saved_wbufsize = GLAME_WBUFSIZE;
		GLAME_WBUFSIZE = 32768;
	}
}

void net_restore_default()
{
	if (saved_wbufsize != -1) {
		GLAME_WBUFSIZE = saved_wbufsize;
		saved_wbufsize = -1;
	}
}

char *net_get_error_str(filter_t *net)
{
	char msg[256];
	filter_t *node;

	if (!net)
		return NULL;
	filter_foreach_node(net, node) {
		if (filter_errstr(node)) {
			snprintf(msg, 256, "Error processing net
work:\n%s: %s\n", filter_name(node), filter_errstr(node));
			return strdup(msg);
		}
	}

	return NULL;
}

