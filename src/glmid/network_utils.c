/*
 * network_utils.c
 *
 * $Id: network_utils.c,v 1.2 2001/06/28 12:44:15 richi Exp $
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

	start = start + gpsm_item_hposition(swfile);
	if (start < 0
	    || (length != -1 && (start + length > gpsm_item_hsize(swfile))))
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
			      long start, long length)
{
	filter_t *f;
	long swname;

	if (!net || !swfile)
		return NULL;

	start = start + gpsm_item_hposition(swfile);
	if (start < 0
	    || (length != -1 && (start + length > gpsm_item_hsize(swfile))))
		return NULL;
	swname = gpsm_swfile_filename(swfile);

	if (!(f = filter_instantiate(plugin_get("swapfile_out"))))
		return NULL;
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "filename"), &swname);
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "offset"), &start);
	if (length != -1)
		filterparam_set(filterparamdb_get_param(filter_paramdb(f), "size"), &length);

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
		if (!(out = filterportdb_get_port(filter_portdb(f),
						  PORTNAME_OUT)))
			continue;
		if (filterport_get_pipe(out))
			continue;

		if (!(e = filter_creat(effect)))
			return -1;
		if (filter_add_node(net, e, "effect") == -1)
			return -1;
		if (!(in = filterportdb_get_port(filter_portdb(e), 
						 PORTNAME_IN)))
			return -1;
		if (!filterport_connect(out, in))
			return -1;
		DPRINTF("connected effect to %s\n", filter_name(f));
	}

	return 0;
}

int net_apply_node(filter_t *net, filter_t *node)
{
	filter_port_t *out, *in;
	filter_t *f;

	if (!net || !node)
		return -1;

	if (!(in = filterportdb_get_port(filter_portdb(node), PORTNAME_IN)))
		return -1;

	filter_foreach_node(net, f) {
		if (f == node)
			continue;
		if (!(out = filterportdb_get_port(filter_portdb(f),
						  PORTNAME_OUT)))
			continue;
		if (filterport_get_pipe(out))
			continue;

		if (!filterport_connect(out, in))
			return -1;
		DPRINTF("connected node to %s\n", filter_name(f));
	}

	return 0;
}

filter_t *net_apply_audio_out(filter_t *net)
{
	filter_port_t *out, *in;
	filter_pipe_t *pipe;
	float pos;
	filter_t *aout, *render, *f;

	if (!net)
		return NULL;

	/* create the render node */
	if (!(render = filter_instantiate(plugin_get("render")))
	    || filter_add_node(net, render, "render") == -1)
		return NULL;
	in = filterportdb_get_port(filter_portdb(render), PORTNAME_IN);

	/* attach the nodes to the render plugin */
	filter_foreach_node(net, f) {
		if (f == render)
			continue;
		if (!(out = filterportdb_get_port(filter_portdb(f),
						  PORTNAME_OUT)))
			continue;
		if (filterport_get_pipe(out))
			continue;
		if (!filterport_connect(out, in))
			return NULL;
	}

	/* create the audio out node */
	if (!(aout = filter_instantiate(plugin_get("audio_out")))
	    || filter_add_node(net, aout, "audio-out") == -1)
		return NULL;
	in = filterportdb_get_port(filter_portdb(aout), PORTNAME_IN);

	/* connect render and audio out - FIXME */
	out = filterportdb_get_port(filter_portdb(render), PORTNAME_OUT);
	if (!(pipe = filterport_connect(out, in)))
		return NULL;
	pos = FILTER_PIPEPOS_LEFT;
	filterparam_set(filterparamdb_get_param(
		filterpipe_sourceparamdb(pipe), "position"), &pos);
	if (!(pipe = filterport_connect(out, in)))
		return NULL;
	pos = FILTER_PIPEPOS_RIGHT;
	filterparam_set(filterparamdb_get_param(
		filterpipe_sourceparamdb(pipe), "position"), &pos);

	return aout;
}

static saved_wbufsize = -1;
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
