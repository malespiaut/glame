/*
 * ssp.c
 * $Id: ssp.c,v 1.3 2001/05/14 22:57:59 mag Exp $
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

#include <config.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "math.h"
#include "gpsm.h"

PLUGIN_SET(ssp, "ssp_streamer maxrms normalize")

static int ssp_streamer_connect_out(filter_t *n, filter_port_t *port,
				   filter_pipe_t *p)
{
	filterpipe_settype_ssp(p, 44100, 64);
	return 0;
}

static int ssp_streamer_f(filter_t *n)
{
	filter_pipe_t *in, *out;
	filter_buffer_t *buf, *obuf;
	filter_param_t *param;
	
	SAMPLE  *ringbuf, *s, *ssp, accu, fak, ss;
	int	bsize, pos, cnt;
	in = filternode_get_input(n, PORTNAME_IN);
	out = filternode_get_output(n, PORTNAME_OUT);
	
	param = filternode_get_param(n,"bsize");
	bsize = filterparam_val_int(param);
	if (bsize < 1)
		FILTER_ERROR_RETURN("bsize <= 0");
	
	DPRINTF("bsize = %d\n", bsize);
	fak = 1.0/(SAMPLE)bsize;
	accu = 0.0;
	ringbuf = ALLOCN(bsize, SAMPLE);
	if (ringbuf == NULL)
		FILTER_ERROR_RETURN("Error allocating ringbuf\n");
	pos = 0;

	FILTER_AFTER_INIT;
	
	goto entry;
	while (buf) {
		FILTER_CHECK_STOP;
		
		/* got an input buffer */
		s = sbuf_buf(buf);
		cnt = sbuf_size(buf);
		obuf = ssp_make_private(ssp_alloc(cnt, n));
		ssp = ssp_buf(obuf);
		
		while (cnt--) {
			ss = *s * *s;
			accu += fak * (ss - ringbuf[pos]);
			ringbuf[pos++] = ss;
			if (pos == bsize)
				pos = 0;
			s++;
			*(ssp++) = accu;
		}

		sbuf_unref(buf);
		ssp_queue(out, obuf);
entry:
		buf = sbuf_make_private(sbuf_get(in));
	};
	
	ssp_queue(out, NULL);
	
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	free(ringbuf);

	FILTER_RETURN;
}

int ssp_streamer_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdb_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "audio stream in", FILTER_PORTTYPE_SAMPLE);
	filter_add_output(f, PORTNAME_OUT, "ssp stream out", FILTER_PORTTYPE_SSP);
	
	f->f = ssp_streamer_f;
	f->connect_out = ssp_streamer_connect_out;
	
	param = filter_paramdb(f);

	filterparamdb_add_param_int(param, "bsize", FILTER_PARAMTYPE_INT, 64 ,
				    FILTERPARAM_DESCRIPTION, "length of running average",
				    FILTERPARAM_END);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "ssp_streamer");
	plugin_set(p, PLUGIN_PIXMAP, "ssp.png"); 
	plugin_set(p, PLUGIN_CATEGORY, "Analyze");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Analyze");

	return filter_register(f, p);
}

static int maxrms_f(filter_t *n)
{
	filter_pipe_t	*in;
	filter_buffer_t	*buf;
	filter_param_t	*param;
	
	SAMPLE	maxrms = 0.0, *s;
	int	cnt;
	
	if (!(in = filternode_get_input(n, PORTNAME_IN)))
		FILTER_ERROR_RETURN("no input");
	
	param = filternode_get_param(n, "maxrms");

	filterparam_set(param, &maxrms);

	DPRINTF("crash vorher?\n");
	
	FILTER_AFTER_INIT;
	
	goto entry;
	while (buf) {
		FILTER_CHECK_STOP;
		
		/* got an input buffer */
		s = sbuf_buf(buf);
		cnt = sbuf_size(buf);
		
		while (cnt--) {
			if (*s > maxrms)
				maxrms = *s;
			s++;
		}

		sbuf_unref(buf);
entry:
		buf = sbuf_make_private(sbuf_get(in));
	};

	maxrms = sqrtf(maxrms);

	filterparam_set(param, &maxrms);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int maxrms_register(plugin_t *p)
{
	filter_t *f;
	filter_paramdb_t *param;
	
	if (!(f = filter_creat(NULL)))
		return -1;
	
	filter_add_input(f, PORTNAME_IN, "ssp stream in", FILTER_PORTTYPE_SSP);
	
	f->f = maxrms_f;
	
	param = filter_paramdb(f);

	filterparamdb_add_param_float(param, "maxrms", FILTER_PARAMTYPE_FLOAT, 0.0 ,
				    FILTERPARAM_DESCRIPTION, "maximum rms",
				    FILTERPARAM_END);
	
	plugin_set(p, PLUGIN_DESCRIPTION, "maxrms");
	plugin_set(p, PLUGIN_PIXMAP, "maxrms.png"); 
	plugin_set(p, PLUGIN_CATEGORY, "Analyze");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Analyze");

	return filter_register(f, p);
}

static int normalize_gpsm(gpsm_grp_t *grp, long start, long length)
{
	filter_t *net, *swap_in, *swap_out, *swap, *ssp, 
		 *maxrms, *vadjust, *mix, *adjust;
	gpsm_item_t	*item;
	filter_param_t	*param;
	int err = -1, bsize = 1;
	long	filename, rate;
	float	rms, gain;
	
	swap_in = filter_instantiate(plugin_get("swapfile_in"));
	swap_out= filter_instantiate(plugin_get("swapfile_out"));
	mix	= filter_instantiate(plugin_get("mix"));
	ssp	= filter_instantiate(plugin_get("ssp_streamer"));
	maxrms	= filter_instantiate(plugin_get("maxrms"));
	vadjust	= filter_instantiate(plugin_get("volume_adjust"));

	net = filter_creat(NULL);
	filter_add_node(net, ssp, "ssp");
	filter_add_node(net, mix, "mix");
	filter_add_node(net, maxrms, "maxrms");

	gpsm_grp_foreach_item(grp, item) {
		swap = filter_creat(swap_in);
		filename =  gpsm_swfile_filename(item);
		rate = gpsm_swfile_samplerate(item);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "filename"), 
				&filename);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "offset"), 
				&start);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "rate"), 
				&rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "size"), 
				&length);
		filter_add_node(net, swap, "swapin");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(swap), PORTNAME_OUT), 
					filterportdb_get_port(filter_portdb(mix), PORTNAME_IN)))
			goto cleanup;
	}

	if (!filterport_connect(filterportdb_get_port(filter_portdb(mix), PORTNAME_OUT), 
				filterportdb_get_port(filter_portdb(ssp), PORTNAME_IN)))
		goto cleanup;
	
	if (!filterport_connect(filterportdb_get_port(filter_portdb(ssp), PORTNAME_OUT), 
				filterportdb_get_port(filter_portdb(maxrms), PORTNAME_IN)))
		goto cleanup;

	filterparam_set(filterparamdb_get_param(filter_paramdb(ssp), "bsize"), &bsize);
	
	DPRINTF("First Pass\n");

	if ((filter_launch(net) == -1) ||
	    (filter_start(net) == -1))
		goto cleanup;

	filter_wait(net);

	param = filterparamdb_get_param(filter_paramdb(maxrms), "maxrms");
	rms = filterparam_val_float(param);
	gain = 1.0 / rms;
	filter_delete(net);

	DPRINTF("Found RMS = %f setting gain = %f\n", rms, gain);
	
	net = filter_creat(NULL);
	
	if (gpsm_op_prepare((gpsm_item_t*)grp) == -1)
		DPRINTF("Error preparing for undo\n");
	
	gpsm_grp_foreach_item(grp, item) {
		adjust = filter_creat(vadjust);
		filter_add_node(net, adjust, "adjust");
		swap = filter_creat(swap_in);
		filename =  gpsm_swfile_filename(item);
		rate = gpsm_swfile_samplerate(item);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "filename"), 
				&filename);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "offset"), 
				&start);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "rate"), 
				&rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "size"), 
				&length);
		filter_add_node(net, swap, "swapin");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(swap), PORTNAME_OUT), 
					filterportdb_get_port(filter_portdb(adjust), PORTNAME_IN)))
			goto cleanup;

		swap = filter_creat(swap_out);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "filename"), 
				&filename);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "offset"), 
				&start);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap), "size"), 
				&length);

		filter_add_node(net, swap, "swapout");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(adjust), PORTNAME_OUT), 
					filterportdb_get_port(filter_portdb(swap), PORTNAME_IN)))
			goto cleanup;
	
		filterparam_set(filterparamdb_get_param(filter_paramdb(adjust), "factor"), 
				&gain);
	}
	
	DPRINTF("Second pass\n");

	if ((filter_launch(net) == -1) ||
	    (filter_start(net) == -1))
		goto cleanup;

	filter_wait(net);
	
	gpsm_grp_foreach_item(grp, item) {
		if (start >= 0) 
			gpsm_notify_swapfile_change(gpsm_swfile_filename(item), start, 
						    length); 
		else 
			gpsm_invalidate_swapfile(gpsm_swfile_filename(item));
	}
	
	err = 0;

cleanup:
	DPRINTF("err = %d\n", err);
	filter_delete(swap_in);
	filter_delete(swap_out);
	filter_delete(vadjust);
	filter_delete(net);

	return err;
}

int normalize_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_GPSMOP, normalize_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION, "normalizes a gpsm subtree");
	plugin_set(p, PLUGIN_CATEGORY, "Volume");
	
	return 0;
}
