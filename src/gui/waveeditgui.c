/*
 * waveeditgui.c
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/param.h>
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <gtk/gtk.h>
#include <gnome.h>
#include "gtkwaveview.h"
#include "gtkswapfilebuffer.h"
#include "glame_types.h"
#include "glplugin.h"
#include "swapfile.h"
#include "glmid.h"
#include "glame_gui_utils.h"
#include "waveeditgui.h"
#include "edit_filter/canvas.h"
#include "clipboard.h"



/* The GUI is single-threaded, so this should actually work for
 * callbacks that cant take the waveview. */
static GtkWaveView *actual_waveview;



/*
 * The VIEW submenu and its callbacks.
 */

static void zoomin_cb(GtkWidget *bla, GtkWaveView *waveview);
static void zoomfull_cb(GtkWidget *bla, GtkWaveView *waveview);
static void zoomout_cb(GtkWidget *bla, GtkWaveView *waveview);
static void zoomsel_cb(GtkWidget *bla, GtkWaveView *waveview);
static void gotomarker_cb(GtkWidget *bla, GtkWaveView *waveview);

static GnomeUIInfo view_menu[] = {
	GNOMEUIINFO_ITEM("Zoom to selection", "zommsel", zoomsel_cb, NULL),
	GNOMEUIINFO_ITEM("Zoom in", "zommin", zoomin_cb, NULL),
	GNOMEUIINFO_ITEM("Zoom out", "zommout", zoomout_cb, NULL),
	GNOMEUIINFO_ITEM("View all", "zommfull", zoomfull_cb, NULL),
	GNOMEUIINFO_ITEM("Goto marker", "gotomarker", gotomarker_cb, NULL),
	GNOMEUIINFO_END
};

/* Menu event - Zoom in. */
static void zoomin_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	gdouble  zoomlevel;
	zoomlevel = gtk_wave_view_get_zoom(waveview);
	gtk_wave_view_set_zoom(waveview,zoomlevel/2.0);
}

/* Menu event - Zoom Full. */
static void zoomfull_cb(GtkWidget *bla, GtkWaveView *waveview)
{
        gtk_wave_view_set_zoom_all(waveview);
}

/* Menu event - Zoom out. */
static void zoomout_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	gdouble  zoomlevel;
	zoomlevel = gtk_wave_view_get_zoom(waveview);
	gtk_wave_view_set_zoom(waveview,zoomlevel*2.0);
}

/* Menu event - Zoom to selection. */
static void zoomsel_cb(GtkWidget *bla, GtkWaveView *waveview)
{
        gtk_wave_view_set_zoom_selection(waveview);
}

/* Menu event - Goto marker. */
static void gotomarker_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	gtk_wave_view_set_marker_and_scroll(
		waveview, gtk_wave_view_get_marker(waveview));
}





/*
 * The SELECT submenu and its callbacks.
 */

static void selectnone_cb(GtkWidget *w, GtkWaveView *waveview);
static void selectall_cb(GtkWidget *w, GtkWaveView *waveview);

static GnomeUIInfo select_menu[] = {
	GNOMEUIINFO_ITEM("Select none", "selectnone", selectnone_cb, NULL),
	GNOMEUIINFO_ITEM("Select all", "selectall", selectall_cb, NULL),
	GNOMEUIINFO_END
};

/* Menu event - select nothing. */
static void selectnone_cb(GtkWidget *w, GtkWaveView *waveview)
{
	gtk_wave_view_set_selection(waveview, 0, 0);
}

/* Menu event - select all. */
static void selectall_cb(GtkWidget *w, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);

	gtk_wave_view_set_selection(waveview, 0,
				    gtk_wave_buffer_get_length(wavebuffer));
}


/*
 * The EDIT submenu and its callbacks.
 */

static void copy_cb(GtkWidget *bla, GtkWaveView *waveview);
static void paste_cb(GtkWidget *bla, GtkWaveView *waveview);
static void cut_cb(GtkWidget *bla, GtkWaveView *waveview);
static void delete_cb(GtkWidget *bla, GtkWaveView *waveview);
static void undo_cb(GtkWidget *bla, GtkWaveView *waveview);
static void redo_cb(GtkWidget *bla, GtkWaveView *waveview);

static GnomeUIInfo edit_menu[] = {
	GNOMEUIINFO_ITEM("Cut", "cut", cut_cb, NULL),
	GNOMEUIINFO_ITEM("Copy", "copy", copy_cb, NULL),
	GNOMEUIINFO_ITEM("Paste", "paste", paste_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Undo", "undo", undo_cb, NULL),
	GNOMEUIINFO_ITEM("Redo", "redo", redo_cb, NULL),	
	GNOMEUIINFO_END
};
#define EDIT_MENU_CUT_INDEX 0
#define EDIT_MENU_COPY_INDEX 1
#define EDIT_MENU_PASTE_INDEX 2
#define EDIT_MENU_DELETE_INDEX 4
#define EDIT_MENU_UNDO_INDEX 6
#define EDIT_MENU_REDO_INDEX 7

/* Menu event - Copy. */
static void copy_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start, length;
	gpsm_item_t *item;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		DERROR("Ensured by rmb callback");

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (clipboard_copy(item, start, length) == -1)
		DPRINTF("Failed copying\n");
}

/* Menu event - Paste. */
static void paste_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 pos;
	gpsm_item_t *item;

	pos = gtk_wave_view_get_marker (waveview);
	if (pos < 0)
		return;
	item = gtk_swapfile_buffer_get_item(swapfile);
	if (!clipboard_can_paste(item))
		return;
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");
	if (clipboard_paste(item, pos) == -1)
		DPRINTF("Error pasting\n");
}

/* Menu event - Cut. */
static void cut_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start, length;
	gpsm_item_t *item;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		DERROR("Ensured by rmb callback");

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");
	if (clipboard_cut(item, start, length) == -1)
		DPRINTF("Error cutting\n");

	/* Remove the selection. */
	selectnone_cb(bla, waveview);
}

/* Menu event - Delete. */
static void delete_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start, length;
	gpsm_item_t *item;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		DERROR("Ensured by rmb callback");

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");
	if (clipboard_delete(item, start, length) == -1)
		DPRINTF("Error deleting\n");

	/* Remove the selection. */
	selectnone_cb(bla, waveview);
}

/* Menu event - Undo. */
static void undo_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkSwapfileBuffer *swapfile;
	gpsm_item_t *item;

	swapfile = GTK_SWAPFILE_BUFFER(gtk_wave_view_get_buffer(waveview));
	item = gtk_swapfile_buffer_get_item(swapfile);

	if (gpsm_op_undo(item) == -1)
		gnome_dialog_run_and_close(
			GNOME_DIALOG(gnome_error_dialog("Error during undo")));
}

/* Menu event - Redo. */
static void redo_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkSwapfileBuffer *swapfile;
	gpsm_item_t *item;

	swapfile = GTK_SWAPFILE_BUFFER(gtk_wave_view_get_buffer(waveview));
	item = gtk_swapfile_buffer_get_item(swapfile);

	if (gpsm_op_redo(item) == -1)
		gnome_dialog_run_and_close(
			GNOME_DIALOG(gnome_error_dialog("Error during redo")));
}





/*
 * Complex stuff like apply, play, record, etc.
 */



/* Helpers for creation of swapfile_in/out nodes with appropriate
 * parameters out of a gpsm swfile. Start and length are positions
 * in the scope of parent (or if NULL swfile). -1 length is whole file. */
static filter_t *create_swapfile_out(gpsm_swfile_t *swfile, int nonlocal,
				     long start, long length)
{
	filter_t *f;
	long swname;

	if (!swfile)
		return NULL;
	if (nonlocal)
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

	return f;
}
static filter_t *create_swapfile_in(gpsm_swfile_t *swfile, int nonlocal,
				    long start, long length)
{
	filter_t *f;
	long swname, swrate;

	if (!swfile)
		return NULL;
	if (nonlocal)
		start = start + gpsm_item_hposition(swfile);
	if (start < 0
	    || (length != -1 && (start + length > gpsm_item_hsize(swfile))))
		return NULL;
	swname = gpsm_swfile_filename(swfile);
	swrate = gpsm_swfile_samplerate(swfile);

	if (!(f = filter_instantiate(plugin_get("swapfile_in"))))
		return NULL;
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "filename"), &swname);
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "rate"), &swrate);
	filterparam_set(filterparamdb_get_param(filter_paramdb(f), "offset"), &start);
	if (length != -1)
		filterparam_set(filterparamdb_get_param(filter_paramdb(f), "size"), &length);

	return f;
}

/* shitty workaround for failing gnome_dialog_run_and_close */
static void setBoolean_cb(GtkWidget *foo, gboolean* bar)
{
	*bar = TRUE;
}

/* Cleanup helpers for waveedit operations.
 */
struct network_run_s {
	/* Cleanup part. */
	filter_t *net;
	gpsm_item_t *item;
	int invalidate;
	/* Timeout (marker/progress bar) part. */
	GtkWaveView *waveview;
	filter_param_t *pos_param;
	long start, end, oldmarker;
	guint tid;
};
static gint network_run_timeout_cb(struct network_run_s *cs)
{
	long pos = filterparam_val_pos(cs->pos_param);
	gtk_wave_view_set_marker_and_scroll(cs->waveview, cs->start + pos);
	return TRUE;
}
static void network_run_cleanup_cb(struct network_run_s *cs)
{
	DPRINTF("starting cleanup\n");
	if (cs->tid != -1) {
		gtk_timeout_remove(cs->tid);
		gtk_wave_view_set_marker(cs->waveview, cs->oldmarker);
	}
	if (cs->net) {
		filter_terminate(cs->net);
		filter_wait(cs->net);
		filter_delete(cs->net);
		DPRINTF("deleted filternetwork\n");
	}
	if (cs->invalidate && cs->item) {
		if (GPSM_ITEM_IS_SWFILE(cs->item)) {
			if (cs->start >= 0)
				gpsm_notify_swapfile_change(gpsm_swfile_filename(cs->item), cs->start, cs->end - cs->start + 1);
			else
				gpsm_invalidate_swapfile(gpsm_swfile_filename(cs->item));
		} else {
			gpsm_item_t *it;
			gpsm_grp_foreach_item(cs->item, it) {
				if (!GPSM_ITEM_IS_SWFILE(it))
					continue;
				if (cs->start >= 0)
					gpsm_notify_swapfile_change(gpsm_swfile_filename(it), cs->start - gpsm_item_hposition(it), cs->end - cs->start + 1);
				else
					gpsm_invalidate_swapfile(gpsm_swfile_filename(it));
			}
		}
		DPRINTF("invalidated swfiles\n");
	} else if (cs->item) {
		gpsm_item_destroy(cs->item);
		DPRINTF("destroyed item\n");
	}
	free(cs);
}
static struct network_run_s *network_run_create(filter_t *net,
						gpsm_item_t *item,
						int invalidate,
						GtkWaveView *waveview,
						filter_param_t *pos,
						long start, long end)
{
	struct network_run_s *cs;
	cs = ALLOC(struct network_run_s);
	cs->net = net;
	cs->item = item;
	cs->invalidate = invalidate;
	cs->waveview = waveview;
	cs->pos_param = pos;
	cs->start = start;
	cs->end = end;
	cs->tid = -1;
	if (waveview && pos) {
		cs->oldmarker = gtk_wave_view_get_marker(waveview);
		cs->tid = gtk_timeout_add(10, (GtkFunction)network_run_timeout_cb, cs);
	}
	return cs;
}


/* 
 * Transforms menu and its callbacks 
 */

static void normalize_cb(GtkWidget *w, plugin_t *plugin);

static GnomeUIInfo transform_menu[] = {
	GNOMEUIINFO_ITEM("Normalize", "normalize", normalize_cb, NULL),
	GNOMEUIINFO_END
};

static void normalize_second_cb(struct network_run_s *cs)
{
	filter_param_t	*param;
	filter_t	*maxrms;
	SAMPLE		gain;
	gpsm_item_t	*grp, *item;
	filter_t	*net, *vadjust, *swout;
	gint32 		start, end;
	GtkWaveView	*waveview;
	gpsm_swfile_t	**files;
	int		i;
	
	DPRINTF("Second step here I come\n");
	
	maxrms = filter_get_node(cs->net, "maxrms");
	param = filterparamdb_get_param(filter_paramdb(maxrms), "maxrms");
	gain = 1.0 / filterparam_val_float(param);
	DPRINTF("gain = %f\n", gain);
	
	if (cs->net) {
		filter_terminate(cs->net);
		filter_wait(cs->net);
		filter_delete(cs->net);
		DPRINTF("deleted filternetwork\n");
	}
	
	grp = cs->item;
	start = cs->start;
	end = cs->end;
	waveview = cs->waveview;

	free(cs);
	
	net = filter_creat(NULL);
	vadjust = filter_instantiate(plugin_get("volume_adjust"));
	param = filterparamdb_get_param(filter_paramdb(vadjust), "gain");
	filterparam_set(param, &gain);
	i=0;
	
	gpsm_grp_foreach_item(grp, item) {
		filter_t *swin, *eff;
		if (!GPSM_ITEM_IS_SWFILE(item))
			continue;
		swin = create_swapfile_in((gpsm_swfile_t *)item, 0,
					  start, end);
		if (!swin)
			goto fail;
		filter_add_node(net, swin, "swin");
		
		swout = create_swapfile_out(files[i], GPSM_ITEM_IS_GRP(item),
					    start, end);
		if (!swout)
			goto fail;
		
		eff = filter_creat(vadjust);
		
		if (!filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), filterportdb_get_port(filter_portdb(eff), PORTNAME_IN)))
			goto fail;
		if (!filterport_connect(filterportdb_get_port(filter_portdb(eff), PORTNAME_OUT), filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
			goto fail;
		i++;
	}

	filter_delete(vadjust);

	/* Prepare for undo. */
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");
	
	/* Run the network through play window */
	glame_gui_play_network(net, NULL, TRUE,
			       (GtkFunction)network_run_cleanup_cb,
			       network_run_create(net, item, TRUE,
						  waveview,
						  filterparamdb_get_param(filter_paramdb(swout), FILTERPARAM_LABEL_POS),
						  start, end),
			       "Normalize", NULL, NULL, 0);

	return;

fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create network")));
	filter_delete(vadjust);
	filter_delete(net);
}

static void normalize_cb(GtkWidget *w, plugin_t *plugin)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start, length;
	gpsm_item_t *item;
	gpsm_grp_t *grp;
	filter_t *net, *mix, *ssp, *maxrms;
	int rate;

	gtk_wave_view_get_selection (waveview, &start, &length);

	if (length <= 0)
		return;
	DPRINTF("Normalizing [%li, +%li]\n", (long)start, (long)length);

	grp = gpsm_flatten(gtk_swapfile_buffer_get_item(swapfile));
	if (!grp)
		return;

	rate = gtk_wave_buffer_get_rate(wavebuffer);

	/* Create the basic network - audio_out. */
	net = filter_creat(NULL);
	mix = filter_instantiate(plugin_get("mix"));
	ssp = filter_instantiate(plugin_get("ssp_streamer"));
	maxrms = filter_instantiate(plugin_get("maxrms"));
	filter_add_node(net, mix, "mix");
	filter_add_node(net, ssp, "ssp_streamer");
	filter_add_node(net, maxrms, "maxrms");
	
	filterport_connect(filterportdb_get_port(filter_portdb(mix), PORTNAME_OUT),
			   filterportdb_get_port(filter_portdb(ssp), PORTNAME_IN));
	filterport_connect(filterportdb_get_port(filter_portdb(ssp), PORTNAME_OUT),
			   filterportdb_get_port(filter_portdb(maxrms), PORTNAME_IN));

	gpsm_grp_foreach_item(grp, item) {
		filter_t *swin;
		if (!GPSM_ITEM_IS_SWFILE(item))
			continue;
		swin = create_swapfile_in((gpsm_swfile_t *)item, 0,
					  start, length);
		if (!swin)
			goto fail;
		filter_add_node(net, swin, "swin");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), filterportdb_get_port(filter_portdb(mix), PORTNAME_IN)))
			goto fail;
	}

	glame_gui_play_network(net, NULL, TRUE,
			       (GtkFunction)normalize_second_cb,
			       network_run_create(net, (gpsm_item_t *)grp, FALSE,
						  waveview,
						  NULL,
						  start, start + length - 1),
			       "Analyze", NULL, NULL, 0);
	return;
fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create network")));
	filter_delete(mix);
	filter_delete(ssp);
	filter_delete(maxrms);
	filter_delete(net);
}

/* Menu event - Apply filter. */
static void apply_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	GtkWidget *prop;
	gboolean ok_pressed;
	gint32 start, length;
	long nrtracks;
	gpsm_swfile_t **files;
	gpsm_item_t *item;
	filter_t *net, *effect, *swout;
	int rate, i;
	
	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Nothing selected")));
		return;
	}
	item = gtk_swapfile_buffer_get_item(swapfile);
	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	rate = gtk_wave_buffer_get_rate(wavebuffer);
	DPRINTF("Applying to [%li, +%li]\n", (long)start, (long)length);

	/* Create one instance of the effect and query the parameters. */
	effect = filter_instantiate(plugin);
	/* Bad and ugly fade hack, but you know there a gimps of audio
	 * and soundforges of linux and they just do want you want */
	if (strcmp(plugin_name(plugin),"fade")==0)
		filterparam_set(filterparamdb_get_param(filter_paramdb(effect), "size"), 
				&length);
	
	if (filterparamdb_nrparams(filter_paramdb(effect)) > 0) {
		prop = glame_gui_filter_properties(filter_paramdb(effect), plugin_name(plugin));
		ok_pressed = FALSE;
		gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(prop)->ok_button),"clicked",setBoolean_cb,&ok_pressed);
		gnome_dialog_run_and_close(GNOME_DIALOG(prop));
		if(!ok_pressed)
			return;
	}

	/* Create the network, add nrtracks instances of swapfile_in -
	 * effect - swapfile_out. */
	net = filter_creat(NULL);
	for (i=0; i<nrtracks; i++) {
		filter_t *swin, *eff;
		swin = create_swapfile_in(files[i], GPSM_ITEM_IS_GRP(item),
					  start, length);
		if (!swin)
			goto fail;
		filter_add_node(net, swin, "swin");
		swout = create_swapfile_out(files[i], GPSM_ITEM_IS_GRP(item),
					    start, length);
		if (!swout)
			goto fail;
		filter_add_node(net, swout, "swout");
		eff = filter_creat(effect);
		if (!eff)
			goto fail;
		filter_add_node(net, eff, "eff");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(eff), PORTNAME_OUT),
					filterportdb_get_port(filter_portdb(swout), PORTNAME_IN))
		    || !filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT),
					   filterportdb_get_port(filter_portdb(eff), PORTNAME_IN)))
			goto fail;
	}
	filter_delete(effect);

	/* Prepare for undo. */
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");
	
	/* Run the network through play window */
	glame_gui_play_network(net, NULL, TRUE,
			       (GtkFunction)network_run_cleanup_cb,
			       network_run_create(net, item, TRUE,
						  waveview,
						  filterparamdb_get_param(filter_paramdb(swout), FILTERPARAM_LABEL_POS),
						  start, start + length -1),
			       "Apply", NULL, NULL, 0);
	return;

 fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create network")));
	filter_delete(effect);
	filter_delete(net);
}

/* Menu event - play actual selection using audio_out. */
static void playselection_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start, length;
	gpsm_item_t *item;
	gpsm_grp_t *grp;
	filter_t *net, *aout;
	int rate;

	if (!plugin_get("audio_out")) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("No audio output support")));
		return;
	}

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;
	DPRINTF("Playing [%li, +%li]\n", (long)start, (long)length);

	grp = gpsm_flatten(gtk_swapfile_buffer_get_item(swapfile));
	if (!grp)
		return;

	rate = gtk_wave_buffer_get_rate(wavebuffer);

	/* Create the basic network - audio_out. */
	net = filter_creat(NULL);
	aout = filter_instantiate(plugin_get("audio_out"));
	filter_add_node(net, aout, "aout");

	gpsm_grp_foreach_item(grp, item) {
		filter_t *swin;
		if (!GPSM_ITEM_IS_SWFILE(item))
			continue;
		swin = create_swapfile_in((gpsm_swfile_t *)item, 0,
					  start, length);
		if (!swin)
			goto fail;
		filter_add_node(net, swin, "swin");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), filterportdb_get_port(filter_portdb(aout), PORTNAME_IN)))
			goto fail;
	}

	glame_gui_play_network(net, NULL, TRUE,
			       (GtkFunction)network_run_cleanup_cb,
			       network_run_create(net, (gpsm_item_t *)grp, FALSE,
						  waveview,
						  filterparamdb_get_param(filter_paramdb(aout), FILTERPARAM_LABEL_POS),
						  start, start + length - 1),
			       "Play", "Pause", "Stop", 0);
	return;

 fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("Cannot play")));
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)grp);
}

static void playall_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gpsm_item_t *item;
	gpsm_grp_t *grp;
	filter_t *net, *aout;
	int rate;

	if (!plugin_get("audio_out")) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("No audio output support")));
		return;
	}

	grp = gpsm_flatten(gtk_swapfile_buffer_get_item(swapfile));
	if (!grp)
		return;

	rate = gtk_wave_buffer_get_rate(wavebuffer);

	/* Create the basic network - audio_out. */
	net = filter_creat(NULL);
	aout = filter_instantiate(plugin_get("audio_out"));
	filter_add_node(net, aout, "aout");

	gpsm_grp_foreach_item(grp, item) {
		filter_t *swin;
		if (!GPSM_ITEM_IS_SWFILE(item))
			continue;
		swin = create_swapfile_in((gpsm_swfile_t *)item, 0, 0, -1);
		if (!swin)
			goto fail;
		filter_add_node(net, swin, "swin");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), filterportdb_get_port(filter_portdb(aout), PORTNAME_IN)))
			goto fail;
	}

	glame_gui_play_network(net, NULL, TRUE,
			       (GtkFunction)network_run_cleanup_cb,
			       network_run_create(net, (gpsm_item_t *)grp, FALSE,
						  waveview,
						  filterparamdb_get_param(filter_paramdb(aout), FILTERPARAM_LABEL_POS),
						  0, gpsm_item_hsize(grp)-1),
			       "Play", "Pause", "Stop", 0);

	return;

 fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("Cannot play")));
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)grp);
}

/* Menu event - record into selection. */
static void recordselection_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start, length;
	gpsm_item_t *grp, *item, *left, *right;
	filter_t *net, *ain;
	int rate;
	filter_t *swout;
	float duration;

	if (!plugin_get("audio_in")) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("No audio input support")));
		return;
	}

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;
	DPRINTF("Recording into [%li, +%li]\n", (long)start, (long)length);

	left = right = NULL;
	grp = gtk_swapfile_buffer_get_item(swapfile);
	if (GPSM_ITEM_IS_SWFILE(grp)) {
		left = grp;
	} else {
		gpsm_grp_foreach_item(grp, item) {
			if (!GPSM_ITEM_IS_SWFILE(item))
				return;
			if (!left)
				left = item;
			else if (!right)
				right = item;
			else {
				gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("GLAME only supports recording of up to two channels")));
				return;
			}
		}
	}
	if (!left && !right)
		return;

	rate = gtk_wave_buffer_get_rate(wavebuffer);

	/* Create the basic network - audio_in. */
	net = filter_creat(NULL);
	ain = filter_instantiate(plugin_get("audio_in"));
	duration = length/(float)rate;
	filterparam_set(filterparamdb_get_param(filter_paramdb(ain), "duration"), &duration);
	filterparam_set(filterparamdb_get_param(filter_paramdb(ain), "rate"), &rate);
	filter_add_node(net, ain, "ain");

	/* Left - or mono. */
	swout = create_swapfile_out((gpsm_swfile_t *)left, GPSM_ITEM_IS_GRP(grp),
				    start, length);
	if (!swout)
		goto fail;
	filter_add_node(net, swout, "swout");
	if (!filterport_connect(filterportdb_get_port(filter_portdb(ain), PORTNAME_OUT),
				filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
		goto fail;

	/* Right. */
	if (right) {
		swout = create_swapfile_out((gpsm_swfile_t *)right, TRUE, start, length);
		if (!swout)
			goto fail;
		filter_add_node(net, swout, "swout");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(ain), PORTNAME_OUT),
					filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
			goto fail;
	}

	/* Prepare for undo. */
	if (gpsm_op_prepare(grp) == -1)
		DPRINTF("Error preparing for undo\n");

	glame_gui_play_network(net, NULL, TRUE,
			       (GtkFunction)network_run_cleanup_cb,
			       network_run_create(net, (gpsm_item_t *)grp, TRUE,
						  NULL, NULL, start, start + length - 1),
			       "Record", "Pause", "Stop", 1);
	return;

 fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("Cannot record")));
	filter_delete(net);
}

/* Menu event - record starting at marker position. */
static void recordmarker_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start;
	gpsm_item_t *grp, *item, *left, *right;
	filter_t *net, *ain;
	int rate;
	filter_t *swout;

	if (!plugin_get("audio_in")) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("No audio input support")));
		return;
	}

	start = gtk_wave_view_get_marker(waveview);
	if (start < 0)
		return;
	DPRINTF("Recording at %li\n", (long)start);

	left = right = NULL;
	grp = gtk_swapfile_buffer_get_item(swapfile);
	if (GPSM_ITEM_IS_SWFILE(grp)) {
		left = grp;
	} else {
		gpsm_grp_foreach_item(grp, item) {
			if (!GPSM_ITEM_IS_SWFILE(item))
				return;
			if (!left)
				left = item;
			else if (!right)
				right = item;
			else {
				gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("GLAME only supports recording of up to two channels")));
				return;
			}
		}
	}
	if (!left && !right)
		return;

	rate = gtk_wave_buffer_get_rate(wavebuffer);

	/* Create the basic network - audio_in. */
	net = filter_creat(NULL);
	ain = filter_instantiate(plugin_get("audio_in"));
	filterparam_set(filterparamdb_get_param(filter_paramdb(ain), "rate"), &rate);
	filter_add_node(net, ain, "ain");

	/* Left - or mono. */
	swout = create_swapfile_out((gpsm_swfile_t *)left, GPSM_ITEM_IS_GRP(grp),
				    start, -1);
	if (!swout)
		goto fail;
	filter_add_node(net, swout, "swout");
	if (!filterport_connect(filterportdb_get_port(filter_portdb(ain), PORTNAME_OUT),
				filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
		goto fail;

	/* Right. */
	if (right) {
		swout = create_swapfile_out((gpsm_swfile_t *)right, TRUE,
					    start, -1);
		if (!swout)
			goto fail;
		filter_add_node(net, swout, "swout");
		if (!filterport_connect(filterportdb_get_port(filter_portdb(ain), PORTNAME_OUT),
					filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
			goto fail;
	}

	/* Prepare for undo. */
	if (gpsm_op_prepare(grp) == -1)
		DPRINTF("Error preparing for undo\n");

	glame_gui_play_network(net, NULL, TRUE,
			       (GtkFunction)network_run_cleanup_cb,
			       network_run_create(net, (gpsm_item_t *)grp, TRUE,
						  NULL, NULL, -1, -1),
			       "Record", "Pause", "Stop", 1);
	return;

 fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("Cannot record")));
	filter_delete(net);
}

static void apply_custom_cb_cleanup(GtkWidget *foo, gpsm_item_t *item)
{
	if (GPSM_ITEM_IS_SWFILE(item))
		gpsm_invalidate_swapfile(gpsm_swfile_filename(item));
	else if (GPSM_ITEM_IS_GRP(item)) {
		gpsm_item_t *it;
		gpsm_grp_foreach_item(item, it) {
			if (!GPSM_ITEM_IS_SWFILE(it))
				continue;
			gpsm_invalidate_swapfile(gpsm_swfile_filename(it));
		}
	}
}
static void apply_custom_cb(GtkWidget * foo, gpointer bar)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	GlameCanvas *canvas;
	gint32 start, length, marker, wavesize;
	long nrtracks;
	gpsm_swfile_t **files;
	gpsm_item_t *item;
	filter_t *net;
	int rate, i;
	float y_position = 20.0;
	char position_buffer[20];

	wavesize = gtk_wave_buffer_get_length(wavebuffer);
	marker = gtk_wave_view_get_marker(waveview);
	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0 && marker < 0)
		return;

	item = gtk_swapfile_buffer_get_item(swapfile);
	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	rate = gtk_wave_buffer_get_rate(wavebuffer);
	DPRINTF("Applying to [%li, +%li]\n", (long)start, (long)length);

	/* Create the network, add nrtracks instances of swapfile_in/out */
	net = filter_creat(NULL);
	for (i=0; i<nrtracks; i++) {
		filter_t *swin, *swout;
		if (length <= 0 && wavesize - marker <= 0)
			goto no_swin;
		swin = create_swapfile_in(files[i], GPSM_ITEM_IS_GRP(item),
					  length > 0 ? start : marker,
					  length > 0 ? length : -1);
		if (!swin)
			goto fail;
		filter_set_property(swin,"immutable","1");
		sprintf(position_buffer,"%8f",20.0);
		filter_set_property(swin,"canvas_x",position_buffer);
		sprintf(position_buffer,"%8f",y_position);
		filter_set_property(swin,"canvas_y",position_buffer);
		filter_add_node(net, swin, "swin");

	no_swin:
		swout = create_swapfile_out(files[i], GPSM_ITEM_IS_GRP(item),
					    length > 0 ? start : marker,
					    length > 0 ? length : -1);
		if (!swout)
			goto fail;
		filter_set_property(swout,"immutable","1");
		sprintf(position_buffer,"%8f",420.0);
		filter_set_property(swout,"canvas_x",position_buffer);
		sprintf(position_buffer,"%8f",y_position);
		filter_set_property(swout,"canvas_y",position_buffer);
		filter_add_node(net, swout, "swout");

		y_position += 100;
	}

	/* Prepare for undo -- NO(!?). FIXME
	if (gpsm_op_prepare(item) == -1)
	DPRINTF("Error preparing for undo\n"); */

	/* Pop up the custom generated canvas - the wave widget is
	 * updated after destruction. FIXME - if gpsm is modified, the
	 * signal handler data is invalid. */
	canvas = glame_filtereditgui_new(net);
	gtk_signal_connect(GTK_OBJECT(canvas), "destroy",
			   (GtkSignalFunc)apply_custom_cb_cleanup, item);
	return;

 fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create network")));
	filter_delete(net);
}

static void wave_help(GtkWidget *foo, void*bar)
{
	gnome_help_goto(NULL,"info:glame#The_Wave_Editor");
}

static void wave_close_cb(GtkWidget *foo, void *bar)
{
	gtk_object_destroy(GTK_OBJECT(actual_waveview));
}


static GnomeUIInfo dummy_menu[] = {
	GNOMEUIINFO_END
};

static GnomeUIInfo rmb_menu[] = {
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Edit", edit_menu),
	GNOMEUIINFO_SUBTREE("View", view_menu),
	GNOMEUIINFO_SUBTREE("Select", select_menu),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Play all", "Plays the whole wave", playall_cb, NULL),
	GNOMEUIINFO_ITEM("Play selection", "Plays the actual selection", playselection_cb, NULL),	
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Record at marker", "Records starting at marker position", recordmarker_cb, NULL),	
	GNOMEUIINFO_ITEM("Record into selection", "Records into the actual selection", recordselection_cb, NULL),	
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Transforms", transform_menu),
	GNOMEUIINFO_SUBTREE("Apply filter", dummy_menu),
	GNOMEUIINFO_ITEM("Apply custom...", "Creates a filternetwork window for applying it to the selection",apply_custom_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Close","Close",wave_close_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Help","help",wave_help,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_END
};
#define RMB_MENU_PLAY_SELECTION_INDEX 6
#define RMB_MENU_RECORD_SELECTION_INDEX 9
#define RMB_MENU_APPLY_FILTER_INDEX 12


/* Somehow only select "effects" (one input, one output) type of
 * filters... */
static int choose_effects(plugin_t *plugin)
{
	filter_t *filter = plugin_query(plugin, PLUGIN_FILTER);
	char *cat;

	/* We need "in" and "out" ports. */
	if (!filterportdb_get_port(filter_portdb(filter), PORTNAME_IN)
	    || !filterportdb_get_port(filter_portdb(filter), PORTNAME_OUT))
		return 0;
	/* We dont like plugin categories "Routing". */
	if (!(cat = plugin_query(plugin, PLUGIN_CATEGORY))
	    || (strcmp(cat, "Routing") == 0))
		return 0;
	/* We dont like plugins "ping". */
	if (strcmp(plugin_name(plugin), "ping") == 0
	    || strcmp(plugin_name(plugin), "fft") == 0
	    || strcmp(plugin_name(plugin), "ifft") == 0)
		return 0;

	return 1;
}

/* Button press event. */
static void waveedit_rmb_cb(GtkWidget *widget, GdkEventButton *event,
			    gpointer user_data) 
{
	GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
	GtkWaveBuffer *wavebuffer;
	GtkSwapfileBuffer *swapfile;
	GtkWidget *menu;
	GtkMenu *filter_menu;
	gpsm_item_t *item;
	gint32 sel_start, sel_length, marker_pos;
	guint32 nrtracks;
  
	if (event->button != 3)
		return;

	/* Get stuff we need for enabling/disabling items. */
	wavebuffer = gtk_wave_view_get_buffer (waveview);
	swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	item = gtk_swapfile_buffer_get_item(swapfile);
	nrtracks = gtk_wave_buffer_get_num_channels(wavebuffer);
	gtk_wave_view_get_selection(waveview, &sel_start, &sel_length);
	marker_pos = gtk_wave_view_get_marker(waveview);

	/* Build the menu, fixup lots of stuff. */
	menu = gnome_popup_menu_new(rmb_menu);
	filter_menu = glame_gui_build_plugin_menu(choose_effects, apply_cb);
	gtk_widget_show(GTK_WIDGET(filter_menu));
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(rmb_menu[RMB_MENU_APPLY_FILTER_INDEX].widget), GTK_WIDGET(filter_menu));
	gtk_widget_set_sensitive(rmb_menu[RMB_MENU_APPLY_FILTER_INDEX].widget,
				 (sel_length > 0) ? TRUE : FALSE);

	gtk_widget_set_sensitive(rmb_menu[RMB_MENU_PLAY_SELECTION_INDEX].widget,
				 (sel_length > 0) ? TRUE : FALSE);
	gtk_widget_set_sensitive(rmb_menu[RMB_MENU_RECORD_SELECTION_INDEX].widget,
				 (sel_length > 0) ? TRUE : FALSE);

	gtk_widget_set_sensitive(edit_menu[EDIT_MENU_CUT_INDEX].widget,
				 (sel_length > 0) ? TRUE : FALSE);
	gtk_widget_set_sensitive(edit_menu[EDIT_MENU_COPY_INDEX].widget,
				 (sel_length > 0) ? TRUE : FALSE);
	gtk_widget_set_sensitive(edit_menu[EDIT_MENU_PASTE_INDEX].widget,
				 clipboard_can_paste(item)
				 && (marker_pos >= 0) ? TRUE : FALSE);
	gtk_widget_set_sensitive(edit_menu[EDIT_MENU_DELETE_INDEX].widget,
				 (sel_length > 0) ? TRUE : FALSE);
	gtk_widget_set_sensitive(edit_menu[EDIT_MENU_UNDO_INDEX].widget,
				 gpsm_op_can_undo(item) ? TRUE : FALSE);
	gtk_widget_set_sensitive(edit_menu[EDIT_MENU_REDO_INDEX].widget,
				 gpsm_op_can_redo(item) ? TRUE : FALSE);

	actual_waveview = waveview;
	gnome_popup_menu_do_popup(menu, NULL, NULL, event, waveview);
}


/* Delete callback - promote to destroy. */
static void waveedit_window_delete_cb(GtkWidget *widget, gpointer data)
{
	gtk_object_destroy(GTK_OBJECT(widget));
}

/* Waveedit GUI cleanup stuff, we need to destroy the created
 * gpsm group of links to the actual swfiles on window destroy. */
static void waveedit_wavebuffer_destroy_cb(GtkWidget *widget,
					   gpsm_item_t *item)
{
	gpsm_item_destroy(item);
}

GtkWidget *glame_waveedit_gui_new(const char *title, gpsm_item_t *item)
{
	GtkWidget *window, *waveview, *toolbar;
	GtkObject *wavebuffer;
	gpsm_grp_t *swfiles;

	/* Create a data source object. We need a gpsm_grp_t for
	 * gtk_swapfile_buffer_new which is "flat", i.e. entirely
	 * consists of gpsm_swfile_t only. The easies way to get
	 * this is to use gpsm_collect_swfiles() - but cleanup is
	 * necessary for this solution. */
	swfiles = gpsm_collect_swfiles(item);
	if (!swfiles)
		return NULL;

	/* Create a Gtk+ window. */
	window = gnome_app_new(title, _(title));
  
	/* Create a GtkWaveView widget. */
	waveview = gtk_wave_view_new ();
	gtk_wave_view_set_select_channels (GTK_WAVE_VIEW (waveview), ~0);
	gtk_widget_set_usize (waveview, 400, 250);

	/* Set the zoom factor such that 1 pixel = 5 frames.
	 * A frame is equal to n samples at one point in time
	 * where n = number of channels. */
	gtk_wave_view_set_zoom (GTK_WAVE_VIEW (waveview), 50);

	/* Set the cache size to hold 8192 pixel columns of data.
	 * This means the user can scroll the widget's contents
	 * back and forth and we will cache the most recently
	 * displayed 8192 columns of data. */
	gtk_wave_view_set_cache_size (GTK_WAVE_VIEW (waveview), 8192);

	/* Create the swapfile buffer. */
	wavebuffer = gtk_swapfile_buffer_new(swfiles);
	if (!wavebuffer) {
		DPRINTF("Unable to create wavebuffer\n");
		gtk_object_destroy(GTK_OBJECT(waveview));
		gtk_object_destroy(GTK_OBJECT(window));
		return NULL;
	}

	/* Add GtkWaveView to the window. */
	gnome_app_set_contents(GNOME_APP(window), waveview);

	/* Set the Waveform widget's data stream to point to our wavebuffer. */
	gtk_wave_view_set_buffer (GTK_WAVE_VIEW (waveview),
				  GTK_WAVE_BUFFER (wavebuffer));

	/* Install the rmb menu callback. */
	gtk_signal_connect(GTK_OBJECT(waveview), "button_press_event",
			   (GtkSignalFunc)waveedit_rmb_cb, NULL);

	/* As we have set up links to the actual swfiles, we dont get
	 * those (links/group) removed under us. But we have to destroy
	 * the linked group afterwards. The only events we expect are
	 * delete and destroy events from the window. */
	gtk_signal_connect(GTK_OBJECT(window), "delete_event",
			   (GtkSignalFunc)waveedit_window_delete_cb, NULL);
	gtk_signal_connect_after(GTK_OBJECT(wavebuffer), "destroy",
				 (GtkSignalFunc)waveedit_wavebuffer_destroy_cb,
				 swfiles);

	/* Add the toolbar. */
	toolbar = gtk_toolbar_new(GTK_ORIENTATION_VERTICAL, GTK_TOOLBAR_ICONS);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Zoom in", "Zoom in", "Zoom in",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_DOWN),
				zoomin_cb, waveview);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Zoom out", "Zoom out", "Zoom out",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_UP),
				zoomout_cb, waveview);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"View all", "View all", "View all",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_REFRESH),
				zoomfull_cb, waveview);
	/* gtk_toolbar_append_space(GTK_TOOLBAR(toolbar)); */
	gnome_app_add_toolbar(GNOME_APP(window), GTK_TOOLBAR(toolbar),
			      "waveedit::toolbar",
			      GNOME_DOCK_ITEM_BEH_EXCLUSIVE|GNOME_DOCK_ITEM_BEH_NEVER_FLOATING,
			      GNOME_DOCK_RIGHT, 0, 0, 0);

	return window;
}
