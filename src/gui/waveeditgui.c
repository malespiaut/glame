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
#include <guile/gh.h>
#include "libgtkwaveform/gtkwaveview.h"
#include "libgtkwaveform/gtkswapfilebuffer.h"
#include "glame_types.h"
#include "glscript.h"
#include "glplugin.h"
#include "swapfile.h"
#include "glmid.h"
#include "util/glame_gui_utils.h"
#include "waveeditgui.h"
#include "edit_filter/filtereditgui.h"
#include "clipboard.h"
#include "network_utils.h"
#include "glame_accelerator.h"



/* The GUI is single-threaded, so this should actually work for
 * callbacks that cant take the waveview. */
static WaveeditGui *active_waveedit = NULL;



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

	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
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
	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
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

	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
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

	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
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
	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);

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
	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);

	if (gpsm_op_redo(item) == -1)
		gnome_dialog_run_and_close(
			GNOME_DIALOG(gnome_error_dialog("Error during redo")));
}





/*
 * Complex stuff like apply, play, record, etc.
 */

static void playmarker_cb(GtkWidget *bla, GtkWaveView *waveview);
static void playtoolbar_cb(GtkWidget *bla, GtkWaveView *waveview);

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
	WaveeditGui *waveedit;
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
	cs->waveedit->locked = 0;
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
	cs->waveedit = active_waveedit;
	cs->waveedit->locked = 1;
	return cs;
}

/* Menu event - Apply operation. */
static void applyop_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = GTK_WAVE_VIEW(active_waveedit->waveview);
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start, length;
	gpsm_grp_t *grp;
	int (*operation)(gpsm_item_t *, long, long);

	gtk_wave_view_get_selection (waveview, &start, &length);
	grp = gtk_swapfile_buffer_get_item(swapfile);

	if (!(operation = plugin_query(plugin, PLUGIN_GPSMOP))) {
		DPRINTF("No such operation %s\n", plugin_name(plugin));
		return;
	}
	DPRINTF("Executing operation %s on %s [%li, %li[\n",
		plugin_name(plugin), gpsm_item_label(grp),
		(long)start, (long)start+length);

	if (operation((gpsm_item_t *)grp, start, length) == -1)
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Error executing")));

	DPRINTF("%s finished.\n", plugin_name(plugin));
}

/* Menu event - Apply filter. */
int gpsmop_apply_plugin(gpsm_item_t *item, plugin_t *plugin,
			long start, long length);
static void apply_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = GTK_WAVE_VIEW(active_waveedit->waveview);
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gint32 start, length;
	gpsm_item_t *item;
	
	gtk_wave_view_get_selection (waveview, &start, &length);
	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
	gpsmop_apply_plugin(item, plugin, start, length);
}


/* Generic play stuff. */
static void play_cleanup(glsig_handler_t *handler,
			 long sig, va_list va)
{
	WaveeditGui *waveedit;
	waveedit = (WaveeditGui *)glsig_handler_private(handler);
	waveedit->pm_net = NULL;
	waveedit->locked = 0;

	/* restore marker position, if "valid". */
	if (waveedit->pm_marker >= -1)
		gtk_wave_view_set_marker(GTK_WAVE_VIEW(waveedit->waveview),
					 waveedit->pm_marker);

	/* restore normal play button -- wheee, gtk suxx. */
	gtk_widget_destroy(g_list_nth(gtk_container_children(
		GTK_CONTAINER(waveedit->toolbar)), 5)->data);
	gtk_toolbar_insert_item(GTK_TOOLBAR(waveedit->toolbar),
				"Play", "Play", "Play",
				glame_load_icon_widget("play.png",24,24),
				playtoolbar_cb, waveedit->waveview, 7);
}
static void play_update_marker(glsig_handler_t *handler,
			       long sig, va_list va)
{
	WaveeditGui *waveedit;
	long pos;

	waveedit = (WaveeditGui *)glsig_handler_private(handler);
	pos = filterparam_val_pos(waveedit->pm_param);
	gtk_wave_view_set_marker_and_scroll(
		GTK_WAVE_VIEW(waveedit->waveview), waveedit->pm_start + pos);
}
static void play(GtkWaveView *waveview,
		 gint32 start, gint32 end,
		 gboolean restore_marker, gboolean loop)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	gpsm_item_t *item;
	gpsm_grp_t *grp;
	filter_t *net, *aout;
	int rate;
	glsig_emitter_t *emitter;

	DPRINTF("%p - %i, %i, %i, %i\n", waveview, start, end,
		(int)restore_marker, (int)loop);

	/* A simple state machine with two states:
	 * - not playing, a button press will start playing
	 * - playing, a button press will stop playing
	 */

	if (active_waveedit->pm_net) {
		/* Playing state - abort the network.
		 * Cleanup will happen automatically. */

		filter_terminate(active_waveedit->pm_net);
		return;
	}

	/* Not playing state - create the network.
	 */

	if (!plugin_get("audio_out")) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("No audio output support")));
		return;
	}

	active_waveedit->pm_marker = -1000;
	if (restore_marker)
		active_waveedit->pm_marker = gtk_wave_view_get_marker(waveview);
	rate = gtk_wave_buffer_get_rate(wavebuffer);

	grp = gpsm_flatten((gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile));
	if (!grp)
		return;

	/* Create the network. */
	net = filter_creat(NULL);
	gpsm_grp_foreach_item(grp, item) {
		filter_t *swin;
		swin = net_add_gpsm_input(net, (gpsm_swfile_t *)item,
					  start, end - start + 1);
		if (!swin)
			goto fail;
	}
	if (!(aout = net_apply_audio_out(net)))
		goto fail;

	emitter = glame_network_notificator_creat(net);
	glsig_add_handler(emitter, GLSIG_NETWORK_TICK,
			  play_update_marker, active_waveedit);
	glsig_add_handler(emitter, GLSIG_NETWORK_DONE,
			  play_cleanup, active_waveedit);
	glsig_add_handler(emitter, GLSIG_NETWORK_DONE,
			  glame_network_notificator_delete_network, NULL);
	glsig_add_handler(emitter, GLSIG_NETWORK_DONE,
			  glame_network_notificator_destroy_gpsm, grp);
	active_waveedit->pm_net = net;
	active_waveedit->pm_param = filterparamdb_get_param(
		filter_paramdb(aout), FILTERPARAM_LABEL_POS);
	active_waveedit->pm_start = start;
	if (glame_network_notificator_run(emitter, 10) == -1) {
		active_waveedit->pm_net = NULL;
		glame_network_error_dialog(net, "Cannot play wave");
		filter_delete(net);
		gpsm_item_destroy((gpsm_item_t *)grp);
		return;
	}

	/* exchange play for stop button -- wheee, gtk suxx. */
	gtk_widget_destroy(g_list_nth(gtk_container_children(
		GTK_CONTAINER(active_waveedit->toolbar)), 5)->data);
	gtk_toolbar_insert_item(GTK_TOOLBAR(active_waveedit->toolbar),
				"Stop", "Stop", "Stop",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_STOP),
				playtoolbar_cb, active_waveedit->waveview, 7);
	active_waveedit->locked = 1;

	return;

 fail:
	glame_network_error_dialog(net, "Cannot play wave");
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)grp);
}


static void playtoolbar_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	gint32 start, end;

	/* Choose between playing the actual selection and from
	 * the current marker position (where the first one restores
	 * marker position after play). */
	gtk_wave_view_get_selection(waveview, &start, &end);
	end = start + end;
	if (end - start <= 0) {
		start = MAX(0, gtk_wave_view_get_marker (waveview));
		end = gtk_wave_buffer_get_length(wavebuffer);
		play(waveview, start, end, FALSE, FALSE);
	} else
		play(waveview, start, end, TRUE, FALSE);
}

static void playmarker_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	gint32 start, end;

	/* Play from marker to end, dont restore marker position. */
	start = MAX(0, gtk_wave_view_get_marker (waveview));
	end = gtk_wave_buffer_get_length(wavebuffer) - start - 1;
	play(waveview, start, end, FALSE, FALSE);
}

static void playselection_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	gint32 start, end;

	/* Play the current selection, restore marker at stop. */
	gtk_wave_view_get_selection(waveview, &start, &end);
	end = start + end;
	if (end - start <= 0)
		return;
	play(waveview, start, end, TRUE, FALSE);
}

static void playall_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	gint32 start, end;

	/* Play whole track, restore marker at stop. */
	start = 0;
	end = gtk_wave_buffer_get_length(wavebuffer);
	play(waveview, start, end, TRUE, FALSE);
}



/* Menu event - record into selection. */
static void recordselection_cb(GtkWidget *bla, GtkWaveView *waveview)
{
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
	grp = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
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
	swout = net_add_gpsm_output(net, (gpsm_swfile_t *)left, start, length, 0);
	if (!swout)
		goto fail;
	if (!filterport_connect(filterportdb_get_port(filter_portdb(ain), PORTNAME_OUT),
				filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
		goto fail;

	/* Right. */
	if (right) {
		swout = net_add_gpsm_output(net, (gpsm_swfile_t *)right, start, length, 0);
		if (!swout)
			goto fail;
		if (!filterport_connect(filterportdb_get_port(filter_portdb(ain), PORTNAME_OUT),
					filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
			goto fail;
	}

	/* Prepare for undo. */
	if (gpsm_op_prepare(grp) == -1)
		DPRINTF("Error preparing for undo\n");

	glame_gui_play_network(net, FALSE /* FIXME TRUE */,
			       (GtkFunction)network_run_cleanup_cb,
			       network_run_create(net, (gpsm_item_t *)grp, TRUE,
						  NULL, NULL, start, start + length - 1),
			       "Record", "Pause", "Stop", 1);
	return;

 fail:
	glame_network_error_dialog(net, "Cannot record");
	filter_delete(net);
}

/* Menu event - record starting at marker position. */
static void recordmarker_cb(GtkWidget *bla, GtkWaveView *waveview)
{
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

	start = MAX(0, gtk_wave_view_get_marker(waveview));
	DPRINTF("Recording at %li\n", (long)start);

	left = right = NULL;
	grp = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
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
	if (!left && !right)
		return;

	rate = gtk_wave_buffer_get_rate(wavebuffer);

	/* Create the basic network - audio_in. */
	net = filter_creat(NULL);
	ain = filter_instantiate(plugin_get("audio_in"));
	filterparam_set(filterparamdb_get_param(filter_paramdb(ain), "rate"), &rate);
	filter_add_node(net, ain, "ain");

	/* Left - or mono. */
	swout = net_add_gpsm_output(net, (gpsm_swfile_t *)left, start, -1, 0);
	if (!swout)
		goto fail;
	if (!filterport_connect(filterportdb_get_port(filter_portdb(ain), PORTNAME_OUT),
				filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
		goto fail;

	/* Right. */
	if (right) {
		swout = net_add_gpsm_output(net, (gpsm_swfile_t *)right, start, -1, 0);
		if (!swout)
			goto fail;
		if (!filterport_connect(filterportdb_get_port(filter_portdb(ain), PORTNAME_OUT),
					filterportdb_get_port(filter_portdb(swout), PORTNAME_IN)))
			goto fail;
	}

	/* Prepare for undo. */
	if (gpsm_op_prepare(grp) == -1)
		DPRINTF("Error preparing for undo\n");

	glame_gui_play_network(net, FALSE /* FIXME TRUE */,
			       (GtkFunction)network_run_cleanup_cb,
			       network_run_create(net, (gpsm_item_t *)grp, TRUE,
						  NULL, NULL, -1, -1),
			       "Record", "Pause", "Stop", 1);
	return;

 fail:
	glame_network_error_dialog(net, "Cannot record");
	filter_delete(net);
}

static void apply_custom_cb(GtkWidget * foo, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer(waveview);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	GtkWidget *canvas;
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

	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	rate = gtk_wave_buffer_get_rate(wavebuffer);
	DPRINTF("Applying to [%li, +%li]\n", (long)start, (long)length);

	/* Create the network, add nrtracks instances of swapfile_in/out */
	net = filter_creat(NULL);
	for (i=0; i<nrtracks; i++) {
		filter_t *swin, *swout;
		if (length <= 0 && wavesize - marker <= 0)
			goto no_swin;
		swin = net_add_gpsm_input(net, files[i],
					  length > 0 ? start : marker,
					  length > 0 ? length : -1);
		if (!swin)
			goto fail;
		filter_set_property(swin,"immutable","1");
		sprintf(position_buffer,"%8f",20.0);
		filter_set_property(swin,"canvas_x",position_buffer);
		sprintf(position_buffer,"%8f",y_position);
		filter_set_property(swin,"canvas_y",position_buffer);

	no_swin:
		swout = net_add_gpsm_output(net, files[i],
					    length > 0 ? start : marker,
					    length > 0 ? length : -1,
					    0);
		if (!swout)
			goto fail;
		filter_set_property(swout,"immutable","1");
		sprintf(position_buffer,"%8f",420.0);
		filter_set_property(swout,"canvas_x",position_buffer);
		sprintf(position_buffer,"%8f",y_position);
		filter_set_property(swout,"canvas_y",position_buffer);

		y_position += 100;
	}

	/* Prepare for undo -- NO(!?). FIXME -- Yes, still FIXME. */
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");

	/* Pop up the custom generated canvas - the wave widget is
	 * updated by filtereditgui, if changed. */
	canvas = glame_filtereditgui_new(net, FALSE);
	return;

 fail:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create network")));
	filter_delete(net);
}

static void wave_help_cb(GtkWidget *foo, void*bar)
{
	gnome_help_goto(NULL,"info:glame#The_Wave_Editor");
}

static void wave_close_cb(GtkWidget *foo, GtkObject *window)
{
	if (active_waveedit && !active_waveedit->locked)
		gtk_object_destroy(GTK_OBJECT(active_waveedit));
}


static GnomeUIInfo dummy1_menu[] = {
	GNOMEUIINFO_END
};
static GnomeUIInfo dummy2_menu[] = {
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
	GNOMEUIINFO_ITEM("Play from marker", "Plays from marker position", playmarker_cb, NULL),	
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Record at marker", "Records starting at marker position", recordmarker_cb, NULL),	
	GNOMEUIINFO_ITEM("Record into selection", "Records into the actual selection", recordselection_cb, NULL),	
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Apply operation", dummy1_menu),
	GNOMEUIINFO_SUBTREE("Apply filter", dummy2_menu),
	GNOMEUIINFO_ITEM("Apply custom...", "Creates a filternetwork window for applying it to the selection", apply_custom_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_END
};
#define RMB_MENU_PLAY_SELECTION_INDEX 6
#define RMB_MENU_RECORD_SELECTION_INDEX 10
#define RMB_MENU_APPLY_OP_INDEX 12
#define RMB_MENU_APPLY_FILTER_INDEX 13


/* Somehow only select "effects" (one input, one output) type of
 * filters... */
static int choose_effects(plugin_t *plugin)
{
	filter_t *filter;
	filter_port_t *port;
	char *cat;
	int in = 0, out = 0;

	/* Only use filters. */
	if (!(filter = plugin_query(plugin, PLUGIN_FILTER)))
		return 0;

	/* We need input and output port(s) of the same count, if
         * greater than one it needs to match the channel count. */
	filterportdb_foreach_port(filter_portdb(filter), port) {
		if (!FILTER_PORTS_ARE_COMPATIBLE(filterport_type(port), FILTER_PORTTYPE_SAMPLE))
			return 0;
		if (filterport_is_input(port))
			in++;
		if (filterport_is_output(port))
			out++;
	}
	if (in != 1 || out != 1)
		return 0;
#if 0 /* we dont handle complex stuff in apply yet. */
	if (in == 0 || out == 0 || in != out
	    || (in > 1 && in != gtk_wave_buffer_get_num_channels(gtk_wave_view_get_buffer(GTK_WAVE_VIEW(active_waveedit->waveview)))))
		return 0;
#endif

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

/* Somehow only select "operations" */
static int choose_ops(plugin_t *plugin)
{
	/* Only use filters. */
	if (!plugin_query(plugin, PLUGIN_GPSMOP)
	    || strcmp(plugin_name(plugin), "import") == 0)
		return 0;

	return 1;
}

static GtkWidget *waveedit_build_menu(GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer;
	GtkSwapfileBuffer *swapfile;
	GtkWidget *menu;
	GtkMenu *filter_menu, *op_menu;
	gpsm_item_t *item;
	gint32 sel_start, sel_length, marker_pos;
	guint32 nrtracks;

	/* Get stuff we need for enabling/disabling items. */
	wavebuffer = gtk_wave_view_get_buffer (waveview);
	swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);
	item = (gpsm_item_t *)gtk_swapfile_buffer_get_item(swapfile);
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

	op_menu = glame_gui_build_plugin_menu(choose_ops, applyop_cb);
	gtk_widget_show(GTK_WIDGET(op_menu));
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(rmb_menu[RMB_MENU_APPLY_OP_INDEX].widget), GTK_WIDGET(op_menu));

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

	return menu;
}

/* Button press event. */
static void waveedit_rmb_cb(GtkWidget *widget, GdkEventButton *event,
			    gpointer user_data) 
{
	GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
	GtkWidget *menu;
  
	if (event->button != 3 || active_waveedit->locked)
		return;

	menu = waveedit_build_menu(waveview);
	gnome_popup_menu_do_popup(menu, NULL, NULL, event, waveview);
}

/* Button press event. */
static void waveedit_rmb_cb2(GtkWidget *widget, GtkWaveView *waveview)
{
	GtkWidget *menu;
  
	if (active_waveedit->locked)
		return;

	menu = waveedit_build_menu(waveview);
	gnome_popup_menu_do_popup(menu, NULL, NULL, NULL, waveview);
}

static void handle_enter(GtkWidget *tree, GdkEventCrossing *event,
			 WaveeditGui *waveedit)
{
	if (event->type == GDK_ENTER_NOTIFY)
		active_waveedit = waveedit;
}

static gint event_block(GtkObject *object, GdkEventAny *event, gpointer data)
{
	return TRUE;
}


/*
 * Global API and scripting.
 */

static SCM gls_waveedit_get_marker()
{
	if (!active_waveedit)
		return SCM_UNSPECIFIED;
	return gh_long2scm(gtk_wave_view_get_marker(
		GTK_WAVE_VIEW(active_waveedit->waveview)));
}

static SCM gls_waveedit_set_marker(SCM s_pos)
{
	SCM_ASSERT(gh_exact_p(s_pos), s_pos, SCM_ARG1, "waveedit-set-marker!");
	if (!active_waveedit)
		return SCM_UNSPECIFIED;
	gtk_wave_view_set_marker(GTK_WAVE_VIEW(active_waveedit->waveview),
				 gh_scm2long(s_pos));
	return SCM_UNSPECIFIED;
}

static SCM gls_waveedit_get_selection()
{
	gint32 start, length;
	if (!active_waveedit)
		return SCM_UNSPECIFIED;
	gtk_wave_view_get_selection(GTK_WAVE_VIEW(active_waveedit->waveview),
				    &start, &length);
	return gh_cons(gh_long2scm(start), gh_long2scm(length));
}

static SCM gls_waveedit_set_selection(SCM s_selection)
{
	SCM_ASSERT(gh_pair_p(s_selection)
		   && gh_exact_p(gh_car(s_selection))
		   && gh_exact_p(gh_cdr(s_selection)), s_selection,
		   SCM_ARG1, "waveedit-set-selection!");
	if (!active_waveedit)
		return SCM_UNSPECIFIED;
	gtk_wave_view_set_selection(GTK_WAVE_VIEW(active_waveedit->waveview),
				    gh_scm2long(gh_car(s_selection)),
				    gh_scm2long(gh_cdr(s_selection)));
	return SCM_UNSPECIFIED;
}

static SCM gls_waveedit_get_zoom()
{
	if (!active_waveedit)
		return SCM_UNSPECIFIED;
	return gh_double2scm(gtk_wave_view_get_zoom(
		GTK_WAVE_VIEW(active_waveedit->waveview)));
}

static SCM gls_waveedit_set_zoom(SCM s_zoom)
{
	SCM_ASSERT(gh_number_p(s_zoom), s_zoom,
		   SCM_ARG1, "waveedit-set-zoom!");
	if (!active_waveedit)
		return SCM_UNSPECIFIED;
	gtk_wave_view_set_zoom(GTK_WAVE_VIEW(active_waveedit->waveview),
			       gh_scm2double(s_zoom));
	return SCM_UNSPECIFIED;
}

static SCM gls_waveedit_get_scroll()
{
	GtkAdjustment *adjustment;
	SCM s_res = SCM_LIST0;
	if (!active_waveedit)
		return SCM_UNSPECIFIED;
	adjustment = GTK_ADJUSTMENT(
		GTK_WAVE_VIEW(active_waveedit->waveview)->adjust);
	s_res = gh_cons(gh_long2scm((long)adjustment->lower), s_res);
	s_res = gh_cons(gh_long2scm((long)adjustment->upper), s_res);
	s_res = gh_cons(gh_long2scm((long)adjustment->value), s_res);
	s_res = gh_cons(gh_long2scm((long)adjustment->step_increment), s_res);
	s_res = gh_cons(gh_long2scm((long)adjustment->page_increment), s_res);
	s_res = gh_cons(gh_long2scm((long)adjustment->page_size), s_res);
	return s_res;
}

static SCM gls_waveedit_set_scroll_position(SCM s_pos)
{
	SCM_ASSERT(gh_exact_p(s_pos), s_pos,
		   SCM_ARG1, "waveedit-set-scroll-position!");
	if (!active_waveedit)
		return SCM_UNSPECIFIED;
	gtk_adjustment_set_value(GTK_ADJUSTMENT(
		GTK_WAVE_VIEW(active_waveedit->waveview)->adjust),
				 gh_scm2long(s_pos));
	return SCM_UNSPECIFIED;
}

static SCM gls_waveedit_new(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item, SCM_ARG1, "waveedit-new");
	item = scm2gpsmitem(s_item);
	gtk_widget_show_all(GTK_WIDGET(
		glame_waveedit_gui_new(gpsm_item_label(item), item)));

	return SCM_UNSPECIFIED;
}

static SCM gls_waveedit_gpsm_grp()
{
	if (active_waveedit)
		return gpsmitem2scm(active_waveedit->swfiles);
	return SCM_BOOL_F;
}

static SCM gls_waveedit_play(SCM s_start, SCM s_end, SCM s_restore, SCM s_loop)
{
	SCM_ASSERT(gh_exact_p(s_start), s_start, SCM_ARG1, "waveedit-play");
	SCM_ASSERT(gh_exact_p(s_end), s_end, SCM_ARG2, "waveedit-play");
	SCM_ASSERT(gh_boolean_p(s_restore), s_restore,
		   SCM_ARG3, "waveedit-play");
	SCM_ASSERT(gh_boolean_p(s_loop), s_loop,
		   SCM_ARG4, "waveedit-play");
	if (active_waveedit)
		play(GTK_WAVE_VIEW(active_waveedit->waveview),
		     gh_scm2long(s_start), gh_scm2long(s_end),
		     gh_scm2bool(s_restore), gh_scm2bool(s_loop));
	return SCM_UNSPECIFIED;
}

void glame_waveeditgui_init()
{
	gh_new_procedure1_0("waveedit-new",
			    gls_waveedit_new);
	gh_new_procedure0_0("waveedit-gpsm-grp",
			    gls_waveedit_gpsm_grp);
	gh_new_procedure0_0("waveedit-get-marker",
			    gls_waveedit_get_marker);
	gh_new_procedure1_0("waveedit-set-marker!",
			    gls_waveedit_set_marker);
	gh_new_procedure0_0("waveedit-get-selection",
			    gls_waveedit_get_selection);
	gh_new_procedure1_0("waveedit-set-selection!",
			    gls_waveedit_set_selection);
	gh_new_procedure0_0("waveedit-get-zoom",
			    gls_waveedit_get_zoom);
	gh_new_procedure1_0("waveedit-set-zoom!",
			    gls_waveedit_set_zoom);
	gh_new_procedure0_0("waveedit-get-scroll",
			    gls_waveedit_get_scroll);
	gh_new_procedure1_0("waveedit-set-scroll-position!",
			    gls_waveedit_set_scroll_position);
	gh_new_procedure4_0("waveedit-play", gls_waveedit_play);
}

static void waveedit_gui_destroy(GtkObject *waveedit)
{
	GnomeAppClass* parent_class;
	parent_class = gtk_type_class(gnome_app_get_type());
	GTK_OBJECT_CLASS(parent_class)->destroy(waveedit);
	if (WAVEEDIT_GUI(waveedit)->swfiles)
		gpsm_item_destroy((gpsm_item_t *)WAVEEDIT_GUI(waveedit)->swfiles);
}

static void waveedit_gui_class_init(WaveeditGuiClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = waveedit_gui_destroy;
}

static void waveedit_gui_init(WaveeditGui *waveedit)
{
	waveedit->root = NULL;
	waveedit->swfiles = NULL;
	waveedit->waveview = NULL;
	waveedit->wavebuffer = NULL;
	waveedit->toolbar = NULL;
	waveedit->locked = 0;
	waveedit->pm_net = NULL;
	waveedit->pm_param = NULL;
	waveedit->pm_start = 0;
}

GtkType waveedit_gui_get_type(void)
{
	static GtkType waveedit_gui_type = 0;
	
	if (!waveedit_gui_type){
		GtkTypeInfo waveedit_gui_info = {
			"WaveeditGui",
			sizeof(WaveeditGui),
			sizeof(WaveeditGuiClass),
			(GtkClassInitFunc)waveedit_gui_class_init,
			(GtkObjectInitFunc)waveedit_gui_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		waveedit_gui_type = gtk_type_unique(
			gnome_app_get_type(), &waveedit_gui_info);
		gtk_type_set_chunk_alloc(waveedit_gui_type, 8);
	}

	return waveedit_gui_type;
}

WaveeditGui *glame_waveedit_gui_new(const char *title, gpsm_item_t *item)
{
	WaveeditGui *window;
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
	window = WAVEEDIT_GUI(gtk_type_new(waveedit_gui_get_type()));
	gnome_app_construct(GNOME_APP(window), "glame0.5", _(title));
	window->root = item;
	window->swfiles = swfiles;

	/* Create a GtkWaveView widget. */
	window->waveview = gtk_wave_view_new ();
	gtk_wave_view_set_select_channels (GTK_WAVE_VIEW(window->waveview), ~0);
	gtk_widget_set_usize(window->waveview, 400, 250);

	/* Set the zoom factor such that 1 pixel = 5 frames.
	 * A frame is equal to n samples at one point in time
	 * where n = number of channels. */
	gtk_wave_view_set_zoom (GTK_WAVE_VIEW(window->waveview), 50);
	gtk_wave_view_set_marker_scrolling_boundaries(GTK_WAVE_VIEW(window->waveview), 0.5, 0.5);
	/* Set the cache size to hold 8192 pixel columns of data.
	 * This means the user can scroll the widget's contents
	 * back and forth and we will cache the most recently
	 * displayed 8192 columns of data. */
	gtk_wave_view_set_cache_size (GTK_WAVE_VIEW(window->waveview), 8192);

	/* Create the swapfile buffer. */
	window->wavebuffer = GTK_SWAPFILE_BUFFER(gtk_swapfile_buffer_new(window->swfiles));
	if (!window->wavebuffer) {
		DPRINTF("Unable to create wavebuffer\n");
		gtk_object_destroy(GTK_OBJECT(window->waveview));
		gtk_object_destroy(GTK_OBJECT(window));
		return NULL;
	}

	/* Add GtkWaveView to the window. */
	{
		GtkWidget *vbox;
		vbox = gtk_hbox_new(TRUE, 5);
		gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(window->waveview),
				   TRUE, TRUE, 5);
		gtk_widget_show(window->waveview);
		gtk_widget_show(vbox);
		gnome_app_set_contents(GNOME_APP(window), vbox);
	}

	/* Set the Waveform widget's data stream to point to our wavebuffer. */
	gtk_wave_view_set_buffer (GTK_WAVE_VIEW(window->waveview),
				  GTK_WAVE_BUFFER(window->wavebuffer));


	/* Add the toolbar. */
	window->toolbar = gtk_toolbar_new(GTK_ORIENTATION_VERTICAL,
					  GTK_TOOLBAR_ICONS);
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"Zoom in", "Zoom in", "Zoom in",
				glame_load_icon_widget("zoom_in.png",24,24),
				zoomin_cb, window->waveview);
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"Zoom out", "Zoom out", "Zoom out",
				glame_load_icon_widget("zoom_out.png",24,24),
				zoomout_cb, window->waveview);
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"View all", "View all", "View all",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_REFRESH),
				zoomfull_cb, window->waveview);
	gtk_toolbar_append_space(GTK_TOOLBAR(window->toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"Select all", "Select all", "Select all",
				glame_load_icon_widget("select_all.png",24,24),
				selectall_cb, window->waveview);
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"Select none", "Select none", "Select none",
				glame_load_icon_widget("select_none.png",24,24),
				selectnone_cb, window->waveview);
	/* Play button that should change to Stop if pressed, different
	 * callback than "Play all"/"Play selection" - play from marker.
	 * FIXME. */
	gtk_toolbar_append_space(GTK_TOOLBAR(window->toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"Play", "Play", "Play",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_FORWARD),
				playtoolbar_cb, window->waveview);
	gtk_toolbar_append_space(GTK_TOOLBAR(window->toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"Menu", "Menu", "Menu",
				glame_load_icon_widget("dots.png",24,24),
				waveedit_rmb_cb2, window->waveview);
	/* Keep last. */
	gtk_toolbar_append_space(GTK_TOOLBAR(window->toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"Close", "Close", "Close",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CLOSE),
				wave_close_cb, window);
	gtk_toolbar_append_item(GTK_TOOLBAR(window->toolbar),
				"Help", "Help", "Help",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_HELP),
				wave_help_cb, window->waveview);
	gnome_app_add_toolbar(GNOME_APP(window), GTK_TOOLBAR(window->toolbar),
			      "waveedit::toolbar",
			      GNOME_DOCK_ITEM_BEH_EXCLUSIVE|GNOME_DOCK_ITEM_BEH_NEVER_FLOATING,
			      GNOME_DOCK_TOP, 0, 0, 0);


	/* Install the rmb menu and enter/leave callbacks. */
	gtk_signal_connect(GTK_OBJECT(window->waveview), "button_press_event",
			   (GtkSignalFunc)waveedit_rmb_cb, NULL);
	gtk_signal_connect(GTK_OBJECT(window), "enter_notify_event",
			   (GtkSignalFunc)handle_enter, window);
	gtk_signal_connect(GTK_OBJECT(window), "delete_event",
			   (GtkSignalFunc)event_block, NULL);

	/* Add accelerator handler. */
	glame_accel_install(GTK_WIDGET(window), "waveview", NULL);

	return window;
}
