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

/* Temporary storage for editing functions and the edit functions
 * itself.
 */

static int temp_nrtracks = 0;
static long temp_fname[GTK_SWAPFILE_BUFFER_MAX_TRACKS];
static swfd_t temp_fd[GTK_SWAPFILE_BUFFER_MAX_TRACKS];

static void reset_temp(int nrtracks)
{
	int i;

	/* Kill unnecessary temporary tracks. */
	for (i=nrtracks; i<temp_nrtracks; i++) {
		sw_close(temp_fd[i]);
		sw_unlink(temp_fname[i]);
	}

	/* Generate additionally needed tracks. */
	for (i=temp_nrtracks; i<nrtracks; i++) {
		while ((temp_fd[i] = sw_open((temp_fname[i] = rand()), O_CREAT|O_EXCL|O_RDWR, TXN_NONE)) == -1)
			;
	}
	temp_nrtracks = nrtracks;

	/* Reset temporary tracks. */
	for (i=0; i<temp_nrtracks; i++) {
		sw_ftruncate(temp_fd[i], 0);
		sw_lseek(temp_fd[i], 0, SEEK_SET);
	}
}

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
	gtk_wave_view_set_marker_and_scroll(waveview,
					    gtk_wave_view_get_marker(waveview));
}

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

/* Menu event - Copy. */
static void copy_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;
	long nrtracks;
	gpsm_swfile_t **files;
	gpsm_item_t *item;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Nothing selected")));
		return;
	}

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (GPSM_ITEM_IS_SWFILE(item))
		start -= gpsm_item_hposition(item);

	DPRINTF("Copying selection from %i of length %i\n", start, length);
	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	reset_temp(nrtracks);
	for (i=0; i<nrtracks; i++) {
		fd = sw_open(gpsm_swfile_filename(files[i]), O_RDONLY, TXN_NONE);
		sw_lseek(fd, (start+gpsm_item_hposition(files[i]))*SAMPLE_SIZE, SEEK_SET);
		if (sw_sendfile(temp_fd[i], fd, length*SAMPLE_SIZE, SWSENDFILE_INSERT) == -1)
			DPRINTF("*** sw_sendfile failed\n");
		sw_ftruncate(temp_fd[i], length*SAMPLE_SIZE);
		sw_close(fd);
	}
}

/* Menu event - Paste. */
static void paste_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start;
	struct sw_stat st;
	swfd_t fd;
	long nrtracks;
	gpsm_swfile_t **files;
	gpsm_item_t *item;
	int i;

	start = gtk_wave_view_get_marker (waveview);
	if (start < 0)
		return;

	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	if (nrtracks != temp_nrtracks) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Clipboard has different number of tracks")));
		return;
	}

	if (sw_fstat(temp_fd[0], &st) == -1)
		return;
	DPRINTF("Pasting to %i size %i\n", start, st.size/SAMPLE_SIZE);

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");
	if (GPSM_ITEM_IS_SWFILE(item))
		start -= gpsm_item_hposition(item);

	for (i=0; i<nrtracks; i++) {
		fd = sw_open(gpsm_swfile_filename(files[i]), O_RDWR, TXN_NONE);
		if (sw_lseek(fd, (start+gpsm_item_hposition(files[i]))*SAMPLE_SIZE, SEEK_SET) == -1)
			DPRINTF("*** sw_lseek(swapfile->fd) failed\n");
		if (sw_lseek(temp_fd[i], 0, SEEK_SET) != 0)
			DPRINTF("*** sw_lseek(temp_fd, 0) failed\n");
		if (sw_sendfile(fd, temp_fd[i], st.size, SWSENDFILE_INSERT) == -1)
			DPRINTF("*** sw_sendfile failed\n");
		sw_close(fd);
		gpsm_notify_swapfile_insert(gpsm_swfile_filename(files[i]),
					    start+gpsm_item_hposition(files[i]), st.size/SAMPLE_SIZE);
	}
}

/* Menu event - Cut. */
static void cut_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;
	long nrtracks;
	gpsm_swfile_t **files;
	gpsm_item_t *item;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Nothing selected")));
		return;
	}
	DPRINTF("Cutting selection from %i of length %i\n", start, length);

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");
	if (GPSM_ITEM_IS_SWFILE(item))
		start -= gpsm_item_hposition(item);

	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	reset_temp(nrtracks);
	for (i=0; i<nrtracks; i++) {
		long sstart = start+gpsm_item_hposition(files[i]);
		long ssize = MIN(length, MAX(0, gpsm_item_hsize(files[i])-sstart));
		if (ssize > 0) {
			fd = sw_open(gpsm_swfile_filename(files[i]), O_RDWR, TXN_NONE);
			sw_lseek(fd, sstart*SAMPLE_SIZE, SEEK_SET);
			if (sw_sendfile(temp_fd[i], fd, ssize*SAMPLE_SIZE, SWSENDFILE_CUT|SWSENDFILE_INSERT) == -1)
				DPRINTF("*** sw_sendfile failed\n");
			sw_close(fd);
			gpsm_notify_swapfile_cut(gpsm_swfile_filename(files[i]),
						 sstart, ssize);
		}
		sw_ftruncate(temp_fd[i], length*SAMPLE_SIZE);
	}

	/* Remove the selection. */
	selectnone_cb(bla, waveview);
}

/* Menu event - Delete. */
static void delete_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;
	long nrtracks;
	gpsm_swfile_t **files;
	gpsm_item_t *item;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Nothing selected")));
		return;
	}
	DPRINTF("Deleting selection from %i of length %i\n", start, length);

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (gpsm_op_prepare(item) == -1)
		DPRINTF("Error preparing for undo\n");
	if (GPSM_ITEM_IS_SWFILE(item))
		start -= gpsm_item_hposition(item);

	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	for (i=0; i<nrtracks; i++) {
		long sstart = start+gpsm_item_hposition(files[i]);
		long ssize = MIN(length, MAX(0, gpsm_item_hsize(files[i])-sstart));
		if (ssize > 0) {
			fd = sw_open(gpsm_swfile_filename(files[i]), O_RDWR, TXN_NONE);
			sw_lseek(fd, sstart*SAMPLE_SIZE, SEEK_SET);
			if (sw_sendfile(SW_NOFILE, fd, ssize*SAMPLE_SIZE, SWSENDFILE_CUT) == -1)
				DPRINTF("*** sw_sendfile failed\n");
			sw_close(fd);
			gpsm_notify_swapfile_cut(gpsm_swfile_filename(files[i]),
						 sstart, ssize);
		}
	}

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

	if (!gpsm_op_can_undo(item)) {
		DPRINTF("No undo for %p possible\n", item);
		return;
	}

	if (gpsm_op_undo(item) == -1) {
		DPRINTF("Error during undo for %p\n", item);
		return;
	}
}

/* Menu event - Redo. */
static void redo_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkSwapfileBuffer *swapfile;
	gpsm_item_t *item;

	swapfile = GTK_SWAPFILE_BUFFER(gtk_wave_view_get_buffer(waveview));
	item = gtk_swapfile_buffer_get_item(swapfile);

	if (!gpsm_op_can_redo(item)) {
		DPRINTF("No redo for %p possible\n", item);
		return;
	}

	if (gpsm_op_redo(item) == -1) {
		DPRINTF("Error during redo for %p\n", item);
		return;
	}
}


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

/* GUI is single-threaded, so this should actually work... */
static GtkWaveView *actual_waveview;

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
			gpsm_invalidate_swapfile(gpsm_swfile_filename(cs->item));
		} else {
			gpsm_item_t *it;
			gpsm_grp_foreach_item(cs->item, it) {
				if (!GPSM_ITEM_IS_SWFILE(it))
					continue;
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


/* Menu event - Apply filter. */
static void apply_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
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
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
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
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
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
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
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
						  NULL, NULL, 0, 0),
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
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
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
						  NULL, NULL, 0, 0),
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
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
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
	canvas = draw_network(net);
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

static GnomeUIInfo view_menu[] = {
	GNOMEUIINFO_ITEM("Zoom to selection", "zommsel", zoomsel_cb, NULL),
	GNOMEUIINFO_ITEM("Zoom in", "zommin", zoomin_cb, NULL),
	GNOMEUIINFO_ITEM("Zoom out", "zommout", zoomout_cb, NULL),
	GNOMEUIINFO_ITEM("View all", "zommfull", zoomfull_cb, NULL),
	GNOMEUIINFO_ITEM("Goto marker", "gotomarker", gotomarker_cb, NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo select_menu[] = {
	GNOMEUIINFO_ITEM("Select none", "selectnone", selectnone_cb, NULL),
	GNOMEUIINFO_ITEM("Select all", "selectall", selectall_cb, NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo rmb_menu[] = {
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Cut", "cut", cut_cb, NULL),
	GNOMEUIINFO_ITEM("Copy", "copy", copy_cb, NULL),
	GNOMEUIINFO_ITEM("Paste", "paste", paste_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Undo", "undo", undo_cb, NULL),
	GNOMEUIINFO_ITEM("Redo", "redo", redo_cb, NULL),	
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("View", view_menu),
	GNOMEUIINFO_SUBTREE("Select", select_menu),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Play all", "Plays the whole wave", playall_cb, NULL),
	GNOMEUIINFO_ITEM("Play selection", "Plays the actual selection", playselection_cb, NULL),	
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Record at marker", "Records starting at marker position", recordmarker_cb, NULL),	
	GNOMEUIINFO_ITEM("Record into selection", "Records into the actual selection", recordselection_cb, NULL),	
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Apply filter", NULL),
	GNOMEUIINFO_ITEM("Apply custom...", "Creates a filternetwork window for applying it to the selection",apply_custom_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Close","Close",wave_close_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Help","help",wave_help,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_END
};
#define RMB_MENU_UNDO_INDEX 7
#define RMB_MENU_REDO_INDEX 8
#define RMB_MENU_APPLY_FILTER_INDEX 19


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
static void press (GtkWidget *widget, GdkEventButton *event,
		   gpointer user_data) 
{
	GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
	GtkWaveBuffer *wavebuffer;
	GtkSwapfileBuffer *swapfile;
	GtkWidget *menu;
	GtkMenu *filter_menu;
  
	if (event->button != 3)
		return;

	wavebuffer = gtk_wave_view_get_buffer (waveview);
	if (!GTK_IS_EDITABLE_WAVE_BUFFER (wavebuffer)) {
		DPRINTF("not editable\n");
		return;
	}
	swapfile = GTK_SWAPFILE_BUFFER(wavebuffer);

	/* Build the menu, fixup lots of stuff. */
	menu = gnome_popup_menu_new(rmb_menu);
	filter_menu = glame_gui_build_plugin_menu(choose_effects, apply_cb);
	gtk_widget_show(GTK_WIDGET(filter_menu));
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(rmb_menu[RMB_MENU_APPLY_FILTER_INDEX].widget), GTK_WIDGET(filter_menu));
	if (gpsm_op_can_undo(gtk_swapfile_buffer_get_item(swapfile)))
		gtk_widget_set_sensitive(rmb_menu[RMB_MENU_UNDO_INDEX].widget, TRUE);
	else
		gtk_widget_set_sensitive(rmb_menu[RMB_MENU_UNDO_INDEX].widget, FALSE);
	if (gpsm_op_can_redo(gtk_swapfile_buffer_get_item(swapfile)))
		gtk_widget_set_sensitive(rmb_menu[RMB_MENU_REDO_INDEX].widget, TRUE);
	else
		gtk_widget_set_sensitive(rmb_menu[RMB_MENU_REDO_INDEX].widget, FALSE);

	actual_waveview = waveview;
	gnome_popup_menu_do_popup(menu, NULL, NULL, event, waveview);
}


/* Delete callback. */
static void delete (GtkWidget *widget, gpointer user_data)
{
	gtk_widget_hide(widget);
	gtk_object_destroy(GTK_OBJECT(widget));
}

/* Try to handle group/item deletion/addition events. */
struct weg_compound {
	GtkWidget *widget;
	gpsm_item_t *item;
	glsig_handler_t *handler;
};
static void destroy(GtkWidget *widget, struct weg_compound *weg)
{
	if (weg->handler)
		glsig_delete_handler(weg->handler);
	weg->handler = NULL;
	weg->widget = NULL;
}
static void handle_item(glsig_handler_t *handler, long sig, va_list va)
{
	struct weg_compound *weg = glsig_handler_private(handler);
	if (weg->handler)
		glsig_delete_handler(weg->handler);
	weg->handler = NULL;
	if (weg->widget)
		gtk_object_destroy(GTK_OBJECT(weg->widget));
	weg->widget = NULL;
}
static void destroy_win(GtkWidget *w, GtkObject *win)
{
	gtk_object_destroy(win);
}

GtkWidget *glame_waveedit_gui_new(const char *title, gpsm_item_t *item)
{
	GtkWidget *window, *waveview, *toolbar;
	GtkObject *wavebuffer;
	struct weg_compound *weg;

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

	/* Create a data source object. */
	wavebuffer = gtk_swapfile_buffer_new(item);
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

	/* Call the usual Gtk+ cruft. */
	gtk_signal_connect (GTK_OBJECT (window), "delete_event",
			    (GtkSignalFunc) delete, NULL);
	gtk_signal_connect (GTK_OBJECT (waveview), "button_press_event",
			    (GtkSignalFunc) press, NULL);

	/* Handle GPSM_SIG_ITEM_DESTROY and GPSM_GRP_ADD/REMOVE_ITEM
	 * and close the window in these cases. Also handle window
	 * deletion and remove the signal handler in this case.
	 * FIXME: memleak (weg) */
	weg = malloc(sizeof(struct weg_compound));
	weg->widget = window;
	weg->item = item;
	weg->handler = glsig_add_handler(gpsm_item_emitter(item), GPSM_SIG_ITEM_DESTROY|GPSM_SIG_GRP_NEWITEM|GPSM_SIG_GRP_REMOVEITEM, handle_item, weg);
	gtk_signal_connect(GTK_OBJECT(window), "destroy",
			   (GtkSignalFunc)destroy, weg);
	gtk_signal_connect(GTK_OBJECT(waveview), "destroy",
			   (GtkSignalFunc)destroy_win, window);

	/* Add the toolbar. */
	toolbar = gtk_toolbar_new(GTK_ORIENTATION_VERTICAL, GTK_TOOLBAR_ICONS);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Zoom in", "Zooms in", "Zooms in",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_DOWN),
				zoomin_cb, waveview);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Zoom out", "Zooms out", "Zooms out",
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

void glame_waveedit_cleanup()
{
	reset_temp(0);
}
