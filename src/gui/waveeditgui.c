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
	if (length <= 0)
		return;

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
		DPRINTF("clipboard nrtracks do not match\n");
		return;
	}

	if (sw_fstat(temp_fd[0], &st) == -1)
		return;
	DPRINTF("Pasting to %i size %i\n", start, st.size/SAMPLE_SIZE);

	item = gtk_swapfile_buffer_get_item(swapfile);
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
	if (length <= 0)
		return;
	DPRINTF("Cutting selection from %i of length %i\n", start, length);

	item = gtk_swapfile_buffer_get_item(swapfile);
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
	if (length <= 0)
		return;
	DPRINTF("Deleting selection from %i of length %i\n", start, length);

	item = gtk_swapfile_buffer_get_item(swapfile);
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
}

/* shitty workaround for failing gnome_dialog_run_and_close */
static void setBoolean_cb(GtkWidget *foo, gboolean* bar)
{
	*bar = TRUE;
}

/* GUI is single-threaded, so this should actually work... */
static GtkWaveView *actual_waveview;

/* Menu event - Apply filter. */
static void apply_cb(GtkWidget *bla, plugin_t *plugin)
{

	/* FIXME FIXME FIXME 
	   enable cancel button.
	   fix richis bogus loop ;) 
	   well. cancel is fine,but that's about it :-( 	*/
	   
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
	filter_t *net, *effect;
	int rate, i;
	
	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;
	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	rate = gtk_wave_buffer_get_rate(wavebuffer);
	DPRINTF("Applying to [%li, +%li]\n", (long)start, (long)length);

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (GPSM_ITEM_IS_SWFILE(item))
		start -= gpsm_item_hposition(item);

	/* Create one instance of the effect and query the parameters. */
	effect = filter_instantiate(plugin);
	prop = glame_gui_filter_properties(filter_paramdb(effect), plugin_name(plugin));
	ok_pressed = FALSE;
	gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(prop)->ok_button),"clicked",setBoolean_cb,&ok_pressed);
	gnome_dialog_run_and_close(GNOME_DIALOG(prop));
	
	if(!ok_pressed)
		return;

	/* Create the network, add nrtracks instances of swapfile_in -
	 * effect - swapfile_out. */
	net = filter_creat(NULL);
	for (i=0; i<nrtracks; i++) {
		filter_t *swin, *eff, *swout;
		int swname = gpsm_swfile_filename(files[i]);
		long sstart = start+gpsm_item_hposition(files[i]);
		swin = filter_instantiate(plugin_get("swapfile_in"));
		eff = filter_creat(effect);
		swout = filter_instantiate(plugin_get("swapfile_out"));
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "rate"), &rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "offset"), &sstart);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "size"), &length);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swout), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swout), "offset"), &start);
		filter_add_node(net, swin, "swin");
		filter_add_node(net, eff, "eff");
		filter_add_node(net, swout, "swout");
		filterport_connect(filterportdb_get_port(filter_portdb(eff), PORTNAME_OUT), filterportdb_get_port(filter_portdb(swout), PORTNAME_IN));
		filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), filterportdb_get_port(filter_portdb(eff), PORTNAME_IN));
	}
	
	/* Run the network through play window */
	//	glame_gui_play_network(net,NULL);
	glame_gui_play_network(net,NULL);
	/* Run the network and cleanup after it. */
/*  	filter_launch(net); */
/* 	filter_start(net); */
/* 	filter_wait(net); */
/* 	filter_delete(net); */
/* 	filter_delete(effect); */

	for (i=0; i<nrtracks; i++) {
		glsig_emit(gpsm_item_emitter(files[i]), GPSM_SIG_SWFILE_CHANGED, files[i], start, length);
		glsig_emit(gpsm_item_emitter(files[i]), GPSM_SIG_ITEM_CHANGED, files[i]);
	}
}


/* Menu event - feed into filter. */
static void feed_cb(GtkWidget *bla, plugin_t *plugin)
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
	filter_t *net, *effect;
	int rate, i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	rate = gtk_wave_buffer_get_rate(wavebuffer);
	DPRINTF("Applying to [%li, +%li]\n", (long)start, (long)length);

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (GPSM_ITEM_IS_SWFILE(item))
		start -= gpsm_item_hposition(item);

	/* Create one instance of the effect and query the parameters. */
	effect = filter_instantiate(plugin);
	prop = glame_gui_filter_properties(filter_paramdb(effect), plugin_name(plugin));
	ok_pressed = FALSE;
	gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(prop)->ok_button),"clicked",setBoolean_cb,&ok_pressed);
	gnome_dialog_run_and_close(GNOME_DIALOG(prop));
	
	if(!ok_pressed)
		return;
	/* Create the network, add nrtracks instances of swapfile_in -
	 * effect - swapfile_out. */
	net = filter_creat(NULL);

	for (i=0; i<nrtracks; i++) {
		filter_t *swin, *eff;
		int swname = gpsm_swfile_filename(files[i]);
		long sstart = start+gpsm_item_hposition(files[i]);
		swin = filter_instantiate(plugin_get("swapfile_in"));
		eff = filter_creat(effect);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "rate"), &rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "offset"), &sstart);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "size"), &length);
		filter_add_node(net, swin, "swin");
		filter_add_node(net, eff, "eff");
		filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), filterportdb_get_port(filter_portdb(eff), PORTNAME_IN));
	}
	glame_gui_play_network(net,NULL);
}

static void feed_custom_cb(GtkWidget * foo, gpointer bar)
{
	
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	long nrtracks;
	gpsm_swfile_t **files;
	gpsm_item_t *item;
	filter_t *net;
	int rate, i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	rate = gtk_wave_buffer_get_rate(wavebuffer);

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (GPSM_ITEM_IS_SWFILE(item))
		start -= gpsm_item_hposition(item);

	net = filter_creat(NULL);
	for(i=0;i<nrtracks;i++){
		filter_t *swin;
		int swname = gpsm_swfile_filename(files[i]);
		long sstart = start+gpsm_item_hposition(files[i]);
		swin = filter_instantiate(plugin_get("swapfile_in"));
		fprintf(stderr,"rate: %d\n",rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "rate"), &rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "offset"), &sstart);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "size"), &length);
		filter_add_node(net, swin, "swin");
		filter_set_property(swin,"immutable","1");
				
	}

	draw_network(net);
}

static void apply_custom_cb(GtkWidget * foo, gpointer bar)
{
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	long nrtracks;
	gpsm_swfile_t **files;
	gpsm_item_t *item;
	filter_t *net;
	int rate, i;
	float y_position = 20.0;
	char position_buffer[20];

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	item = gtk_swapfile_buffer_get_item(swapfile);
	if (GPSM_ITEM_IS_SWFILE(item))
		start -= gpsm_item_hposition(item);

	nrtracks = gtk_swapfile_buffer_get_swfiles(swapfile, &files);
	rate = gtk_wave_buffer_get_rate(wavebuffer);
	DPRINTF("Applying to [%li, +%li]\n", (long)start, (long)length);

	/* Create the network, add nrtracks instances of swapfile_in */
	
	net = filter_creat(NULL);
	for (i=0; i<nrtracks; i++) {
		filter_t *swin, *swout;
		int swname = gpsm_swfile_filename(files[i]);
		long sstart = start+gpsm_item_hposition(files[i]);
		swin = filter_instantiate(plugin_get("swapfile_in"));
		swout = filter_instantiate(plugin_get("swapfile_out"));
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "rate"), &rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "offset"), &sstart);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "size"), &length);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swout), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swout), "offset"), &start);
		filter_add_node(net, swin, "swin");
		filter_add_node(net, swout, "swout");
		filter_set_property(swin,"immutable","1");
		sprintf(position_buffer,"%8f",20.0);
		filter_set_property(swin,"canvas_x",position_buffer);
		sprintf(position_buffer,"%8f",420.0);
		filter_set_property(swout,"canvas_x",position_buffer);
		sprintf(position_buffer,"%8f",y_position);
		filter_set_property(swin,"canvas_y",position_buffer);
		filter_set_property(swout,"canvas_y",position_buffer);
		y_position += 100;
		filter_set_property(swout,"immutable","1");
	}
	draw_network(net);
	/* FIXME wave widget has to be asyncronously notified :-\ 
	for (i=0; i<nrtracks; i++) {
		glsig_emit(gpsm_item_emitter(files[i]), GPSM_SIG_SWFILE_CHANGED, files[i], start, length);
		glsig_emit(gpsm_item_emitter(files[i]), GPSM_SIG_ITEM_CHANGED, fles[i]);
	}
	*/
}

static GnomeUIInfo view_menu[] = {
	GNOMEUIINFO_ITEM("Zoom to selection", "zommsel", zoomsel_cb, NULL),
	GNOMEUIINFO_ITEM("Zoom full", "zommfull", zoomfull_cb, NULL),
	GNOMEUIINFO_ITEM("Zoom in", "zommin", zoomin_cb, NULL),
	GNOMEUIINFO_ITEM("Zoom out", "zommout", zoomout_cb, NULL),
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
	GNOMEUIINFO_SUBTREE("View", view_menu),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Apply filter", NULL),
	GNOMEUIINFO_SUBTREE("Feed into filter", NULL),
	GNOMEUIINFO_ITEM("Apply Custom", "Creates a filternetwork window for applying it to the selection",apply_custom_cb,NULL),
	GNOMEUIINFO_ITEM("Feed into Custom", "Creates a filternetworkwindow which and feeds the selection into it", feed_custom_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_END
};


/* Somehow only select "effects" (one input, one output) type of
 * filters... */
static int choose_effects(plugin_t *plugin)
{
	filter_t *filter = plugin_query(plugin, PLUGIN_FILTER);
	if (!filterportdb_get_port(filter_portdb(filter), PORTNAME_IN)
	    || !filterportdb_get_port(filter_portdb(filter), PORTNAME_OUT))
		return 0;
	return 1;
}

/* same as above, but only effects with one input. */
static int choose_effects_input_only(plugin_t * plugin)
{
	filter_t *filter = plugin_query(plugin, PLUGIN_FILTER);
	if (filterportdb_get_port(filter_portdb(filter), PORTNAME_IN)
	    && !filterportdb_get_port(filter_portdb(filter), PORTNAME_OUT))
		return 1;
	return 0;
}

/* Button press event. */
static void press (GtkWidget *widget, GdkEventButton *event,
		   gpointer user_data) 
{
	GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
	GtkWaveBuffer *wavebuffer;
	GtkWidget *menu;
	GtkMenu *filter_menu;
	GtkMenu *feed_menu;
  
	if (event->button != 3)
		return;

	wavebuffer = gtk_wave_view_get_buffer (waveview);
	if (!GTK_IS_EDITABLE_WAVE_BUFFER (wavebuffer)) {
		DPRINTF("not editable\n");
		return;
	}

	menu = gnome_popup_menu_new(rmb_menu);
	filter_menu = glame_gui_build_plugin_menu(choose_effects, apply_cb);
	feed_menu = glame_gui_build_plugin_menu(choose_effects_input_only, feed_cb);
	gtk_widget_show(GTK_WIDGET(filter_menu));
	gtk_widget_show(GTK_WIDGET(feed_menu));
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(rmb_menu[9].widget), GTK_WIDGET(filter_menu));
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(rmb_menu[10].widget), GTK_WIDGET(feed_menu));
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

GtkWidget *glame_waveedit_gui_new(const char *title, gpsm_item_t *item)
{
	GtkWidget *window, *waveview;
	GtkObject *wavebuffer;
	struct weg_compound *weg;

	/* Create a Gtk+ window. */
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), title);
  
	/* Create a GtkWaveView widget. */
	waveview = gtk_wave_view_new ();
	gtk_wave_view_set_select_channels (GTK_WAVE_VIEW (waveview), ~0);
	gtk_widget_set_usize (waveview, 300, 200);

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
	gtk_container_add (GTK_CONTAINER (window), waveview);

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

	return window;
}

void glame_waveedit_cleanup()
{
	reset_temp(0);
}
