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
#include "glame_gui_utils.h"
#include "waveeditgui.h"


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
		while ((temp_fd[i] = sw_open(rand(), O_CREAT|O_EXCL|O_RDWR, TXN_NONE)) == -1)
			;
	}
	temp_nrtracks = nrtracks;

	/* Reset temporary tracks. */
	for (i=0; i<temp_nrtracks; i++) {
		sw_ftruncate(temp_fd[i], 0);
		sw_lseek(temp_fd[i], 0, SEEK_SET);
	}
}

/* Menu event - Copy. */
static void copy_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;
	long *names, nrtracks;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	DPRINTF("Copying selection from %i of length %i\n", start, length);

	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	reset_temp(nrtracks);
	for (i=0; i<nrtracks; i++) {
		fd = sw_open(names[i], O_RDONLY, TXN_NONE);
		sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
		if (sw_sendfile(temp_fd[i], fd, length*SAMPLE_SIZE, SWSENDFILE_INSERT) == -1)
			fprintf(stderr, "*** sw_sendfile failed\n");
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
	long *names, nrtracks;
	int i;

	start = gtk_wave_view_get_marker (waveview);
	if (start < 0)
		return;

	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	if (nrtracks != temp_nrtracks) {
		DPRINTF("clipboard nrtracks do not match\n");
		return;
	}

	if (sw_fstat(temp_fd[0], &st) == -1)
		return;

	g_print ("Pasting to %i size %i\n", start, st.size/SAMPLE_SIZE);

	for (i=0; i<nrtracks; i++) {
		fd = sw_open(names[i], O_RDWR, TXN_NONE);
		if (sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET) != start*SAMPLE_SIZE)
			fprintf(stderr, "*** sw_lseek(swapfile->fd) failed\n");
		if (sw_lseek(temp_fd[i], 0, SEEK_SET) != 0)
			fprintf(stderr, "*** sw_lseek(temp_fd, 0) failed\n");
		if (sw_sendfile(fd, temp_fd[i], st.size, SWSENDFILE_INSERT) == -1)
			fprintf(stderr, "*** sw_sendfile failed\n");
		sw_close(fd);
	}

	gtk_editable_wave_buffer_insert(editable, start, st.size/SAMPLE_SIZE);
}

/* Menu event - Cut. */
static void cut_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;
	long *names, nrtracks;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	g_print ("Deleting selection from %i of length %i\n", start, length);

	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	reset_temp(nrtracks);
	for (i=0; i<nrtracks; i++) {
		fd = sw_open(names[i], O_RDWR, TXN_NONE);
		sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
		if (sw_sendfile(temp_fd[i], fd, length*SAMPLE_SIZE, SWSENDFILE_CUT|SWSENDFILE_INSERT) == -1)
			fprintf(stderr, "*** sw_sendfile failed\n");
		sw_close(fd);
	}

	gtk_editable_wave_buffer_delete (editable, start, length);
}

/* Menu event - Delete. */
static void delete_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;
	long *names, nrtracks;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	g_print ("Cutting selection from %i of length %i\n", start, length);

	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	for (i=0; i<nrtracks; i++) {
		fd = sw_open(names[i], O_RDWR, TXN_NONE);
		sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
		if (sw_sendfile(SW_NOFILE, fd, length*SAMPLE_SIZE, SWSENDFILE_CUT) == -1)
			fprintf(stderr, "*** sw_sendfile failed\n");
		sw_close(fd);
	}

	gtk_editable_wave_buffer_delete (editable, start, length);
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
	long *names, nrtracks;
	filter_t *net, *effect;
	int rate, i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;
	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	rate = gtk_wave_buffer_get_rate(wavebuffer);
	DPRINTF("Applying to [%li, +%li]\n", (long)start, (long)length);

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
		int swname = names[i];
		swin = filter_instantiate(plugin_get("swapfile_in"));
		eff = filter_creat(effect);
		swout = filter_instantiate(plugin_get("swapfile_out"));
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "rate"), &rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "offset"), &start);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "size"), &length);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swout), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swout), "offset"), &start);
		filter_add_node(net, swin, "swin");
		filter_add_node(net, eff, "eff");
		filter_add_node(net, swout, "swout");
		filterport_connect(filterportdb_get_port(filter_portdb(eff), PORTNAME_OUT), filterportdb_get_port(filter_portdb(swout), PORTNAME_IN));
		filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), filterportdb_get_port(filter_portdb(eff), PORTNAME_IN));
	}
	
	/* Run the network and cleanup after it. */
	filter_launch(net);
	filter_start(net);
	filter_wait(net);
	filter_delete(net);
	filter_delete(effect);
	gtk_editable_wave_buffer_queue_modified (editable, start, length);
}

/* kill the running network */

static void stop_network(GtkWidget* bla, plugin_t *net)
{
	/* FIXME FIXME FIXME 
	 * The effect is not cleaned up! */
	
	filter_terminate(net);
	filter_delete(net);
}

/* Menu event - feed into filter. */
static void feed_cb(GtkWidget *bla, plugin_t *plugin)
{

	/* FIXME FIXME FIXME 
	   fix richis bogus loop ;) */
	   
	GtkWaveView *waveview = actual_waveview;
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	GtkWidget *prop;
	gboolean ok_pressed;
	gint32 start, length;
	long *names, nrtracks;
	filter_t *net, *effect;
	int rate, i;
	GtkWidget * stop_window;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;
	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	rate = gtk_wave_buffer_get_rate(wavebuffer);
	DPRINTF("Applying to [%li, +%li]\n", (long)start, (long)length);

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
		int swname = names[i];
		swin = filter_instantiate(plugin_get("swapfile_in"));
		eff = filter_creat(effect);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "filename"), &swname);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "rate"), &rate);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "offset"), &start);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swin), "size"), &length);
		filter_add_node(net, swin, "swin");
		filter_add_node(net, eff, "eff");
		filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), filterportdb_get_port(filter_portdb(eff), PORTNAME_IN));
	}
	/*   FIXME THIS gets mapped after the network has finished :( useless*/
	stop_window = gnome_dialog_new("Network running...","Cancel?",NULL);
	gtk_widget_show(stop_window);
	gtk_widget_draw_default(stop_window);
	gnome_dialog_button_connect(GNOME_DIALOG(stop_window),0,stop_network,net);
	gnome_dialog_set_close(GNOME_DIALOG(stop_window),TRUE);


	/* Run the network and cleanup after it. */
	filter_launch(net);
	filter_start(net);
	filter_wait(net);
	filter_delete(net);
	filter_delete(effect);
	gtk_editable_wave_buffer_queue_modified (editable, start, length);
}



static GnomeUIInfo rmb_menu[] = {
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Cut", "cut", cut_cb, NULL),
	GNOMEUIINFO_ITEM("Copy", "copy", copy_cb, NULL),
	GNOMEUIINFO_ITEM("Paste", "paste", paste_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Apply filter", NULL),
	GNOMEUIINFO_SUBTREE("Feed into filter", NULL),
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
		fprintf(stderr, "not editable\n");
		return;
	}

	menu = gnome_popup_menu_new(rmb_menu);
	filter_menu = glame_gui_build_plugin_menu(choose_effects, apply_cb);
	feed_menu = glame_gui_build_plugin_menu(choose_effects_input_only, feed_cb);
	gtk_widget_show(filter_menu);
	gtk_widget_show(feed_menu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(rmb_menu[7].widget), filter_menu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(rmb_menu[8].widget), feed_menu);
	actual_waveview = waveview;
	gnome_popup_menu_do_popup(menu, NULL, NULL, event, waveview);
}


/* Delete callback. */
static void delete (GtkWidget *widget, gpointer user_data)
{
	gtk_widget_hide(widget);
	gtk_object_destroy(GTK_OBJECT(widget));
}



GtkWidget *glame_waveedit_gui_new_a(const char *title, int nrtracks, 
				    int samplerate, long *names)
{
	GtkWidget *window, *waveview;
	GtkObject *wavebuffer;

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
	wavebuffer = gtk_swapfile_buffer_new_a(nrtracks, samplerate, names);
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

	return window;
}
GtkWidget *glame_waveedit_gui_new_va(const char *title, int nrtracks,
				     int samplerate, va_list va)
{
	long names[GTK_SWAPFILE_BUFFER_MAX_TRACKS];
	int i;

	if (nrtracks > GTK_SWAPFILE_BUFFER_MAX_TRACKS)
		return NULL;
	for (i=0; i<nrtracks; i++)
		names[i] = va_arg(va, long);
	return glame_waveedit_gui_new_a(title, nrtracks, samplerate, names);
}
GtkWidget *glame_waveedit_gui_new(const char *title, int nrtracks,
				  int samplerate, ...)
{
	GtkWidget *w;
	va_list va;

	va_start(va, samplerate);
	w = glame_waveedit_gui_new_va(title, nrtracks, samplerate, va);
	va_end(va);
	return w;
}


void glame_waveedit_cleanup()
{
	reset_temp(0);
}
