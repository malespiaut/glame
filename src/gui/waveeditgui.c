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
#include "edit_filter/gui.h"
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

/* GUI is single-threaded, so this should actually work... */
static GtkWaveView *actual_waveview;

/* Menu event - Apply filter. */
static void apply_cb(GtkWidget *bla, plugin_t *plugin)
{
	GtkWaveView *waveview = actual_waveview;
	gint32 start, length;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	DPRINTF("FIXME: apply %s\n", plugin_name(plugin));
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

/* Button press event. */
static void press (GtkWidget *widget, GdkEventButton *event,
		   gpointer user_data) 
{
	GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
	GtkWaveBuffer *wavebuffer;
	GtkWidget *menu;
	GtkMenu *filter_menu;
  
	if (event->button != 3)
		return;

	wavebuffer = gtk_wave_view_get_buffer (waveview);
	if (!GTK_IS_EDITABLE_WAVE_BUFFER (wavebuffer)) {
		fprintf(stderr, "not editable\n");
		return;
	}

	menu = gnome_popup_menu_new(rmb_menu);
	filter_menu = glame_gui_build_plugin_menu(choose_effects, apply_cb);
	gtk_widget_show(filter_menu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(rmb_menu[7].widget), filter_menu);
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
	gtk_wave_view_set_select_channels (GTK_WAVE_VIEW (waveview), 0x03);
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
