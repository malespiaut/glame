#ifndef _GLAME_GUI_UTILS_H
#define _GLAME_GUI_UTILS_H

/*
 * glame_gui_utils.h
 *
 * $Id: glame_gui_utils.h,v 1.15 2003/04/21 12:16:07 richi Exp $
 *
 * Copyright (C) 2001 Johannes Hirche
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
 * * This file (will) have all the visible gui functions 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gnome.h>
#include <libgnomecanvas/gnome-canvas.h>
#include "filter.h"
#include "util.h"
#include "edit_filter/glamecanvas.h"


#define GLAME_LOGO PKGPIXMAPSDIR "/glame-logo.jpg"
#define GLAME_DEFAULT_ICON "default.png"
#define GLAME_PIXMAP_PATH PKGPIXMAPSDIR
#define GLAME_EMERGENCY_PIXMAP "/usr/X11R6/include/X11/bitmaps/xlogo32"


/* creates a hbox with the label and the widget and adds it 
 * to the specified vbox */
void create_label_widget_pair(GtkWidget* vbox, const char* label,
			      GtkWidget* widget);

/* creates a hbox with a label and an gnome edit widget with the provided
 * default text, a signal handler is installed that updates the provided
 * destination string. You should provide a result string with space for
 * at least 256 characters. */
void create_label_edit_pair(GtkWidget *vbox,
			    const char *label, const char *history,
			    char *result);

/* creates a hbox with a label and a spin button with the provided default
 * value, a signal handler is installed that updates the provided value. */
void create_label_long_pair(GtkWidget *vbox,
			    const char *label, long *value,
			    long vmin, long vmax);

/* creates a hbox with a label and a spin button with the provided default
 * value, a signal handler is installed that updates the provided value. */
void create_label_double_pair(GtkWidget *vbox,
			      const char *label, double *value,
			      double vmin, double vmax);

/* Query the nr of the selected item in a Gtk(Option)Menu. */
gint glame_menu_get_active_index(GtkMenu *menu);


/* useful callback for dialog boxes which query for string types. */
void changeString(GtkEditable *wid, char *returnbuffer);

/* auch useful callback */
void glame_help_cb(GtkWidget *bla, const char *helppath);

/* Build a menu (hidden) out of the registered plugins which you can
 * either select all (NULL select callback) or select by returning
 * non-zero from the select callback. Each menu item is connected
 * to the signal handler gtksighand, if specified, which is passed the
 * actual plugin. */
GtkMenu *glame_gui_build_plugin_menu(int (*select)(plugin_t *),
				     void (*gtksighand)(GtkWidget *, plugin_t *));


/* Open a property box for changing the paramdb's parameters. Returns
 * the widget. */
GtkWidget *glame_gui_filter_properties(filter_paramdb_t *pdb,
				       const char *caption,
				       const char *helppath);

/* Construct a widget (without window) to allow changing parameters
 * of a parameter database. Returns a widget or NULL on error. */
GtkWidget *glame_gui_from_paramdb(filter_paramdb_t *pdb);


/* Open a file selection dialog with the specified title, file entry label
 * and file selection dialog pattern (optional). You need to provide a
 * result character buffer with space for at least 256 characters. */
GtkWidget *glame_dialog_file_request(const char *windowtitle,
				     const char *history_id, const char *label,
				     const char *pattern,
				     char *returnbuffer);


/* Creates a notificator for network finish. Add handlers _before_
 * launching the network! Returns a signal emitter on success, NULL
 * on error.
 * The returned emitter will emit GLSIG_NETWORK_DONE after network
 * finish. See below for a set of standard handlers you may want
 * to execute.
 * The returned emitter will emit GLSIG_NETWORK_TICK for each tick
 * (see timeout specification below) the network is running. */
#define GLSIG_NETWORK_DONE 1
#define GLSIG_NETWORK_TICK 2
glsig_emitter_t *glame_network_notificator_creat(filter_t *net);

/* Sets the wbufsize for running the network. Defaults to preference
 * configured bufsize. */
void glame_network_notificator_set_wbufsize(glsig_emitter_t *emitter,
                                            int wbufsize);

/* Runs (launches the network and starts the polling timeout handler)
 * a previously created (glame_network_notificator_creat) notificator.
 * Specify the timeout value for the gtk_timeout_handler (10-100 is
 * a good range).
 * Returns 0 on success, -1 on network launch/start error. */
int glame_network_notificator_run(glsig_emitter_t *emitter, int timeout);

/* Standard handler which deletes the finished network. Provide NULL
 * as private handler data. */
void glame_network_notificator_delete_network(glsig_handler_t *,
					      long, va_list);

/* Standard handler which destroys a gpsm item. Provide the gpsm item
 * as private handler data. */
void glame_network_notificator_destroy_gpsm(glsig_handler_t *, long, va_list);



/* Load a GdkPixbuf which is suitable for canvas operation/taskbars(?)
 * if nothing is found it will try to return the glame default icon. */
GdkPixbuf* glame_load_icon(const char* filename, int x, int y);

/* Same as above, but returns the image as a gnomepixmap (a widget) */
GtkWidget* glame_load_icon_widget(const char* filename, int x, int y);


/* Displays an error dialog with the specified header, listing nodes
 * and corresponding error strings. */
void glame_network_error_dialog(filter_t *net, const char *header);

/* In the style of the gnome1 gnome_help_goto, display the given url.
 * Can be used directly as callback function. */
void glame_help_goto(void *ignore, const char *url);

#endif
