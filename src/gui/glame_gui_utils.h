#ifndef _GLAME_GUI_UTILS_H
#define _GLAME_GUI_UTILS_H

/*
 * glame_gui_utils.h
 *
 * $Id: glame_gui_utils.h,v 1.13 2001/05/09 10:57:06 xwolf Exp $
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
 *
 * This file (will) have all the visible gui functions 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gnome.h>
#include <libgnomeui/gnome-canvas.h>
#include "filter.h"
#include "util.h"
#include "glamecanvas.h"


#define GLAME_LOGO PKGPIXMAPSDIR "/glame-logo.jpg"
#define GLAME_DEFAULT_ICON "gnome-question.png"
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


/* useful callback for dialog boxes which query for string types. */
void changeString(GtkEditable *wid, char ** returnbuffer);


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
				       const char *caption);

/* Opens a play/pause/stop/cancel window that plays a given network.
 * If modal is TRUE, the dialog is modal, on dialog destroy the optional
 * atExit is called with parameter data. Returns 0, if the dialog could
 * be created, -1 on error. */
int glame_gui_play_network(filter_t *network, GlameCanvas *gui, int modal,
			   GtkFunction atExit, gpointer data,
			   const char *start_label,
			   const char *pause_label,
			   const char *stop_label, int close_on_stop);

/* Open a file selection dialog with the specified title, file entry label
 * and file selection dialog pattern (optional). You need to provide a
 * result character buffer with space for at least 256 characters. */
GtkWidget *glame_dialog_file_request(const char *windowtitle,
				     const char *history_id, const char *label,
				     const char *pattern,
				     char *returnbuffer);

/* Create a progress indicator (gtk progress bar, if size > 0, gnome
 * animator if size == -1) that polls parameter pos. The progress indicator
 * widget is destroyed on parameter destruction, early destruction of
 * the widget is handled fine. */
GtkWidget *glame_progress_indicator(filter_param_t *pos, long size);


/* Run a network asynchronily and clean up (network) after completion
 * (displaying an error log, if a failure occured). Returns 0, if network
 * start was ok, -1 on error in which case the network is not deleted.
 * Runs the provided callback after completion (or not, if NULL). */
int glame_async_run_network(filter_t *net, GtkFunction callback, gpointer data);


#endif
