#ifndef _GLAME_GUI_UTILS_H
#define _GLAME_GUI_UTILS_H


/*
 * glame_gui_utils.h
 *
 * $Id: glame_gui_utils.h,v 1.3 2001/03/28 23:18:32 xwolf Exp $
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

#define GLAME_LOGO PKGDATADIR "/glame-logo.jpg"
#define GLAME_DEFAULT_ICON "gnome-question.png"
#define GLAME_PIXMAP_PATH PKGDATADIR
#define GLAME_EMERGENCY_PIXMAP "/usr/X11R6/include/X11/bitmaps/xlogo32"
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gnome.h>
#include <libgnomeui/gnome-canvas.h>
#include "filter.h"
#include "util.h"
#include "canvas_types.h"


/* creates a hbox with the label and the widget and adds it 
 * to the specified vbox */
void create_label_widget_pair(GtkWidget* vbox, const char* label, GtkWidget* widget);

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

int glame_gui_play_network(filter_t * network, gui_network* gui_net);
int glame_gui_play_network_with_exit(filter_t * network, gui_network* gui_net, void (*atExitFunc)(va_list va) , ... );


#endif

