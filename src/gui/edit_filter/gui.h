#ifndef _GUI_H
#define _GUI_H


/*
 * gui.h
 *
 * $Id: gui.h,v 1.9 2001/03/19 13:40:32 richi Exp $
 *
 * Copyright (C) 2000 Johannes Hirche
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
 *
 * This is broken code, just a beginning, beware
 */

#define GUI_BUTTONS_X 4
#define GUI_BUTTONS_Y 2
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


/* FIXME: have one place with a nice, useful external visible API */

gui_network* gui_network_new();

void create_label_widget_pair(GtkWidget* vbox, const char* label, GtkWidget* widget);

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



#endif
