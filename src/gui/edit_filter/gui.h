#ifndef _GUI_H
#define _GUI_H


/*
 * gui.h
 *
 * $Id: gui.h,v 1.5 2001/03/02 00:05:34 xwolf Exp $
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

#include "filter.h"
#include "util.h"
#include <gnome.h>
#include <libgnomeui/gnome-canvas.h>


typedef struct _gui_network {
	char *pixname;
	char *caption;
	char *descr;
	int iports,oports;
	filter_t * net;
	GtkWidget *canvas;
	gboolean paused;
} gui_network;


gui_network* gui_network_new(const char *caption, const char * pixname);
gui_network* gui_network_new_wizard(void);

int gui_network_filter_add(gui_network* net, filter_t *fil);

GtkWidget* gui_create_about(void);


void gui_handle_icon_sel (GnomeIconList *iconlist,
			  gint arg1,
			  GdkEvent *event,
			  gpointer user_data);

void gui_exit(GtkWidget *w,GdkEvent *e, gpointer d);

void handle_about(GtkWidget *menuitem,gpointer bla);
void handle_properties(GtkWidget *menuitem, gpointer bla);
void handle_new_filter_net(GtkWidget *menuitem, gpointer bla);
void handle_filter_net_open(GtkWidget *menuitem, gpointer bla);
void handle_load_filter_plugin(GtkWidget *menuitem,gpointer bla);

void icon_prop_activate                (gpointer user_data);

// these are just dummies for later
void on_preferences_activate(GtkWidget *m,gpointer bla);
void on_cut_activate(GtkWidget *m,gpointer bla);
void on_copy_activate(GtkWidget *m, gpointer bla);
void on_paste_activate(GtkWidget *m, gpointer bla);
void on_clear_activate(GtkWidget *m, gpointer bla);


GtkWidget* gui_create_commandwin(void);


GSList* gui_browse_registered_filters(void);

void edit_paramdesc(filter_t *f);

void create_label_widget_pair(GtkWidget* vbox, const char* label, GtkWidget* widget);
				    



#endif
