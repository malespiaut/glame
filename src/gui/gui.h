#ifndef _GUI_H
#define _GUI_H


/*
 * gui.h
 *
 * $Id: gui.h,v 1.10 2000/03/15 15:46:59 xwolf Exp $
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
#define GLAME_LOGO "pixmaps/glame-logo.jpg"
#define GLAME_DEFAULT_ICON "pixmaps/default.png"

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
	filter_network_t * net;
	GSList *filters;
	gboolean paused;
} gui_network;

typedef struct _glame_gui {
	GtkWidget *app;
	GtkWidget *table;
	GtkWidget *dock;
	
	GtkWidget *iconlist;
	//  too lazy to use richis list... someone can hack this up if he feels motivated
	GArray * filters;
	GtkTargetEntry * target;
	char **buttonlabels;
	int selectedIcon;

} glame_gui;


typedef struct _gui_filter {
	char * pixname;
	char * caption;
	char * instance;
	// Filter data here
	filter_t * filter;
	filter_node_t *node;
	
} gui_filter;




glame_gui * gui;

int gui_filter_add(gui_filter *filter);

gui_filter* gui_filter_new(const char *pixname, filter_t *filter);

gui_network* gui_network_new(const char *caption, const char * pixname);
gui_network* gui_network_new_wizard(void);

int gui_network_filter_add(gui_network* net, gui_filter *fil);

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

int gui_browse_registered_filters(void);
int gui_filter_init(void);

void edit_paramdesc(gui_filter *f);




// canvas stuff
GtkWidget * create_new_canvas(const char *name, gui_network*);


GtkWidget* create_new_node(GnomeCanvas *canvas, gui_filter *filter,double x, double y);

void
create_ports(GnomeCanvasGroup* grp,gui_filter*f);



#endif
