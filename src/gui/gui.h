#ifndef _GUI_H
#define _GUI_H


/*
 * gui.h
 *
 * $Id: gui.h,v 1.3 2000/02/17 07:15:01 xwolf Exp $
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

#define GUI_BUTTONS_X 2
#define GUI_BUTTONS_Y 2

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "filter.h"

typedef struct _glame_gui {
	GtkWidget *app;
	GtkWidget *table;
	GtkWidget *dock;
	
	GtkWidget *iconlist;
	//  too lazy to use richis list... someone can hack this up if he feels motivated
	GArray * filters;
	char **buttonlabels;
	int selectedIcon;

} glame_gui;

typedef struct _gui_filter {
	char * pixname;
	char * caption;
	// Filter data here
	filter_t * filter;
} gui_filter;

glame_gui * gui;

int gui_filter_add(gui_filter *filter);

gui_filter* gui_filter_new(const char *pixname, filter_t *filter);

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

// these are just dummies for later
void on_preferences_activate(GtkWidget *m,gpointer bla);
void on_cut_activate(GtkWidget *m,gpointer bla);
void on_copy_activate(GtkWidget *m, gpointer bla);
void on_paste_activate(GtkWidget *m, gpointer bla);
void on_clear_activate(GtkWidget *m, gpointer bla);


GtkWidget* gui_create_commandwin(void);

int gui_browse_registered_filters(void);
int gui_init_filter(void);



#endif
