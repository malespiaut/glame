/*
 * filtereditgui.c
 *
 * $Id: filtereditgui.c,v 1.5 2001/05/17 22:38:36 xwolf Exp $
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
 *
 */

#include <gnome.h>
#include "glamecanvas.h"
#include "canvasitem.h"

/* FIXME remove these later on */
guint nPopupTimeout;
gboolean bMac;
/* FIXME */

/* yuck! */
static double eventx,eventy;
static GlameCanvas *glcanvas;



/* callbacks */


void glame_canvas_execute_cb(GtkObject*foo, GlameCanvas* canv);
void glame_canvas_register_cb(GtkObject* foo, GlameCanvas* canv){}
void glame_canvas_save_as_cb(GtkObject* foo, GlameCanvas* canv){}
void glame_canvas_property_dialog_cb(GtkObject*foo, GlameCanvas* canv){}
void glame_canvas_zoom_in_cb(GtkObject*foo,GlameCanvas* canv){}
void glame_canvas_zoom_out_cb(GtkObject*foo, GlameCanvas* canv){}
void glame_canvas_view_all_cb(GtkObject*foo, GlameCanvas* canv){}

void window_close(GtkWidget *dummy, GtkWidget* window)
{
	gtk_object_destroy(GTK_OBJECT(window));
}

/* /callbacks */






void
add_filter_by_plugin_cb(GtkWidget*wid, plugin_t *plugin)
{
	GlameCanvasFilter* filter;
	DPRINTF("foo\n");
	filter = glame_canvas_add_filter_by_plugin(glcanvas, plugin);
	glame_canvas_filter_move(filter,eventx, eventy);
}

/* static gboolean */
/* button_on_item_event(GlameCanvas* canvas, GdkEvent* event, GnomeCanvasItem* item) */
/* { */
/* 	switch(event->button.button){ */
/* 	case 1: */
		

static void group_all(GlameCanvas* canv)
{
	filter_t* iter;
	GlameCanvasFilter* filter, *f2;
	GlameCanvasGroup* group;
	
	filter_foreach_node(canv->net,iter){
		filter = glame_canvas_find_filter(iter);
	}
	group = GLAME_CANVAS_GROUP(GNOME_CANVAS_ITEM(filter)->parent);
	
	filter_foreach_node(canv->net,iter){
		f2 = glame_canvas_find_filter(iter);
		if(f2!=filter)
			glame_canvas_group_add_item(group,f2);
	}
}
	
static gboolean 
root_event(GnomeCanvas * canvas, GdkEvent *event, GlameCanvas* glCanv)
{

	GtkWidget *menu;
	
	GdkEventButton *event_button;
	
	switch(event->type){


	case GDK_BUTTON_PRESS:
		
		/* check if on some item */
		gnome_canvas_window_to_world(canvas,event->button.x,event->button.y,&eventx,&eventy);
		glcanvas = GLAME_CANVAS(canvas);
		
		if(gnome_canvas_get_item_at(canvas,eventx, eventy)){
			return FALSE;
			/* hit item! */
		}else{
			if(event->button.button==1){
				group_all(glCanv);
				return TRUE;
			}else{
			menu = GTK_WIDGET(glame_gui_build_plugin_menu(NULL, add_filter_by_plugin_cb));
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,NULL);
			return TRUE;
			}
		}
		break;
	default:
		break;
	}
	return FALSE;
}




GtkWidget * 
glame_filtereditgui_new(filter_t *net)
{
	GtkWidget *window, *canvas, *sw, *toolbar;

	GnomeDock *dock;
	
	const char *name;
	
	if(net && filter_name(net))
		name = strdup(filter_name(net));
	else
		name = strdup("Untitled");
	


	window = gnome_app_new(name,_(name));
	dock = GNOME_DOCK(GNOME_APP(window)->dock);
	gtk_widget_ref(GTK_WIDGET(dock));
	
	gtk_widget_show(GTK_WIDGET(dock));
	toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,GTK_TOOLBAR_BOTH);


	sw = gtk_scrolled_window_new(NULL,NULL);
	gtk_widget_show(sw);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_widget_push_visual(gdk_rgb_get_visual());
	gtk_widget_push_colormap(gdk_rgb_get_cmap());
	canvas = glame_canvas_new(net);
	gtk_widget_pop_colormap();
	gtk_widget_pop_visual();
	//gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas),0,0,600,400);
	

	gtk_container_add(GTK_CONTAINER(sw),canvas);
	
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Execute","Executes Filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_EXEC),glame_canvas_execute_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Register","Registers actual filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CONVERT),glame_canvas_register_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Save","Saves Filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_SAVE_AS),glame_canvas_save_as_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Properties","Edit Filternetwork Properties","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_PROPERTIES),glame_canvas_property_dialog_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom in","Zooms in","foo",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_DOWN),
				glame_canvas_zoom_in_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom out","Zooms out","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_UP),glame_canvas_zoom_out_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"View all","Adjusts scroll region","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_REFRESH),glame_canvas_view_all_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Close","Close","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CLOSE),window_close,window);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Help","Help","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_HELP),gnome_help_goto,"info:glame#The_Filternetwork_Editor");
	gnome_app_add_toolbar(GNOME_APP(window), GTK_TOOLBAR(toolbar),
			      "canvas::toolbar",
			      GNOME_DOCK_ITEM_BEH_EXCLUSIVE|GNOME_DOCK_ITEM_BEH_NEVER_FLOATING,
			      GNOME_DOCK_TOP, 0, 0, 0);
	
	gnome_app_set_contents(GNOME_APP(window),sw);	
	gtk_widget_show(GTK_WIDGET(dock));
	
	gtk_window_set_default_size(GTK_WINDOW(window),400,300);
	gtk_widget_show_all(window);
	gtk_signal_connect(GTK_OBJECT(canvas),"event",GTK_SIGNAL_FUNC(root_event),canvas);
	return window;
}



GtkWidget *
glame_filtereditgui_new_cb(GtkObject* foo, filter_t* net)
{
	return glame_filtereditgui_new(NULL);
}


void
glame_load_network(GtkWidget *foo, gpointer bla)
{
	GtkWidget *dialog;
	filter_t *filter;
	char filenamebuffer[256];
	
	dialog = glame_dialog_file_request("Load filternetwork",
					   "editfilter:load", "Filename",
					   NULL, filenamebuffer);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;
	
	filter = glame_load_instance(filenamebuffer);
	if (filter) {
		glame_filtereditgui_new(filter);
	} else {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Error in loading network")));
	} 
}


void glame_canvas_execute_cb(GtkObject* foo,GlameCanvas* canv)
{
	DPRINTF("foo\n");
	glame_gui_play_network(canv->net,canv,TRUE, NULL,NULL,  "Start", "Pause", "Stop", 0);
}

void
glame_filtereditgui_draw_error(GlameCanvas* canv)
{
	glame_canvas_draw_errors(canv);
}

void
glame_filtereditgui_reset_error(GlameCanvas* canv)
{
	glame_canvas_reset_errors(canv);
}
