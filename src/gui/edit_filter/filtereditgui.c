/*
 * filtereditgui.c
 *
 * $Id: filtereditgui.c,v 1.9 2001/05/28 09:19:50 xwolf Exp $
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
#ifdef HAVE_LIBSTROKE
#include <stroke.h>
#endif

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
void glame_canvas_property_dialog_cb(GtkObject*foo, GlameCanvas* canv);
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

#ifdef HAVE_LIBSTROKE		
static guint button_down_sighandlerid;

static gboolean 
button_down_cb(GnomeCanvas * canvas, GdkEvent *event, GlameCanvas* glCanv)
{
	char translation[50];
	switch(event->type){
	case GDK_BUTTON_RELEASE:
		if(stroke_trans(translation)){
			fprintf(stderr,"translation: %s\n",translation);
		}else{
			fprintf(stderr,"failed translation: %s\n",translation);
		}
		gtk_signal_disconnect(canvas,button_down_sighandlerid);
		break;
	case GDK_MOTION_NOTIFY:
		stroke_record(event->button.x, event->button.y);
		break;
	default:
		break;
	}
	return FALSE;
}
#endif
		

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
	GnomeCanvasItem* onItem;	
	switch(event->type){

	case GDK_BUTTON_PRESS:
		  /* check if on some item */
		gnome_canvas_window_to_world(canvas,event->button.x,event->button.y,&eventx,&eventy);
		glcanvas = GLAME_CANVAS(canvas);
		onItem = gnome_canvas_get_item_at(canvas,eventx, eventy);

		switch(event->button.button){
		case 1:
#if 0
			if(!onItem){
				group_all(glCanv);
			}
#endif
			break;
		case 2:
			if(!onItem){
#ifdef HAVE_LIBSTROKE			  
			button_down_sighandlerid = gtk_signal_connect(GTK_OBJECT(canvas),"event",GTK_SIGNAL_FUNC(button_down_cb),glCanv);
#endif
			}
			break;
		case 3:
			if(!onItem){
				menu = GTK_WIDGET(glame_gui_build_plugin_menu(NULL, add_filter_by_plugin_cb));
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,NULL);
				return TRUE;
			}
			break;
		default:
			break;
		}
	default:
		break;
	}
	return FALSE;
}



static void canvas_delete_redirection_param_cb(GtkWidget*foo, filter_param_t* param)
{
	filterparam_delete(param);
}

static void canvas_delete_redirection_port_cb(GtkWidget*foo, filter_port_t* port)
{
	filterport_delete(port);
}



static GnomeUIInfo redirection_menu_port[] = 
{
	GNOMEUIINFO_ITEM_STOCK("Delete Port","Deletes selected redirection",
			       canvas_delete_redirection_port_cb,
			       GNOME_STOCK_MENU_TRASH),
	GNOMEUIINFO_END
};

static GnomeUIInfo redirection_menu_param[] = 
{
	GNOMEUIINFO_ITEM_STOCK("Delete Parameter","Deletes selected redirection",
			       canvas_delete_redirection_param_cb,
			       GNOME_STOCK_MENU_TRASH),
	GNOMEUIINFO_END
};

static void redirection_list_port_cb(GtkCList *list, gint row, gint column,
			       GdkEvent* event, GList *reds)
{
	GtkWidget * menu;
	filter_port_t *port;
	menu = gnome_popup_menu_new(redirection_menu_port);
	port = g_list_nth_data(reds,row);
	gtk_clist_unselect_row(list,row,0);
	if(port)
		if(gnome_popup_menu_do_popup_modal(menu,NULL,NULL,&event->button,port)>=0)
			gtk_clist_remove(list,row);

}
static void redirection_list_param_cb(GtkCList *list, gint row, gint column,
			       GdkEvent* event, GList *reds)
{
	GtkWidget * menu;
	filter_param_t* foo;
	menu = gnome_popup_menu_new(redirection_menu_param);
	foo = g_list_nth_data(reds,row);
	gtk_clist_unselect_row(list,row,0);
	if(foo)
		if(gnome_popup_menu_do_popup_modal(menu,NULL,NULL,&event->button,foo)>=0)
			gtk_clist_remove(list,row);

}

void glame_canvas_property_dialog_cb(GtkObject* foo, GlameCanvas *canvas)
{
	GtkWidget * dialog;
	GtkWidget * vbox;
	GtkWidget * notebook;
	GtkWidget * tablabel;
	GtkCList * list;

		
	filter_portdb_t * ports;
	filter_paramdb_t * params;
	filter_param_t * param;
	filter_t * filter;
	filter_port_t * port;
	GList * redPorts=NULL;
	GList * redParms=NULL;
	
	char *labels[] = {"Name","Type","Description","Source"};
	char *plabels[] = {"Name","Value","Description","Source"};
	char ** line;
	const char * buffer;
	filter = canvas->net;
	
	ports = filter_portdb(filter);
	
	notebook = gtk_notebook_new();
	
	dialog = gnome_dialog_new("Properties",GNOME_STOCK_BUTTON_OK,NULL);
	


	tablabel = gtk_label_new("Ports");
	list = GTK_CLIST(gtk_clist_new_with_titles(4,labels));
	gtk_clist_set_column_auto_resize(list,0,TRUE);
	gtk_clist_set_column_auto_resize(list,1,TRUE);
	gtk_clist_set_column_auto_resize(list,2,TRUE);
	gtk_clist_set_column_auto_resize(list,3,TRUE);
	
	filterportdb_foreach_port(ports,port){
		line = calloc(4,sizeof(char*));
		buffer = filterport_label(port);
		if(buffer)
			line[0] = strdup(filterport_label(port));
		else
			line[0] = strdup("Empty");
		line[1] = (filterport_is_input(port)?"In":"Out");
		buffer = filterport_get_property(port,FILTERPORT_DESCRIPTION);
		if(buffer)
			line[2] = strdup(buffer);
		else
			line[2] = strdup("Empty");
		
		buffer = filterport_get_property(port,FILTERPORT_MAP_NODE);
		if(buffer)
			line[3] = strdup(buffer);
		else
			line[3]= strdup("Unkown");
		redPorts = g_list_append(redPorts,port);
		gtk_clist_append(list,line);
	}
	//if(!canvas->net->openedUp)
		gtk_signal_connect(GTK_OBJECT(list),"select-row",redirection_list_port_cb,redPorts);
	gtk_widget_show(GTK_WIDGET(list));
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),GTK_WIDGET(list),tablabel);

	tablabel = gtk_label_new("Properties");
	gtk_widget_show(tablabel);
	
	list = GTK_CLIST(gtk_clist_new_with_titles(4,plabels));
	gtk_clist_set_column_auto_resize(list,0,TRUE);
	gtk_clist_set_column_auto_resize(list,1,TRUE);
	gtk_clist_set_column_auto_resize(list,2,TRUE);
	gtk_clist_set_column_auto_resize(list,3,TRUE);
	
	params = filter_paramdb(filter);
	filterparamdb_foreach_param(params,param){
		line = calloc(4,sizeof(char*));
		buffer = filterparam_label(param);
		if(buffer)
			line[0] = strdup(buffer);
		else
			line[0] = strdup("Empty");
		buffer = filterparam_to_string(param);
		if(buffer)
			line[1] = strdup(buffer);
		else
			line[1] = strdup("Empty");
		buffer = filterparam_get_property(param,FILTERPARAM_DESCRIPTION);
		if(buffer)
			line[2] = strdup(buffer);
		else
			line[2] = strdup("Empty");
		buffer = filterparam_get_property(param,FILTERPARAM_MAP_NODE);
		if(buffer)
			line[3] = strdup(buffer);
		else
			line[3] = strdup("Unkown");
		redParms = g_list_append(redParms,param);
		gtk_clist_append(list,line);
	}
	gtk_widget_show(GTK_WIDGET(list));
	gtk_signal_connect(GTK_OBJECT(list),"select-row",redirection_list_param_cb,redParms);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),GTK_WIDGET(list),tablabel);
	
	gtk_widget_show(notebook);
	vbox = GNOME_DIALOG(dialog)->vbox;
	gtk_container_add(GTK_CONTAINER(vbox),notebook);
	
	gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
	g_list_free(redPorts);
	g_list_free(redParms);
	
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
	


	window = gnome_app_new("glame0.5", _(name));
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
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Save","Saves Filternetwork","foo",
				glame_load_icon_widget("save.png"),
				glame_canvas_save_as_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Properties","Edit Filternetwork Properties","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_PROPERTIES),glame_canvas_property_dialog_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom in","Zooms in","foo",
				glame_load_icon_widget("zoom_in.png"),
				glame_canvas_zoom_in_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom out","Zooms out","foo",
				glame_load_icon_widget("zoom_out.png"),
				glame_canvas_zoom_out_cb,canvas);
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
#ifdef HAVE_LIBSTROKE
	stroke_init();
#endif
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
