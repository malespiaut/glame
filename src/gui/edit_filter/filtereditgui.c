/*
 * filtereditgui.c
 *
 * $Id: filtereditgui.c,v 1.40 2001/11/28 14:10:56 richi Exp $
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
#include <sys/param.h>
#include <stdio.h>
#include <math.h>
#include <gnome.h>
#ifdef HAVE_LIBSTROKE
#include <stroke.h>
#endif
#include "glscript.h"
#include "util/glame_gui_utils.h"
#include "glame_accelerator.h"
#include "glconfig.h"
#include "glamecanvas.h"
#include "canvasitem.h"
#include "filtereditgui.h"

/* FIXME remove these later on */
long nPopupTimeout;
long bMac;
/* FIXME */

/* yuck! */
static double eventx,eventy;
static GlameCanvas *glcanvas;



/* callbacks */


static void glame_canvas_execute_cb(GtkObject*foo, FiltereditGui *gui);
static void glame_canvas_register_cb(GtkWidget* ignore, GlameCanvas* canv);
static void glame_canvas_save_as_cb(GtkWidget* ignore, GlameCanvas* canv);
static void glame_canvas_property_dialog_cb(GtkObject*foo, GlameCanvas* canv);
static void glame_canvas_zoom_in_cb(GtkObject*foo,GlameCanvas* canv);
static void glame_canvas_zoom_out_cb(GtkObject*foo, GlameCanvas* canv);
static void glame_canvas_view_all_cb(GtkObject*foo, GlameCanvas* canv);
static void glame_canvas_add_last_cb(GtkObject* foo, GlameCanvas* canv);
static void window_close(GtkWidget *dummy, GtkWidget* window);





void
add_filter_by_plugin_cb(GtkWidget*wid, plugin_t *plugin)
{
	GlameCanvasFilter* filter;
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
		
#if 0
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
#endif

static void
filtereditgui_paste_cb(GtkMenuItem *foo,
		       GlameCanvas *canv)
{
	glame_canvas_paste_selection(canv);
}


static gboolean 
root_event(GnomeCanvas * canvas, GdkEvent *event, GlameCanvas* glCanv)
{

	GtkWidget *menu;
	
	GnomeCanvasItem* onItem;	
	/* assign global for accels :( */
	

	switch(event->type){
	case GDK_ENTER_NOTIFY:
		glcanvas = glCanv;
		break;
	case GDK_BUTTON_PRESS:
		  /* check if on some item */
		gnome_canvas_window_to_world(canvas,event->button.x,event->button.y,&eventx,&eventy);
		glcanvas = GLAME_CANVAS(canvas);
		onItem = gnome_canvas_get_item_at(canvas,eventx, eventy);

		switch(event->button.button){
		case 1:
			if(bMac){
				if(!onItem){
					menu = GTK_WIDGET(glame_gui_build_plugin_menu(NULL, add_filter_by_plugin_cb));
					gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,NULL);
					return TRUE;
				}
			}
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
				GtkWidget* edit;
				menu = GTK_WIDGET(glame_gui_build_plugin_menu(NULL, add_filter_by_plugin_cb));
				edit = gtk_menu_item_new_with_label("Paste Selection");
				gtk_widget_show(edit);
				gtk_signal_connect(GTK_OBJECT(edit),"activate",filtereditgui_paste_cb,glCanv);
				gtk_menu_append(menu,edit);
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
			line[0] = filterport_label(port);
		else
			line[0] = "Empty";
		line[1] = (filterport_is_input(port)?"In":"Out");
		buffer = filterport_get_property(port,FILTERPORT_DESCRIPTION);
		if(buffer)
			line[2] = buffer;
		else
			line[2] = "Empty";
		
		buffer = filterport_get_property(port,FILTERPORT_MAP_NODE);
		if(buffer)
			line[3] = buffer;
		else
			line[3]= "Unkown";
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
			line[0] = buffer;
		else
			line[0] = "Empty";
		buffer = filterparam_to_string(param);
		if(buffer)
			line[1] = buffer;
		else
			line[1] = "Empty";
		buffer = filterparam_get_property(param,FILTERPARAM_DESCRIPTION);
		if(buffer)
			line[2] = buffer;
		else
			line[2] = "Empty";
		buffer = filterparam_get_property(param,FILTERPARAM_MAP_NODE);
		if(buffer)
			line[3] = buffer;
		else
			line[3] = "Unkown";
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

void
glame_filtereditgui_install_accels(GtkWidget* window)
{
	glame_accel_install(window, "filteredit", NULL);
}


static SCM gls_editfilter_zoom_in(SCM parms)
{
	if(gh_number_p(parms)){
		glame_canvas_set_zoom(glcanvas,GNOME_CANVAS(glcanvas)->pixels_per_unit*gh_scm2double(parms));
	} else {
		/* choosing default */
		glame_canvas_zoom_in_cb(NULL,glcanvas);
	}
	return SCM_UNSPECIFIED;
}
static SCM gls_editfilter_zoom_out(SCM parms)
{
	if(gh_number_p(parms)){
		glame_canvas_set_zoom(glcanvas,GNOME_CANVAS(glcanvas)->pixels_per_unit/gh_scm2double(parms));
	} else {
		/* choosing default */
		glame_canvas_zoom_out_cb(NULL,glcanvas);
	}
	return SCM_UNSPECIFIED;
}
		

static SCM gls_editfilter_view_all()
{
	glame_canvas_view_all(glcanvas);
	return SCM_UNSPECIFIED;
}

static SCM gls_editfilter_delete_selection()
{
	GList *iter;
	iter = g_list_first(glcanvas->selectedItems);
	while(iter){
		glame_canvas_group_delete(GLAME_CANVAS_GROUP(iter->data));
		iter = g_list_next(iter);
	}
	g_list_free(glcanvas->selectedItems);
	glcanvas->selectedItems = NULL;
	return SCM_UNSPECIFIED;
}
	

static SCM gls_editfilter_selected_items()
{
	GList *selected;
	SCM s_items = SCM_LIST0;
	selected = g_list_first(glcanvas->selectedItems);
	while(selected){
		GlameCanvasFilter *filter = GLAME_CANVAS_FILTER(selected->data);
		s_items = gh_cons(gh_long2scm((long)filter),s_items);
		selected = g_list_next(selected);
	}
	return s_items;
}

static SCM gls_editfilter_group_selected()
{
	glame_canvas_group_selected(glcanvas);
	return SCM_UNSPECIFIED;
}

static SCM gls_editfilter_ungroup_selected()
{
	glame_canvas_ungroup_selected(glcanvas);
	return SCM_UNSPECIFIED;
}

static SCM gls_editfilter_new(SCM s_filter)
{
	SCM_ASSERT(filter_p(s_filter), s_filter, SCM_ARG1, "editfilter-new");
	gtk_widget_show(glame_filtereditgui_new(filter_creat(scm2filter(s_filter)), FALSE));
	return SCM_UNSPECIFIED;
}

void
glame_filtereditgui_init(void)
{
	gh_new_procedure0_0("editfilter-view-all",
			    gls_editfilter_view_all);
	gh_new_procedure0_1("editfilter-zoom-in",
			    gls_editfilter_zoom_in);
	gh_new_procedure0_1("editfilter-zoom-out",
			    gls_editfilter_zoom_out);
	gh_new_procedure0_0("editfilter-delete-selection",
			    gls_editfilter_delete_selection);
	gh_new_procedure0_0("editfilter-get-selection",
			    gls_editfilter_selected_items);
	gh_new_procedure0_0("editfilter-group-selection",
			    gls_editfilter_group_selected);
	gh_new_procedure0_0("editfilter-ungroup-selection",
			    gls_editfilter_ungroup_selected);
	gh_new_procedure1_0("editfilter-new",
			    gls_editfilter_new);

	nPopupTimeout = glame_config_get_long_with_default(
		"edit_filter/popupTimeout", 200);
	bMac = glame_config_get_long_with_default(
		"edit_filter/macMode", FALSE);
}



GtkType filteredit_gui_get_type(void);

static void filteredit_gui_destroy(GtkObject *filteredit)
{
	GnomeAppClass* parent_class;
	DPRINTF("Destroying filtereditgui\n");
	if (FILTEREDIT_GUI(filteredit)->pm_playing)
		filter_terminate(FILTEREDIT_GUI(filteredit)->canvas->net);
	parent_class = gtk_type_class(gnome_app_get_type());
	GTK_OBJECT_CLASS(parent_class)->destroy(filteredit);
}

static void filteredit_gui_class_init(FiltereditGuiClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = filteredit_gui_destroy;
}

static void filteredit_gui_init(FiltereditGui *filteredit)
{
	filteredit->canvas = NULL;
	filteredit->toolbar = NULL;
}

GtkType filteredit_gui_get_type(void)
{
	static GtkType filteredit_gui_type = 0;
	
	if (!filteredit_gui_type){
		GtkTypeInfo filteredit_gui_info = {
			"FiltereditGui",
			sizeof(FiltereditGui),
			sizeof(FiltereditGuiClass),
			(GtkClassInitFunc)filteredit_gui_class_init,
			(GtkObjectInitFunc)filteredit_gui_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		filteredit_gui_type = gtk_type_unique(
			gnome_app_get_type(), &filteredit_gui_info);
		gtk_type_set_chunk_alloc(filteredit_gui_type, 8);
	}

	return filteredit_gui_type;
}	

GtkWidget * 
glame_filtereditgui_new(filter_t *net, gboolean protected)
{
	FiltereditGui *window;
	GtkWidget *canvas, *sw, *toolbar;
	GnomeDock *dock;
	const char *name;
	
	if(net && filter_name(net))
		name = filter_name(net);
	else
		name = "Untitled";

	window = FILTEREDIT_GUI(gtk_type_new(filteredit_gui_get_type()));
	gnome_app_construct(GNOME_APP(window), "glame0.5", _(name));

	glame_filtereditgui_install_accels(GTK_WIDGET(window));
	dock = GNOME_DOCK(GNOME_APP(window)->dock);
	gtk_widget_ref(GTK_WIDGET(dock));
	
	gtk_widget_show(GTK_WIDGET(dock));
	toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,GTK_TOOLBAR_BOTH);
	window->toolbar = GTK_TOOLBAR(toolbar);

	sw = gtk_scrolled_window_new(NULL,NULL);
	gtk_widget_show(sw);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_widget_push_colormap((GdkColormap*)(gdk_rgb_get_cmap()));
	gtk_widget_push_visual(gdk_rgb_get_visual());
	canvas = GTK_WIDGET(glame_canvas_new(net));
	window->canvas = GLAME_CANVAS(canvas);
	gtk_widget_pop_visual();
	gtk_widget_pop_colormap();
//	GNOME_CANVAS(canvas)->aa = 1;
	GLAME_CANVAS(canvas)->openedUp = protected;
	//gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas),0,0,600,400);
	

	gtk_container_add(GTK_CONTAINER(sw),canvas);
	
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Execute","Executes Filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_EXEC),glame_canvas_execute_cb,window);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Register","Registers actual filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CONVERT),glame_canvas_register_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Save","Saves Filternetwork","foo",
				glame_load_icon_widget("save.png",24,24),
				glame_canvas_save_as_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Properties","Edit Filternetwork Properties","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_PROPERTIES),glame_canvas_property_dialog_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom in","Zooms in","foo",
				glame_load_icon_widget("zoom_in.png",24,24),
				glame_canvas_zoom_in_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom out","Zooms out","foo",
				glame_load_icon_widget("zoom_out.png",24,24),
				glame_canvas_zoom_out_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"View all","Adjusts scroll region","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_REFRESH),glame_canvas_view_all_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));

	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Add Last","Adds last filter","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_MULTIPLE),
				glame_canvas_add_last_cb, canvas);

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
	gtk_widget_show_all(GTK_WIDGET(window));
	gtk_signal_connect(GTK_OBJECT(canvas),"event",GTK_SIGNAL_FUNC(root_event),canvas);
#ifdef HAVE_LIBSTROKE
	stroke_init();
#endif
	return GTK_WIDGET(window);
}



void
glame_load_network(GtkWidget *foo, gpointer bla)
{
	GtkWidget *dialog;
	filter_t *filter;
	char filenamebuffer[256];

	filenamebuffer[0] = '\0';
	dialog = glame_dialog_file_request("Load filternetwork",
					   "editfilter:load", "Filename",
					   NULL, filenamebuffer);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(dialog))
	    || !filenamebuffer[0])
		return;
	
	filter = glame_load_instance(filenamebuffer);
	if (filter) {
		glame_filtereditgui_new(filter, FALSE);
	} else {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Error in loading network\nCheck out the glame-console output for more information")));
	} 
}

static void execute_cleanup(glsig_handler_t *handler, long sig, va_list va)
{
	FiltereditGui *gui;
	filter_t *n;
	gui = (FiltereditGui *)glsig_handler_private(handler);
	gui->pm_playing = 0;
	filter_terminate(gui->canvas->net);

	/* Scan network for swapfile_out nodes and issue gpsm invalidate
	 * signals. */
	filter_foreach_node(gui->canvas->net, n) {
		filter_param_t *changed_start, *changed_end, *filename;
		if (!(filename = filterparamdb_get_param(
			filter_paramdb(n), "filename"))
		    || !(changed_start = filterparamdb_get_param(
			    filter_paramdb(n), "changed_start"))
		    || !(changed_end = filterparamdb_get_param(
			    filter_paramdb(n), "changed_end")))
			continue;
		DPRINTF("Found swapfile_out node, issuing invalidate\n");
		gpsm_notify_swapfile_change(
			filterparam_val_int(filename),
			filterparam_val_int(changed_start),
			filterparam_val_int(changed_end)
			- filterparam_val_int(changed_start) + 1);
	}

	gtk_widget_destroy(g_list_nth(gtk_container_children(
		GTK_CONTAINER(gui->toolbar)), 0)->data);
	gtk_toolbar_insert_item(GTK_TOOLBAR(gui->toolbar),
				"Execute", "Executes Filternetwork", "Execute",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_EXEC),
				glame_canvas_execute_cb, gui, 0);
}

static void glame_canvas_execute_cb(GtkObject* foo, FiltereditGui *gui)
{
	glsig_emitter_t *emitter;

	/* A simple state machine, like the one in waveeditgui. */

	if (gui->pm_playing) {
		/* Playing - abort the network, cleanup will
		 * happen automagically. */
		filter_terminate(gui->canvas->net);
		return;
	}

	/* Not playing - setup and play.
	 */

	emitter = glame_network_notificator_creat(gui->canvas->net);
	glsig_add_handler(emitter, GLSIG_NETWORK_DONE,
			  execute_cleanup, gui);
	gui->pm_playing = 1;
	if (glame_network_notificator_run(emitter, 10) == -1) {
		gui->pm_playing = 0;
		glame_canvas_draw_errors(gui->canvas);
		return;
	}

	glame_canvas_reset_errors(gui->canvas);

	gtk_widget_destroy(g_list_nth(gtk_container_children(
		GTK_CONTAINER(gui->toolbar)), 0)->data);
	gtk_toolbar_insert_item(GTK_TOOLBAR(gui->toolbar),
				"Stop", "Stop", "Stop",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_STOP),
				glame_canvas_execute_cb, gui, 0);
}

static void glame_canvas_save_as_cb(GtkWidget*ignore,GlameCanvas *canvas)
{
	GtkWidget *dialog;
	GtkWidget *dialogVbox;
	char filenamebuffer[256] = "";
	char filternamebuffer[256] = "unnamed";
	char categorynamebuffer[256] = "unnamed";
	char *buffer;
	FILE* outf;

	/* Open a file request dialog with additional fields for
	 * filter name and category. */
	filenamebuffer[0] = '\0';
	dialog = glame_dialog_file_request("Save network as...",
					   "editfilter:saveas", "Filename",
					   NULL, filenamebuffer);
	dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));
	create_label_edit_pair(dialogVbox, "Filter name",
			       "editfilter:saveas:name", filternamebuffer);
	create_label_edit_pair(dialogVbox, "Category",
			       "editfilter:saveas:category", categorynamebuffer);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;

	if (!filenamebuffer[0]
	    || !filternamebuffer[0]
	    || !categorynamebuffer[0]) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Empty file/filter or category name")));
		return;
	}

	outf = fopen(filenamebuffer,"w");
	buffer = filter_to_string(GLAME_CANVAS(canvas)->net);
	DPRINTF("Network .scm is:\n%s\n", buffer);
	fprintf(outf, "(let ((newplugin (glame_plugin_define %s \"%s\")\n)) (if (filter? newplugin) newplugin (plugin_set newplugin PLUGIN_CATEGORY \"%s\")))", buffer, filternamebuffer, categorynamebuffer);
	free(buffer);
	fclose(outf);
}

static void glame_canvas_register_as_cb(gchar* name, GlameCanvas* glCanv)
{
	plugin_t *newplug;
	filter_t *copy;
	filter_t * bla = GLAME_CANVAS(glCanv)->net;
	
	newplug = plugin_add(name);
	copy = filter_creat(bla);
	filter_register(copy,newplug);
}

static void glame_canvas_register_cb(GtkWidget*ignore, GlameCanvas* canvas)
{	
	gnome_request_dialog(0, "Filtername", "filter", 16,
			     (GnomeStringCallback)glame_canvas_register_as_cb,
			     canvas, NULL);
}

static void glame_canvas_zoom_in_cb(GtkObject*foo,GlameCanvas* canv)
{
	glame_canvas_set_zoom(canv,GNOME_CANVAS(canv)->pixels_per_unit*1.5);
}

static void glame_canvas_zoom_out_cb(GtkObject*foo, GlameCanvas* canv)
{
	glame_canvas_set_zoom(canv,GNOME_CANVAS(canv)->pixels_per_unit/1.5);
}

static void glame_canvas_view_all_cb(GtkObject*foo, GlameCanvas* canv)
{
	glame_canvas_view_all(canv);
}

static void glame_canvas_add_last_cb(GtkObject* foo, GlameCanvas* canv)
{
	glame_canvas_add_last(canv);
}

static void window_close(GtkWidget *dummy, GtkWidget* window)
{
	gtk_widget_destroy(GTK_WIDGET(window));
}
