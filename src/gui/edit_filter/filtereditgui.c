/*
 * filtereditgui.c
 *
 * $Id: filtereditgui.c,v 1.66 2005/01/16 15:30:33 richi Exp $
 *
 * Copyright (C) 2001, 2002, 2003 Johannes Hirche
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/param.h>
#include <stdio.h>
#include <math.h>
#include <gnome.h>
#include <gtk/gtkstock.h>
#include <bonobo.h>
#include "glscript.h"
#include "util/glame_gui_utils.h"
#include "util/glame_dnd.h"
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
static void glame_canvas_register_cb(GtkWidget* ignore, FiltereditGui *gui);
static void glame_canvas_save_as_cb(GtkWidget* ignore, FiltereditGui *gui);
static void glame_canvas_property_dialog_cb(GtkObject*foo, FiltereditGui *gui);
static void glame_canvas_zoom_in_cb(GtkObject*foo, FiltereditGui *gui);
static void glame_canvas_zoom_out_cb(GtkObject*foo, FiltereditGui *gui);
static void glame_canvas_view_all_cb(GtkObject*foo, FiltereditGui *gui);
static void glame_canvas_add_last_cb(GtkObject* foo,  FiltereditGui *gui);
static void window_close(GtkWidget *dummy, GtkWidget* window);
static void filteredit_gui_data_received(GtkWidget *widget, GdkDragContext *drag_context,
					 gint x,gint y, GtkSelectionData *data,
					 guint info,guint time, gpointer user_data);

static void filteredit_gui_data_get(GtkWidget *widget, GdkDragContext *drag_context,
				    GtkSelectionData *data,
				    guint info, guint time, gpointer user_data);


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
root_event(GnomeCanvas * canvas, GdkEvent *event, GlameCanvas* glCanv);

gdouble lastx,lasty;
GnomeCanvasRect* selectFrame=NULL;

gint
glame_canvas_selecting(GlameCanvas* canvas,GdkEvent* event,GlameCanvas* glCanv)
{
	gdouble x,y;
	GList* selection;
	switch(event->type){
	case GDK_BUTTON_RELEASE:
		gtk_object_destroy(GTO(selectFrame));
		gtk_signal_disconnect_by_func(GTO(canvas),GTK_SIGNAL_FUNC(glame_canvas_selecting),glCanv);
		gtk_signal_handler_unblock_by_func(GTO(canvas),GTK_SIGNAL_FUNC(root_event),glCanv);
		gnome_canvas_window_to_world(GNOME_CANVAS(glCanv),event->button.x,event->button.y,&x,&y);
		selection = glame_canvas_find_items_in_region(glCanv,lastx,lasty,x,y);
		if(selection){
			GList *iter = g_list_first(selection);
			if(!(GDK_SHIFT_MASK&event->button.state))
				glame_canvas_select_clear(canvas);
			
			while(iter){
				glame_canvas_select_item(glCanv,GLAME_CANVAS_FILTER(iter->data));
				iter = g_list_next(iter);

			}
		}
					
		return TRUE;
		break;
	case GDK_MOTION_NOTIFY:
		if(selectFrame){
			gnome_canvas_window_to_world(GNOME_CANVAS(glCanv),
						     event->button.x,event->button.y,
						     &x,&y);
			
			gnome_canvas_item_set(GCI(selectFrame),
					      "x2",x,
					      "y2",y,NULL);
		}
		break;
	default:
		break;
		
	}
	return TRUE;
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
					gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,NULL,GTK_WIDGET(glcanvas));
					return TRUE;
				}
			}else{
				if(!onItem){
					gnome_canvas_window_to_world(GNOME_CANVAS(glCanv),
							     event->button.x,
							     event->button.y,
							     &lastx,&lasty);
					selectFrame = GNOME_CANVAS_RECT(gnome_canvas_item_new(
						 gnome_canvas_root(GNOME_CANVAS(glCanv)),
						 gnome_canvas_rect_get_type(),
						 "x1",lastx,
						 "x2",lastx,
						 "y1",lasty,
						 "y2",lasty,
						 "outline_color","black",
						 "width_units",1.0,
						 NULL));
				
						 
										     
					gtk_signal_handler_block_by_func(GTO(canvas),GTK_SIGNAL_FUNC(root_event),glCanv);
					gtk_signal_connect(GTO(canvas),"event",GTK_SIGNAL_FUNC(glame_canvas_selecting),glCanv);
					return TRUE;
				}
			}
			break;
		case 2:
			break;
		case 3:
			if(!onItem){
				GtkWidget* edit;
				menu = GTK_WIDGET(glame_gui_build_plugin_menu(NULL, add_filter_by_plugin_cb));
				if (glCanv->clipBoard) {
					edit = gtk_menu_item_new_with_label(_("_Paste"));
					gtk_widget_show(edit);
					gtk_signal_connect(GTK_OBJECT(edit),"activate",GTK_SIGNAL_FUNC(filtereditgui_paste_cb),glCanv);
					gtk_menu_append(GTK_MENU(menu), edit);
				}
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,NULL,GTK_WIDGET(glCanv));
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



static GnomeUIInfo redirection_menu_port[] = {
	GNOMEUIINFO_ITEM_STOCK(N_("Delete Port"),
			       NULL,
			       canvas_delete_redirection_port_cb,
			       GNOME_STOCK_MENU_TRASH),
	GNOMEUIINFO_END
};

static GnomeUIInfo redirection_menu_param[] = {
	GNOMEUIINFO_ITEM_STOCK(N_("Delete Parameter"),
			       NULL,
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
		if(gnome_popup_menu_do_popup_modal(menu,NULL,NULL,&event->button,port,GTK_WIDGET(CANVAS_ITEM_CANVAS(port)))>=0)
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
		if(gnome_popup_menu_do_popup_modal(menu,NULL,NULL,&event->button,foo,NULL)>=0)
			gtk_clist_remove(list,row);

}

void glame_canvas_property_dialog_cb(GtkObject* foo, FiltereditGui *window)
{
	GlameCanvas *canvas = window->canvas;
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
	
	static char *labels[] = {"Name","Type","Description","Source"};
	static char *plabels[] = {"Name","Value","Description","Source"};
	const char ** line;
	const char * buffer;
	filter = canvas->net;
	
	ports = filter_portdb(filter);
	
	notebook = gtk_notebook_new();
	
	dialog = gnome_dialog_new(_("Properties"),GNOME_STOCK_BUTTON_OK,NULL);
	


	tablabel = gtk_label_new(_("Ports"));
	list = GTK_CLIST(gtk_clist_new_with_titles(4,labels));
	gtk_clist_set_column_auto_resize(list,0,TRUE);
	gtk_clist_set_column_auto_resize(list,1,TRUE);
	gtk_clist_set_column_auto_resize(list,2,TRUE);
	gtk_clist_set_column_auto_resize(list,3,TRUE);
	
	filterportdb_foreach_port(ports,port){
		line = calloc(4,sizeof(char*));
		line[0] = filterport_label(port);
		line[1] = (filterport_is_input(port)?"In":"Out");
		buffer = filterport_get_property(port,FILTERPORT_DESCRIPTION);
		if(buffer)
			line[2] = buffer;
		else
			line[2] = _("Empty");
		buffer = filterport_get_property(port,FILTERPORT_MAP_NODE);
		if(buffer)
			line[3] = buffer;
		else
			line[3]= _("Unknown");
		redPorts = g_list_append(redPorts,port);
		gtk_clist_append(list,line);
	}
	//if(!canvas->net->openedUp)
		gtk_signal_connect(GTK_OBJECT(list),"select-row",GTK_SIGNAL_FUNC(redirection_list_port_cb),redPorts);
	gtk_widget_show(GTK_WIDGET(list));
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),GTK_WIDGET(list),tablabel);

	tablabel = gtk_label_new(_("Parameters"));
	gtk_widget_show(tablabel);
	
	list = GTK_CLIST(gtk_clist_new_with_titles(4,plabels));
	gtk_clist_set_column_auto_resize(list,0,TRUE);
	gtk_clist_set_column_auto_resize(list,1,TRUE);
	gtk_clist_set_column_auto_resize(list,2,TRUE);
	gtk_clist_set_column_auto_resize(list,3,TRUE);
	
	params = filter_paramdb(filter);
	filterparamdb_foreach_param(params,param){
		line = calloc(4,sizeof(char*));
		line[0] = filterparam_label(param);
		buffer = filterparam_to_string(param);
		if(buffer)
			line[1] = buffer;
		else
			line[1] = _("Empty");
		buffer = filterparam_get_property(param,FILTERPARAM_DESCRIPTION);
		if(buffer)
			line[2] = buffer;
		else
			line[2] = _("Empty");
		buffer = filterparam_get_property(param,FILTERPARAM_MAP_NODE);
		if(buffer)
			line[3] = buffer;
		else
			line[3] = _("Unknown");
		redParms = g_list_append(redParms,param);
		gtk_clist_append(list,line);
	}
	gtk_widget_show(GTK_WIDGET(list));
	gtk_signal_connect(GTK_OBJECT(list),"select-row",GTK_SIGNAL_FUNC(redirection_list_param_cb),redParms);
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
	double factor = 1.5;
	if (gh_number_p(parms))
		factor = gh_scm2double(parms);
	glame_canvas_set_zoom(glcanvas,GNOME_CANVAS(glcanvas)->pixels_per_unit*factor);
	return SCM_UNSPECIFIED;
}
static SCM gls_editfilter_zoom_out(SCM parms)
{
	double factor = 1.5;
	if (gh_number_p(parms))
		factor = gh_scm2double(parms);
	glame_canvas_set_zoom(glcanvas,GNOME_CANVAS(glcanvas)->pixels_per_unit/factor);
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
	

static SCM gls_editfilter_get_selection()
{
	GList *selected;
	SCM s_items = SCM_EOL;
	selected = glame_canvas_get_selected_items(glcanvas);
	while(selected){
		filter_t *filter = (filter_t *)selected->data;
		s_items = scm_cons(filter2scm(filter), s_items);
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

static SCM gls_editfilter_set_clipboard(SCM s_net)
{
	SCM_ASSERT(filter_p(s_net),
		   s_net, SCM_ARG1, "editfilter-set-clipboard!");
	glcanvas->clipBoard = scm2filter(s_net);
	scminvalidatefilter(s_net);
	return SCM_UNSPECIFIED;
}

static SCM gls_editfilter_clipboard()
{
	if (!glcanvas->clipBoard)
		GLAME_THROW();
	return filter2scm(glcanvas->clipBoard);
}

static SCM gls_editfilter_collapse_selection()
{
	glame_canvas_collapse_selection(glcanvas);
	return SCM_UNSPECIFIED;
}

static SCM gls_editfilter_expand_selected()
{
	GlameCanvasFilter* gfilter;
	
	GList * selection = glame_canvas_get_selected_items(glcanvas);
	if(selection)
		if(g_list_length(selection)==1){
			GList * first = g_list_first(selection);
			gfilter = glame_canvas_find_filter(first->data);
			if(gfilter)
				glame_canvas_filter_expand_node(gfilter);
		}
	return SCM_UNSPECIFIED;
}

static SCM gls_editfilter_network()
{
	return filter2scm(glcanvas->net);
}

static SCM gls_editfilter_redraw()
{
	glame_canvas_full_redraw(glcanvas);
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
			    gls_editfilter_get_selection);
	gh_new_procedure0_0("editfilter-group-selection",
			    gls_editfilter_group_selected);
	gh_new_procedure0_0("editfilter-ungroup-selection",
			    gls_editfilter_ungroup_selected);
	gh_new_procedure1_0("editfilter-new",
			    gls_editfilter_new);
	gh_new_procedure0_0("editfilter-collapse-selection",
			    gls_editfilter_collapse_selection);
	gh_new_procedure0_0("editfilter-expand-selected",
			    gls_editfilter_expand_selected);
	gh_new_procedure0_0("editfilter-clipboard",
			    gls_editfilter_clipboard);
	gh_new_procedure1_0("editfilter-set-clipboard!",
			    gls_editfilter_set_clipboard);
	gh_new_procedure0_0("editfilter-network",
			    gls_editfilter_network);
	gh_new_procedure0_0("editfilter-redraw",
			    gls_editfilter_redraw);
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
	FILTEREDIT_GUI(filteredit)->deleted = TRUE;
	parent_class = gtk_type_class(gnome_app_get_type());
	GTK_OBJECT_CLASS(parent_class)->destroy(filteredit);
}

static void filteredit_gui_class_init(FiltereditGuiClass *klass)
{
	GtkObjectClass *object_class;
	GtkWidgetClass *widget_class;
	object_class = GTK_OBJECT_CLASS(klass);
	object_class->destroy = filteredit_gui_destroy;
	widget_class = GTK_WIDGET_CLASS(klass);
	widget_class->drag_data_received = filteredit_gui_data_received;
	widget_class->drag_data_get = filteredit_gui_data_get;
}

static void filteredit_gui_init(FiltereditGui *filteredit)
{
	filteredit->deleted = FALSE;
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
	}

	return filteredit_gui_type;
}	

static void glame_canvas_copy_selected_w_cb(GtkWidget *w, FiltereditGui *gui)
{
	glame_accel_widget_data_cb(NULL, "filteredit/y");
}
static void glame_canvas_paste_selected_w_cb(GtkWidget *w, FiltereditGui *gui)
{
	glame_accel_widget_data_cb(NULL, "filteredit/p");
}
static void glame_canvas_delete_selected_w_cb(GtkWidget *w, FiltereditGui *gui)
{
	glame_gh_safe_eval_str("(editfilter-delete-selection)");
}
static void glame_canvas_group_selected_w_cb(GtkWidget *w, FiltereditGui *gui)
{
	glame_gh_safe_eval_str("(editfilter-group-selection)");
}
static void glame_canvas_collapse_selected_w_cb(GtkWidget *w, FiltereditGui *gui)
{
	glame_gh_safe_eval_str("(editfilter-collapse-selection)");
}
static void help_cb(GtkWidget *menu, void *blah)
{
	glame_help_goto(NULL, "info:glame#The_Filternetwork_Editor");
}
static void list_keybindings_cb(GtkWidget *menu, void *blah)
{
	glame_accel_widget_data_cb(NULL, "list_keybindings_filteredit");
}

static GnomeUIInfo window_file_menu[] = {
        GNOMEUIINFO_ITEM(N_("Save as..."), NULL, glame_canvas_save_as_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Register..."), NULL, glame_canvas_register_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Properties..."), NULL, glame_canvas_property_dialog_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Execute"), NULL, glame_canvas_execute_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_MENU_CLOSE_ITEM(window_close, NULL),
        GNOMEUIINFO_END
};
static GnomeUIInfo window_edit_menu[] = {
        GNOMEUIINFO_ITEM(N_("Copy"), NULL, glame_canvas_copy_selected_w_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Paste"), NULL, glame_canvas_paste_selected_w_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Delete"), NULL, glame_canvas_delete_selected_w_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Group"), NULL, glame_canvas_group_selected_w_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Collapse"), NULL, glame_canvas_collapse_selected_w_cb, NULL),
        GNOMEUIINFO_END
};
static GnomeUIInfo window_view_menu[] = {
        GNOMEUIINFO_ITEM(N_("Zoom in"), NULL, glame_canvas_zoom_in_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Zoom out"), NULL, glame_canvas_zoom_out_cb, NULL),
        GNOMEUIINFO_ITEM(N_("View all"), NULL, glame_canvas_view_all_cb, NULL),
        GNOMEUIINFO_END
};
static GnomeUIInfo window_help_menu[] = {
	GNOMEUIINFO_ITEM(N_("_Help"),N_("Opens a gnome help browser"), help_cb, NULL),
	GNOMEUIINFO_ITEM(N_("List key-bindings"), N_("Lists the current key-bindings"), list_keybindings_cb, NULL),
	GNOMEUIINFO_END
};
static GnomeUIInfo window_menu[] = {
	{
            GNOME_APP_UI_SUBTREE, N_("_Network"),
            NULL,
            window_file_menu, NULL, NULL,
            GNOME_APP_PIXMAP_NONE, NULL,
            0, 0, NULL
        },
	{
            GNOME_APP_UI_SUBTREE, N_("_Edit"),
            NULL,
            window_edit_menu, NULL, NULL,
            GNOME_APP_PIXMAP_NONE, NULL,
            0, 0, NULL
        },
	{
            GNOME_APP_UI_SUBTREE, N_("_View"),
            NULL,
            window_view_menu, NULL, NULL,
            GNOME_APP_PIXMAP_NONE, NULL,
            0, 0, NULL
        },
	GNOMEUIINFO_MENU_HELP_TREE (window_help_menu),
	GNOMEUIINFO_END
};


GtkWidget * 
glame_filtereditgui_new(filter_t *net, gboolean prot)
{
	FiltereditGui *window;
	GtkWidget *canvas, *sw, *toolbar;
	BonoboDock *dock;
	const char *name;
	GtkTargetEntry desttargets[] = { GLAME_DND_TARGET_STRING_SCM, 
					 GLAME_DND_TARGET_STRING_SCM_NETWORK,
					 GLAME_DND_TARGET_POINTER_FILTER_T,
					 };
	
	if(net && filter_name(net))
		name = filter_name(net);
	else
		name = "Untitled";

	window = FILTEREDIT_GUI(gtk_type_new(filteredit_gui_get_type()));
	gnome_app_construct(GNOME_APP(window), "glame0.7", name);

	glame_filtereditgui_install_accels(GTK_WIDGET(window));
	dock = BONOBO_DOCK(GNOME_APP(window)->dock);
	gtk_widget_ref(GTK_WIDGET(dock));
	
	gtk_widget_show(GTK_WIDGET(dock));
	toolbar = gtk_toolbar_new();
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
	GLAME_CANVAS(canvas)->openedUp = prot;
	//gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas),0,0,600,400);
	

	gtk_container_add(GTK_CONTAINER(sw),canvas);
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_EXECUTE,_("Execute"),_("Executes Filternetwork"),GTK_SIGNAL_FUNC(glame_canvas_execute_cb),window,-1);

	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_CONVERT,_("Register"),_("Registers actual filternetwork"),GTK_SIGNAL_FUNC(glame_canvas_register_cb),window,-1);
	
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));

	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),_("Save"),_("Saves Filternetwork"),"foo",
				glame_load_icon_widget("save.png",24,24),
				GTK_SIGNAL_FUNC(glame_canvas_save_as_cb),window);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_PROPERTIES,_("Properties"),_("Edit Filternetwork Properties"),GTK_SIGNAL_FUNC(glame_canvas_property_dialog_cb),window,-1);

	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_ZOOM_IN,_("Zoom in"),_("Adjust scroll region"),GTK_SIGNAL_FUNC(glame_canvas_zoom_in_cb),window,-1);
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_ZOOM_OUT,_("Zoom out"),_("Adjust scroll region"),GTK_SIGNAL_FUNC(glame_canvas_zoom_out_cb),window,-1);
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_ZOOM_FIT,_("View all"),_("Adjust scroll region"),GTK_SIGNAL_FUNC(glame_canvas_view_all_cb),window,-1);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_COPY,_("Add last"),_("Adds last filter"),GTK_SIGNAL_FUNC(glame_canvas_add_last_cb), window,-1);

	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_CLOSE,_("Close"),_("Close"),GTK_SIGNAL_FUNC(window_close),window,-1);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),GTK_STOCK_HELP,_("Help"),_("Help"),GTK_SIGNAL_FUNC(glame_help_goto),"info:glame#The_Filternetwork_Editor",-1);

        /* Create menubar - FIXME copy all uiinfos, restructure to
	 * match nice menu layout, etc.
	 * menu hints not w/o status bar */
        gnome_app_create_menus_with_data(GNOME_APP(window), window_menu, window);
        //gnome_app_install_menu_hints(GNOME_APP(window), window_menu);

	gnome_app_set_toolbar(GNOME_APP(window), GTK_TOOLBAR(toolbar));
#if 0
	gnome_app_add_toolbar(GNOME_APP(window), GTK_TOOLBAR(toolbar),
			      "canvas::toolbar",
			      BONOBO_DOCK_ITEM_BEH_EXCLUSIVE|BONOBO_DOCK_ITEM_BEH_NEVER_FLOATING,
			      BONOBO_DOCK_TOP, 0, 0, 0);
#endif

	gnome_app_set_contents(GNOME_APP(window),sw);	
	gtk_widget_show(GTK_WIDGET(dock));
	
	gtk_window_set_default_size(GTK_WINDOW(window),400,300);
	gtk_widget_show_all(GTK_WIDGET(window));
	gtk_signal_connect(GTK_OBJECT(canvas),"event",GTK_SIGNAL_FUNC(root_event),canvas);

	
	gtk_drag_source_set(GTK_WIDGET(window),GDK_BUTTON2_MASK,desttargets,3,GDK_ACTION_MOVE);
	gtk_drag_dest_set(GTK_WIDGET(window),GTK_DEST_DEFAULT_MOTION | GTK_DEST_DEFAULT_HIGHLIGHT | GTK_DEST_DEFAULT_DROP, desttargets,3,GDK_ACTION_COPY|GDK_ACTION_MOVE|GDK_ACTION_DEFAULT);


	return GTK_WIDGET(window);
}



void
glame_load_network(GtkWidget *foo, gpointer bla)
{
	GtkWidget *dialog;
	filter_t *filter;
	char filenamebuffer[256];

	filenamebuffer[0] = '\0';
	dialog = glame_dialog_file_request(_("Load filternetwork"),
					   "editfilter:load", _("Filename"),
					   NULL, filenamebuffer);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(dialog))
	    || !filenamebuffer[0])
		return;
	
	filter = glame_load_instance(filenamebuffer);
	if (filter) {
		GtkWidget *feg;
		feg = glame_filtereditgui_new(filter, FALSE);
		gtk_quit_add_destroy(1, GTK_OBJECT(feg));
		gtk_widget_show(feg);
	} else {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog(_("Error in loading network\nCheck out the glame-console output for more information"))));
	} 
}

static void execute_cleanup(glsig_handler_t *handler, long sig, va_list va)
{
	FiltereditGui *gui;
	filter_t *n;

	gui = (FiltereditGui *)glsig_handler_private(handler);
	if (gui->deleted)
		return;

	gui->pm_playing = 0;
	filter_terminate(gui->canvas->net->launch_context);

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
			filterparam_val_long(filename),
			filterparam_val_long(changed_start),
			filterparam_val_long(changed_end)
			- filterparam_val_long(changed_start) + 1);
	}

	gtk_widget_destroy(g_list_nth(gtk_container_children(
		GTK_CONTAINER(gui->toolbar)), 0)->data);
	gtk_toolbar_insert_stock(GTK_TOOLBAR(gui->toolbar),
				_("Execute"), _("Executes Filternetwork"),
				GNOME_STOCK_PIXMAP_EXEC,
				GTK_SIGNAL_FUNC(glame_canvas_execute_cb), gui, 0);
}

static void glame_canvas_execute_cb(GtkObject* foo, FiltereditGui *gui)
{
	glsig_emitter_t *emitter;

	/* A simple state machine, like the one in waveeditgui. */

	if (gui->pm_playing) {
		/* Playing - abort the network, cleanup will
		 * happen automagically. */
		filter_terminate(gui->canvas->net->launch_context);
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
	gtk_toolbar_insert_stock(GTK_TOOLBAR(gui->toolbar),
				 _("Stop"), _("Stop"), 
				 GNOME_STOCK_PIXMAP_STOP,
				 GTK_SIGNAL_FUNC(glame_canvas_execute_cb), gui, 0);
}

static void glame_canvas_save_as_cb(GtkWidget*ignore, FiltereditGui *window)
{
	GlameCanvas *canvas = window->canvas;
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
	dialog = glame_dialog_file_request(_("Save network as..."),
					   "editfilter:saveas", _("Filename"),
					   NULL, filenamebuffer);
	dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));
	create_label_edit_pair(dialogVbox, _("Filter name"),
			       "editfilter:saveas:name", filternamebuffer);
	create_label_edit_pair(dialogVbox, _("Category"),
			       "editfilter:saveas:category", categorynamebuffer);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;

	if (!filenamebuffer[0]
	    || !filternamebuffer[0]
	    || !categorynamebuffer[0]) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog(_("Empty file/filter or category name"))));
		return;
	}

	outf = fopen(filenamebuffer,"w");
	buffer = filter_to_string(GLAME_CANVAS(canvas)->net);
	DPRINTF("Network .scm is:\n%s\n", buffer);
	fprintf(outf, "(let ((newplugin (glame_plugin_define %s \"%s\")\n)) (if (filter? newplugin) newplugin (plugin-set! newplugin PLUGIN_CATEGORY \"%s\")))", buffer, filternamebuffer, categorynamebuffer);
	free(buffer);
	fclose(outf);
}

static void glame_canvas_register_as_cb(gchar* name, GlameCanvas *glCanv)
{
	plugin_t *newplug;
	filter_t *copy;
	filter_t * bla = GLAME_CANVAS(glCanv)->net;
	
	newplug = plugin_add(name);
	copy = filter_creat(bla);
	filter_register(copy,newplug);
}

static void glame_canvas_register_cb(GtkWidget*ignore, FiltereditGui *window)
{	
	GlameCanvas *canvas = window->canvas;
	gnome_request_dialog(0, _("Filtername"), "filter", 16,
			     (GnomeStringCallback)glame_canvas_register_as_cb,
			     canvas, NULL);
}

static void glame_canvas_zoom_in_cb(GtkObject*foo, FiltereditGui *window)
{
	GlameCanvas *canv = window->canvas;
	glame_canvas_set_zoom(canv,GNOME_CANVAS(canv)->pixels_per_unit*1.5);
}

static void glame_canvas_zoom_out_cb(GtkObject*foo, FiltereditGui *window)
{
	GlameCanvas *canv = window->canvas;
	glame_canvas_set_zoom(canv,GNOME_CANVAS(canv)->pixels_per_unit/1.5);
}

static void glame_canvas_view_all_cb(GtkObject*foo, FiltereditGui *window)
{
	GlameCanvas *canv = window->canvas;
	glame_canvas_view_all(canv);
}

static void glame_canvas_add_last_cb(GtkObject* foo, FiltereditGui *window)
{
	GlameCanvas *canv = window->canvas;
	glame_canvas_add_last(canv);
}

static void window_close(GtkWidget *dummy, GtkWidget* window)
{
	if (FILTEREDIT_GUI(window)->pm_playing)
		filter_terminate(FILTEREDIT_GUI(window)->canvas->net->launch_context);
	gtk_widget_destroy(GTK_WIDGET(window));
}

static void 
filteredit_gui_data_received(GtkWidget *widget,
                                            GdkDragContext *drag_context,
                                            gint x,
                                            gint y,
                                            GtkSelectionData *data,
                                            guint info,
                                            guint time,
                                            gpointer user_data)
{
	
	fprintf(stderr,"data received: %s %d\n",data->data,info);
}

static void 
filteredit_gui_data_get      (GtkWidget *widget,
                                            GdkDragContext *drag_context,
                                            GtkSelectionData *data,
                                            guint info,
                                            guint time,
                                            gpointer user_data)
{
	char * bla=strdup("ficken\0");
	gtk_selection_data_set(data,data->target,8,bla,strlen(bla)+1);
	fprintf(stderr,"data get %d\n",info);
}
