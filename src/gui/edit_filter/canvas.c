/*
 * canvas.c
 *
 * $Id: canvas.c,v 1.65 2001/04/19 16:10:46 xwolf Exp $
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
 */


#include "glmid.h"
#include "canvas.h"
#include <values.h>
/*******************************************************
 * Turn on super annoying event debugging. 
 * nobody wants this apart from me :-) 
 * xwolf
 *******************************************************/
//#define EVENT_DEBUGGING 1

#ifdef HAVE_GCC
#ifdef EVENT_DEBUGGING
#define DPRINTF_FOO(x,bar...) DPRINTF( x, ## bar )
#else
#define DPRINTF_FOO(x,bar...)
#endif
#else
#define DPRINTF_FOO(x,bar...)
#endif
static void canvas_item_show_properties(GnomeCanvasItem * item);
static void canvas_item_delete_property_list(gpointer item, gpointer bla);
static void canvas_item_hide_properties(GnomeCanvasItem * item);
static int canvas_add_filter_by_name(const char *name);
void network_draw_error(gui_network *net);
void network_error_reset(gui_network *net);
static void network_play(GtkWidget *button,GlameCanvas *glCanv);
static gint root_event(GnomeCanvas *canv,GdkEvent*event,GlameCanvas* data);
static void canvas_port_calculate_docking_coords(GlameCanvasPort* port,double *x, double *y, int id);
static gint canvas_connection_update_points(GlameConnection *connection);
static gint canvas_connection_do_connect(GlameConnection* connection);
static gint canvas_output_port_motion(GnomeCanvasItem *pitem,GdkEvent *event, gpointer data);
static void canvas_outout_port_reorder_connections(GlameCanvasPort* port);
static void canvas_input_port_reorder_connections(GlameCanvasPort* port);
static gint canvas_connection_select(GnomeCanvasItem* item, GdkEvent *event,gpointer data);
static GtkObject* canvas_add_node_from_filter(GnomeCanvas *canvas, filter_t *filter,double x, double y);
static void canvas_connection_connect_from_pipe(GlameConnection *c);
static int canvas_connection_connect(GlameConnection *c);
static void canvas_input_port_update_connections(GlameCanvasPort*p, gdouble x, gdouble y);
static void canvas_output_port_update_connections(GlameCanvasPort*p, gdouble x, gdouble y);
static void canvas_item_destroy(GlameCanvasItem* it);
static void canvas_connection_destroy(GlameConnection* connection);
static void set_file_selection_filter(GnomeFileEntry* entry, const char * filter);
static void canvas_update_scroll_region(GlameCanvas* canv);
static void update_string(GtkListItem* item,char ** returnbuffer);
static void update_entry_text(GtkListItem* item,GtkEntry* entry);

/* Menu callbacks */
static void canvas_item_edit_properties_cb(GtkWidget* m,GlameCanvasItem *item);
static void canvas_item_delete_cb(GtkWidget* m,GlameCanvasItem* it);
static void canvas_connection_edit_source_properties_cb(GtkWidget* bla, GlameConnection* conn);
static void canvas_connection_edit_dest_properties_cb(GtkWidget* bla, GlameConnection* conn);
static void canvas_add_filter_by_name_cb(GtkWidget*wid, plugin_t *plugin);
static gint canvas_input_port_event_cb(GnomeCanvasItem*item,GdkEvent* event, gpointer data);
static void canvas_connection_destroy_cb(GtkWidget*bla,GlameConnection* conn);
static gint canvas_output_port_event_cb(GnomeCanvasItem*item,GdkEvent* event, gpointer data);
static void canvas_register_as_cb(gchar* name, GlameCanvas* data);
static void register_filternetwork_cb(GtkWidget*bla,GlameCanvas *data);
static void draw_network_cb(GtkWidget *bla, GlameCanvasItem *item);

static void canvas_item_redirect_parameters(GtkWidget *bla, GlameCanvasItem *item);
static void canvas_item_show_description(GtkWidget* wid,GlameCanvasItem* it);
void canvas_load_network(GtkWidget *bla, void *blu);
static void canvas_save_as(GtkWidget*w,GlameCanvas *data);
static void canvas_port_redirect(GtkWidget*bla,GlameCanvasPort *blu);
static void canvas_zoom_in_cb(GtkWidget*bla, GlameCanvas* canv);
static void canvas_zoom_out_cb(GtkWidget*bla, GlameCanvas* canv);
static void canvas_update_scroll_region_cb(GtkWidget*bla, GlameCanvas* canv);

guint nPopupTimeout = 200;
gboolean bMac;
static guint nPopupTimeoutId;
int event_x,event_y;
GtkWidget *win;

int inItem;
static GnomeUIInfo node_menu[]=
{
	GNOMEUIINFO_MENU_PROPERTIES_ITEM(canvas_item_edit_properties_cb,NULL),
	GNOMEUIINFO_ITEM("_Redirect parameter","redirect",canvas_item_redirect_parameters,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("_Delete","Delete node",canvas_item_delete_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("_Open Up","Open up",draw_network_cb,NULL),
	GNOMEUIINFO_ITEM("_About node...","bout",canvas_item_show_description,NULL),
//	GNOMEUIINFO_ITEM("Reroute","Reroute from this item",reroute_cb,NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo pipe_menu[]=
{
	GNOMEUIINFO_ITEM("_Source properties...", "Source properties", canvas_connection_edit_source_properties_cb, NULL),
	GNOMEUIINFO_ITEM("D_estination properties...", "Destination properties", canvas_connection_edit_dest_properties_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("_Delete","Delete pipe",canvas_connection_destroy_cb,NULL),
	GNOMEUIINFO_END
};


GnomeUIInfo *node_select_menu;

static GnomeUIInfo root_menu[]=
{
	GNOMEUIINFO_SUBTREE("_Add Node...",&node_select_menu),
	GNOMEUIINFO_END
};

static GnomeUIInfo port_menu[] = 
{
	GNOMEUIINFO_ITEM("_Connect to external port","Connect",canvas_port_redirect,NULL),
	GNOMEUIINFO_END
};


static void canvas_item_edit_properties_cb(GtkWidget* m,GlameCanvasItem *item)
{
	GtkWidget *p;

	if(item->immutable)
		return;
	p = glame_gui_filter_properties(filter_paramdb(item->filter),
						   filter_name(item->filter));
	gnome_dialog_run_and_close(GNOME_DIALOG(p));
}

static void canvas_item_delete_cb(GtkWidget* m,GlameCanvasItem* it)
{
	if(!it->undeletable)
		canvas_item_destroy(it);
}

static void canvas_connection_edit_source_properties_cb(GtkWidget* bla, GlameConnection* conn)
{
	GtkWidget *p = glame_gui_filter_properties(filterpipe_sourceparamdb(conn->pipe),
						   filterport_label(conn->begin->port));
	gnome_dialog_run_and_close(GNOME_DIALOG(p));
}

static void canvas_connection_edit_dest_properties_cb(GtkWidget* bla, GlameConnection* conn)
{
	GtkWidget *p = glame_gui_filter_properties(filterpipe_destparamdb(conn->pipe),
						   filterport_label(conn->end->port));
	gnome_dialog_run_and_close(GNOME_DIALOG(p));
}

static void 
canvas_item_show_properties(GnomeCanvasItem * item)
{
	char fontbuffer[256];
	GnomeCanvasItem * text;
	filter_param_t * iter;
	GlameCanvasItem* gitem = GLAME_CANVAS_ITEM(item->parent);
	float y_coord=100.0;
	char buffer[100];
	gint fontsize;
	
	if(gitem->timeout_id){
		gtk_timeout_remove(gitem->timeout_id);
		gitem->timeout_id = 0;
	}
	if(GLAME_CANVAS_ITEM(item->parent)->property_texts){
		DPRINTF("props != zero!\n");
		return;
	}
	fontsize = (int)(GNOME_CANVAS(item->canvas)->pixels_per_unit*12.0);
	fontsize = (fontsize<2.0)?2.0:fontsize;
	sprintf(fontbuffer,CANVAS_FONT_STRING,fontsize);
	filterparamdb_foreach_param(filter_paramdb(GLAME_CANVAS_ITEM(item->parent)->filter),iter){
		sprintf(buffer,"%s %s",filterparam_label(iter),filterparam_to_string(iter));
		text = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item->parent),
					     gnome_canvas_text_get_type(),
					     "x",0.0,
					     "y",y_coord,
					     "text",buffer,
					     "clip_width",94.0,
					     "clip_height",16.0,
					     "fill_color","black",
					     "anchor",GTK_ANCHOR_WEST,
					     "justification",GTK_JUSTIFY_LEFT, 
					     "font", fontbuffer,
					     "clip",0,
					     NULL);
		y_coord+=16.0;
		GLAME_CANVAS_ITEM(item->parent)->property_texts = g_list_append(GLAME_CANVAS_ITEM(item->parent)->property_texts,text);
		
	}
	y_coord+=10.0;
	if(filter_has_error(GLAME_CANVAS_ITEM(item->parent)->filter)){
		sprintf(buffer,"ERROR: %s",filter_errstr(GLAME_CANVAS_ITEM(item->parent)->filter));
		text = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item->parent),
					     gnome_canvas_text_get_type(),
					     "x",0.0,
					     "y",y_coord,
					     "text",buffer,
					     "clip_width",94.0,
					     "clip_height",16.0,
					     "fill_color","red",
					     "anchor",GTK_ANCHOR_WEST,
					     "justification",GTK_JUSTIFY_LEFT, 
					     "font", fontbuffer,
					     "clip",0,
					     NULL);
		GLAME_CANVAS_ITEM(item->parent)->property_texts = g_list_append(GLAME_CANVAS_ITEM(item->parent)->property_texts,text);
			
	}
	gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(item->parent));

}

static void 
show_port_properties(GlameCanvasPort * item)
{
	GnomeCanvasItem * text;
	char fontbuffer[256];	
	float y_coord=100.0;
	char buffer[100];
	int inOut;
	gint fontsize;
	gtk_timeout_remove(nPopupTimeoutId);
	
	if(item->property_texts){
		DPRINTF("props != zero!\n");
		return;
	}

	inOut = (item->port_type&GUI_PORT_TYPE_IN)?1:0;
	fontsize = (int)(GNOME_CANVAS(GNOME_CANVAS_ITEM(item)->canvas)->pixels_per_unit*12.0);
	fontsize = (fontsize<2.0)?2.0:fontsize;
	sprintf(fontbuffer,CANVAS_FONT_STRING,fontsize);
	sprintf(buffer,"Name: %s",filterport_label(item->port));
	text = gnome_canvas_item_new(GNOME_CANVAS_GROUP(GNOME_CANVAS_ITEM(item)->parent),
				     gnome_canvas_text_get_type(),
				     "x",(inOut?0.0:100.0),
				     "y",y_coord,
				     "text",buffer,
				     "clip_width",94.0,
				     "clip_height",16.0,
				     "fill_color","blue",
				     "anchor",(inOut?GTK_ANCHOR_EAST:GTK_ANCHOR_WEST),
				     "justification",(inOut?GTK_JUSTIFY_RIGHT:GTK_JUSTIFY_LEFT), 
				     "font", fontbuffer,
				     "clip",0,
				     NULL);
	y_coord+=16.0;
	item->property_texts = g_list_append(item->property_texts,text);
	
#if 0 /* these are just the default parameters. */
	filterparamdb_foreach_param(filterport_paramdb(item->port),iter){
		sprintf(buffer,"%s %s",filterparam_label(iter),filterparam_to_string(iter));
		text = gnome_canvas_item_new(GNOME_CANVAS_GROUP(GNOME_CANVAS_ITEM(item)->parent),
					     gnome_canvas_text_get_type(),
					     "x",(inOut?0.0:100.0),
					     "y",y_coord,
					     "text",buffer,
					     "clip_width",94.0,
					     "clip_height",16.0,
					     "fill_color","black",
					     "anchor",(inOut?GTK_ANCHOR_EAST:GTK_ANCHOR_WEST),
					     "justification",(inOut?GTK_JUSTIFY_RIGHT:GTK_JUSTIFY_LEFT), 
					     "font", fontbuffer,
					     "clip",0,
					     NULL);
		y_coord+=16.0;
		item->property_texts = g_list_append(item->property_texts,text);
		
	}
#endif	
}


static void 
hide_port_properties(GlameCanvasPort * item)
{
        if(!item->property_texts){
		return;
	}
	g_list_foreach(item->property_texts,canvas_item_delete_property_list,NULL);
	g_list_free(item->property_texts);
	item->property_texts=NULL;
}

static void
canvas_item_delete_property_list(gpointer item,gpointer bla)
{
        gtk_object_destroy(GTK_OBJECT(item));
}
static void 
canvas_item_hide_properties(GnomeCanvasItem * item)
{
        if(!GLAME_CANVAS_ITEM(item->parent)->property_texts){
		return;
	}
	g_list_foreach(GLAME_CANVAS_ITEM(item->parent)->property_texts,canvas_item_delete_property_list,NULL);
	g_list_free(GLAME_CANVAS_ITEM(item->parent)->property_texts);
	GLAME_CANVAS_ITEM(item->parent)->property_texts=NULL;
}

gint
canvas_item_node_selected(GnomeCanvasItem*item, GdkEvent *event, gpointer data)
{
	GdkCursor *fleur;
	double x,y;
	double dx,dy;
	GlameCanvasItem *it = GLAME_CANVAS_ITEM(data);
	GList *list;
	GtkWidget* menu;
	char numberbuffer[10];
	x = event->button.x;
	y = event->button.y;
	DPRINTF_FOO("Ev: %d\n",event->type);
	if(!GLAME_IS_CANVAS_ITEM(data)){
		DPRINTF("Not a GlameCanvasItem!\n");
		return FALSE;
	}
	switch(event->type){
	case GDK_ENTER_NOTIFY:
	      DPRINTF_FOO("enter\n");
	      inItem=1;
	      if(it->timeout_id){
		      DPRINTF("BUGBUG!\ntimeout_id not 0!\nRemoving anyway\n");
		      gtk_timeout_remove(it->timeout_id);
	      }
	      it->timeout_id = gtk_timeout_add(nPopupTimeout,(GtkFunction)canvas_item_show_properties,item);
	      //canvas_item_show_properties(item);
	      break;
	case GDK_LEAVE_NOTIFY:
		DPRINTF_FOO("leave\n");
		if(it->dragging){
			DPRINTF("Leave while dragging!\n");
			
			gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(data),event->button.time);
			it->dragging=FALSE;
			goto foo;
		}
	      inItem=0;
	      if(it->timeout_id)
		      gtk_timeout_remove(it->timeout_id);
	      it->timeout_id = 0;
	      canvas_item_hide_properties(item);
	foo:
	      break;
	case GDK_BUTTON_RELEASE:
		DPRINTF_FOO("Release\n");
		gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(data),event->button.time);
		it->dragging = FALSE;
		//update coord.strings
		sprintf(numberbuffer,"%8f",GNOME_CANVAS_ITEM(it)->x1);
		if(filter_set_property(it->filter,"canvas_x",numberbuffer))
			fprintf(stderr,"set prop failed\n");
		sprintf(numberbuffer,"%8f",GNOME_CANVAS_ITEM(it)->y1);
		if(filter_set_property(it->filter,"canvas_y",numberbuffer))
			fprintf(stderr,"set prop failed\n");
		
		break;

	case GDK_2BUTTON_PRESS:
		DPRINTF_FOO("2butt\n");
		gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(data),event->button.time);
		if(bMac){
			                     // handle borken single button mouse
			switch(event->button.button){
			case 1:
				menu = gnome_popup_menu_new(node_menu);
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,it);
				break;	
			}
		}else{
			switch(event->button.button){
			case 1:
				canvas_item_show_description(NULL,GNOME_CANVAS_ITEM(item)->parent);
				break;
			}
		}
		break;

	case GDK_BUTTON_PRESS:
		DPRINTF_FOO("press\n");
	      switch(event->button.button){
	      case 1:
		        it->last_x = x;
			it->last_y = y;
			fleur = gdk_cursor_new(GDK_FLEUR);
			gnome_canvas_item_grab(GNOME_CANVAS_ITEM(data),
					       GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK|GDK_BUTTON_PRESS_MASK,//|GDK_LEAVE_NOTIFY_MASK,//|GDK_ENTER_NOTIFY_MASK,
					       fleur,
					       event->button.time);
			gdk_cursor_destroy(fleur);
			it->dragging=TRUE;
			break;
		case 3:
			menu = gnome_popup_menu_new(node_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,it);
			break;

		default:
			break;
		}
		break;
	case GDK_MOTION_NOTIFY:
		DPRINTF_FOO("motion\n");
		if(!inItem)
			inItem = 1;
		if(it->dragging && (event->motion.state & GDK_BUTTON1_MASK)){
			dx = x-(it->last_x);
			dy = y-(it->last_y);
			gnome_canvas_item_move(GNOME_CANVAS_ITEM(it),x-(it->last_x),y-(it->last_y));
			list = g_list_first(it->input_ports);
			while(list){
				canvas_input_port_update_connections((GLAME_CANVAS_PORT(list->data)),x-(it->last_x),y-(it->last_y));
				list=g_list_next(list);
			}
			list = g_list_first(it->output_ports);
			while(list){
				canvas_output_port_update_connections((GLAME_CANVAS_PORT(list->data)),x-(it->last_x),y-(it->last_y));
				list=g_list_next(list);
			}
			it->last_x = x;
			it->last_y = y;
		}
		break;
	default:
		break;
	}
	return TRUE;
}


static int
canvas_add_filter_by_name(const char *name)
{
        GnomeCanvasItem * item;
	filter_t *filter;
	double dx,dy;
	GlameCanvas* canv;
	plugin_t *plug;
	char numberbuffer[10];
	double x1,x2,y1,y2;

	canv = GLAME_CANVAS(win);
	plug = plugin_get(name);

	filter = filter_instantiate(plug);
	if(!filter){
		fprintf(stderr,"Error in instantiate\n");
		return -1;
	}
	if(filter_add_node(canv->net->net,filter,name) == -1) {
		fprintf(stderr,"Error adding node!\n");
		return -1;
	}
	gnome_canvas_window_to_world(GNOME_CANVAS(canv),event_x,event_y,&dx,&dy); 
	// fprintf(stderr,"%f %f %f %f\n",event_x,event_y,dx,dy);
 	item = GNOME_CANVAS_ITEM(canvas_add_node_from_filter(GNOME_CANVAS(canv),filter,dx,dy)); 
	gnome_canvas_item_request_update(item);
	gnome_canvas_item_get_bounds(item,&x1,&y1,&x2,&y2);
	sprintf(numberbuffer,"%8f",x1);
	if(filter_set_property(filter,"canvas_x",numberbuffer))
	      DPRINTF("Set Prop failed!\n");
	sprintf(numberbuffer,"%8f",y1);
	if(filter_set_property(filter,"canvas_y",numberbuffer))
	      DPRINTF("Set Prop failed!\n");
	inItem=0;
	return 0;

}


void 
network_draw_error(gui_network *net)
{
	filter_t *node;
	GlameCanvasItem *item;
	filter_foreach_node(net->net,node){
		if(node->gui_priv){
			item = (GlameCanvasItem*)node->gui_priv;
			if(filter_has_error(node)){
				gnome_canvas_item_set(item->nameBox,
						      "fill_color","red",
						      NULL);
			}
		}
	}
}

void
network_error_reset(gui_network *net)
{
	filter_t *node;
	GlameCanvasItem *item;
	filter_foreach_node(net->net,node){
		if(node->gui_priv){
			item = (GlameCanvasItem*)node->gui_priv;
			gnome_canvas_item_set(item->nameBox,
					      "fill_color","white",
					      NULL);
		}
	}
}

static void 
network_play(GtkWidget *button,GlameCanvas* glCanv)
{
	glame_gui_play_network(glCanv->net->net, glCanv->net, TRUE, NULL, NULL);
}

static void canvas_add_filter_by_name_cb(GtkWidget*wid, plugin_t *plugin)
{
	canvas_add_filter_by_name(plugin_name(plugin));
}

	
static gint
root_event(GnomeCanvas *canv,GdkEvent*event,GlameCanvas *glCanv)
{
	GtkWidget*menu;
	GtkWidget*par;
	GdkEventButton *event_button;

	gint mouseButton = bMac?1:3;
	switch(event->type){
	case GDK_BUTTON_PRESS:
		if(!inItem){
			
			event_button = (GdkEventButton *) event;
//			gnome_canvas_c2w(canv,event->button.x,event->button.y,&event_x,&event_y);
			event_x = event->button.x;
			event_y = event->button.y;
			win=GTK_WIDGET(canv);
			if(event->button.button==mouseButton){
				menu = GTK_WIDGET(glame_gui_build_plugin_menu(NULL, canvas_add_filter_by_name_cb));
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,glCanv);
				inItem=0;
			}
		}
	default:
		break;
	}
	return FALSE;
}
		       



GtkWidget * 
canvas_new_from_network(gui_network* net)
{
	GtkWidget *window, *canvas, *sw, *toolbar;

	GnomeDock *dock;
	
	const char *name;


	if(net->caption)
		name = strdup(net->caption);
	else{
		if(net->net && filter_name(net->net))
			name = strdup(filter_name(net->net));
		else
			name = strdup("Untitled");
	}

	window = gnome_app_new(name,_(name));
	dock = GNOME_DOCK(GNOME_APP(window)->dock);
	gtk_widget_ref(GTK_WIDGET(dock));
	
	gtk_widget_show(GTK_WIDGET(dock));
	toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,GTK_TOOLBAR_BOTH);


	inItem=0;
	
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
	gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas),0,0,600,400);
	

	gtk_container_add(GTK_CONTAINER(sw),canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Execute","Executes Filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_EXEC),network_play,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Register","Registers actual filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CONVERT),register_filternetwork_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Save","Saves Filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_SAVE_AS),canvas_save_as,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Properties","Edit Filternetwork Properties","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_PROPERTIES),NULL,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom in","Zooms in","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_DOWN),canvas_zoom_in_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom out","Zooms out","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_UP),canvas_zoom_out_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"View all","Adjusts scroll region","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_REFRESH),canvas_update_scroll_region_cb,canvas);
	gnome_app_set_toolbar(GNOME_APP(window),GTK_TOOLBAR(toolbar));
	gnome_app_set_contents(GNOME_APP(window),sw);	
	gtk_widget_show(GTK_WIDGET(dock));
	
	gtk_window_set_default_size(GTK_WINDOW(window),400,300);
	gtk_widget_show_all(window);
	gtk_signal_connect_after(GTK_OBJECT(canvas),"event",GTK_SIGNAL_FUNC(root_event),canvas);
	return window;
}


static gint canvas_input_port_event_cb(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{

	GtkWidget * menu;

	gint mouseButton = bMac?1:3;
	switch(event->type){
	case GDK_ENTER_NOTIFY:
		inItem=1;
		nPopupTimeoutId = gtk_timeout_add(nPopupTimeout,(GtkFunction)show_port_properties,GLAME_CANVAS_PORT(item));
		break;
	case GDK_LEAVE_NOTIFY:
		inItem=0;
		gtk_timeout_remove(nPopupTimeoutId);
		hide_port_properties(GLAME_CANVAS_PORT(item));
		break;
	case GDK_BUTTON_PRESS:
		if(event->button.button== mouseButton){
			menu = gnome_popup_menu_new(port_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,item);
		}
		break;
	default:
		break;		
		
		
	}
		
	
	
	return TRUE;
}


static void
canvas_port_calculate_docking_coords(GlameCanvasPort* port,double *x, double *y, int id)
{
	GnomeCanvasRE* rect = GNOME_CANVAS_RE(port);

	int items = g_list_length(port->connected_ports);

	if(items>1){
		*y=(rect->y1) + (double)(id)/(double)(items+1)*(rect->y2-rect->y1);
	}else{
		*y=(rect->y1+rect->y2)*0.5;
		
	}
	*x=rect->x1;
	gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(rect),x,y);
}

static gint
canvas_connection_update_points(GlameConnection *connection)
{
      GlameCanvasPort *source_port=connection->begin;
      GlameCanvasPort *dest_port=connection->end;
      
      double xs,ys,xd,yd;
      double xOffset = 25.0;
      double dist;
      
      canvas_port_calculate_docking_coords(source_port, &xs, &ys,connection->begin_id);
      canvas_port_calculate_docking_coords(dest_port, &xd, &yd,connection->end_id);
      
      xs += GNOME_CANVAS_RE(source_port)->x2-GNOME_CANVAS_RE(source_port)->x1;
      if(fabs(yd-ys)<40.0)
	      if(xd<xs)
		      connection->dy = 40.0;
      dist = xd-xs;
      if(dist<50.0)
	      xOffset = dist/2.0;
      xOffset=(xOffset<5.0)?5.0:xOffset;
	      

      connection->points->coords[0]=xs;
      connection->points->coords[1]=ys;

      connection->points->coords[2]=xs+xOffset+(connection->begin_id*4);
      connection->points->coords[3]=ys;

      connection->points->coords[4]=xs+xOffset+(connection->begin_id*4);
      connection->points->coords[5]=(ys+yd)/2.0-connection->dy;

      connection->points->coords[6]=xd-25.0-(connection->end_id*4);
      connection->points->coords[7]=(ys+yd)/2.0-connection->dy;

      connection->points->coords[8]=xd-25.0-(connection->end_id*4);
      connection->points->coords[9]=yd;

      connection->points->coords[10]=xd;
      connection->points->coords[11]=yd; 

      gnome_canvas_item_set(GNOME_CANVAS_ITEM(connection->line),
			    "points",connection->points,
			    NULL);
      
      gnome_canvas_item_set(GNOME_CANVAS_ITEM(connection->circle),
			    "x1",((xs+xd)/2.0)-6.0,
			    "x2",((xs+xd)/2.0)+6.0,
			    "y1",((ys+yd)/2.0-connection->dy)-6.0,
			    "y2",((ys+yd)/2.0-connection->dy)+6.0,
			    NULL);
      return 0;
}


static gint
canvas_connection_do_connect(GlameConnection* connection)
{
      GlameCanvasPort *source_port=connection->begin;
      GlameCanvasPort *dest_port=connection->end;
     
      double xs,ys,xd,yd;

      
      canvas_port_calculate_docking_coords(source_port, &xs, &ys,connection->begin_id);
      canvas_port_calculate_docking_coords(dest_port, &xd, &yd,connection->end_id);
      
      xs += GNOME_CANVAS_RE(source_port)->x2-GNOME_CANVAS_RE(source_port)->x1;

      if(fabs(yd-ys)<40.0)
	      if(xd<xs)
		      connection->dy = 40.0;
      connection->dy = 0.0;
      connection->points = gnome_canvas_points_new(6);

      
      connection->points->coords[0]=xs;
      connection->points->coords[1]=ys;

      connection->points->coords[2]=xs+25+(connection->begin_id*4);
      connection->points->coords[3]=ys;

      connection->points->coords[4]=xs+25+(connection->begin_id*4);
      connection->points->coords[5]=(ys+yd)/2.0-connection->dy;;

      connection->points->coords[6]=xd-25-(connection->end_id*4);
      connection->points->coords[7]=(ys+yd)/2.0-connection->dy;

      connection->points->coords[8]=xd-25-(connection->end_id*4);
      connection->points->coords[9]=yd;

      connection->points->coords[10]=xd;
      connection->points->coords[11]=yd;
      fprintf(stderr,"%d %d\n",connection->begin_id,connection->end_id);

      if(connection->line)
	    gtk_object_destroy(GTK_OBJECT(connection->line));
      connection->line = GNOME_CANVAS_LINE(gnome_canvas_item_new(GNOME_CANVAS_GROUP(GNOME_CANVAS_ITEM(source_port)->parent->parent),
								    gnome_canvas_line_get_type(),
								    "points",connection->points,
								    "fill_color","black",
								    "width_units",2.0,
								    "arrow_shape_a",18.0,
								    "arrow_shape_b",20.0,
								    "arrow_shape_c",5.0,
								    "last_arrowhead",TRUE,
								    NULL));
      gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(connection->line));

      connection->circle = GNOME_CANVAS_ELLIPSE(gnome_canvas_item_new(GNOME_CANVAS_GROUP(GNOME_CANVAS_ITEM(source_port)->parent->parent),
								      gnome_canvas_ellipse_get_type(),
								      "x1",((xs+xd)/2.0)-6.0,
								      "x2",((xs+xd)/2.0)+6.0,
								      "y1",((ys+yd)/2.0-connection->dy)-6.0,
								      "y2",((ys+yd)/2.0-connection->dy)+6.0,
								      "fill_color","black",
								      "width_pixels",5,
								      NULL));
      gtk_signal_connect(GTK_OBJECT(connection->circle),
			 "event",GTK_SIGNAL_FUNC(canvas_connection_select),
			 connection);
      connection->property_texts=NULL;
      canvas_input_port_update_connections(source_port,0.0,0.0);
      canvas_output_port_update_connections(dest_port,0.0,0.0);
      connection->begin = GLAME_CANVAS_PORT(source_port);
      connection->end   = GLAME_CANVAS_PORT(dest_port);
      return 0;
}
						
      


static gint
canvas_output_port_motion(GnomeCanvasItem *pitem,GdkEvent *event, gpointer data)
{
	GlameCanvasItem *item=GLAME_CANVAS_ITEM(pitem);
	GnomeCanvasItem *released;
	GlameCanvasPort *port;
	double xs,ys;
	double x,y;
	int bx,by;
	
		//gnome_canvas_window_to_world(pitem->canvas,event->button.x,event->button.y,&x,&y);
	bx = event->button.x*pitem->canvas->pixels_per_unit;
	by = event->button.y*pitem->canvas->pixels_per_unit;
	
	gnome_canvas_c2w(pitem->canvas,bx,by,&x,&y);
	xs = pitem->canvas->scroll_x1;
	ys = pitem->canvas->scroll_y1;
	x-=xs;
	y-=ys;
	switch(event->type){
	case GDK_MOTION_NOTIFY:
		if(event->motion.state & GDK_BUTTON1_MASK){
			gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(item),&x,&y);
			item->connection->points->coords[2]=x;
			item->connection->points->coords[3]=y;
			gnome_canvas_item_set(GNOME_CANVAS_ITEM(item->connection->line),
					      "points",item->connection->points,
					      NULL);

		} 
		return FALSE;
	case GDK_BUTTON_RELEASE:
		gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(item),event->button.time);
		if(item->connecting){
			gtk_object_destroy(GTK_OBJECT(item->connection->line));

			// Why does this not work correctly??   libgnomeui bug??
			released = gnome_canvas_get_item_at(pitem->canvas,x,y);
			//released = GNOME_CANVAS_ITEM(find_output_port(GLAME_CANVAS(pitem->canvas),x,y));
			if(released){
				if(GLAME_IS_CANVAS_PORT(released)){
					DPRINTF("port hit!\n");
					port = GLAME_CANVAS_PORT(released);
					if(port->port_type==GUI_PORT_TYPE_IN){
						DPRINTF("inputport! \n");
						item->connection->end = port;
						if(canvas_connection_connect(item->connection)<0){
							DPRINTF("inner connection failed\n");
							free(item->connection);	
						}else{
							DPRINTF("inner connection succeded!\n");

						}
						// connected!!
					} else {
						DPRINTF("not inputport! %d %s",port->port_type,filterport_label(port->port));
						free(item->connection);	
					}	
				} else {
					DPRINTF("not canvas port %s\n",gtk_type_name(
							released->object.klass->type));
					free(item->connection);	
										
				}
			}else{
				free(item->connection);	
			}
			item->connecting = FALSE;
		}
		return FALSE;
	default:
		break;
	}
	return FALSE;
}



static void
canvas_outout_port_reorder_connections(GlameCanvasPort* port)
{
	GList *list = g_list_first(port->connected_ports);
	int count=1;
	while(list){
	      ((GlameConnection*)(list->data))->end_id = count++;
		list = g_list_next(list);
	}
	canvas_input_port_update_connections(port,0.0,0.0);
}


static void
canvas_input_port_reorder_connections(GlameCanvasPort* port)
{
	GList *list = g_list_first(port->connected_ports);
	int count=1;
	while(list){
	      ((GlameConnection*)(list->data))->begin_id = count++;
		list = g_list_next(list);
	}
	canvas_input_port_update_connections(port,0.0,0.0);
}
		

static void canvas_connection_destroy_cb(GtkWidget*bla,GlameConnection* conn)
{
	canvas_connection_destroy(conn);
}
	
static gint
canvas_connection_select(GnomeCanvasItem* item, GdkEvent *event,gpointer data)
{
	GdkCursor *fleur;
	GlameConnection* connection = (GlameConnection*)data;
	GtkWidget* menu;
	double y;

	y = event->button.y;
	
	switch(event->type){
	case GDK_ENTER_NOTIFY:
		inItem=1;
		//		glame_connection_show_props(connection);  //todo
		break;
	case GDK_LEAVE_NOTIFY:
		inItem=0;
		//		glame_connection_hide_props(connection);  //todo
		break;
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			connection->last_y=y+connection->dy;
			fleur = gdk_cursor_new(GDK_FLEUR);
			gnome_canvas_item_grab(GNOME_CANVAS_ITEM(connection->circle),
					       GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK,fleur,
					       event->button.time);
			gdk_cursor_destroy(fleur);
			break;
		case 2:
			//			DPRINTF("Breaking connection\n");
			//			connection_break(connection);
			//			return TRUE;
			break;
		case 3:
			menu = gnome_popup_menu_new(pipe_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,connection);
			break;
		default:
					
			break;
		}
	case GDK_MOTION_NOTIFY:
		if (event->motion.state & GDK_BUTTON1_MASK){
			connection->dy = connection->last_y - y;
			canvas_connection_update_points(connection);
		}
		break;
	case GDK_BUTTON_RELEASE:
		gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(connection->circle),event->button.time);
		connection->dy = connection->last_y - y;
		canvas_connection_update_points(connection);
	default: 
		break;
	}
	return FALSE;
}

static gint canvas_output_port_event_cb(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{
	double x,y,x1,y1,x2,y2;
	double xs,ys;
	int bx,by;
	GnomeCanvasItem *newitem;
	GtkWidget * menu;
	GlameCanvasItem *parent = GLAME_CANVAS_ITEM(item->parent);
	GdkCursor *fleur;
	GlameConnection *newconn;

	bx = event->button.x*item->canvas->pixels_per_unit;
	by = event->button.y*item->canvas->pixels_per_unit;
	
	gnome_canvas_c2w(item->canvas,bx,by,&x,&y);
	
	xs = item->canvas->scroll_x1;
	ys = item->canvas->scroll_y1;
	x-=xs;
	y-=ys;
	x1=item->x1/item->canvas->pixels_per_unit;
	x2=item->x2/item->canvas->pixels_per_unit;
	y1=item->y1/item->canvas->pixels_per_unit;
	y2=item->y2/item->canvas->pixels_per_unit;
	x1+=xs;
	y1+=ys;
	x2+=xs;
	y2+=ys;
	
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(parent),&x1,&y1);
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(parent),&x2,&y2);	
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(parent),&x,&y);
		
	switch(event->type){
	case GDK_LEAVE_NOTIFY:
		inItem = 0;
		gtk_timeout_remove(nPopupTimeoutId);
		hide_port_properties(GLAME_CANVAS_PORT(item));
		break;
	case GDK_ENTER_NOTIFY:
		inItem = 1;
		nPopupTimeoutId = gtk_timeout_add(nPopupTimeout,(GtkFunction)show_port_properties,GLAME_CANVAS_PORT(item));
		break;
	case GDK_2BUTTON_PRESS:
		if(bMac){
			gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(parent),event->button.time);
			menu = gnome_popup_menu_new(port_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,item);
			
			if(parent->connecting){
			gtk_object_destroy(GTK_OBJECT(parent->connection->line));
			free(parent->connection);	
			parent->connecting = FALSE;
			}
		}
		break;
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			gtk_timeout_remove(nPopupTimeoutId);
			hide_port_properties(GLAME_CANVAS_PORT(item));
			inItem = 0;
			newconn = malloc(sizeof(GlameConnection));
			newconn->points=gnome_canvas_points_new(2);
			newconn->points->coords[0]=(x1+x2)/2.0;
			newconn->points->coords[1]=(y1+y2)/2.0;
			newconn->points->coords[2]=x;
			newconn->points->coords[3]=y;
			fleur = gdk_cursor_new(GDK_FLEUR);
			newitem = GNOME_CANVAS_ITEM(gnome_canvas_item_new(GNOME_CANVAS_GROUP(parent),
									  gnome_canvas_line_get_type(),
									  "points",newconn->points,
									  "fill_color","black",
									  "width_units",2.0,
									  "last_arrowhead",TRUE,
									  "arrow_shape_a",18.0,
									  "arrow_shape_b",20.0,
									  "arrow_shape_c",5.0,
									  NULL));
			gnome_canvas_item_raise_to_top(newitem);
			newconn->line = GNOME_CANVAS_LINE(newitem);
			newconn->begin = GLAME_CANVAS_PORT(item);
			parent->connection = newconn;
			gnome_canvas_item_grab(GNOME_CANVAS_ITEM(parent),
					       GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK|GDK_BUTTON_PRESS_MASK,
					       fleur,
					       event->button.time);
			gdk_cursor_destroy(fleur);
			parent->connecting=TRUE;
			return TRUE;
			break;
		case 3:
			menu = gnome_popup_menu_new(port_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,item);
			break;
		default:
			break;
		}
	default:
		break;		
		
		
	}
		
	
	
	return FALSE;
}

gint
handle_events(GnomeCanvasItem* item,GdkEvent *event, gpointer data)
{
	DPRINTF_FOO("%s\n",gtk_type_name(item->canvas->current_item->object.klass->type));
	if((GLAME_CANVAS_ITEM(item))->dragging)
		canvas_item_node_selected(item,event,data);
	if((GLAME_CANVAS_ITEM(item))->connecting)
		canvas_output_port_motion(item,event,data);
	
	return TRUE;
	
}

	


void
canvas_item_create_ports(GnomeCanvasGroup* grp,filter_t *f)
{
	filter_t *filter = f;
	filter_port_t *port;
	GlameCanvasPort *item;
	//	GtkTooltips* tt;
	double step;
	double border;
	int portcount;

	portcount = 0;
	filterportdb_foreach_port(filter_portdb(filter), port) {
		if (filterport_is_input(port))
			portcount++;
	}
	if (!portcount)
		goto _no_inputs;
	
	step = 64.0/(float)portcount;
	border = 0.0;
	filterportdb_foreach_port(filter_portdb(filter), port) {
		if (!filterport_is_input(port))
			continue;
		item = glame_canvas_port_new(grp,port,
					     0.0,border,16.0,step,
					     0xff000090);
		item->port_type = GUI_PORT_TYPE_IN;
		(GLAME_CANVAS_ITEM(grp))->input_ports=g_list_append((GLAME_CANVAS_ITEM(grp))->input_ports,item);
		border+=step;
		gtk_signal_connect(GTK_OBJECT(item),
				   "event",GTK_SIGNAL_FUNC(canvas_input_port_event_cb),
				   port);
		
	}

_no_inputs:
	portcount = 0;
	filterportdb_foreach_port(filter_portdb(filter), port) {
		if (filterport_is_output(port))
			portcount++;
	}

	if (!portcount)
		return;

	step = 64.0/(float)portcount;
	border=0.0;
	filterportdb_foreach_port(filter_portdb(filter), port) {
		if (!filterport_is_output(port))
			continue;
		item = glame_canvas_port_new(grp,port,
					     80.0,border,16.0,step,
					     0x0000ff90);
		item->port_type = GUI_PORT_TYPE_OUT;
		(GLAME_CANVAS_ITEM(grp))->output_ports=g_list_append((GLAME_CANVAS_ITEM(grp))->output_ports,item);
		border+=step;

		gtk_signal_connect(GTK_OBJECT(item),
				   "event",GTK_SIGNAL_FUNC(canvas_output_port_event_cb),
				   port);
	}
}
	
				      

		
	



static GtkObject*
canvas_add_node_from_filter(GnomeCanvas *canvas, filter_t *filter,double x, double y)
{

	GlameCanvasItem *item;
	GnomeCanvasGroup* root;
	char numberbuffer[20];

	root = gnome_canvas_root(GNOME_CANVAS(canvas));

	item = glame_canvas_item_new(root,
				     filter,
				     0.0,0.0);
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(item),x,y);
	sprintf(numberbuffer,"%8f",x);
	if(filter_set_property(filter,"canvas_x",numberbuffer))
				fprintf(stderr,"set prop failed\n");
	sprintf(numberbuffer,"%8f",y);
	if(filter_set_property(filter,"canvas_y",numberbuffer))
		fprintf(stderr,"set prop failed\n");
	return GTK_OBJECT(item);
}


static void
canvas_connection_connect_from_pipe(GlameConnection *c)
{
	filter_port_t *dest;
	GlameCanvasItem * item;
	GList * ports;
	GlameCanvasPort * destPort=NULL;
	c->line=NULL;
	dest = filterpipe_dest(c->pipe);
	if(filterport_filter(dest)->gui_priv){
		item = GLAME_CANVAS_ITEM(filterport_filter(dest)->gui_priv);
		ports = g_list_first(item->input_ports);
		while(ports){
			if(GLAME_CANVAS_PORT(ports->data)->port == dest)
				destPort=GLAME_CANVAS_PORT(ports->data);
			ports = g_list_next(ports);
		}
		if(!destPort){
			fprintf(stderr,"Fatal! Port not found, this can't happen!\n");
			return;
		}
		c->end = destPort;
		c->begin->connected_ports=g_list_append(c->begin->connected_ports,c);
		c->end->connected_ports=g_list_append(c->end->connected_ports,c);
		c->begin_id = g_list_length(c->begin->connected_ports);
		c->end_id = g_list_length(c->end->connected_ports);
		canvas_connection_do_connect(c);
	}
}
	
static int
canvas_connection_connect(GlameConnection *c)
{
	filter_port_t *source, *dest;

	// ooooohhh f**k this does not look nice!
	source = filterportdb_get_port(filter_portdb((GLAME_CANVAS_ITEM((GNOME_CANVAS_ITEM(c->begin))->parent))->filter), filterport_label(c->begin->port));
	dest = filterportdb_get_port(filter_portdb((GLAME_CANVAS_ITEM((GNOME_CANVAS_ITEM(c->end))->parent))->filter), filterport_label(c->end->port));
	
	c->pipe = filterport_connect(source, dest);
	if(!c->pipe)
	{
		DPRINTF("Connection failed!!\n");
		return -1;
	}else {
		DPRINTF("success!\n");
	}

	c->begin->connected_ports=g_list_append(c->begin->connected_ports,c);
	c->end->connected_ports=g_list_append(c->end->connected_ports,c);
	c->begin_id = g_list_length(c->begin->connected_ports);
	c->end_id = g_list_length(c->end->connected_ports);
	canvas_connection_do_connect(c);
	return 0;
	
}

	    

static void
canvas_input_port_update_connections(GlameCanvasPort*p, gdouble x, gdouble y)
{
	GList* connection = g_list_first(p->connected_ports);
	
	while(connection){
	      canvas_connection_update_points((GlameConnection*)(connection->data));
	      connection = g_list_next(connection);
	}
}

static void
canvas_output_port_update_connections(GlameCanvasPort*p, gdouble x, gdouble y)
{
	GList* connection = g_list_first(p->connected_ports);
	
	while(connection){
	      canvas_connection_update_points((GlameConnection*)(connection->data));
	      connection = g_list_next(connection);
	}
}
	
	

static void
canvas_item_destroy(GlameCanvasItem* it)
{
	GList * plist;
	GList * clist;
	plist = g_list_first(it->input_ports);
	while(plist){
		clist = g_list_first(GLAME_CANVAS_PORT(plist->data)->connected_ports);
		while(clist){
			canvas_connection_destroy((GlameConnection*)(clist->data));
			clist = g_list_first(GLAME_CANVAS_PORT(plist->data)->connected_ports);
		}
		plist = g_list_next(plist);
	}
	plist = g_list_first(it->output_ports);
	while(plist){
		clist = g_list_first(GLAME_CANVAS_PORT(plist->data)->connected_ports);
		while(clist){
			canvas_connection_destroy((GlameConnection*)(clist->data));
			clist = g_list_first(GLAME_CANVAS_PORT(plist->data)->connected_ports);
		}
		plist = g_list_next(plist);
	}
	filter_delete(it->filter);
	gtk_object_destroy(GTK_OBJECT(it));
}

static void
canvas_connection_destroy(GlameConnection* connection)
{
	filterpipe_delete(connection->pipe);
	connection->begin->connected_ports=g_list_remove(connection->begin->connected_ports,connection);
	connection->end->connected_ports=g_list_remove(connection->end->connected_ports,connection);
	gtk_object_destroy(GTK_OBJECT(connection->line));
	gtk_object_destroy(GTK_OBJECT(connection->circle));
	canvas_outout_port_reorder_connections(connection->end);
	canvas_input_port_reorder_connections(connection->begin);
	free (connection);
}


static void canvas_register_as_cb(gchar* name, GlameCanvas* glCanv)
{
	plugin_t *newplug;
	filter_t *copy;
	filter_t * bla = GLAME_CANVAS(glCanv)->net->net;
	
	newplug = plugin_add(name);
	copy = filter_creat(bla);
	filter_register(copy,newplug);
	
}


static void set_file_selection_filter(GnomeFileEntry* entry, const char * filter)
{
      gtk_file_selection_complete(GTK_FILE_SELECTION(entry->fsw),filter);
}

static void canvas_save_as(GtkWidget*w,GlameCanvas *glCanv)
{
	GtkWidget * dialog;
	GtkWidget * fileEntry;
	GtkWidget * filternameentry;
	GtkWidget * categoryEntry;
	GtkWidget * dialogVbox;
	GtkWidget * errorbox;
	char *buffer;
	char * filenamebuffer;
	char * filternamebuffer;
	char * categorynamebuffer;
	FILE* outf;
	filter_t *bla;

	filenamebuffer=calloc(100,sizeof(char));
	filternamebuffer=calloc(100,sizeof(char));
	categorynamebuffer=calloc(100,sizeof(char));
	
	dialog = gnome_dialog_new("Save network as...",GNOME_STOCK_BUTTON_CANCEL,GNOME_STOCK_BUTTON_OK,NULL);
	dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));
	
	fileEntry = gnome_file_entry_new(NULL,"Filename");
	gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fileEntry))),
			   "changed",changeString,&filenamebuffer);
	gtk_signal_connect_after(GTK_OBJECT(fileEntry),"browse_clicked",GTK_SIGNAL_FUNC(set_file_selection_filter),"*.scm");
	create_label_widget_pair(dialogVbox,"Filename",fileEntry);
	
	filternameentry = gtk_entry_new();
	gtk_signal_connect(GTK_OBJECT(filternameentry),"changed",
			   changeString,&filternamebuffer);
	create_label_widget_pair(dialogVbox,"Filtername",filternameentry);
	
	categoryEntry = gtk_entry_new();
	gtk_signal_connect(GTK_OBJECT(categoryEntry),"changed",
			   changeString,&categorynamebuffer);
	create_label_widget_pair(dialogVbox,"Category",categoryEntry);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog))){

		bla = GLAME_CANVAS(glCanv)->net->net;

		if(*filenamebuffer){
			if(!(*filternamebuffer))
				strcpy(filternamebuffer,filenamebuffer);
			if(!(*categorynamebuffer))
				strcpy(categorynamebuffer,"Default");
			outf = fopen(filenamebuffer,"w");
			buffer = filter_to_string(bla);
			fprintf(stderr,"%s\n",buffer);
			fprintf(outf,"(let ((newplugin (glame_plugin_define %s \"%s\")\n)) (if (filter_p newplugin) newplugin (plugin_set newplugin PLUGIN_CATEGORY \"%s\")))",buffer,filternamebuffer,categorynamebuffer);
			free(buffer);
			fclose(outf);
		}else{
			errorbox = gnome_warning_dialog("Please enter filename next time...");
			gnome_dialog_run_and_close(GNOME_DIALOG(errorbox));
		}
	}
	free(filenamebuffer);
	free(filternamebuffer);
	free(categorynamebuffer);
}

static void register_filternetwork_cb(GtkWidget*bla, GlameCanvas* glCanv)
{
	
	gnome_request_dialog(0,"Filtername","filter",16,(GnomeStringCallback)canvas_register_as_cb,glCanv,NULL);
}

static void
canvas_zoom_out_cb(GtkWidget *bla, GlameCanvas*canv)
{
	filter_t *filter;
	char fontbuffer[256];
	gint fontsize;
	gnome_canvas_set_pixels_per_unit(GNOME_CANVAS(canv),GNOME_CANVAS(canv)->pixels_per_unit/2.0);
	gnome_canvas_update_now(GNOME_CANVAS(canv));
	fontsize = (int)(GNOME_CANVAS(canv)->pixels_per_unit*12.0);
	fontsize = (fontsize<2.0)?2.0:fontsize;
	sprintf(fontbuffer,CANVAS_FONT_STRING,fontsize);
	filter_foreach_node(canv->net->net,filter){
		gnome_canvas_item_set(GLAME_CANVAS_ITEM(filter->gui_priv)->text,"font",fontbuffer,NULL);
	}
}

static void
canvas_zoom_in_cb(GtkWidget *bla, GlameCanvas*canv)
{
	filter_t *filter;
	char fontbuffer[256];
	gint fontsize;

	gnome_canvas_set_pixels_per_unit(GNOME_CANVAS(canv),GNOME_CANVAS(canv)->pixels_per_unit*2.0);
	gnome_canvas_update_now(GNOME_CANVAS(canv));
	fontsize = (int)(GNOME_CANVAS(canv)->pixels_per_unit*12.0);
	fontsize = (fontsize<2.0)?2.0:fontsize;
	sprintf(fontbuffer,CANVAS_FONT_STRING,fontsize);
	filter_foreach_node(canv->net->net,filter){
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(GLAME_CANVAS_ITEM(filter->gui_priv)->text),"font",fontbuffer,NULL);
	}
}

static void
canvas_update_scroll_region_cb(GtkWidget*bla, GlameCanvas* canv)
{
	canvas_update_scroll_region(canv);
}

/**************************************
 * FIXME
 * Beware, this function may destroy your 
 * scroll region.
 * use it only for initial setup of the
 * scroll region. coord-system trouble
 * xw 
 **************************************/

static void 
canvas_update_scroll_region(GlameCanvas* canv)
{
	filter_t *filter;
	double minX,minY,maxX,maxY;
	double x1,x2,y1,y2;
	
	minX=minY=99999.0;
	maxX=maxY=-99999.0;

	filter_foreach_node(canv->net->net,filter){
		gnome_canvas_item_get_bounds(GNOME_CANVAS_ITEM(filter->gui_priv),&x1,&y1,&x2,&y2);
		minX=(minX>x1)?x1:minX;
		minY=(minY>y1)?y1:minY;
		maxX=(maxX<x2)?x2:maxX;
		maxY=(maxY<y2)?y2:maxY;
	}
	minX-=30.0;
	minY-=30.0;
	maxX+=30.0;
	maxY+=30.0;

	gnome_canvas_get_scroll_region(GNOME_CANVAS(canv),&x1,&y1,&x2,&y2);
	x1=(x1>minX)?minX:x1;
	y1=(y1>minY)?minY:y1;
	x2=(x2<maxX)?maxX:x2;
	y2=(y2<maxY)?maxY:y2;
	
//	gnome_canvas_set_scroll_region(GNOME_CANVAS(canv),x1,y1,x2,y2);
	gnome_canvas_set_scroll_region(GNOME_CANVAS(canv),minX,minY,maxX,maxY);
	gnome_canvas_update_now(GNOME_CANVAS(canv));
	filter_foreach_node(canv->net->net,filter){
		// FIXME gnomebug?
		gnome_canvas_item_move(GNOME_CANVAS_ITEM(filter->gui_priv),0.0,0.0);
		gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(filter->gui_priv));
	}

}

void canvas_load_network(GtkWidget *bla, void *blu)
{
      
	GtkWidget * fileEntry;
	GtkWidget * dialog;
	GtkWidget * vbox;
	filter_t *filter;
	char * filenamebuffer;
	filenamebuffer = calloc(100,sizeof(char));

	dialog = gnome_dialog_new("Load scheme code",GNOME_STOCK_BUTTON_CANCEL,GNOME_STOCK_BUTTON_OK,NULL);
	vbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));

	fileEntry = gnome_file_entry_new("Load","Filename");
	gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fileEntry))),"changed",changeString,&filenamebuffer);
	create_label_widget_pair(vbox,"Filename",fileEntry);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog))){
		filter = glame_load_instance(filenamebuffer);
		if(filter){
			draw_network(filter);
		}else{
			gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("Error in loading .scm file")));
		} 
 
	}
	free(filenamebuffer);
}


static void canvas_port_redirect(GtkWidget*bla,GlameCanvasPort *blu)
{
	GtkWidget * nameEntry;
	GtkWidget * dialog;
	GtkWidget * vbox;
	char * filenamebuffer;

	filter_portdb_t *ports;
	filter_port_t * newport;

	filenamebuffer = calloc(100,sizeof(char));

	dialog = gnome_dialog_new("Export as...",GNOME_STOCK_BUTTON_CANCEL,GNOME_STOCK_BUTTON_OK,NULL);
	vbox = GNOME_DIALOG(dialog)->vbox;

	nameEntry = gtk_entry_new();
	gtk_signal_connect(GTK_OBJECT(nameEntry),"changed",
			   changeString,&filenamebuffer);
	create_label_widget_pair(vbox,"New port name",nameEntry);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog))){
		ports = filter_portdb(GLAME_CANVAS(GNOME_CANVAS_ITEM(blu)->canvas)->net->net);  
		if((blu->port_type)&GUI_PORT_TYPE_OUT){
			newport = filterportdb_add_port(ports,filenamebuffer,FILTER_PORTTYPE_ANY,FILTER_PORTFLAG_OUTPUT,FILTERPORT_DESCRIPTION,filenamebuffer,FILTERPORT_END);
			filterport_redirect(newport,blu->port);
			blu->port_type|=GUI_PORT_TYPE_EXTERNAL;
		}else if((blu->port_type)&GUI_PORT_TYPE_IN){
			newport = filterportdb_add_port(ports,filenamebuffer,FILTER_PORTTYPE_ANY,FILTER_PORTFLAG_INPUT,FILTERPORT_DESCRIPTION,filenamebuffer,FILTERPORT_END);
			filterport_redirect(newport,blu->port);
			blu->port_type|=GUI_PORT_TYPE_EXTERNAL;
		}
	}
	canvas_item_redraw(GLAME_CANVAS_ITEM(GNOME_CANVAS_ITEM(blu)->parent));
	free(filenamebuffer);
}

static void update_string(GtkListItem* item,char ** returnbuffer)
{
	// this is a little kludgy.. FIXME
	char *label;
	
	label = strdup(GTK_LABEL(GTK_BIN(item)->child)->label);
	strncpy(*returnbuffer,label,100);
	free(label);  
}

static void update_entry_text(GtkListItem* item,GtkEntry* entry)
{
	// this is a little kludgy.. FIXME
	char *label;
	label = strdup(GTK_LABEL(GTK_BIN(item)->child)->label);
	gtk_entry_set_text(entry,label);
	free(label);  
}

static void canvas_item_redirect_parameters(GtkWidget *bla, GlameCanvasItem *item)
{
	GtkWidget * dialog;
	GtkWidget * vbox;
	GtkWidget * entry;
	GtkWidget * list;
	GtkWidget * listItems;
	filter_paramdb_t * db, *topleveldb; 
	filter_param_t *iter,*newparam;
	GList * items=NULL;
	char * paramnamebuffer;
	char * externnamebuffer;

	paramnamebuffer = calloc(100,sizeof(char));
	externnamebuffer = calloc(100,sizeof(char));

	dialog = gnome_dialog_new("Export parameters...",GNOME_STOCK_BUTTON_CANCEL,"Remap All",GNOME_STOCK_BUTTON_OK,NULL);
		
	vbox = GNOME_DIALOG(dialog)->vbox;
	
	list = gtk_list_new();
	entry = gtk_entry_new();
	
	db = filter_paramdb(item->filter);
	topleveldb = filter_paramdb(GLAME_CANVAS(GNOME_CANVAS_ITEM(item)->canvas)->net->net);

	filterparamdb_foreach_param(db,iter){
		listItems = gtk_list_item_new_with_label(filterparam_label(iter));
		gtk_signal_connect(GTK_OBJECT(listItems),"select",update_entry_text,entry);
		gtk_signal_connect(GTK_OBJECT(listItems),"select",update_string,&paramnamebuffer);
		gtk_signal_connect(GTK_OBJECT(entry),"changed",changeString,&externnamebuffer);
		gtk_widget_show(listItems);
		items = g_list_append(items,listItems);
	}
	gtk_list_append_items(GTK_LIST(list),items);
	gtk_widget_show(list);
	create_label_widget_pair(vbox,"Select parameter:",list);
	create_label_widget_pair(vbox,"External name:",entry);
	switch(gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
	{
	case 2:
		fprintf(stderr,"%s\n",paramnamebuffer);
		if(paramnamebuffer){
			iter = filterparamdb_get_param(db,paramnamebuffer);
			if(iter){
				if(*externnamebuffer)
					newparam = filterparamdb_add_param(topleveldb,externnamebuffer,filterparam_type(iter),filterparam_val(iter),FILTERPARAM_END);
				else
					newparam = filterparamdb_add_param(topleveldb,filterparam_label(iter),filterparam_type(iter),filterparam_val(iter),FILTERPARAM_END);
				if(newparam)
					if(filterparam_redirect(newparam,iter))
						fprintf(stderr,"Failed to redirect: %s\n",filterparam_label(iter));
			}
		}
			
		break;
	case 1:
		
		filterparamdb_foreach_param(db,iter){
			newparam = filterparamdb_add_param(topleveldb,filterparam_label(iter),filterparam_type(iter),filterparam_val(iter),FILTERPARAM_END);
			if(newparam)
				if(filterparam_redirect(newparam,iter))
					fprintf(stderr,"Failed to redirect: %s\n",filterparam_label(iter));
		}
		break;
	default:
		break;
	}
	
	free(paramnamebuffer);
	free(externnamebuffer);
								    
}


static void canvas_item_show_description(GtkWidget* wid,GlameCanvasItem* it)
{
	GtkWidget * dialog;
	GtkWidget * text;
	GtkWidget * vbox;
	GtkWidget * notebook;
	GtkWidget * tablabel;
	GtkCList * list;
	char * desc;
	int pos=0;

	filter_portdb_t * ports;
	filter_paramdb_t * params;
	filter_param_t * param;
	filter_t * filter;
	filter_port_t * port;
	
	char *labels[] = {"Name","Type","Description"};
	char *plabels[] = {"Name","Value","Description"};
	char ** line;
	char * buffer;
	filter = it->filter;

	ports = filter_portdb(filter);
	
	notebook = gtk_notebook_new();
	
	dialog = gnome_dialog_new(plugin_name(it->filter->plugin),GNOME_STOCK_BUTTON_OK,NULL);
	

	desc = (char*)plugin_query(it->filter->plugin,PLUGIN_DESCRIPTION);
	text = gtk_text_new(NULL,NULL);
	gtk_widget_show(text);
	
	if(desc)
		gtk_editable_insert_text(GTK_EDITABLE(text),desc,strlen(desc),&pos);
	else
		gtk_editable_insert_text(GTK_EDITABLE(text),"This item does not have a description",38,&pos);
	tablabel = gtk_label_new("Description");
	gtk_widget_show(tablabel);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),text,tablabel);

	tablabel = gtk_label_new("Ports");
	list = GTK_CLIST(gtk_clist_new_with_titles(3,labels));
	gtk_clist_set_column_auto_resize(list,0,TRUE);
	gtk_clist_set_column_auto_resize(list,1,TRUE);
	gtk_clist_set_column_auto_resize(list,2,TRUE);

	filterportdb_foreach_port(ports,port){
		line = calloc(3,sizeof(char*));
		if(filterport_label(port))
			buffer = strdup(filterport_label(port));
		else
			buffer = strdup("Empty");
		line[0] = buffer;
		line[1] = (filterport_is_input(port)?"In":"Out");
		if( filterport_get_property(port,FILTERPORT_DESCRIPTION))
			buffer = filterport_get_property(port,FILTERPORT_DESCRIPTION);
		else
			buffer = strdup("Empty");
		line[2] = buffer;
		gtk_clist_append(list,line);
	}
	gtk_widget_show(GTK_WIDGET(list));
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),GTK_WIDGET(list),tablabel);

	tablabel = gtk_label_new("Properties");
	gtk_widget_show(tablabel);
	
	list = GTK_CLIST(gtk_clist_new_with_titles(3,plabels));
	gtk_clist_set_column_auto_resize(list,0,TRUE);
	gtk_clist_set_column_auto_resize(list,1,TRUE);
	gtk_clist_set_column_auto_resize(list,2,TRUE);
	
	params = filter_paramdb(filter);
	filterparamdb_foreach_param(params,param){
		line = calloc(3,sizeof(char*));
		if(filterparam_label(param))
			buffer = filterparam_label(param);
		else
			buffer = strdup("Empty");
		line[0] = buffer;
		if(filterparam_to_string(param))
			buffer = strdup(filterparam_to_string(param));
		else
			buffer = strdup("Empty");
		line[1] = buffer;
		if(filterparam_get_property(param,FILTERPARAM_DESCRIPTION))
			buffer = strdup(filterparam_get_property(param,FILTERPARAM_DESCRIPTION));
		else
			buffer = strdup("Empty");
		line[2] = buffer;
		gtk_clist_append(list,line);
	}
	gtk_widget_show(GTK_WIDGET(list));
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),GTK_WIDGET(list),tablabel);

	gtk_widget_show(notebook);
	vbox = GNOME_DIALOG(dialog)->vbox;
	gtk_container_add(GTK_CONTAINER(vbox),notebook);
	
	gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}

GlameCanvas*
draw_network(filter_t *filter)
{
   	gui_network * net;       
	GtkWidget * canv;
	filter_t * node;
	filter_pipe_t *pipe;
	GList *list;
	GlameCanvasItem* new_item;
	GlameConnection *connection;
	double x,y;
	char * numberbuffer;
	char * cimmutable;
	gboolean immutable; 
	
	if(!FILTER_IS_NETWORK(filter)){
		fprintf(stderr,"Not a network!\n");
		return NULL;
	}
	net = malloc(sizeof(gui_network));

	net->caption = NULL;
	net->pixname = NULL;
	net->descr = NULL;
	net->net = filter;
	
	canvas_new_from_network(net);
	canv = net->canvas;
	//	gtk_signal_connect(GTK_OBJECT(canv),"delete-event",GTK_SIGNAL_FUNC(gui_exit),NULL);
	filter_foreach_node(filter,node){
		
		new_item = glame_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(canv)),node,0.0,0.0);
		numberbuffer = filter_get_property(node,"canvas_x");
		cimmutable = filter_get_property(node,"immutable");
		if(cimmutable)
			immutable = atoi(cimmutable);
		else 
			immutable = FALSE;
		new_item->immutable = immutable;
		cimmutable = filter_get_property(node,"undeletable");
		if(cimmutable)
			immutable = atoi(cimmutable);
		else 
			immutable = FALSE;
		new_item->undeletable = immutable;
		if(numberbuffer)
			x = atof(numberbuffer);
		else
			x += 120.0;
		numberbuffer = filter_get_property(node,"canvas_y");
		if(numberbuffer)
			y = atof(numberbuffer);
		else
			y += 20.0;
		gnome_canvas_item_move(GNOME_CANVAS_ITEM(new_item),x,y);
	}
	filter_foreach_node(filter,node){
		list = g_list_first(((GlameCanvasItem*)(node->gui_priv))->output_ports);
		while(list){
			filterport_foreach_pipe(GLAME_CANVAS_PORT(list->data)->port,pipe){
				connection = malloc(sizeof(GlameConnection));
				connection->pipe = pipe;
				connection->begin = GLAME_CANVAS_PORT(list->data);
				canvas_connection_connect_from_pipe(connection);
			}
			list = g_list_next(list);
		}
	}
	canvas_update_scroll_region(GLAME_CANVAS(canv));
	return GLAME_CANVAS(canv);
}

static void draw_network_cb(GtkWidget *bla, GlameCanvasItem *item)
{
	draw_network(item->filter);
}
