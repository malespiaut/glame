/*
 * canvas.c
 *
 * $Id: canvas.c,v 1.33 2001/03/02 10:28:13 xwolf Exp $
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


static void canvas_create_new_cb(GtkWidget *bla,void*blu){}
static void canvas_open_filter(GtkWidget*bla,void*blu){}
static void canvas_save_filter(GtkWidget*bla,void*blu){}

int event_x,event_y;
GtkWidget *win;
GtkObject *globalcanvas;
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

static GnomeUIInfo file_menu[]=
{
//	GNOMEUIINFO_ITEM("_New Filter...","New Filter",canvas_create_new_cb,NULL),
	GNOMEUIINFO_MENU_OPEN_ITEM(canvas_load_network,NULL),
//	GNOMEUIINFO_MENU_SAVE_ITEM(canvas_save_filter,NULL),
	GNOMEUIINFO_MENU_SAVE_AS_ITEM(canvas_save_as,NULL),
	GNOMEUIINFO_END
};

GnomeUIInfo *node_select_menu;

static GnomeUIInfo help_menu[]=
{
//	GNOMEUIINFO_ITEM("About...","About",gui_create_about,NULL),
	GNOMEUIINFO_MENU_ABOUT_ITEM(gui_create_about,NULL),
	GNOMEUIINFO_END
};
static GnomeUIInfo root_menu[]=
{
	GNOMEUIINFO_SUBTREE("_Add Node...",&node_select_menu),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("_Load scheme plugin...","Loads scm source file",canvas_load_scheme,NULL),
	GNOMEUIINFO_ITEM("_Register as plugin...","Tries to register current network as a plugin",register_filternetwork_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_MENU_FILE_TREE(file_menu),
	GNOMEUIINFO_MENU_HELP_TREE(&help_menu),
	GNOMEUIINFO_END
};

static GnomeUIInfo port_menu[] = 
{
	GNOMEUIINFO_ITEM("_Connect to external port","Connect",canvas_port_redirect,NULL),
	GNOMEUIINFO_END
};


void canvas_item_edit_properties_cb(GtkWidget* m,GlameCanvasItem *item)
{
	canvas_item_edit_properties(filter_paramdb(item->filter),
				    filter_name(item->filter));
};

void canvas_item_delete_cb(GtkWidget* m,GlameCanvasItem* it)
{
	canvas_item_destroy(it);
}

void
canvas_connection_edit_source_properties_cb(GtkWidget* bla, GlameConnection* conn)
{
	canvas_item_edit_properties(filterpipe_sourceparamdb(conn->pipe),
				    filterport_label(conn->begin->port));
}

void
canvas_connection_edit_dest_properties_cb(GtkWidget* bla, GlameConnection* conn)
{
	canvas_item_edit_properties(filterpipe_destparamdb(conn->pipe),
				    filterport_label(conn->end->port));
}

void 
canvas_item_show_properties(GnomeCanvasItem * item)
{
	GnomeCanvasItem * text;
	filter_param_t * iter;
	
	float y_coord=88.0;
	char buffer[100];
	if(GLAME_CANVAS_ITEM(item->parent)->property_texts){
		DPRINTF("props != zero!\n");
		return;
	}
		

	filterparamdb_foreach_param(filter_paramdb(GLAME_CANVAS_ITEM(item->parent)->filter),iter){
		sprintf(buffer,"%s %s",filterparam_label(iter),filterparam_to_string(iter));
		DPRINTF("%s\n",buffer);
		text = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item->parent),
					     gnome_canvas_text_get_type(),
					     "x",48.0,
					     "y",y_coord,
					     "text",buffer,
					     "clip_width",94.0,
					     "clip_height",16.0,
					     "fill_color","black",
					     "anchor",GTK_ANCHOR_NORTH,
					     "justification",GTK_JUSTIFY_CENTER, // GTK_JUSTIFY_LEFT doesn't work?? why??? wtf?? gnomeui broken?? Me stupid??
					     "font", "-adobe-helvetica-medium-r-normal--12-*-72-72-p-*-iso8859-1",
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
					     "x",48.0,
					     "y",y_coord,
					     "text",buffer,
					     "clip_width",94.0,
					     "clip_height",16.0,
					     "fill_color","red",
					     "anchor",GTK_ANCHOR_NORTH,
					     "justification",GTK_JUSTIFY_CENTER, // GTK_JUSTIFY_LEFT doesn't work?? why??? wtf?? gnomeui broken?? Me stupid??
					     "font", "-adobe-helvetica-medium-r-normal--12-*-72-72-p-*-iso8859-1",
					     "clip",0,
					     NULL);
		GLAME_CANVAS_ITEM(item->parent)->property_texts = g_list_append(GLAME_CANVAS_ITEM(item->parent)->property_texts,text);
			
	}
}

void
canvas_item_delete_property_list(gpointer item,gpointer bla)
{
        gtk_object_destroy(GTK_OBJECT(item));
}
void 
canvas_item_hide_properties(GnomeCanvasItem * item)
{
        if(!GLAME_CANVAS_ITEM(item->parent)->property_texts){
		DPRINTF("props == zero!\n");
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
	//	fprintf(stderr,"Ev: %d\n",event->type);
	switch(event->type){
	case GDK_ENTER_NOTIFY:
	      inItem=1;
	      canvas_item_show_properties(item);
	      break;
	case GDK_LEAVE_NOTIFY:
	      inItem=0;
	      canvas_item_hide_properties(item);
	      break;
	case GDK_BUTTON_PRESS:
	      switch(event->button.button){
	      case 1:
		        it->last_x = x;
			it->last_y = y;
			fleur = gdk_cursor_new(GDK_FLEUR);
			gnome_canvas_item_grab(GNOME_CANVAS_ITEM(data),
					       GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK,
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
	case GDK_BUTTON_RELEASE:
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
	default:
		break;
	}
	return TRUE;
}


int
canvas_add_filter_by_name(char *name)
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

void 
network_play(GtkWidget *button,gui_network*net)
{
	network_error_reset(net);
	filter_launch(net->net);
	
	if(filter_start(net->net)<0){
		network_draw_error(net);
	}
	net->paused = FALSE;
}

void 
network_pause(GtkWidget *button,gui_network*net)
{
	if(net->paused){
		filter_start(net->net);
		net->paused=FALSE;
	} else { 
		filter_pause(net->net);
		net->paused=TRUE;
	}
}

void 
network_stop(GtkWidget *button,gui_network*net)
{
	filter_terminate(net->net);
}
void
canvas_add_filter_by_name_cb(GtkWidget*wid, char* name)
{
	canvas_add_filter_by_name(name);
	
}

typedef struct {
	char* categorie;
	int count;
	GtkWidget * submenu;
} catmenu;	

	
GSList* append_to_categorie(GSList* cats,plugin_t *plug)
{
	char* catpath;

	GSList * item;
	GtkWidget *newitem;
	catmenu* newcat;
	char* cdefault="Default";
	catpath = plugin_query(plug,PLUGIN_CATEGORY);
	if(!catpath)
		catpath = cdefault;
	item = cats;
	while(item){
		if(!(strcmp(catpath,(char*)(((catmenu*)(item->data))->categorie)))){
			newcat = (catmenu*)(item->data);
			goto found;
		}
		item = g_slist_next(item);
	}
	//not found
	newcat = malloc(sizeof(catmenu));
	newcat->submenu = gtk_menu_new();
	newcat->count = 0;
	gtk_widget_show(newcat->submenu);
	newcat->categorie = strdup(catpath);
	cats=g_slist_append(cats,newcat);
 found:
	newitem = gtk_menu_item_new_with_label((gchar*)strdup(plugin_name(plug)));
	gtk_widget_show(newitem);
	gtk_menu_append(GTK_MENU(newcat->submenu),newitem);
	newcat->count++;
	gtk_signal_connect(GTK_OBJECT(newitem),"activate",canvas_add_filter_by_name_cb,(gchar*)plugin_name(plug));
	return cats;
}

gint
root_event(GnomeCanvas *canv,GdkEvent*event,gpointer data)
{
	GtkWidget*submenu;
	GtkWidget*menu,*item;
	GtkWidget*par;
	GdkEventButton *event_button;
	GSList * nodes,*it;
	int i;

	catmenu  *cat;
	GSList * categories=NULL;
	

	plugin_t * plugin;
	switch(event->type){
	case GDK_BUTTON_PRESS:
		if(!inItem){
			
			event_button = (GdkEventButton *) event;
//			gnome_canvas_c2w(canv,event->button.x,event->button.y,&event_x,&event_y);
			event_x = event->button.x;
			event_y = event->button.y;
			win=GTK_WIDGET(canv);
			if(event->button.button==3){
				cat = malloc(sizeof(catmenu));
				cat->categorie = strdup("Default");
				cat->count = 0;
				cat->submenu = gtk_menu_new();
				categories = g_slist_append(categories,cat);
				
				nodes=gui_browse_registered_filters(); 
				menu = gnome_popup_menu_new(root_menu);
				submenu = gtk_menu_new();
				par = root_menu[0].widget; // eeevil hack to change menu
				
				for(i=0;i<g_slist_length(nodes);i++){ 
					it = g_slist_nth(nodes,i); 
					plugin = (plugin_t*)(it->data);
//					item = gtk_menu_item_new_with_label((gchar*)plugin_name(plugin));
//				        gtk_widget_show(item);
					categories = append_to_categorie(categories,plugin);
				}
				for(i=0;i<g_slist_length(categories);i++){
					cat = (catmenu*)((GSList*)g_slist_nth(categories,i)->data);
					if(cat->count){
					item = gtk_menu_item_new_with_label(cat->categorie);
					gtk_menu_item_set_submenu(GTK_MENU_ITEM(item),cat->submenu);
					gtk_widget_show(item);
					gtk_menu_append(GTK_MENU(submenu),item);
					}
				}
				gtk_widget_show(submenu);
				gtk_menu_item_set_submenu(GTK_MENU_ITEM(par),submenu);
				
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,canv);
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
	GtkWidget *window, *canvas, *sw;
	GtkWidget *buttonbox,*button;

	GnomeDock *dock;

	GnomeDockItem *item;
	const char *name = strdup("Untitled");
	window = gnome_app_new(name,_(name));
	dock = GNOME_DOCK(GNOME_APP(window)->dock);
	gtk_widget_ref(GTK_WIDGET(dock));
	
	gtk_widget_show(GTK_WIDGET(dock));
	

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

	gnome_app_set_contents(GNOME_APP(window),sw);

	buttonbox = gtk_hbutton_box_new();
	item =  GNOME_DOCK_ITEM(gnome_dock_item_new("buttons",GNOME_DOCK_ITEM_BEH_NORMAL));
	
	gtk_container_add(GTK_CONTAINER(item),buttonbox);
	gnome_dock_add_item(dock,item,GNOME_DOCK_BOTTOM,1,0,0,TRUE);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_FORWARD),"Play");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(GTK_OBJECT(button),"clicked",network_play,net);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_TIMER_STOP),"Pause");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(GTK_OBJECT(button),"clicked",network_pause,net);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_STOP),"STOP");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(GTK_OBJECT(button),"clicked",network_stop,net);
	
	gtk_widget_show(GTK_WIDGET(dock));
	
	gtk_window_set_default_size(GTK_WINDOW(window),400,300);
	gtk_widget_show_all(window);
	gtk_signal_connect_after(GTK_OBJECT(canvas),"event",GTK_SIGNAL_FUNC(root_event),NULL);
	globalcanvas = GTK_OBJECT(canvas);
	return window;
}


gint
canvas_input_port_event_cb(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{

	GtkWidget * menu;

		
	switch(event->type){
	case GDK_ENTER_NOTIFY:
		inItem=1;
		break;
	case GDK_LEAVE_NOTIFY:
		inItem=0;
		break;
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 3:
			menu = gnome_popup_menu_new(port_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,item);
		default:
			break;
		}
	default:
		break;		
		
		
	}
		
	
	
	return TRUE;
}

int
canvas_port_is_inside(GlameCanvasPort* gport, double x, double y)
{

	GnomeCanvasRE* port = GNOME_CANVAS_RE(gport);
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(port),&x,&y);
	if((port->x1<=x)&&(port->x2>=x)&&(port->y1<=y)&&(port->y2>=y))
		return 1;
	return 0;
}

		
void
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

gint
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


gint
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
						
      


gint
canvas_output_port_motion(GnomeCanvasItem *pitem,GdkEvent *event, gpointer data)
{
	GlameCanvasItem *item=GLAME_CANVAS_ITEM(pitem);
	GnomeCanvasItem *released;
	GlameCanvasPort *port;

	double x,y;
	//gnome_canvas_window_to_world(pitem->canvas,event->button.x,event->button.y,&x,&y);
	gnome_canvas_c2w(pitem->canvas,event->button.x,event->button.y,&x,&y);
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



void
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


void
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
		

void 
canvas_connection_destroy_cb(GtkWidget*bla,GlameConnection* conn)
{
	canvas_connection_destroy(conn);
}
	
gint
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

gint
canvas_output_port_event_cb(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{
	double x,y,x1,y1,x2,y2;
	GnomeCanvasItem *newitem;
	GtkWidget * menu;
	GlameCanvasItem *parent = GLAME_CANVAS_ITEM(item->parent);
	GdkCursor *fleur;
	GlameConnection *newconn;
	gnome_canvas_c2w(item->canvas,event->button.x,event->button.y,&x,&y);
	x1=item->x1;
	x2=item->x2;
	y1=item->y1;
	y2=item->y2;
	
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(parent),&x1,&y1);
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(parent),&x2,&y2);	
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(parent),&x,&y);
		
	switch(event->type){
	case GDK_LEAVE_NOTIFY:
		inItem = 0;
		break;
	case GDK_ENTER_NOTIFY:
		inItem = 1;
		break;
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 1:
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
					       GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK,
					       fleur,
					       event->button.time);
			gdk_cursor_destroy(fleur);
			parent->connecting=TRUE;
			return TRUE;
			break;
		case 3:
			menu = gnome_popup_menu_new(port_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,item);
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
	//DPRINTF("%s\n",gtk_type_name(item->canvas->current_item->object.klass->type));

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
	
				      

		
	



GtkObject*
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


void
canvas_connection_connect_from_pipe(GlameConnection *c)
{
	filter_port_t *dest;
	GlameCanvasItem * item;
	GList * ports;
	GlameCanvasPort * destPort=NULL;
	c->line=NULL;
	dest = filterpipe_dest(c->pipe);
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
	
int
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

	    

void
canvas_input_port_update_connections(GlameCanvasPort*p, gdouble x, gdouble y)
{
	GList* connection = g_list_first(p->connected_ports);
	
	while(connection){
	      canvas_connection_update_points((GlameConnection*)(connection->data));
	      connection = g_list_next(connection);
	}
}

void
canvas_output_port_update_connections(GlameCanvasPort*p, gdouble x, gdouble y)
{
	GList* connection = g_list_first(p->connected_ports);
	
	while(connection){
	      canvas_connection_update_points((GlameConnection*)(connection->data));
	      connection = g_list_next(connection);
	}
}

enum {PINT,PFLOAT,PSTRING,PFILE};

typedef struct {
	GtkWidget *widget;
	filter_param_t* param;
	int widget_type;
} param_widget_t;

typedef struct {
	GList* paramList;
	char *caption;
} param_callback_t;



void
update_params(GnomePropertyBox *propertybox, param_callback_t* callback)
{
	GList* list = g_list_first(callback->paramList);
	char *strVal; 
	int iVal;
	float fVal;
	char *caption = callback->caption;
	param_widget_t* item;
	
	while(list){
		item = (param_widget_t*)(list->data);
		DPRINTF("param: %s\n", filterparam_label(item->param));
		switch(item->widget_type){
		case PINT:
			iVal = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(item->widget));
			DPRINTF("Setting %s::%s to %i", caption, filterparam_label(item->param), iVal);
			if(filterparam_set(item->param, &iVal) == -1)
				DPRINTF(" - failed!\n");
			else
				DPRINTF(" - success!\n");
			break;
		case PFLOAT:
			fVal = gtk_spin_button_get_value_as_float(GTK_SPIN_BUTTON(item->widget));
			DPRINTF("Setting %s::%s to %f", caption, filterparam_label(item->param), fVal);
			if(filterparam_set(item->param, &fVal) == -1)
				DPRINTF(" - failed!\n");
			else
				DPRINTF(" - success!\n");
			break;
		case PSTRING:
			strVal = gtk_editable_get_chars(GTK_EDITABLE(gnome_entry_gtk_entry(GNOME_ENTRY(item->widget))),0,-1);
			DPRINTF("Setting %s::%s to %s", caption, filterparam_label(item->param), strVal);
			if(filterparam_set(item->param, &strVal) == -1)
				DPRINTF(" - failed!\n");
			else
				DPRINTF(" - success!\n");
			g_free(strVal);
			break;
		case PFILE:
			strVal = gtk_editable_get_chars(GTK_EDITABLE(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(item->widget))),0,-1);
			DPRINTF("Setting %s::%s to %s", caption, filterparam_label(item->param), strVal);
			if(filterparam_set(item->param, &strVal) == -1)
				DPRINTF(" - failed!\n");
			else
				DPRINTF(" - success!\n");
			g_free(strVal);
			break;
		}
		list = g_list_next(list);
	}
}

void
cancel_params(GtkWidget* wig,param_callback_t* callback)
{
	g_list_free(callback->paramList);
	gtk_widget_destroy(GTK_WIDGET(gtk_widget_get_parent_window(wig)));
}

void
canvas_item_edit_properties(filter_paramdb_t *pdb, const char *caption)
{
	GtkWidget *vbox,*entry;
	GtkAdjustment *adjust;
	param_widget_t *pw;
	param_callback_t* cb;
	char * prop;
	GList * list=NULL;
	
	GtkWidget* propBox;
	GtkWidget* tablabel;
	int iVal;
	float fVal;
	char* cVal;
	filter_param_t* param;
	propBox = gnome_property_box_new ();
	
	tablabel=gtk_label_new(_(caption));

	vbox = gtk_vbox_new(FALSE,3);
	
	gtk_widget_show(vbox);

	filterparamdb_foreach_param(pdb, param) {
		if (FILTER_PARAM_IS_INT(param)) {
			adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,(float)-MAXINT,(float)MAXINT,1.0,10.0,10.0));

			entry = gtk_spin_button_new(GTK_ADJUSTMENT(adjust),1.0,5);
			gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(entry),TRUE);
			gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry),0);
			iVal = filterparam_val_int(param);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry),(float)iVal);
			create_label_widget_pair(vbox,filterparam_label(param),entry);
			pw = malloc(sizeof(param_widget_t));
			pw->widget = entry;
			pw->param = param;
			pw->widget_type = PINT;
			list = g_list_append(list,pw);
		} else if (FILTER_PARAM_IS_FLOAT(param)
			   || FILTER_PARAM_IS_SAMPLE(param)) {
			adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,-MAXFLOAT,MAXFLOAT,1.0,10.0,10.0));
			entry = gtk_spin_button_new(GTK_ADJUSTMENT(adjust),1.0,5);
			gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(entry),TRUE);
			gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry),3);
			fVal = filterparam_val_float(param);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry),fVal);
			create_label_widget_pair(vbox,filterparam_label(param),entry);
			pw = malloc(sizeof(param_widget_t));
			pw->widget = entry;
			pw->param = param;
			pw->widget_type = PFLOAT;
			list = g_list_append(list,pw);
		} else if (FILTER_PARAM_IS_STRING(param)) {
			switch(filterparam_type(param)){
			case FILTER_PARAMTYPE_FILENAME:
				entry = gnome_file_entry_new("blahh",filterparam_label(param));
				if((prop = filterparam_get_property(param,FILTER_PARAM_PROPERTY_FILE_FILTER)))
				      gtk_signal_connect_after(GTK_OBJECT(entry),"browse_clicked",GTK_SIGNAL_FUNC(set_file_selection_filter),prop);
				
				create_label_widget_pair(vbox,filterparam_label(param),entry);
				pw = malloc(sizeof(param_widget_t));
				pw->widget = entry;
				pw->param = param;
				pw->widget_type = PFILE;
				list = g_list_append(list,pw);
				break;
			default:
			        entry = gnome_entry_new("blubb");
				create_label_widget_pair(vbox,filterparam_label(param),entry);
				cVal =  filterparam_val_string(param);
				gtk_entry_set_text(GTK_ENTRY(gnome_entry_gtk_entry(GNOME_ENTRY(entry))),cVal);
				free(cVal);
				pw = malloc(sizeof(param_widget_t));
				pw->widget = entry;
				pw->param = param;
				pw->widget_type = PSTRING;
				list = g_list_append(list,pw);
				break;
			}
		} else {
			entry = gnome_entry_new("blubb");
			create_label_widget_pair(vbox,filterparam_label(param),entry);
			pw = malloc(sizeof(param_widget_t));
			pw->widget = entry;
			pw->param = param;
			pw->widget_type = PSTRING;
			list = g_list_append(list,pw);
			break;
		}
	}
	gnome_property_box_append_page(GNOME_PROPERTY_BOX(propBox),vbox,tablabel);

	gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->apply_button));
	gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->help_button));
	gtk_window_set_modal(GTK_WINDOW(propBox),TRUE);
	gtk_widget_show(propBox);
	cb = malloc(sizeof(param_callback_t));
	cb->paramList=list;
	cb->caption = strdup(caption);
	
	gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->ok_button),"clicked",update_params,cb);
	gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->cancel_button),"clicked",cancel_params,cb);	
	
}
	

	
	

void
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

void
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


void canvas_register_as_cb(gchar* name, gpointer data)
{
	plugin_t *newplug;
	filter_t *copy;
	filter_t * bla = GLAME_CANVAS(globalcanvas)->net->net;
	
	newplug = plugin_add(name);
	copy = filter_creat(bla);
	filter_register(copy,newplug);
	
}

void changeString(GtkEditable *wid, char ** returnbuffer)
{
	strncpy(*returnbuffer,gtk_editable_get_chars(wid,0,-1),100);
}


void set_file_selection_filter(GnomeFileEntry* entry, const char * filter)
{
      gtk_file_selection_complete(GTK_FILE_SELECTION(entry->fsw),filter);
}

void canvas_save_as(GtkWidget*w,void*blu)
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

		bla = GLAME_CANVAS(globalcanvas)->net->net;

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

void register_filternetwork_cb(GtkWidget*bla,void*blu)
{
	gnome_request_dialog(0,"Filtername","filter",16,canvas_register_as_cb,NULL,NULL);
}

void 
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
	gnome_canvas_set_scroll_region(GNOME_CANVAS(canv),x1,y1,x2,y2);



}

void
canvas_load_network(GtkWidget *bla, void *blu)
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
		}

	}
	free(filenamebuffer);
}

void canvas_load_scheme(GtkWidget*bla,void*blu)
{
	GtkWidget * fileEntry;
	GtkWidget * dialog;
	GtkWidget * vbox;
	char * filenamebuffer;
	filenamebuffer = calloc(100,sizeof(char));

	dialog = gnome_dialog_new("Load scheme code",GNOME_STOCK_BUTTON_CANCEL,GNOME_STOCK_BUTTON_OK,NULL);
	vbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));

	fileEntry = gnome_file_entry_new("Load","Filename");
	gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fileEntry))),"changed",changeString,&filenamebuffer);
	create_label_widget_pair(vbox,"Filename",fileEntry);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog))){
		if(glame_load_plugin(filenamebuffer)){
			// load_plugin doesn't really give anything sensible back
			//errorbox = gnome_warning_dialog("Loading failed...");
			//gnome_dialog_run_and_close(errorbox);
		}
	}
	free(filenamebuffer);
}



void canvas_port_redirect(GtkWidget*bla,GlameCanvasPort *blu)
{
	GtkWidget * nameEntry;
	GtkWidget * dialog;
	GtkWidget * vbox;
	char * filenamebuffer;

	filter_portdb_t *ports;
	filter_port_t * newport;

	filenamebuffer = calloc(100,sizeof(char));

	dialog = gnome_dialog_new("Load scheme code",GNOME_STOCK_BUTTON_CANCEL,GNOME_STOCK_BUTTON_OK,NULL);
	vbox = GNOME_DIALOG(dialog)->vbox;

	nameEntry = gtk_entry_new();
	gtk_signal_connect(GTK_OBJECT(nameEntry),"changed",
			   changeString,&filenamebuffer);
	create_label_widget_pair(vbox,"New port name",nameEntry);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog))){
		ports = filter_portdb(GLAME_CANVAS(globalcanvas)->net->net);  // ugh FIXME globals suck
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
	free(filenamebuffer);
}

void update_string(GtkListItem* item,char ** returnbuffer)
{
	// this is a little kludgy.. FIXME
	char *label;
	
	label = strdup(GTK_LABEL(GTK_BIN(item)->child)->label);
	strncpy(*returnbuffer,label,100);
	free(label);  
}

void update_entry_text(GtkListItem* item,GtkEntry* entry)
{
	// this is a little kludgy.. FIXME
	char *label;
	label = strdup(GTK_LABEL(GTK_BIN(item)->child)->label);
	gtk_entry_set_text(entry,label);
	free(label);  
}

void canvas_item_redirect_parameters(GtkWidget *bla, GlameCanvasItem *item)
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

	dialog = gnome_dialog_new("Select parameter",GNOME_STOCK_BUTTON_CANCEL,"Remap All",GNOME_STOCK_BUTTON_OK,NULL);
		
	vbox = GNOME_DIALOG(dialog)->vbox;
	
	list = gtk_list_new();
	entry = gtk_entry_new();
	
	db = filter_paramdb(item->filter);
	topleveldb = filter_paramdb(GLAME_CANVAS(globalcanvas)->net->net);

	filterparamdb_foreach_param(db,iter){
		listItems = gtk_list_item_new_with_label(filterparam_label(iter));
		gtk_signal_connect(GTK_OBJECT(listItems),"select",update_entry_text,entry);
		gtk_signal_connect(GTK_OBJECT(listItems),"select",update_string,&paramnamebuffer);
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

								    
}


void canvas_item_show_description(GtkWidget* wid,GlameCanvasItem* it)
{
	GtkWidget *dialog;
	GtkWidget * text;
	GtkWidget * vbox;
	char * desc;
	int pos=0;
	
	dialog = gnome_dialog_new(plugin_name(it->filter->plugin),GNOME_STOCK_BUTTON_OK,NULL);
	vbox = GNOME_DIALOG(dialog)->vbox;
	desc = (char*)plugin_query(it->filter->plugin,PLUGIN_DESCRIPTION);
	text = gtk_text_new(NULL,NULL);
	
	if(desc)
		gtk_editable_insert_text(GTK_EDITABLE(text),desc,strlen(desc),&pos);
	else
		gtk_editable_insert_text(GTK_EDITABLE(text),"This item does not have a description",38,&pos);
	gtk_widget_show(text);
	gtk_box_pack_start (GTK_BOX (GNOME_DIALOG (dialog)->vbox), text, TRUE, TRUE, 0);
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

void
draw_network_cb(GtkWidget *bla, GlameCanvasItem *item)
{
	draw_network(item->filter);
}
