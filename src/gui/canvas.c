


/*
 * canvas.c
 *
 * $Id: canvas.c,v 1.10 2000/03/15 16:52:29 xwolf Exp $
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


#include "canvas.h"






gint
image_select(GnomeCanvasItem*item, GdkEvent *event, gpointer data)
{
	GdkCursor *fleur;
	double x,y;
	double dx,dy;
	GlameCanvasItem *it = GLAME_CANVAS_ITEM(data);
	GList *list;
	char *bla="hurgl";

	x = event->button.x;
	y = event->button.y;

	switch(event->type){
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
		case 2:
					    
		case 3:
			edit_canvas_item_properties(it);
			break;

		default:
			break;
		}
	case GDK_MOTION_NOTIFY:
		if(it->dragging && (event->motion.state & GDK_BUTTON1_MASK)){
			dx = x-(it->last_x);
			dy = y-(it->last_y);
			gnome_canvas_item_move(GNOME_CANVAS_ITEM(it),x-(it->last_x),y-(it->last_y));
			list = g_list_first(it->input_ports);
			while(list){
				update_input_connection((GLAME_CANVAS_PORT(list->data)),x-(it->last_x),y-(it->last_y));
				list=g_list_next(list);
			}
			list = g_list_first(it->output_ports);
			while(list){
				update_output_connection((GLAME_CANVAS_PORT(list->data)),x-(it->last_x),y-(it->last_y));
				list=g_list_next(list);
			}
			it->last_x = x;
			it->last_y = y;
		}
		break;
	case GDK_BUTTON_RELEASE:
		gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(data),event->button.time);
		it->dragging = FALSE;
		break;
	default:
		break;
	}
	return FALSE;
}
		

		
static gint delete_canvas(GtkWidget*win,GdkEventAny*ev, gpointer data)
{
	gtk_object_destroy(win);
	return TRUE;
}


static void
dropped(GtkWidget*win, GdkDragContext*cont,gint x,gint y, GtkSelectionData *data, guint info, guint time)
{

	GnomeCanvasGroup*grp;
	int selected;
	gui_filter *gf;
	gui_filter *inst;
	double dx,dy;
	GlameCanvas* canv;
	char *buff;


	canv = GLAME_CANVAS(win);
	buff= malloc(40);
	inst = malloc(sizeof(gui_filter));
	selected = atoi(data->data);
	gf = g_array_index(gui->filters,gui_filter*,selected);

	memcpy(inst,gf,sizeof(gui_filter));
	
	sprintf(buff,"%s%d",inst->caption,time);

	inst->instance=buff;
	

	gui_network_filter_add(canv->net,inst);
	gnome_canvas_window_to_world(GNOME_CANVAS(canv),x,y,&dx,&dy);
	grp = GNOME_CANVAS_GROUP(create_new_node(GNOME_CANVAS(canv),inst,dx,dy));
	
}

void 
launch_network(GtkWidget *button,gui_network*net)
{
	filternetwork_launch(net->net);
	net->paused=FALSE;
}

void 
play_network(GtkWidget *button,gui_network*net)
{
	filternetwork_start(net->net);
}

void 
pause_network(GtkWidget *button,gui_network*net)
{
	if(net->paused){
			filternetwork_start(net->net);
			net->paused=FALSE;
	} else { 
		filternetwork_pause(net->net);
		net->paused=TRUE;
	}
}

void 
stop_network(GtkWidget *button,gui_network*net)
{
	
	filternetwork_terminate(net->net);
}




GtkWidget * 
create_new_canvas(const char *name, gui_network* net)
{
	GtkWidget *window, *canvas, *sw;
	GtkWidget *vbox,*buttonbox,*button;

	GnomeDock *dock;

	GnomeDockItem *item;

	window = gnome_app_new(name,_(name));
	dock = GNOME_APP(window)->dock;
	gtk_widget_ref(dock);
	
	gtk_widget_show(dock);
	


//	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
//	gtk_window_set_title(GTK_WINDOW(window),name);
	
	
	//gtk_signal_connect(GTK_OBJECT(window),
	//		   "delete_event",
	//		   GTK_SIGNAL_FUNC(delete_canvas),
	//		   NULL);
	
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

	gtk_drag_dest_set(GTK_WIDGET(canvas),GTK_DEST_DEFAULT_ALL,gui->target,1,GDK_ACTION_COPY);
	gtk_signal_connect(GTK_OBJECT(canvas),"drag-data-received",GTK_SIGNAL_FUNC(dropped),NULL);

	//gtk_container_add(GTK_CONTAINER(item),sw);
	gtk_container_add(GTK_CONTAINER(sw),canvas);

	gnome_app_set_contents(GNOME_APP(window),sw);

	

	buttonbox = gtk_hbutton_box_new();
	item =  gnome_dock_item_new("buttons",GNOME_DOCK_ITEM_BEH_NORMAL);
	
	gtk_container_add(GTK_CONTAINER(item),buttonbox);
	gnome_dock_add_item(dock,item,GNOME_DOCK_BOTTOM,1,0,0,TRUE);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_EXEC),"Launch");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(button,"clicked",launch_network,net);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_FORWARD),"Play");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(button,"clicked",play_network,net);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_TIMER_STOP),"Pause");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(button,"clicked",pause_network,net);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_STOP),"STOP");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(button,"clicked",stop_network,net);
	
	gnome_app_add_docked(GNOME_APP(window),buttonbox,"butts",GNOME_DOCK_ITEM_BEH_NORMAL,GNOME_DOCK_TOP,1,0,0);

	gtk_widget_show(dock);
	
	gtk_window_set_default_size(GTK_WINDOW(window),400,300);
	gtk_widget_show_all(window);

	return canvas;
}


static gint
input_port_select(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{
	return FALSE;
}

static int
is_inside(GlameCanvasPort* gport, double x, double y)
{

	GnomeCanvasRE* port = GNOME_CANVAS_RE(gport);
	gnome_canvas_item_w2i(port,&x,&y);
	if((port->x1<=x)&&(port->x2>=x)&&(port->y1<=y)&&(port->y2>=y))
		return 1;
	return 0;
}
static GlameCanvasPort * 
find_output_port(GlameCanvas* canvas, double x, double y)
{
	GList* litem,*pitem;
	
	GlameCanvasPort* ret;
	litem = g_list_first(GNOME_CANVAS_GROUP(GNOME_CANVAS(canvas)->root)->item_list);
	while(litem){
		pitem = g_list_first(((GlameCanvasItem*)litem->data)->input_ports);
		while(pitem){
			if(is_inside((GlameCanvasPort*)pitem->data,x,y))
				return (GlameCanvasPort*)pitem->data;
			pitem=g_list_next(pitem);
		}
		litem=g_list_next(litem);
	}
	return NULL;
}
		

		


static gint
output_port_dragging(GnomeCanvasItem *pitem,GdkEvent *event, gpointer data)
{
	GlameCanvasItem *item=GLAME_CANVAS_ITEM(pitem);
	GnomeCanvasItem *released;
	GlameCanvasPort *port;
	double x,y,wx,wy;
	//gnome_canvas_window_to_world(pitem->canvas,event->button.x,event->button.y,&x,&y);
	gnome_canvas_c2w(pitem->canvas,event->button.x,event->button.y,&x,&y);
	switch(event->type){
	case GDK_MOTION_NOTIFY:
		if(event->motion.state & GDK_BUTTON1_MASK){
			gnome_canvas_item_w2i(item,&x,&y);
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

			// Why does this not work correctly??   libgnomeui bug??
		        //released = gnome_canvas_get_item_at(pitem->canvas,x,y);
			released = find_output_port(pitem->canvas,x,y);
			if(released){
				if(GLAME_IS_CANVAS_PORT(released)){
					DPRINTF("port hit!\n");
					port = GLAME_CANVAS_PORT(released);
					if(port->port_type==GUI_PORT_TYPE_IN){
						DPRINTF("inputport! \n");
						item->connection->end = port;
						if(add_connection(item->connection)<0){
							DPRINTF("inner connection failed\n");
							gtk_object_destroy(GTK_OBJECT(item->connection->line));
							free(item->connection);	
						}else{
							DPRINTF("inner connection succeded!\n");
						}
						// connected!!
					} else {
						DPRINTF("not inputport! %d %s",port->port_type,filterportdesc_label(port->port));
						gtk_object_destroy(GTK_OBJECT(item->connection->line));
						free(item->connection);
					}	
				} else {
					DPRINTF("not canvas port %s\n",gtk_type_name(
						released->object.klass->type));
					
				}
			}else{
				gtk_object_destroy(GTK_OBJECT(item->connection->line));
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
		
		

static gint
output_port_select(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{
	filter_portdesc_t *port = (filter_portdesc_t*)data;
	double x,y,x1,y1,x2,y2;
	GnomeCanvasItem *newitem;
	GlameCanvasItem *parent = item->parent;
	GdkCursor *fleur;
	GlameConnection *newconn;
	gnome_canvas_c2w(item->canvas,event->button.x,event->button.y,&x,&y);
	x1=item->x1;
	x2=item->x2;
	y1=item->y1;
	y2=item->y2;

	gnome_canvas_item_w2i(parent,&x1,&y1);
	gnome_canvas_item_w2i(parent,&x2,&y2);	


	gnome_canvas_item_w2i(parent,&x,&y);
	
	
	
	switch(event->type){
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
			newitem = gnome_canvas_item_new(parent,
							gnome_canvas_line_get_type(),
							"points",newconn->points,
							"fill_color","black",
							"width_units",2.0,
							"last_arrowhead",TRUE,
							"arrow_shape_a",18.0,
							"arrow_shape_b",20.0,
							"arrow_shape_c",5.0,
							NULL);
			gnome_canvas_item_raise_to_top(newitem);
			newconn->line = newitem;
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
	//fprintf(stderr,"%s\n",gtk_type_name(item->canvas->current_item->object.klass->type));
	if((GLAME_CANVAS_ITEM(item))->dragging)
		image_select(item,event,data);
	if((GLAME_CANVAS_ITEM(item))->connecting)
		output_port_dragging(item,event,data);
	return FALSE;
}

	


void
create_ports(GnomeCanvasGroup* grp,gui_filter*f)
{
	filter_t*filter =  filter_get(f->caption);
	filter_portdesc_t * port;
	GlameCanvasPort *item;
//	GtkTooltips* tt;

	int portcount=filter_nrinputs(filter);
	double step = 64.0/(float)portcount;
	double border = 0.0;
	filter_foreach_inputdesc(filter,port){
		
		item = glame_canvas_port_new(grp,port,
					     0.0,border,16.0,step,
					     0xff000090);
		item->port_type = GUI_PORT_TYPE_IN;
		(GLAME_CANVAS_ITEM(grp))->input_ports=g_list_append((GLAME_CANVAS_ITEM(grp))->input_ports,item);
//		gnome_canvas_item_raise(GNOME_CANVAS_ITEM(item),9999999);
		border+=step;
/*		gtk_signal_connect(GTK_OBJECT(item),
				   "event",GTK_SIGNAL_FUNC(input_port_select),
				   port);*/
				   
	}
	portcount=filter_nroutputs(filter);
	step = 64.0/(float)portcount;
	border=0.0;
	filter_foreach_outputdesc(filter,port){
		item = glame_canvas_port_new(grp,port,
					     80.0,border,16.0,step,
					     0x0000ff90);
		item->port_type = GUI_PORT_TYPE_OUT;
		(GLAME_CANVAS_ITEM(grp))->output_ports=g_list_append((GLAME_CANVAS_ITEM(grp))->output_ports,item);
		border+=step;

		gtk_signal_connect(GTK_OBJECT(item),
				   "event",GTK_SIGNAL_FUNC(output_port_select),
				   port);
	}
}
	
				      

		
	



GtkWidget*
create_new_node(GnomeCanvas *canvas, gui_filter *filter,double x, double y)
{

	GlameCanvasItem *item;
	GnomeCanvasGroup* root;

	root = gnome_canvas_root(GNOME_CANVAS(canvas));

	item = glame_canvas_item_new(root,
				     filter,
				     0.0,0.0);
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(item),x,y);
	return GTK_WIDGET(item);
}



int
add_connection(GlameConnection *c)
{
//	filter_network_t *net=(GLAME_CANVAS(c->begin->canvas))->net->net;
	
	// ooooohhh f**k this does not look nice!
	if(!filternetwork_add_connection((GLAME_CANVAS_ITEM((GNOME_CANVAS_ITEM(c->begin))->parent))->filter->node,
				     filterportdesc_label(c->begin->port),
					(GLAME_CANVAS_ITEM((GNOME_CANVAS_ITEM(c->end))->parent))->filter->node,
					filterportdesc_label(c->end->port))){
	  DPRINTF("Connection failed!!\n");
		return -1;
	}else {
		DPRINTF("success!\n");
	}
	
	c->begin->connected_ports=g_list_append(c->begin->connected_ports,c);
	c->end->connected_ports=g_list_append(c->end->connected_ports,c);
	return 0;
	
}


void
update_input_connection(GlameCanvasPort*p, gdouble x, gdouble y)
{
	GList* connection = g_list_first(p->connected_ports);
	while(connection){
		((GlameConnection*)(connection->data))->points->coords[2]+=x;
		((GlameConnection*)(connection->data))->points->coords[3]+=y;
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(((GlameConnection*)(connection->data))->line),
				      "points",((GlameConnection*)(connection->data))->points,
				      NULL);
		connection = g_list_next(connection);
	}
}

void
update_output_connection(GlameCanvasPort*p, gdouble x, gdouble y)
{
	GList* connection = g_list_first(p->connected_ports);
	while(connection){
		((GlameConnection*)(connection->data))->points->coords[2]-=x;
		((GlameConnection*)(connection->data))->points->coords[3]-=y;
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(((GlameConnection*)(connection->data))->line),
				      "points",((GlameConnection*)(connection->data))->points,
				      NULL);
		connection = g_list_next(connection);
	}
}


void
edit_canvas_item_properties(GlameCanvasItem *item)
{
	gui_filter * gfilter = item->filter;
	void * val;
	filter_node_t * node = item->filter->node;
	char *paramName,*paramValue;
	paramName = malloc(50);
	paramValue = malloc(50);
	paramName[0]=paramValue[0]=0;
	gtk_dialog_cauldron ("Set Value", 0,
               " ( (Parameter Name:) | %Eod ) / ( (Parameter Value:) | %Ed) / ( %Bgqrxfp || %Bgqxfp ) ",
                   &paramName,&paramValue, GNOME_STOCK_BUTTON_OK, GNOME_STOCK_BUTTON_CANCEL);

	DPRINTF("%s %s\n",paramName,paramValue);
	val = filterparamval_from_string(filter_get_paramdesc(gfilter->filter,paramName),paramValue);
	if(filternode_set_param(node,paramName,val)<0)
	{
		fprintf(stderr,"Change failed!\n");
	}else{
		fprintf(stderr,"Success\n");
	}
	free(paramName);
	free(paramValue);
	free(val);
		
	
}
	

	
	


	
