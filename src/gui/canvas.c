


/*
 * canvas.c
 *
 * $Id: canvas.c,v 1.12 2000/03/20 17:49:43 xwolf Exp $
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
	gtk_object_destroy(GTK_OBJECT(win));
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
create_new_canvas(gui_network* net)
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
	item =  GNOME_DOCK_ITEM(gnome_dock_item_new("buttons",GNOME_DOCK_ITEM_BEH_NORMAL));
	
	gtk_container_add(GTK_CONTAINER(item),buttonbox);
	gnome_dock_add_item(dock,item,GNOME_DOCK_BOTTOM,1,0,0,TRUE);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_EXEC),"Launch");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(GTK_OBJECT(button),"clicked",launch_network,net);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_FORWARD),"Play");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(GTK_OBJECT(button),"clicked",play_network,net);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_TIMER_STOP),"Pause");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(GTK_OBJECT(button),"clicked",pause_network,net);

	button = gnome_pixmap_button(gnome_stock_pixmap_widget(window,GNOME_STOCK_PIXMAP_STOP),"STOP");
	gtk_container_add(GTK_CONTAINER(buttonbox),button);
	gtk_signal_connect(GTK_OBJECT(button),"clicked",stop_network,net);
	
	gtk_widget_show(GTK_WIDGET(dock));
	
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
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(port),&x,&y);
	if((port->x1<=x)&&(port->x2>=x)&&(port->y1<=y)&&(port->y2>=y))
		return 1;
	return 0;
}
static GlameCanvasPort * 
find_output_port(GlameCanvas* canvas, double x, double y)
{
	GList* litem,*pitem;
	
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
		
static void
port_docking_coords(GlameCanvasPort* port,double *x, double *y)
{
	GnomeCanvasRE* rect = GNOME_CANVAS_RE(port);
	int i,items = g_list_length(port->connected_ports);
	GList* list = g_list_first(port->connected_ports);
	double difference,step;
	if(items>1){
		step=difference = (1.0/(double)(items*(items+1)))*(rect->y2-rect->y1);
		for(i=1;i<items;i++){
			move_single_connection(((GlameConnection*)(list->data)),0.0,-i*step);
			list=g_list_next(list);
		}
		*y=(double)(items)/(double)(items+1)*(rect->y2-rect->y1);
	}else{
		*y=(rect->y1+rect->y2)*0.5;
		
	}
	*x=rect->x1;
	gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(rect),x,y);
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
			
			// Why does this not work correctly??   libgnomeui bug??
			//released = gnome_canvas_get_item_at(pitem->canvas,x,y);
			released = GNOME_CANVAS_ITEM(find_output_port(GLAME_CANVAS(pitem->canvas),x,y));
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
							port_docking_coords(port,&x,&y);
							gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(item->connection->line),&x,&y);
							item->connection->points->coords[2]=x;
							item->connection->points->coords[3]=y;
							gnome_canvas_item_set(GNOME_CANVAS_ITEM(item->connection->line),
									      "points",item->connection->points,NULL);
							wx = item->connection->points->coords[0] + item->connection->points->coords[2];
							wx *= 0.5;
							wx -= 6.0;
							wy = item->connection->points->coords[1] + item->connection->points->coords[3];
							wy *= 0.5;
							wy -= 6.0;
							item->connection->circle = 
								GNOME_CANVAS_ELLIPSE(
									gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
											      gnome_canvas_ellipse_get_type(),
											      "x1",wx,
											      "x2",wx+12.0,
											      "y1",wy,
											      "y2",wy+12.0,
											      "fill_color","black",
											      "width_pixels",5,
											      NULL));
							gtk_signal_connect(GTK_OBJECT(item->connection->circle),
									   "event",GTK_SIGNAL_FUNC(connection_select),
									   item->connection);
							
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


// this funktion expects world coordinates!
void 
set_single_connection(GlameConnection* c, gdouble x, gdouble y)
{


	gdouble mx,my;
	
	gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(c->line)->parent,&x,&y);
	
	c->points->coords[2]=x;
	c->points->coords[3]=y;

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(c->line),
			      "points",c->points,NULL);

	mx = c->points->coords[2]+c->points->coords[0];
	my = c->points->coords[3]+c->points->coords[1];
	
	mx*=0.5;
	my*=0.5;


	/* FIXME this is stupid, but I'm not in the mood for debugging coordinate systems just now....    [xwolf] */
	gtk_object_destroy(GTK_OBJECT(c->circle));
	c->circle = GNOME_CANVAS_ELLIPSE(gnome_canvas_item_new(GNOME_CANVAS_GROUP(GNOME_CANVAS_ITEM(c->line)->parent),
					  gnome_canvas_ellipse_get_type(),
					  "x1",mx-6.0,
					  "x2",mx+6.0,
					  "y1",my-6.0,
					  "y2",my+6.0,
					  "fill_color","black",
					  "width_pixels",5,
					  NULL));
	gtk_signal_connect(GTK_OBJECT(c->circle),
			   "event",GTK_SIGNAL_FUNC(connection_select),
			   c);
/*	gnome_canvas_item_set(GNOME_CANVAS_ITEM(c->circle),
			      "x1",mx-6.0,
			      "y1",my-6.0,
			      "x2",mx+6.0,
			      "y2",my+6.0,
			      NULL);*/
}



static void
reorder_port_connections(GlameCanvasPort* port)
{
	GList *list = g_list_first(port->connected_ports);
	int length  = g_list_length(list);
	double y,ystep,x;
	x = GNOME_CANVAS_RE(port)->x1;
	ystep = (GNOME_CANVAS_RE(port)->y2 - GNOME_CANVAS_RE(port)->y1)/(double)(length+1);
	y =  GNOME_CANVAS_RE(port)->y1+ystep;
	gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(port)->parent,&x,&y);
	while(list){
		set_single_connection(list->data,x,y);
		list = g_list_next(list);
		y+=ystep;
	}
}
		

static void
connection_break(GlameConnection* connection)
{
	filternetwork_break_connection(connection->pipe);
	connection->begin->connected_ports=g_list_remove(connection->begin->connected_ports,connection);
	connection->end->connected_ports=g_list_remove(connection->end->connected_ports,connection);
	gtk_object_destroy(GTK_OBJECT(connection->line));
	gtk_object_destroy(GTK_OBJECT(connection->circle));
	reorder_port_connections(connection->end);
	free (connection);
}
	
static gint
connection_select(GnomeCanvasItem* item, GdkEvent *event,gpointer data)
{

	GlameConnection* connection = (GlameConnection*)data;
	switch(event->type){
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			break;
		case 2:
			DPRINTF("Breaking connection\n");
			connection_break(connection);
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

static gint
output_port_select(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{
	double x,y,x1,y1,x2,y2;
	GnomeCanvasItem *newitem;
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
	
				      

		
	



GtkObject*
create_new_node(GnomeCanvas *canvas, gui_filter *filter,double x, double y)
{

	GlameCanvasItem *item;
	GnomeCanvasGroup* root;

	root = gnome_canvas_root(GNOME_CANVAS(canvas));

	item = glame_canvas_item_new(root,
				     filter,
				     0.0,0.0);
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(item),x,y);
	return GTK_OBJECT(item);
}



int
add_connection(GlameConnection *c)
{
//	filter_network_t *net=(GLAME_CANVAS(c->begin->canvas))->net->net;
	
	// ooooohhh f**k this does not look nice!
	c->pipe=filternetwork_add_connection((GLAME_CANVAS_ITEM((GNOME_CANVAS_ITEM(c->begin))->parent))->filter->node,
				     filterportdesc_label(c->begin->port),
					(GLAME_CANVAS_ITEM((GNOME_CANVAS_ITEM(c->end))->parent))->filter->node,
					     filterportdesc_label(c->end->port));
	if(!c->pipe)
	{
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
move_single_connection(GlameConnection* c, gdouble x, gdouble y)
{
	c->points->coords[2]+=x;
	c->points->coords[3]+=y;
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(c->line),"points",c->points,NULL);
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(c->circle),x*0.5,y*0.5);
}

	    

void
update_input_connection(GlameCanvasPort*p, gdouble x, gdouble y)
{
	GList* connection = g_list_first(p->connected_ports);
	GnomeCanvasRE* circle;
	gdouble x_2,y_2;
	x_2 = x*0.5;
	y_2 = y*0.5;
	while(connection){
		((GlameConnection*)(connection->data))->points->coords[2]+=x;
		((GlameConnection*)(connection->data))->points->coords[3]+=y;
		circle = GNOME_CANVAS_RE(((GlameConnection*)(connection->data))->circle);
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(((GlameConnection*)(connection->data))->line),
				      "points",((GlameConnection*)(connection->data))->points,
				      NULL);
		if(circle)
			gnome_canvas_item_set(GNOME_CANVAS_ITEM(circle),
					      "x1",circle->x1+x_2,
					      "x2",circle->x2+x_2,
					      "y1",circle->y1+y_2,
					      "y2",circle->y2+y_2,
					      NULL);
		connection = g_list_next(connection);
	}
}

void
update_output_connection(GlameCanvasPort*p, gdouble x, gdouble y)
{
	GList* connection = g_list_first(p->connected_ports);
	GnomeCanvasRE* circle;
	gdouble x_2,y_2;
	x_2 = x*0.5;
	y_2 = y*0.5;
	while(connection){
		((GlameConnection*)(connection->data))->points->coords[2]-=x;
		((GlameConnection*)(connection->data))->points->coords[3]-=y;
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(((GlameConnection*)(connection->data))->line),
				      "points",((GlameConnection*)(connection->data))->points,
				      NULL);
		circle = GNOME_CANVAS_RE(((GlameConnection*)(connection->data))->circle);
		if(circle)
			gnome_canvas_item_set(GNOME_CANVAS_ITEM(circle),
					      "x1",circle->x1-x_2,
					      "x2",circle->x2-x_2,
					      "y1",circle->y1-y_2,
					      "y2",circle->y2-y_2,
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
	

	
	


	
