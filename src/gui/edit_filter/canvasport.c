/*
 * canvasport.c
 *
 * $Id: canvasport.c,v 1.8 2001/05/28 13:07:55 xwolf Exp $
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
#include "hash.h"


HASH(gcport, GlameCanvasPort, 8,
	(gcport->port == key ),
	((long)key/4),
	((long)gcport->port/4),
	filter_port_t * key)

     /*  Forward decls */



/*Yukky gtk class stuff    
 * Ignore these
 */


enum {
	MOVED,
	CONNECTIONS_CHANGED,
	LAST_SIGNAL
};
static guint port_signals[LAST_SIGNAL] = { 0 };





static void
glame_canvas_port_destroy (GtkObject *object)
{

	hash_remove_gcport(GLAME_CANVAS_PORT(object));
	gtk_object_destroy(object);
	//GTK_OBJECT_CLASS(gtk_type_class(GLAME_CANVAS_PORT_TYPE))->destroy(object);
}


static void
glame_canvas_port_connections_changed_cb(GlameCanvasPort *p, gpointer userdata)
{
	filter_pipe_t *pipe;
	GlameCanvasPipe* gPipe;

	int count = 1;
	filterport_foreach_pipe(p->realPort, pipe){
		gPipe = glame_canvas_find_pipe(pipe);
		if(gPipe){
			if(filterport_is_input(p->port)){
				gPipe->destId = count++;
			}else{
				if(filterport_is_output(p->port)){
					gPipe->sourceId = count++;

				}
			}
		}
	}
}

static void
glame_canvas_port_class_init(GlameCanvasPortClass* class)
{
	
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = glame_canvas_port_destroy;

	port_signals[MOVED] = gtk_signal_new("moved",
					     GTK_RUN_LAST,
					     object_class->type,
					     GTK_SIGNAL_OFFSET(GlameCanvasPortClass, moved),
					     glame_canvas_marshal_NONE__DOUBLE_DOUBLE,
					     GTK_TYPE_NONE,
					     2,
					     GTK_TYPE_DOUBLE,
					     GTK_TYPE_DOUBLE);
	port_signals[CONNECTIONS_CHANGED] = gtk_signal_new("connections_changed",
							   GTK_RUN_FIRST,
							   object_class->type,
							   GTK_SIGNAL_OFFSET(GlameCanvasPortClass,connections_changed),
							   gtk_marshal_NONE__NONE,
							   GTK_TYPE_NONE,
							   0);
	
	gtk_object_class_add_signals(object_class,port_signals,LAST_SIGNAL);
	
	class->moved = NULL;
	class->connections_changed = glame_canvas_port_connections_changed_cb;
}

static void
glame_canvas_port_init(GlameCanvasPort* p)
{
	p->port = NULL;
	p->x = 0.0;
	p->y = 0.0;
	p->x_size = 0.0;
	p->y_size = 0.0;
	p->name = NULL;
	p->external = FALSE;

}		


GtkType
glame_canvas_port_get_type(void)
{
	static GtkType canvas_port_type = 0;
	
	if(!canvas_port_type){
		GtkTypeInfo canvas_port_info={
			"GlameCanvasPort",
			sizeof(GlameCanvasPort),
			sizeof(GlameCanvasPortClass),
			(GtkClassInitFunc) glame_canvas_port_class_init,
			(GtkObjectInitFunc) glame_canvas_port_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		canvas_port_type = gtk_type_unique(GNOME_TYPE_CANVAS_RECT,
						   &canvas_port_info);
		gtk_type_set_chunk_alloc(canvas_port_type,8);
	}
	
	return canvas_port_type;
}

void glame_canvas_port_pipe_deleted_cb(GlameCanvasPipe* pipe, GlameCanvasPort* port)
{
	/* remove pipe from sighandlers */
	if(is_hashed_gcport(port)){
		gtk_signal_disconnect_by_data(GTK_OBJECT(port),pipe);
		glame_canvas_port_connections_changed_cb(port,NULL);
	}

}

/*****************************
 *
 * real stuff
 *
 *****************************/

GlameCanvasPort* glame_canvas_find_port(filter_port_t *p)
{
	return hash_find_gcport(p);
}

gboolean 
glame_canvas_port_moved_cb(GlameCanvasFilter* f, double dx, double dy, GlameCanvasPort *p)
{
	glame_canvas_port_move(p, dx, dy);
	gtk_signal_emit(GTK_OBJECT(p),port_signals[MOVED],dx,dy,p);
	return FALSE;
}

void
glame_canvas_port_deleted_cb(GlameCanvasFilter* f, GlameCanvasPort *p)
{
	glame_canvas_port_destroy(p);
}



static gboolean
glame_canvas_port_event_cb(GnomeCanvasItem* item, GdkEvent* event, GlameCanvasPort* port);

static int last_x, last_y;
static GnomeCanvasPoints * points;

static gboolean
glame_canvas_port_grabbing_cb(GnomeCanvasLine* line, GdkEvent* event, GlameCanvasPort* port)
{
	//	GnomeCanvasPoints* points;
	GnomeCanvasItem* item;
	GnomeCanvas * canvas;
	GlameCanvasPort* otherPort;
	double eventx, eventy;
	filter_pipe_t* pipe;
	double pixperunt;
	switch(event->type){
	case GDK_MOTION_NOTIFY:
		
		// hmmm whats this? why is gtk so broken?? fuck
		//gtk_object_get(GTK_OBJECT(line),"points",points,NULL);
		
		points->coords[2] = event->button.x;
		points->coords[3] = event->button.y;
		last_x = event->button.x;
		last_y = event->button.y;
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(line),
				      "points",points,
				      NULL);
		
		return TRUE;
		break;
	case GDK_BUTTON_RELEASE:
		gnome_canvas_item_ungrab(line,event->button.time);
		gtk_signal_disconnect_by_func(line,glame_canvas_port_grabbing_cb,port);
		gtk_signal_handler_unblock_by_func(port,glame_canvas_port_event_cb,port);
		gnome_canvas_points_free(points);
		gtk_object_destroy(GTK_OBJECT(line));
		canvas = CANVAS_ITEM_CANVAS(line);
		
		item = gnome_canvas_get_item_at(canvas, event->button.x,event->button.y);
		if(item){
			if(GLAME_IS_CANVAS_PORT(item)){
				/* kool. found other port */
				otherPort = GLAME_CANVAS_PORT(item);
				if(filterport_is_output(port->port) && filterport_is_input(otherPort->port)){
					pipe=filterport_connect(port->port,otherPort->port);
					if(pipe)
						glame_canvas_pipe_new(CANVAS_ITEM_ROOT(item),pipe);
				}else{
					if(filterport_is_input(port->port) && filterport_is_output(otherPort->port)){
						pipe=filterport_connect(otherPort->port,port->port);
						if(pipe)
							glame_canvas_pipe_new(CANVAS_ITEM_ROOT(item),pipe);
					}
				}
			}
		}else
			return TRUE;
	default:
		return FALSE;
	}
}

static void update_string_from_editable(GtkEntry* entry, char** retbuffer)
{
	strncpy(*retbuffer,gtk_editable_get_chars(entry,0,-1),100);
}

static void 
glame_canvas_port_redirect_cb(GtkWidget* foo, GlameCanvasPort *port)
{
	GtkWidget * nameEntry;
	GtkWidget * dialog;
	GtkWidget * vbox;
	char * filenamebuffer;
	filter_portdb_t *ports;
	filter_port_t * newport;
	char nameBuffer[50];
	
	if(port->external){
		DPRINTF("Already exported\n");
		return;
	}
	
	filenamebuffer = calloc(100,sizeof(char));
	
	dialog = gnome_dialog_new("Export as...",GNOME_STOCK_BUTTON_CANCEL,GNOME_STOCK_BUTTON_OK,NULL);
	vbox = GNOME_DIALOG(dialog)->vbox;

	nameEntry = gtk_entry_new();
	gtk_signal_connect(GTK_OBJECT(nameEntry),"changed",
			   update_string_from_editable,&filenamebuffer);
	sprintf(nameBuffer,"%s_%s",filter_name(filterport_filter(port->port)),filterport_label(port->port));
	gtk_entry_set_text(GTK_ENTRY(nameEntry),nameBuffer);
	create_label_widget_pair(vbox,"New port name",nameEntry);
	
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog))){
		ports = filter_portdb(CANVAS_ITEM_NETWORK(port));
		if(filterport_is_output(port->port)){
			newport = filterportdb_add_port(ports,filenamebuffer,FILTER_PORTTYPE_ANY,FILTER_PORTFLAG_OUTPUT,FILTERPORT_DESCRIPTION,filenamebuffer,FILTERPORT_END);
			filterport_redirect(newport,port->port);
			port->external = TRUE;
		}else if(filterport_is_input(port->port)){
			newport = filterportdb_add_port(ports,filenamebuffer,FILTER_PORTTYPE_ANY,FILTER_PORTFLAG_INPUT,FILTERPORT_DESCRIPTION,filenamebuffer,FILTERPORT_END);
			filterport_redirect(newport,port->port);
			port->external = TRUE;
		}
	}
	glame_canvas_port_redraw(port);
	free(filenamebuffer);
}	

static GnomeUIInfo port_menu[] = 
{
	GNOMEUIINFO_ITEM("_Redirect port","redirect", glame_canvas_port_redirect_cb, NULL),
	GNOMEUIINFO_END
};

static gboolean
glame_canvas_port_event_cb(GnomeCanvasItem* item, GdkEvent* event, GlameCanvasPort* port)
{
	GnomeCanvasLine*line;
	GtkWidget *menu;
	switch(event->type){
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			
			/* block other handlers (this one ;-) */
			gtk_signal_handler_block_by_func(GTK_OBJECT(port),glame_canvas_port_event_cb,port);
			/* create line and attach handler */
			last_x = event->button.x;
			last_y = event->button.y;
			points = gnome_canvas_points_new(2);
			points->coords[0]=last_x;
			points->coords[1]=last_y;
			points->coords[2]=last_x;
			points->coords[3]=last_y;

			line = GNOME_CANVAS_LINE(gnome_canvas_item_new(CANVAS_ITEM_ROOT(item),
								       gnome_canvas_line_get_type(),
								       "points",points,
								       "fill_color","black",
								       "width_units",2.0,
								       "arrow_shape_a",18.0,
								       "arrow_shape_b",20.0,
								       "arrow_shape_c",5.0,
								       "line_style",GDK_LINE_ON_OFF_DASH,
								       "smooth",TRUE,
								       "spline_steps",100,
								       "last_arrowhead",TRUE,
								       NULL));
			
			gtk_signal_connect(GTK_OBJECT(line),"event", glame_canvas_port_grabbing_cb, port);
			
			/* grab the thing */
			gnome_canvas_item_grab(GNOME_CANVAS_ITEM(line),GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK,NULL,
					       event->button.time);
			return TRUE;
			break;
		case 3:
			menu = gnome_popup_menu_new(port_menu);
			gnome_popup_menu_do_popup_modal(menu, NULL,NULL,&event->button, port);
			return TRUE;
			break;
		default:
			return FALSE;
			break;
		}
	default:
		return FALSE;
		break;
	}




}

GlameCanvasPort* glame_canvas_port_new(GnomeCanvasGroup* group, filter_port_t *port, double x, double y, double height)
{
	GlameCanvasPort *p;
	char * buffer;
	filter_t* red_node;
	p = GLAME_CANVAS_PORT(gnome_canvas_item_new(group,
						    glame_canvas_port_get_type(),
						    "x1",x,
						    "x2",x+16.0,
						    "y1",y,
						    "y2",y+height,
						    "outline_color","black",
						    "width_units",1.0,
						    "fill_color_rgba",
						    (filterport_is_input(port)?0xff000000:0x0000ff00),
						    NULL));


	p->port = port;
	/* ugly... check for redirection */
	buffer = filterport_get_property(port,FILTERPORT_MAP_NODE);
	if(buffer){
		/* we're being redirected! */
		red_node = filter_get_node(filterport_filter(port),buffer);
		if(!red_node)
			abort();
		p->realPort = filterportdb_get_port(filter_portdb(red_node),filterport_get_property(port,FILTERPORT_MAP_LABEL));
	}else
		p->realPort = port;

	hash_add_gcport(p);
	gtk_signal_connect(GTK_OBJECT(p),"event",glame_canvas_port_event_cb,p);
	gtk_signal_connect(GTK_OBJECT(glame_canvas_find_filter(filterport_filter(port))),
			   "moved",
			   glame_canvas_port_moved_cb,
			   p);
	gtk_signal_connect(GTK_OBJECT(glame_canvas_find_filter(filterport_filter(port))),
			   "deleted",
			   glame_canvas_port_deleted_cb,
			   p);
	return p;
}


void
glame_canvas_port_move(GlameCanvasPort* port, double dx, double dy)
{
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(port),
			       dx,dy);
}
		
gboolean
glame_canvas_port_is_external(GlameCanvasPort* port)
{
	return port->external;
}

void
glame_canvas_port_set_external(GlameCanvasPort* port,
			       gboolean ext)
{
	port->external = ext;
}

void
glame_canvas_port_show_properties(GlameCanvasPort* port)
{
	filter_param_t* param;
	float y = 100.0;
	GnomeCanvasGroup * group = GNOME_CANVAS_GROUP(port);
	
	char * font = glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(port)->canvas));
	char buffer[256];

	sprintf(buffer,"%s",filterport_label(port->port));
	
	port->name = GNOME_CANVAS_TEXT(gnome_canvas_item_new(group,
							     gnome_canvas_text_get_type(),
							     "x",filterport_is_input(port->port)?-4.0:20.0,
							     "y",GNOME_CANVAS_ITEM(port)->y1,
							     "text",buffer,
							     "clip_width",94.0,
							     "clip_height",16.0,
							     "fill_color","blue",
							     "anchor",(filterport_is_input(port->port)?GTK_ANCHOR_EAST:GTK_ANCHOR_WEST),
							     "justification",(filterport_is_input(port->port)?GTK_JUSTIFY_RIGHT:GTK_JUSTIFY_LEFT), 
							     "font", font,
							     "clip",0,
							     NULL));
}

void
glame_canvas_port_hide_properties(GlameCanvasPort* port)
{
	if(!port->name)
		return;
	
	gtk_object_destroy(GTK_OBJECT(port->name));
}

void
glame_canvas_port_redraw(GlameCanvasPort * port)
{
	/* check for external */

	/*	if(glame_canvas_port_is_external(port){
		
	}
	*/
} 

