/*
 * canvasport.c
 *
 * $Id: canvasport.c,v 1.27 2001/12/13 14:48:06 richi Exp $
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
#include <errno.h>
#include <gnome.h>
#include "glamecanvas.h"
#include "canvasitem.h"
#include "util/glame_gui_utils.h"
#include "hash.h"
#include <X11/bitmaps/hlines3>
extern long bMac;
extern long nPopupTimeout;

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
	GnomeCanvasRectClass *parent_class;
	hash_remove_gcport(GLAME_CANVAS_PORT(object));
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS_RECT);
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
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
	class->connections_changed = (void(*)(GlameCanvasPort*))glame_canvas_port_connections_changed_cb;
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
	hash_init_gcport(p);

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

/* destroy all ports in a canvas */
void glame_canvas_port_destroy_all(GnomeCanvas* canvas)
{
	GlameCanvasPort * gcp;
	
	int i;
	for(i=0;i<(1<<8);i++){
		gcp = hash_getslot_gcport(i);
		
		while(gcp){
			if(CANVAS_ITEM_CANVAS(gcp) == canvas){
				/* FIXME doesnt take other windows into account! */
				gtk_object_destroy(GTO(gcp));
			}
			gcp = hash_next_gcport(gcp);
		}
	}
}

gboolean 
glame_canvas_port_moved_cb(GlameCanvasFilter* f, double dx, double dy, GlameCanvasPort *p)
{
	if(!hash_find_gcport(p->port)){
		gtk_signal_disconnect_by_data(GTK_OBJECT(f), p);
		return TRUE;
	}
	glame_canvas_port_move(p, dx, dy);
	gtk_signal_emit(GTK_OBJECT(p),port_signals[MOVED],dx,dy,p);
	return FALSE;
}

static void glame_canvas_port_destroy_cb(glsig_handler_t *handler, long sig,
					 va_list va)
{
	GlameCanvasPort *gPort;
	filter_port_t *port;

	/* Ignore not existing ports (and delete handler) */
	GLSIGH_GETARGS1(va, port);
	if (!hash_find_gcport(port)) {
		glsig_delete_handler(handler);
		return;
	}

	gPort = GLAME_CANVAS_PORT(glsig_handler_private(handler));
	gtk_object_destroy(GTK_OBJECT(gPort));
}




static gboolean
glame_canvas_port_event_cb(GnomeCanvasItem* item, GdkEvent* event, GlameCanvasPort* port);
static void 
glame_canvas_port_redirect_cb(GtkWidget* foo, GlameCanvasPort *port);

static int last_x, last_y;
static GnomeCanvasPoints * points = NULL;
static GnomeCanvasLine* line = NULL;
static GnomeUIInfo port_menu[] = 
{
	GNOMEUIINFO_ITEM("_Redirect port","redirect", glame_canvas_port_redirect_cb, NULL),
	GNOMEUIINFO_END
};

static gboolean
glame_canvas_port_grabbing_cb(GlameCanvasPort* port, GdkEvent* event, GlameCanvasPort* port_copy)
{
	//	GnomeCanvasPoints* points;
	GnomeCanvasItem* item;
	GtkWidget* menu;
	GnomeCanvas * canvas;
	GlameCanvasPort* otherPort;
	filter_pipe_t* pipe;

	switch(event->type){
	case GDK_MOTION_NOTIFY:
		
		points->coords[2] = event->button.x;
		points->coords[3] = event->button.y;
		last_x = event->button.x;
		last_y = event->button.y;
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(line),
				      "points",points,
				      NULL);
		
		return TRUE;
		break;
	case GDK_2BUTTON_PRESS:
		gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(port),event->button.time);
		gtk_signal_disconnect_by_func(GTK_OBJECT(port),GTK_SIGNAL_FUNC(glame_canvas_port_grabbing_cb),port);
		gtk_signal_handler_unblock_by_func(GTK_OBJECT(port),GTK_SIGNAL_FUNC(glame_canvas_port_event_cb),port);
		canvas = CANVAS_ITEM_CANVAS(port);
		gtk_object_destroy(GTO(line));
		line = NULL;

		
		if(bMac){
			if(event->button.button == 1){
				menu = gnome_popup_menu_new(port_menu);
				gnome_popup_menu_do_popup_modal(menu, NULL,NULL,&event->button, port);
			}
		}
		return TRUE;
		break;	
	case GDK_BUTTON_RELEASE:
		gnome_canvas_item_ungrab(GNOME_CANVAS_ITEM(port),event->button.time);
		gtk_signal_disconnect_by_func(GTO(port),GTK_SIGNAL_FUNC(glame_canvas_port_grabbing_cb),port);
		gtk_signal_handler_unblock_by_func(GTO(port),GTK_SIGNAL_FUNC(glame_canvas_port_event_cb),port);
		canvas = CANVAS_ITEM_CANVAS(port);
		gtk_object_destroy(GTO(line));
		line = NULL;
		
		item = gnome_canvas_get_item_at(canvas, event->button.x,event->button.y);
		if(item && GLAME_IS_CANVAS_PORT(item)) {
			/* kool. found other port */
			otherPort = GLAME_CANVAS_PORT(item);
			if(filterport_is_output(port->port) && filterport_is_input(otherPort->port)) {
				pipe=filterport_connect(port->port,otherPort->port);
				if(pipe)
					glame_canvas_pipe_new(CANVAS_ITEM_ROOT(item),pipe);
				return TRUE;
			}
			if(filterport_is_input(port->port) && filterport_is_output(otherPort->port)) {
				pipe=filterport_connect(otherPort->port,port->port);
				if(pipe)
					glame_canvas_pipe_new(CANVAS_ITEM_ROOT(item),pipe);
				return TRUE;
			}
			DPRINTF("wrong port type\n");
			return FALSE;
		}
	        break; 
	default:
		return FALSE;
	}
        return FALSE;
}

static void update_string_from_editable(GtkEntry* entry, char** retbuffer)
{
	strncpy(*retbuffer,gtk_editable_get_chars(GTK_EDITABLE(entry),0,-1),100);
}

static void 
glame_canvas_port_redirected_port_deleted_cb(glsig_handler_t* handler, long sig, va_list va)
{
	filter_port_t *port;
	/* Ignore requests from deleted ports (and delete handler) */
	GLSIGH_GETARGS1(va, port);
	if (!hash_find_gcport(port)) {
		glsig_delete_handler(handler);
		return;
	}

	glame_canvas_port_set_external(GLAME_CANVAS_PORT(glsig_handler_private(handler)),FALSE);

}


typedef struct {
	filter_portdb_t *ports;
	filter_t *net;
	char * label;
} portLabel;

#if 0
static void glame_canvas_port_redirected_source_deleted_cb(glsig_handler_t* handler, long sig, va_list va)
{
	portLabel* pl = glsig_handler_private(handler);
	DPRINTF("deleting %s\n",pl->label);
	filterportdb_delete_port(pl->ports,pl->label);
	if(glame_canvas_find_canvas(pl->net))
		glame_canvas_full_redraw(glame_canvas_find_canvas(pl->net));
	free(pl->label);
	free(pl);
}
#endif

		
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
			glame_canvas_port_set_external(port,TRUE);
			glsig_add_handler(filterport_emitter(newport),GLSIG_PORT_DELETED,glame_canvas_port_redirected_port_deleted_cb,port);
		}else if(filterport_is_input(port->port)){
			newport = filterportdb_add_port(ports,filenamebuffer,FILTER_PORTTYPE_ANY,FILTER_PORTFLAG_INPUT,FILTERPORT_DESCRIPTION,filenamebuffer,FILTERPORT_END);
			filterport_redirect(newport,port->port);
			glame_canvas_port_set_external(port, TRUE);
			glsig_add_handler(filterport_emitter(newport),GLSIG_PORT_DELETED,glame_canvas_port_redirected_port_deleted_cb,port);
		}
#if 0 /* broken - handle via properties dialog, explicit delete from user */
		{
			portLabel *pl = malloc(sizeof(portLabel));
			pl->ports = ports;
			pl->net = CANVAS_ITEM_NETWORK(port)->net;
			pl->label = strdup(filenamebuffer);
					       
			glsig_add_handler(filterport_emitter(port->port),GLSIG_PORT_DELETED, glame_canvas_port_redirected_source_deleted_cb, pl);
		}
#endif
	}
	glame_canvas_port_redraw(port);
	{
		filter_t* f;
		GlameCanvas* c;
		f = CANVAS_ITEM_NETWORK(port);
		if(f->net){
			c = glame_canvas_find_canvas(f->net);
			if(c)
				glame_canvas_full_redraw(c);
		}
	}
	free(filenamebuffer);
}	

static void
glame_canvas_port_register_popup(GlameCanvasPort* port)
{
	if(port->timeout_id){
		gtk_timeout_remove(port->timeout_id);
	}
	port->timeout_id = gtk_timeout_add(nPopupTimeout,(GtkFunction)glame_canvas_port_show_properties,port);
}
static void
glame_canvas_port_deregister_popup(GlameCanvasPort* port)
{
	if(port->timeout_id){
		gtk_timeout_remove(port->timeout_id);
		port->timeout_id = 0;
	}else{
		glame_canvas_port_hide_properties(port);
	}
}
static void glame_canvas_port_cleanup_cb(GtkObject * o, GlameCanvasPort* port)
{
	if(port)
		glame_canvas_port_deregister_popup(port);
}
static gboolean
glame_canvas_port_event_cb(GnomeCanvasItem* item, GdkEvent* event, GlameCanvasPort* port)
{

	GtkWidget *menu;
	if(!points)
		points = gnome_canvas_points_new(2);
	if(!line){
		last_x = event->button.x;
		last_y = event->button.y;
		points->coords[0]=last_x;
		points->coords[1]=last_y;
		points->coords[2]=last_x;
		points->coords[3]=last_y;
		
		line = GNOME_CANVAS_LINE(gnome_canvas_item_new(CANVAS_ITEM_ROOT(port),
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
	}
		
	switch(event->type){
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			/* block other handlers (this one ;-) */
			gtk_signal_handler_block_by_func(GTK_OBJECT(port),GTK_SIGNAL_FUNC(glame_canvas_port_event_cb),port);
			/* show line and attach handler */
			last_x = event->button.x;
			last_y = event->button.y;
			points->coords[0]=last_x;
			points->coords[1]=last_y;
			points->coords[2]=last_x;
			points->coords[3]=last_y;
			
			line = GNOME_CANVAS_LINE(gnome_canvas_item_new(CANVAS_ITEM_ROOT(port),
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
			
			gtk_signal_connect(GTK_OBJECT(port),"event", GTK_SIGNAL_FUNC(glame_canvas_port_grabbing_cb), port);
			/* grab the thing */
			gnome_canvas_item_grab(GNOME_CANVAS_ITEM(port),GDK_BUTTON_MOTION_MASK|GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK|GDK_BUTTON_PRESS_MASK,NULL,
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
	case GDK_ENTER_NOTIFY:
		glame_canvas_port_register_popup(port);
		return TRUE;
		break;
	case GDK_DESTROY:		
	case GDK_LEAVE_NOTIFY:
		glame_canvas_port_deregister_popup(port);
		return TRUE;
		break;
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
	p->realPort = port;
	/* ugly... check for redirection */
	if ((buffer = filterport_get_property(port,FILTERPORT_MAP_NODE))
	    && (red_node = filter_get_node(filterport_filter(port),buffer)))
		p->realPort = filterportdb_get_port(filter_portdb(red_node),filterport_get_property(port,FILTERPORT_MAP_LABEL));

	hash_add_gcport(p);
	gtk_signal_connect(GTK_OBJECT(p),"event",GTK_SIGNAL_FUNC(glame_canvas_port_event_cb),p);
	gtk_signal_connect(GTK_OBJECT(p),"destroy",GTK_SIGNAL_FUNC(glame_canvas_port_cleanup_cb),p);
	gtk_signal_connect(GTK_OBJECT(glame_canvas_find_filter(filterport_filter(port))),
			   "moved",
			   GTK_SIGNAL_FUNC(glame_canvas_port_moved_cb),
			   p);
	glsig_add_handler(filterport_emitter(port), GLSIG_PORT_DELETED,
			  glame_canvas_port_destroy_cb, p);
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
	glame_canvas_port_redraw(port);
}

gboolean
glame_canvas_port_show_properties(GlameCanvasPort* port)
{
	GnomeCanvasGroup * group;
	GnomeCanvasText *text;
	double xs,ys,xe,ye;
	double recx1,recx2,recy1,recy2;

	char * font = glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(port)->canvas));
	char buffer[256];
	
	if(port->timeout_id){
		gtk_timeout_remove(port->timeout_id);
		port->timeout_id=0;
	}


	gnome_canvas_item_get_bounds(GNOME_CANVAS_ITEM(port),&xs,&ys,&xe,&ye);

	group = GNOME_CANVAS_GROUP(gnome_canvas_item_new(CANVAS_ITEM_ROOT(port),
							 gnome_canvas_group_get_type(),
							 NULL));
	
	sprintf(buffer,"Label: %s",filterport_label(port->port));
	fprintf(stderr,"%f %f %f %f\n",xs,ys,xe,ye);
	text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(group,
							     gnome_canvas_text_get_type(),
							     "x",0.0, //xs + filterport_is_input(port->port)?-4.0:20.0,
							     "y",0.0,//ys,
							     "text",buffer,
							     "clip_width",94.0,
							     "clip_height",16.0,
							     "fill_color","black",
							     "anchor",(filterport_is_input(port->port)?GTK_ANCHOR_EAST:GTK_ANCHOR_WEST),
							     "justification",(filterport_is_input(port->port)?GTK_JUSTIFY_RIGHT:GTK_JUSTIFY_LEFT), 
							     "font", font,
							     "clip",0,
							     NULL));

	gnome_canvas_item_get_bounds(GCI(text),&recx1,&recy1,&recx2,&recy2);
	gnome_canvas_item_lower_to_bottom(gnome_canvas_item_new(group,
								gnome_canvas_rect_get_type(),
								"x1",recx1-5.0,
								"x2",recx2+5.0,
								"y1",recy1-5.0,
								"y2",recy2+5.0,
								"outline_color","black",
								"width_units",1.0,
								"fill_color_rgba",0xd0d0ff00,
								NULL));
	gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));

	gnome_canvas_item_move(GNOME_CANVAS_ITEM(group),(filterport_is_input(port->port)?(xs-4.0):(xe+4.0)),ys);

	port->popupGroup = group;
	return FALSE;
}

void
glame_canvas_port_hide_properties(GlameCanvasPort* port)
{
	if(!port->popupGroup)
		return;
	gtk_object_destroy(GTO(port->popupGroup));
	port->popupGroup = NULL;
}

void
glame_canvas_port_redraw(GlameCanvasPort * port)
{
	/* check for external */
	static GdkBitmap *bitmap=NULL;
	
	if(!bitmap)
		bitmap = gdk_bitmap_create_from_data(GTK_WIDGET(CANVAS_ITEM_CANVAS(port))->window,hlines3_bits,1,3);
	
	if(glame_canvas_port_is_external(port)){
		gtk_object_set(GTK_OBJECT(port),"fill_stipple",bitmap,NULL);
		gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(port));
	}else{
		gtk_object_set(GTK_OBJECT(port), "fill_stipple", NULL, NULL);
		gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(port));
	}
	gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(port));
} 

