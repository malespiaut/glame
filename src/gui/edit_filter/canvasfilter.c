/*
 * canvasfilter.c
 *
 * $Id: canvasfilter.c,v 1.1 2001/05/07 00:45:36 xwolf Exp $
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



HASH(gcfilter, GlameCanvasFilter, 8,
	(gcfilter->filter == key ),
	((long)key/4),
	((long)gcfilter->filter/4),
        filter_t * key)


     /*	GlameCanvasFilter* hash_find_GCfilter(filter_t*);
	void hash_add_GCFilter(GlameCanvasFilter*);
     */


     /*  Forward decls */

     gboolean glame_canvas_filter_event(GnomeCanvasItem* foo, GdkEvent* event)
{}

/*Yukky gtk class stuff    
 * Ignore these
 */

enum {
	MOVED,
	DELETED,
	LAST_SIGNAL
};
static guint filter_signals[LAST_SIGNAL] = { 0 };



static void
glame_canvas_filter_destroy (GtkObject *object)
{
	gtk_signal_emit(object,filter_signals[DELETED]);
	GTK_OBJECT_CLASS(gtk_type_class(GLAME_CANVAS_FILTER_TYPE))->destroy(object);
}


static void
glame_canvas_filter_class_init(GlameCanvasFilterClass* class)
{
	
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = glame_canvas_filter_destroy;
	
	/* now for the signal stuff */
	
	filter_signals[MOVED] = 
		gtk_signal_new("moved",
			       GTK_RUN_LAST,
			       object_class->type,
			       GTK_SIGNAL_OFFSET(GlameCanvasFilterClass, moved),
			       glame_canvas_marshal_NONE__DOUBLE_DOUBLE,
			       GTK_TYPE_NONE,
			       2,
			       GTK_TYPE_DOUBLE,
			       GTK_TYPE_DOUBLE);
	filter_signals[DELETED] = 
		gtk_signal_new("deleted",
			       GTK_RUN_LAST,
			       object_class->type,
			       GTK_SIGNAL_OFFSET(GlameCanvasFilterClass, deleted),
			       gtk_marshal_NONE__NONE,
			       GTK_TYPE_NONE,
			       0);

	gtk_object_class_add_signals(object_class,filter_signals, LAST_SIGNAL);
	
	/* default handlers */
	class->moved = NULL;
	class->deleted = NULL;
	
}

static void
glame_canvas_filter_init(GlameCanvasFilter* node)
{
	node->filter = NULL;
	node->x = 0.0;
	node->y = 0.0;
	node->label = NULL;
	node->labelBox = NULL;
	node->immutable = FALSE;
	node->undeletable = FALSE;
	node->connecting = FALSE;
	node->dragging = FALSE;
	node->last_x = 0.0;
	node->last_y = 0.0;
	node->property_texts = NULL;
	node->timeout_id = 0;
}
		
	
	
GtkType
glame_canvas_filter_get_type(void)
{
	static GtkType canvas_filter_type = 0;
	
	if(!canvas_filter_type){
		GtkTypeInfo canvas_filter_info={
			"GlameCanvasFilter",
			sizeof(GlameCanvasFilter),
			sizeof(GlameCanvasFilterClass),
			(GtkClassInitFunc) glame_canvas_filter_class_init,
			(GtkObjectInitFunc) glame_canvas_filter_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		canvas_filter_type = gtk_type_unique(GNOME_TYPE_CANVAS_GROUP,
						   &canvas_filter_info);
		gtk_type_set_chunk_alloc(canvas_filter_type,8);
	}
	
	return canvas_filter_type;
}



/********************************
 *
 *  Whew... Now for the real stuff
 *
 ********************************/


GlameCanvasFilter* glame_canvas_find_filter(filter_t *f)
{
	return hash_find_gcfilter(f);
}
     

void
glame_canvas_filter_create_ports(GlameCanvasFilter* filter)
{
	int iPorts = 0, oPorts = 0;
	filter_port_t* port;
	double portHeight;
	double portPos = 0.0;
	double filter_pos_x, filter_pos_y;
	double xx,yy;
	
	GnomeCanvasGroup *group;
	GlameCanvasPort* gPort;
	
	/* count i/o ports */
	filterportdb_foreach_port(filter_portdb(filter->filter),port){
		if(filterport_is_input(port))
			iPorts++;
		else if(filterport_is_output(port))
			oPorts++;
	}

	/* find canvas root group */
	group = GNOME_CANVAS_GROUP(gnome_canvas_root(GNOME_CANVAS(GNOME_CANVAS_ITEM(filter)->canvas)));

	/* find filter coords */
	
	filter_pos_x = GNOME_CANVAS_ITEM(filter)->x1;
	filter_pos_y = GNOME_CANVAS_ITEM(filter)->y1;
	
	//gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(filter)->parent,&filter_pos_x, &filter_pos_y);
	
	//fprintf(stderr,"%f %f\n",filter_pos_x, filter_pos_y);
	portPos = filter_pos_y;
	
	/* input ports */
	
	if(iPorts){
		portHeight = 64.0/(float)iPorts;
		
		filterportdb_foreach_port(filter_portdb(filter->filter),port){
			if(filterport_is_input(port)){
				gPort = glame_canvas_port_new(group,
							      port,
							      filter_pos_x,
							      portPos,
							      portHeight);
				portPos += portHeight;
			}
		}
	}
	
	if(oPorts){
		
		/* reset upper coordinates */
		portPos = filter_pos_y;
		filter_pos_x += 80.0;
		portHeight = 64.0/(float)oPorts;
		
		filterportdb_foreach_port(filter_portdb(filter->filter),port){
			if(filterport_is_output(port)){
				gPort = glame_canvas_port_new(group,
							      port,
							      filter_pos_x,
							      portPos,
							      portHeight);
				portPos += portHeight;
			}
		}
	}
}
			   
			
	
	
	


GlameCanvasFilter* glame_canvas_filter_new(GnomeCanvasGroup *group,
					   filter_t* filter)
{
	GlameCanvasFilter * gItem;
	GnomeCanvasGroup * gGroup;
	GnomeCanvasItem * item;
	double x,y;
	char numberbuffer[10];
	char * buffer;
	char* cimmutable;
	gboolean immutable;
	
	gItem = GLAME_CANVAS_FILTER(gnome_canvas_item_new(group,
							  GLAME_CANVAS_FILTER_TYPE,
							  NULL));
	gGroup = GNOME_CANVAS_GROUP(gItem);
	gItem->filter = filter;

	

	buffer = filter_get_property(filter,"canvas_x");
	if(buffer)
		x = atoi(buffer);
	else
		x = 0.0;
	buffer = filter_get_property(filter,"canvas_y");
	if(buffer)
		y = atoi(buffer);
	else
		y = 0.0;

	cimmutable = filter_get_property(filter,"immutable");
	if(cimmutable)
		immutable = atoi(cimmutable);
	else 
		immutable = FALSE;
	gItem->immutable = immutable;

	cimmutable = filter_get_property(filter,"undeletable");
	if(cimmutable)
		immutable = atoi(cimmutable);
	else 
		immutable = FALSE;
	gItem->undeletable = immutable;
	
	/* add geometry stuff */
	
	gnome_canvas_item_new(gGroup,
			      gnome_canvas_rect_get_type(),
			      "x1",0.0,
			      "x2",16.0,
			      "y1",0.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0x55555500,
			      NULL);
	
	gnome_canvas_item_new(gGroup,
			      gnome_canvas_rect_get_type(),
			      "x1",80.0,
			      "y1",0.0,
			      "x2",96.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0x55555500,
			      NULL);
	
	gnome_canvas_item_new(gGroup,
			      gnome_canvas_rect_get_type(),
			      "x1",16.0,
			      "y1",0.0,
			      "x2",80.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0xffffff00,
			      NULL);
	
	gItem->labelBox = GNOME_CANVAS_RECT(gnome_canvas_item_new(gGroup,
								  gnome_canvas_rect_get_type(),
								  "x1",0.0,
								  "x2",96.0,
								  "y1",64.0,
								  "y2",84.0,
								  "outline_color","black",
								  "width_units",1.0,
								  "fill_color_rgba",0xffffff00,
								  NULL));
	
								  
	gItem->label = GNOME_CANVAS_TEXT(gnome_canvas_item_new(gGroup,
							       gnome_canvas_text_get_type(),
							       "x",48.0,
							       "y",67.0,
							       "clip_width",94.0,
							       "clip_height",19.0,
							       "fill_color",0x00000000,
							       "anchor",GTK_ANCHOR_NORTH,
							       "justification",GTK_JUSTIFY_CENTER,
							       "font",glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(group)->canvas)),
							       "clip",0,
							       "text",filter_name(filter),
							       NULL));
	
	item = gnome_canvas_item_new(gGroup,
				     gnome_canvas_image_get_type(),
				     "x",48.0,
				     "y",32.0,
				     "width",64.0,
				     "height",64.0,
				     "image",glame_gui_get_icon_from_filter(filter),
				     NULL);
	

	/* register */
	
	hash_add_gcfilter(gItem);
	
	glsig_add_handler(filter_emitter(filter),GLSIG_FILTER_DELETED,glame_canvas_filter_destroy,gItem);
	
	gtk_signal_connect(GTK_OBJECT(item),
			   "event",
			   GTK_SIGNAL_FUNC(glame_canvas_filter_event),gItem);
	
	
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(gItem),x,y);
	
	sprintf(numberbuffer,"%f",x);
	filter_set_property(filter,"canvas_x",numberbuffer);
	sprintf(numberbuffer,"%f",y);
	filter_set_property(filter,"canvas_y",numberbuffer);

	glame_canvas_filter_create_ports(gItem);
	
	return gItem;
}

void 
glame_canvas_filter_set_immutable(GlameCanvasFilter *filter, gboolean immutable)
{
	filter->immutable = immutable;
}

gboolean
glame_canvas_filter_is_immutable(GlameCanvasFilter *filter)
{
	return filter->immutable;
}

void 
glame_canvas_filter_set_undeletable(GlameCanvasFilter *filter, gboolean undeletable)
{
	filter->undeletable = undeletable;
}

gboolean
glame_canvas_filter_is_undeletable(GlameCanvasFilter *filter)
{
	return filter->undeletable;
}

void
glame_canvas_filter_move(GlameCanvasFilter *filter,
			 gdouble dx,
			 gdouble dy)
{
	char buffer[10];
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(filter),
			       dx,dy);
	
	gtk_signal_emit_by_name(GTK_OBJECT(filter),"moved",
				dx,dy);

	sprintf(buffer,"%f",GNOME_CANVAS_ITEM(filter)->x1);
	filter_set_property(filter->filter,
			    "canvas_x",buffer);
	
	sprintf(buffer,"%f",GNOME_CANVAS_ITEM(filter)->y1);
	filter_set_property(filter->filter,
			    "canvas_y",buffer);
}

void
glame_canvas_filter_redraw(GlameCanvasFilter *filter)
{
	

	/* redraw label with right size */

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(filter),
			      "font",glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(filter)->canvas)),
			      NULL);
	
	/* check for error */
	
	if(filter_has_error(filter->filter))
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(filter->labelBox),
				      "fill_color","red",
				      NULL);
	else
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(filter->labelBox),
				      "fill_color","white",
				      NULL);
}

void
glame_canvas_filter_show_properties(GlameCanvasFilter* filter)
{
	filter_param_t* param;
	GnomeCanvasText * text;
	GnomeCanvasGroup *group;
	char buffer[256];
	const char* font = glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(filter)->canvas));
	float y = 100.0;
	
	group = GNOME_CANVAS_GROUP(filter);
	
	filterparamdb_foreach_param(glame_canvas_filter_get_paramdb(filter),param){
		sprintf(buffer,"%s: %s",filterparam_label(param),filterparam_to_string(param));
		text = gnome_canvas_item_new(group,
					     gnome_canvas_text_get_type(),
					     "x",0.0,
					     "y",y,
					     "text",buffer,
					     "font",font,
					     "clip_width",94.0,
					     "clip_height",16.0,
					     "fill_color","black",
					     "anchor",GTK_ANCHOR_WEST,
					     "justification",GTK_JUSTIFY_LEFT, 
					     "clip",0,
					     NULL);
		y+=16.0;
		filter->property_texts = g_list_append(filter->property_texts,text);
	}

	y+=10.0;
	if(filter_has_error(filter->filter)){
		sprintf(buffer,"ERROR: %s",filter_errstr(filter->filter));
		text = gnome_canvas_item_new(group,
					     gnome_canvas_text_get_type(),
					     "x",0.0,
					     "y",y,
					     "text",buffer,
					     "clip_width",94.0,
					     "clip_height",16.0,
					     "fill_color","red",
					     "anchor",GTK_ANCHOR_WEST,
					     "justification",GTK_JUSTIFY_LEFT, 
					     "font", font,
					     "clip",0,
					     NULL);

		filter->property_texts = g_list_append(filter->property_texts,text);
		
	}
}

void
glame_canvas_filter_hide_properties(GlameCanvasFilter* filter)
{
	GList * iter;
	if(!filter->property_texts){
		DPRINTF("property_texts == NULL!\n");
		return;
	}
	iter = g_list_first(filter->property_texts);
	while(iter){
		gtk_object_destroy(GTK_OBJECT(iter->data));
		iter = g_list_next(iter);
	}
	g_list_free(filter->property_texts);
	filter->property_texts = NULL;
}

filter_paramdb_t*
glame_canvas_filter_get_paramdb(GlameCanvasFilter* filter)
{
	return filter_paramdb(filter->filter);
}

