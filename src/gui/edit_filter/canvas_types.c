/*
 * canvas_types.c
 *
 * $Id: canvas_types.c,v 1.22 2001/04/20 08:10:23 richi Exp $
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

#include <sys/param.h>
#include "glmid.h"
#include "canvas.h"
#include "/usr/X11R6/include/X11/bitmaps/hlines3"


static gpointer parent_class = NULL;
static gpointer canvas_parent_class = NULL;
static gpointer canvas_port_parent_class = NULL;
static GlameCanvasItemClass* glame_canvas_item_class = NULL;
static GlameCanvasPortClass* glame_canvas_port_class = NULL;


static void glame_canvas_item_destroy (GtkObject *object);
static void glame_canvas_item_class_init(GlameCanvasItemClass *class);
static void glame_canvas_port_class_init(GlameCanvasPortClass *class);
static void glame_canvas_item_init (GlameCanvasItem *item);
static void glame_canvas_port_init(GlameCanvasPort*item);
static void glame_canvas_destroy(GtkObject *object);
static void glame_canvas_port_destroy(GtkObject *object);
static void glame_canvas_class_init(GlameCanvasClass *class);
static void glame_canvas_init(GlameCanvas *canv);

GtkType
glame_canvas_item_get_type(void)
{
	static GtkType canvas_item_type = 0;
	
	if(!canvas_item_type){
		GtkTypeInfo canvas_item_info={
			"GlameCanvasItem",
			sizeof(GlameCanvasItem),
			sizeof(GlameCanvasItemClass),
			(GtkClassInitFunc) glame_canvas_item_class_init,
			(GtkObjectInitFunc) glame_canvas_item_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		canvas_item_type = gtk_type_unique(GNOME_TYPE_CANVAS_GROUP,
						   &canvas_item_info);
		gtk_type_set_chunk_alloc(canvas_item_type,8);
	}
	
	return canvas_item_type;
}

GtkType
glame_canvas_port_get_type(void)
{
	static GtkType canvas_item_type = 0;
	
	if(!canvas_item_type){
		GtkTypeInfo canvas_item_info={
			"GlameCanvasPort",
			sizeof(GlameCanvasPort),
			sizeof(GlameCanvasPortClass),
			(GtkClassInitFunc) glame_canvas_port_class_init,
			(GtkObjectInitFunc) glame_canvas_port_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		canvas_item_type = gtk_type_unique(GNOME_TYPE_CANVAS_RECT,
						   &canvas_item_info);
		gtk_type_set_chunk_alloc(canvas_item_type,8);
	}
	
	return canvas_item_type;
}

GtkType
glame_canvas_get_type(void)
{
	static GtkType canvas_type = 0;
	
	if(!canvas_type){
		GtkTypeInfo canvas_info={
			"GlameCanvas",
			sizeof(GlameCanvas),
			sizeof(GlameCanvasClass),
			(GtkClassInitFunc) glame_canvas_class_init,
			(GtkObjectInitFunc) glame_canvas_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		canvas_type = gtk_type_unique(GNOME_TYPE_CANVAS,
						   &canvas_info);
		gtk_type_set_chunk_alloc(canvas_type,8);
	}
	
	return canvas_type;
}

static void
glame_canvas_item_class_init(GlameCanvasItemClass *class)
{
	GtkObjectClass *object_class;
	GnomeCanvasItemClass *canvas_item_class;
	GnomeCanvasGroupClass *canvas_group_class;
	
	object_class = GTK_OBJECT_CLASS (class);
	canvas_item_class = GNOME_CANVAS_ITEM_CLASS (class);
	canvas_group_class = GNOME_CANVAS_GROUP_CLASS (class);
	
	glame_canvas_item_class = class;
	parent_class = gtk_type_class (GNOME_TYPE_CANVAS_GROUP);
	
	object_class->destroy = glame_canvas_item_destroy;
}

static void
glame_canvas_port_class_init(GlameCanvasPortClass *class)
{
	GtkObjectClass *object_class;

	GlameCanvasPortClass *canvas_rect_class;
	
	object_class = GTK_OBJECT_CLASS (class);
	canvas_rect_class = GLAME_CANVAS_PORT_CLASS(class);
	
	glame_canvas_port_class = class;
	canvas_port_parent_class = gtk_type_class (GNOME_TYPE_CANVAS_RECT);
	
	object_class->destroy = glame_canvas_port_destroy;
}

static void
glame_canvas_class_init(GlameCanvasClass *class)
{
	GtkObjectClass *object_class;
	GnomeCanvasClass *canvas_item_class;
	
	object_class = GTK_OBJECT_CLASS (class);
	canvas_item_class = GNOME_CANVAS_CLASS (class);
	
	canvas_parent_class = gtk_type_class (GNOME_TYPE_CANVAS);
	
	object_class->destroy = glame_canvas_destroy;
}



static void
glame_canvas_item_init (GlameCanvasItem *item)
{
	item->filter=NULL;
	item->dragging = FALSE;
	item->property_texts=NULL;
	item->output_ports=NULL;
	item->input_ports=NULL;
	item->immutable=FALSE;
	item->undeletable=FALSE;
	item->timeout_id=0;
}

static void
glame_canvas_port_init (GlameCanvasPort *item)
{
	item->port=NULL;
	item->port_type=0;
	item->connected_ports=NULL;
	item->property_texts = NULL;
}

static void
glame_canvas_init (GlameCanvas *item)
{
	item->net=NULL;
}


static void
glame_canvas_item_destroy (GtkObject *object)
{
	GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

static void
glame_canvas_destroy (GtkObject *object)
{
	gtk_object_destroy(object);
}

static void
glame_canvas_port_destroy (GtkObject *object)
{
	GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

GtkWidget * 
glame_canvas_new(gui_network *n)
{
	GlameCanvas *g;
	g = gtk_type_new(glame_canvas_get_type());
	g->net = n;
	n->canvas = GTK_WIDGET(g);
	return GTK_WIDGET(g);
}

GlameCanvasPort * 
glame_canvas_port_new(GnomeCanvasGroup *group,
		      filter_port_t *port,
		      gdouble x, gdouble y,
		      gdouble width, gdouble height,
		      guint color)


{
	GlameCanvasPort * item;
	
	item = GLAME_CANVAS_PORT(gnome_canvas_item_new(group, GLAME_TYPE_CANVAS_PORT,
				     "x1",x,
				     "y1",y,
				     "x2",x+width,
				     "y2",y+height,
				     "outline_color","black",
				     "width_units",1.0,
				     "fill_color_rgba",color,
				     NULL));
	item->port = port;
	return item;
}



GlameCanvasItem*
glame_canvas_item_new(GnomeCanvasGroup *group,
		      filter_t *gfilter,
		      gdouble x,
		      gdouble y)
{
	GnomeCanvasPoints* points;
	GdkImlibImage *image;
	GnomeCanvasItem *iitem;
	GlameCanvasItem *item;
	GnomeCanvasItem *gitem;
	char*filepath;
	char*namebuffer;
	char fontbuffer[250];
	gint fontsize;

	iitem = gnome_canvas_item_new(group,GLAME_TYPE_CANVAS_ITEM,NULL);
	item = GLAME_CANVAS_ITEM(iitem);
	item->filter = gfilter;
	item->last_x = item->last_y = 0.0;

	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1",0.0,
			      "y1",0.0,
			      "x2",16.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0x55555550,
			      NULL);

	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1",80.0,
			      "y1",0.0,
			      "x2",96.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0x55555550,
			      NULL);
	item->nameBox = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1",0.0,
			      "y1",64.0,
			      "x2",96.0,
			      "y2",84.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color","white",
			      NULL);

	points = gnome_canvas_points_new(2);
	points->coords[0] = 16.0;
	points->coords[1] = 0.0;
	points->coords[2] = 80.0;
	points->coords[3] = 0.0;
	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_line_get_type(),
			      "points",points,
			      "fill_color","black",
			      "width_units",1.0,
			      NULL);
	fontsize = (int)(GNOME_CANVAS_ITEM(group)->canvas->pixels_per_unit*12.0);
	fontsize = (fontsize<2.0)?2.0:fontsize;
	sprintf(fontbuffer,CANVAS_FONT_STRING,fontsize);
	item->text = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_text_get_type(),
			      "x",48.0,
			      "y",67.0,
			      "clip_width",94.0,
			      "clip_height",19.0,
			      "fill_color","black",
			      "anchor",GTK_ANCHOR_NORTH,
			      "justification",GTK_JUSTIFY_CENTER,
					   //"font", "-adobe-helvetica-medium-r-normal--12-*-72-72-p-*-iso8859-1",
					   "font",fontbuffer,
			      "clip",0,
			      "text",filter_name(gfilter),
			      NULL);

	/* Find icon for item. */
	image = NULL;
	namebuffer = plugin_query(gfilter->plugin, PLUGIN_PIXMAP);
	if (!namebuffer)
		namebuffer = GLAME_DEFAULT_ICON;
	filepath = g_concat_dir_and_file(GLAME_PIXMAP_PATH, namebuffer);
	if (!g_file_test(filepath, G_FILE_TEST_ISFILE)) {
		g_free(filepath);
		filepath = g_concat_dir_and_file("../data/pixmaps", namebuffer);
	}
	if (!g_file_test(filepath, G_FILE_TEST_ISFILE)) {
		g_free(filepath);
		filepath = gnome_pixmap_file(GLAME_DEFAULT_ICON);
	}
	if (g_file_test(filepath, G_FILE_TEST_ISFILE)) {
		image = gdk_imlib_load_image(filepath);
	}
	g_free(filepath);

	gitem = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
				       gnome_canvas_image_get_type(),
				       "x",48.0,
				       "y",32.0,
				       "width",64.0,
				       "height",64.0,
				       image ? "image" : NULL,image,
				       NULL);

	gtk_signal_connect(GTK_OBJECT(gitem),"event",GTK_SIGNAL_FUNC(canvas_item_node_selected),item);
	canvas_item_create_ports(GNOME_CANVAS_GROUP(item),gfilter);
	gtk_signal_connect(GTK_OBJECT(item),"event",GTK_SIGNAL_FUNC(handle_events),item);
	gfilter->gui_priv = item;
	return GLAME_CANVAS_ITEM(item);
}

void
canvas_item_redraw(GlameCanvasItem* item)
{
	GList * port;
	GlameCanvasPort* gPort;
	static GdkBitmap *bitmap=NULL;
	int foo=1;

	/* Dirty HACK to loop through input and output ports which you
	 * should have been solved using a function [richi]. */
	port = g_list_first(item->input_ports);
	do{
		while(port){
			gPort = GLAME_CANVAS_PORT(port->data);
			if(gPort->port_type&GUI_PORT_TYPE_EXTERNAL){
				if(!bitmap)
					bitmap = gdk_bitmap_create_from_data(GTK_WIDGET(GNOME_CANVAS_ITEM(gPort)->canvas)->window,hlines3_bits,1,3);
				gtk_object_set(GTK_OBJECT(gPort), "fill_stipple",bitmap, NULL);
				gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(gPort));
			} else {
				gtk_object_set(GTK_OBJECT(gPort), "fill_stipple", NULL, NULL);
				gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(gPort));
			}
			port = g_list_next(port);
		}
		port = g_list_first(item->output_ports);
	} while (foo--);
}

