#ifndef _GLAME_CANVAS_H
#define _GLAME_CANVAS_H


/*
 * canvas.h
 *
 * $Id: canvas.h,v 1.5 2000/12/13 16:26:51 xwolf Exp $
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

#include "gui.h"

/*glame canvas group (a filter node) */

typedef struct _GlameCanvas GlameCanvas;

struct _GlameCanvas
{
	GnomeCanvas parent_object;
	
	gui_network * net;
};

typedef struct _GlameCanvasClass GlameCanvasClass;
struct _GlameCanvasClass
{
	GnomeCanvasClass parent_class;
};

GtkType glame_canvas_get_type(void);
GtkWidget* glame_canvas_new(gui_network *);


/* canvas port item */


typedef struct _GlameCanvasPort GlameCanvasPort;

struct _GlameCanvasPort
{
	GnomeCanvasRect parent_object;
	
	GList *connected_ports;
	filter_port_t *port;
	int port_type;
	
};

typedef struct _GlameCanvasPortClass GlameCanvasPortClass;
struct _GlameCanvasPortClass
{
	GnomeCanvasRectClass parent_class;
};

GtkType glame_canvas_port_get_type(void);
GlameCanvasPort *glame_canvas_port_new(GnomeCanvasGroup *grp,filter_port_t *port, gdouble x, gdouble y,gdouble width, gdouble height,guint color);

typedef struct _GlameConnection GlameConnection;
struct _GlameConnection
{
	GlameCanvasPort *begin,*end;
	GnomeCanvasLine *lineStart;
	GnomeCanvasLine *line;
	GnomeCanvasLine *lineEnd;
	GnomeCanvasPoints *points;
	GnomeCanvasEllipse* circle;
	filter_pipe_t *pipe;
};



/* node canvas item  */
typedef struct _GlameCanvasItem GlameCanvasItem;

struct _GlameCanvasItem
{
	GnomeCanvasGroup parent_object;
	
	filter_t *filter;

	double last_x,last_y;
	gboolean dragging;
	gboolean connecting;
	
	GlameConnection *connection;
	GList *property_texts;
	GList *input_ports,*output_ports;
};

typedef struct _GlameCanvasItemClass GlameCanvasItemClass;
struct _GlameCanvasItemClass
{
	GnomeCanvasGroupClass parent_class;
} ;



GtkType glame_canvas_item_get_type(void);
GlameCanvasItem* glame_canvas_item_new(GnomeCanvasGroup * group,
				       filter_t * gfilter,
				       gdouble world_x,
				       gdouble world_y);

#define GLAME_TYPE_CANVAS_ITEM        (glame_canvas_item_get_type())
#define GLAME_CANVAS_ITEM(object)     (GTK_CHECK_CAST ((object), GLAME_TYPE_CANVAS_ITEM, GlameCanvasItem))
#define GLAME_CANVAS_ITEM_CLASS(c)    (GTK_CHECK_CLASS_CAST ((c), GLAME_TYPE_CANVAS_ITEM, GlameCanvasItemClass))
#define GLAME_IS_CANVAS_ITEM(object)      (GTK_CHECK_TYPE ((object), GLAME_TYPE_CANVAS_ITEM))
#define GLAME_IS_CANVAS_ITEM_CLASS(c) (GTK_CHECK_CLASS_TYPE ((c), GLAME_TYPE_CANVAS_ITEM))
#define GLAME_CANVAS_ITEM_GET_CLASS(obj)  ((GlameCanvasItemClass*) (((GtkObject*) (obj))->klass))

#define GLAME_TYPE_CANVAS        (glame_canvas_get_type())
#define GLAME_CANVAS(object)     (GTK_CHECK_CAST ((object), GLAME_TYPE_CANVAS, GlameCanvas))
#define GLAME_CANVAS_CLASS(c)    (GTK_CHECK_CLASS_CAST ((c), GLAME_TYPE_CANVAS, GlameCanvasClass))
#define GLAME_IS_CANVAS(object)      (GTK_CHECK_TYPE ((object), GLAME_TYPE_CANVAS))
#define GLAME_IS_CANVAS_CLASS(c) (GTK_CHECK_CLASS_TYPE ((c), GLAME_TYPE_CANVAS))
#define GLAME_CANVAS_GET_CLASS(obj)  ((GlameCanvasClass*) (((GtkObject*) (obj))->klass))


#define GLAME_TYPE_CANVAS_PORT        (glame_canvas_port_get_type())
#define GLAME_CANVAS_PORT(object)     (GTK_CHECK_CAST ((object), GLAME_TYPE_CANVAS_PORT, GlameCanvasPort))
#define GLAME_CANVAS_PORT_CLASS(c)    (GTK_CHECK_CLASS_CAST ((c), GLAME_TYPE_CANVAS_PORT, GlameCanvasPortClass))
#define GLAME_IS_CANVAS_PORT(object)      (GTK_CHECK_TYPE ((object), GLAME_TYPE_CANVAS_PORT))
#define GLAME_IS_CANVAS_PORT_CLASS(c) (GTK_CHECK_CLASS_TYPE ((c), GLAME_TYPE_CANVAS_PORT))
#define GLAME_CANVAS_PORT_GET_CLASS(obj)  ((GlameCanvasPortClass*) (((GtkObject*) (obj))->klass))

#define GUI_PORT_TYPE_IN 1
#define GUI_PORT_TYPE_OUT 2
#define GUI_PORT_TYPE_EXTERNAL 4

gint handle_events(GnomeCanvasItem* item,GdkEvent *event, gpointer data);
gint image_select(GnomeCanvasItem*item, GdkEvent *event, gpointer data);

int add_connection(GlameConnection* c);
void update_connection(GlameConnection *c);
void delete_canvas_item_cb(GtkWidget* m,GlameCanvasItem* it);

void update_input_connection(GlameCanvasPort *p,gdouble x, gdouble y);
void update_output_connection(GlameCanvasPort *p,gdouble x, gdouble y);
void  move_single_connection(GlameConnection* c, gdouble x, gdouble y);
#endif
	
	
