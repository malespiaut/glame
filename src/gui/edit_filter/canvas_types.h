/*
 * canvas_types.h
 *
 * $Id: canvas_types.h,v 1.2 2001/03/27 21:04:20 xwolf Exp $
 *
 * Copyright (C) 2000, 2001 Johannes Hirche
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
 */

#ifndef _CANVAS_TYPES_H
#define _CANVAS_TYPES_H


/* Type forwards. */
struct _gui_network;
typedef struct _gui_network gui_network;
struct _GlameConnection;
typedef struct _GlameConnection GlameConnection;


/* Glame canvas - a filternetwork */

#define GLAME_TYPE_CANVAS        (glame_canvas_get_type())
#define GLAME_CANVAS(object)     (GTK_CHECK_CAST ((object), GLAME_TYPE_CANVAS, GlameCanvas))
#define GLAME_CANVAS_CLASS(c)    (GTK_CHECK_CLASS_CAST ((c), GLAME_TYPE_CANVAS, GlameCanvasClass))
#define GLAME_IS_CANVAS(object)  (GTK_CHECK_TYPE ((object), GLAME_TYPE_CANVAS))
#define GLAME_IS_CANVAS_CLASS(c) (GTK_CHECK_CLASS_TYPE ((c), GLAME_TYPE_CANVAS))
#define GLAME_CANVAS_GET_CLASS(obj) ((GlameCanvasClass*) (((GtkObject*) (obj))->klass))

typedef struct _GlameCanvas GlameCanvas;
struct _GlameCanvas {
	GnomeCanvas parent_object;
	
	gui_network * net;
};

typedef struct _GlameCanvasClass GlameCanvasClass;
struct _GlameCanvasClass {
	GnomeCanvasClass parent_class;
};

GtkType glame_canvas_get_type(void);
GtkWidget* glame_canvas_new(gui_network *);



/* Canvas port item */

#define GLAME_TYPE_CANVAS_PORT        (glame_canvas_port_get_type())
#define GLAME_CANVAS_PORT(object)     (GTK_CHECK_CAST ((object), GLAME_TYPE_CANVAS_PORT, GlameCanvasPort))
#define GLAME_CANVAS_PORT_CLASS(c)    (GTK_CHECK_CLASS_CAST ((c), GLAME_TYPE_CANVAS_PORT, GlameCanvasPortClass))
#define GLAME_IS_CANVAS_PORT(object)  (GTK_CHECK_TYPE ((object), GLAME_TYPE_CANVAS_PORT))
#define GLAME_IS_CANVAS_PORT_CLASS(c) (GTK_CHECK_CLASS_TYPE ((c), GLAME_TYPE_CANVAS_PORT))
#define GLAME_CANVAS_PORT_GET_CLASS(obj) ((GlameCanvasPortClass*) (((GtkObject*) (obj))->klass))

#define GUI_PORT_TYPE_IN 1
#define GUI_PORT_TYPE_OUT 2
#define GUI_PORT_TYPE_EXTERNAL 4

typedef struct _GlameCanvasPort GlameCanvasPort;
struct _GlameCanvasPort {
	GnomeCanvasRect parent_object;
	
	GList *connected_ports;
	filter_port_t *port;
	int port_type;
};

typedef struct _GlameCanvasPortClass GlameCanvasPortClass;
struct _GlameCanvasPortClass {
	GnomeCanvasRectClass parent_class;
};

GtkType glame_canvas_port_get_type(void);
GlameCanvasPort *glame_canvas_port_new(GnomeCanvasGroup *grp,
				       filter_port_t *port,
				       gdouble x, gdouble y,
				       gdouble width, gdouble height,
				       guint color);


/* Node canvas item  */

#define GLAME_TYPE_CANVAS_ITEM        (glame_canvas_item_get_type())
#define GLAME_CANVAS_ITEM(object)     (GTK_CHECK_CAST ((object), GLAME_TYPE_CANVAS_ITEM, GlameCanvasItem))
#define GLAME_CANVAS_ITEM_CLASS(c)    (GTK_CHECK_CLASS_CAST ((c), GLAME_TYPE_CANVAS_ITEM, GlameCanvasItemClass))
#define GLAME_IS_CANVAS_ITEM(object)  (GTK_CHECK_TYPE ((object), GLAME_TYPE_CANVAS_ITEM))
#define GLAME_IS_CANVAS_ITEM_CLASS(c) (GTK_CHECK_CLASS_TYPE ((c), GLAME_TYPE_CANVAS_ITEM))
#define GLAME_CANVAS_ITEM_GET_CLASS(obj) ((GlameCanvasItemClass*) (((GtkObject*) (obj))->klass))

typedef struct _GlameCanvasItem GlameCanvasItem;
struct _GlameCanvasItem {
	GnomeCanvasGroup parent_object;
	
	filter_t *filter;

	double last_x,last_y;
	GnomeCanvasItem * nameBox;

	gboolean dragging;
	gboolean connecting;
	gboolean immutable;

	GlameConnection *connection;
	GList *property_texts;
	GList *input_ports,*output_ports;
};

typedef struct _GlameCanvasItemClass GlameCanvasItemClass;
struct _GlameCanvasItemClass {
	GnomeCanvasGroupClass parent_class;
};

GtkType glame_canvas_item_get_type(void);
GlameCanvasItem* glame_canvas_item_new(GnomeCanvasGroup* group,
				       filter_t* gfilter,
				       gdouble world_x,
				       gdouble world_y);


/* Node connection. */

struct _GlameConnection {
	GlameCanvasPort *begin,*end;
	GnomeCanvasLine *lineStart;
	GnomeCanvasLine *line;
	GnomeCanvasLine *lineEnd;
	GnomeCanvasPoints *points;
	GnomeCanvasEllipse* circle;
	double dy,last_y;
        int begin_id, end_id;
	filter_pipe_t *pipe;
	GList * property_texts;
};


/* Internal used network representation. */

struct _gui_network {
	char *pixname;
	char *caption;
	char *descr;
	int iports,oports;
	filter_t * net;
	GtkWidget *canvas;
	gboolean paused;
};



#endif
