#ifndef _GLAME_CANVAS_H
#define _GLAME_CANVAS_H


/*
 * canvas.h
 *
 * $Id: canvas.h,v 1.5 2000/02/23 18:55:44 xwolf Exp $
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

/* node canvas item  */
typedef struct _GlameCanvasItem GlameCanvasItem;

struct _GlameCanvasItem
{
	GnomeCanvasGroup parent_object;
	
	gui_filter *filter;
	filter_portdesc_t *selected_port;
	double last_x,last_y;
	gboolean dragging;
	gboolean connecting;
	GnomeCanvasPoints *points;
	GnomeCanvasItem *line;
};

typedef struct _GlameCanvasItemClass GlameCanvasItemClass;
struct _GlameCanvasItemClass
{
	GnomeCanvasGroupClass parent_class;
} ;



GtkType glame_canvas_item_get_type(void);
GlameCanvasItem* glame_canvas_item_new(GnomeCanvasGroup * group,
				       gui_filter * gfilter,
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





#endif
	
	
