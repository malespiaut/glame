/*
 * canvasitem.h
 *
 * $Id: canvasitem.h,v 1.16 2003/04/11 20:10:00 richi Exp $
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
 */

#ifndef _CANVASITEM_H
#define _CANVASITEM_H

#include <gnome.h>
#include "filter.h"

/* Type forwards */

struct _GlameCanvasFilter;
struct _GlameCanvasPort;
struct _GlameCanvasPipe;

struct _GlameCanvasFilterClass;
struct _GlameCanvasPortClass;
struct _GlameCanvasPipeClass;


/* typedefs for glame types */

typedef struct _GlameCanvasFilter GlameCanvasFilter;
typedef struct _GlameCanvasPort GlameCanvasPort;
typedef struct _GlameCanvasPipe GlameCanvasPipe;

typedef struct _GlameCanvasFilterClass GlameCanvasFilterClass;
typedef struct _GlameCanvasPortClass GlameCanvasPortClass;
typedef struct _GlameCanvasPipeClass GlameCanvasPipeClass;

/* GTK Type defines */

/* for GlameCanvasFilter */
#define GLAME_CANVAS_FILTER_TYPE              (glame_canvas_filter_get_type())
#define GLAME_CANVAS_FILTER(object)           (GTK_CHECK_CAST((object), GLAME_CANVAS_FILTER_TYPE, GlameCanvasFilter))
#define GLAME_CANVAS_FILTER_CLASS(object)     (GTK_CHECK_CLASS_CAST((object), GLAME_CANVAS_FILTER_TYPE, GlameCanvasFilterClass))
#define GLAME_IS_CANVAS_FILTER(object)        (GTK_CHECK_TYPE((object), GLAME_CANVAS_FILTER_TYPE))
#define GLAME_IS_CANVAS_FILTER_CLASS(object)  (GTK_CHECK_CLASS_TYPE((object), GLAME_CANVAS_FILTER_TYPE))
#define GLAME_CANVAS_FILTER_GET_CLASS(object) ((GlameCanvasFilterClass*) (GTK_OBJECT_GET_CLASS(GTK_OBJECT(obj))))

/* for GlameCanvasPort */
#define GLAME_CANVAS_PORT_TYPE              (glame_canvas_port_get_type())
#define GLAME_CANVAS_PORT(object)           (GTK_CHECK_CAST((object), GLAME_CANVAS_PORT_TYPE, GlameCanvasPort))
#define GLAME_CANVAS_PORT_CLASS(object)     (GTK_CHECK_CLASS_CAST((object), GLAME_CANVAS_PORT_TYPE, GlameCanvasPortClass))
#define GLAME_IS_CANVAS_PORT(object)        (GTK_CHECK_TYPE((object), GLAME_CANVAS_PORT_TYPE))
#define GLAME_IS_CANVAS_PORT_CLASS(object)  (GTK_CHECK_CLASS_TYPE((object), GLAME_CANVAS_PORT_TYPE))
#define GLAME_CANVAS_PORT_GET_CLASS(object) ((GlameCanvasPortClass*) (GTK_OBJECT_GET_CLASS(GTK_OBJECT(obj))))

/* for GlameCanvasPipe */
#define GLAME_CANVAS_PIPE_TYPE              (glame_canvas_pipe_get_type())
#define GLAME_CANVAS_PIPE(object)           (GTK_CHECK_CAST((object), GLAME_CANVAS_PIPE_TYPE, GlameCanvasPipe))
#define GLAME_CANVAS_PIPE_CLASS(object)     (GTK_CHECK_CLASS_CAST((object), GLAME_CANVAS_PIPE_TYPE, GlameCanvasPipeClass))
#define GLAME_IS_CANVAS_PIPE(object)        (GTK_CHECK_TYPE((object), GLAME_CANVAS_PIPE_TYPE))
#define GLAME_IS_CANVAS_PIPE_CLASS(object)  (GTK_CHECK_CLASS_TYPE((object), GLAME_CANVAS_PIPE_TYPE))
#define GLAME_CANVAS_PIPE_GET_CLASS(object) ((GlameCanvasPipeClass*) (GTK_OBJECT_GET_CLASS(GTK_OBJECT(obj))))


/* The actual data structures/functions */

/*******************************************
 *
 * GlameCanvasFilter
 *
 *******************************************/

struct _GlameCanvasFilter {
	GnomeCanvasGroup parent_object;

	/*  public */
	filter_t *filter;
	double x;
	double y;
	GnomeCanvasText *label;
	GnomeCanvasRect *labelBox;
	GnomeCanvasRect *selbox;

	/* private */
	gboolean immutable;
	gboolean undeletable;
	gboolean connecting;
	gboolean dragging;
	gboolean selected;
	
	double last_x, last_y;
	gint timeout_id;
	GnomeCanvasGroup* popupGroup;
	GnomeCanvasGroup* defaultGroup;
	GlameCanvasFilter **pprev_gcfilter_hash;
	GlameCanvasFilter *next_gcfilter_hash;
};

struct _GlameCanvasFilterClass {
	GnomeCanvasGroupClass parent_class;

	void (* moved) (GlameCanvasFilter *filter,
			double x,
			double y);
	void (* deleted) (GlameCanvasFilter *filter);
};

/* I hope these have descriptive names */

/* public */
GtkType glame_canvas_filter_get_type(void);

GlameCanvasFilter * glame_canvas_filter_new(GnomeCanvasGroup *group,
					    filter_t * filter);
void glame_canvas_filter_set_selected(GlameCanvasFilter* filter, gboolean selected);
void glame_canvas_filter_set_immutable(GlameCanvasFilter* filter, gboolean immutable);
gboolean glame_canvas_filter_is_immutable(GlameCanvasFilter *filter);
void glame_canvas_filter_set_undeletable(GlameCanvasFilter* filter, gboolean undeletable);
gboolean glame_canvas_filter_is_undeletable(GlameCanvasFilter *filter);

void glame_canvas_filter_move(GlameCanvasFilter* filter, double dx, double dy);
void glame_canvas_filter_redraw(GlameCanvasFilter *filter);
guint glame_canvas_filter_show_properties(GlameCanvasFilter * filter);
void glame_canvas_filter_hide_properties(GlameCanvasFilter * filter);

filter_paramdb_t * glame_canvas_filter_get_paramdb(GlameCanvasFilter *filter);
filter_portdb_t* glame_canvas_filter_get_portdb(GlameCanvasFilter* filter);

void _glame_canvas_filter_raise_to_top(GlameCanvasFilter* filter);
void _glame_canvas_filter_move(GlameCanvasFilter* filter,
			       gdouble dx, gdouble dy);

/********************************************
 *
 * GlameCanvasPort
 *
 ********************************************/

struct _GlameCanvasPort {
	GnomeCanvasRect parent_object;

	/* public */

	filter_port_t *port;
  
	/* private */
	double x, y;
	double x_size, y_size;
	
	filter_port_t *realPort;
	GnomeCanvasText* name;
	
	gboolean external;
	guint timeout_id;
	GnomeCanvasGroup *popupGroup;

	GlameCanvasPort **pprev_gcport_hash;
	GlameCanvasPort *next_gcport_hash;
};

struct _GlameCanvasPortClass {
	GnomeCanvasRectClass parent_class;

	void (* connections_changed) (GlameCanvasPort *port);
	void (* moved) (GlameCanvasPort *port, double x, double y);
};

/* public */

GtkType glame_canvas_port_get_type(void);
gboolean glame_canvas_port_is_external(GlameCanvasPort* port);
gboolean glame_canvas_port_show_properties(GlameCanvasPort* port);
void glame_canvas_port_hide_properties(GlameCanvasPort* port);
void glame_canvas_port_redraw(GlameCanvasPort* port);
void glame_canvas_port_set_external(GlameCanvasPort* port, gboolean external);
void glame_canvas_port_pipe_deleted_cb(GlameCanvasPipe* pipe, GlameCanvasPort* port);
GlameCanvasPipe* glame_canvas_pipe_new(GnomeCanvasGroup *group, filter_pipe_t * pipe);
/*filter_paramdb_t* glame_canvas_port_get_paramdb(GlameCanvasPort* port);*/

/* private */
double glame_canvas_port_get_connection_y_offset(GlameCanvasPort *port, GlameCanvasPipe* pipe);
void glame_canvas_port_move(GlameCanvasPort*, double x, double y);

GlameCanvasPort* glame_canvas_port_new(GnomeCanvasGroup* group,filter_port_t*port, double pos_x, double pos_y, double height);

/***********************************************
 *
 * GlameCanvasPipe
 *
 ***********************************************/


struct _GlameCanvasPipe {
	GnomeCanvasGroup parent_object;

	/* public */

	filter_pipe_t *pipe;

	
	/* private */
	gint sourceId;
	gint destId;
	GList *property_texts;
	GnomeCanvasPoints *points;
	GnomeCanvasLine *line;
	GnomeCanvasEllipse *circle;
	guint timeout_id;
	GnomeCanvasGroup* popupGroup;
	double dy, last_y;
	GlameCanvasPipe **pprev_gcpipe_hash;
	GlameCanvasPipe *next_gcpipe_hash;
};


struct _GlameCanvasPipeClass {
	GnomeCanvasGroupClass parent_class;

	void (* deleted) (GlameCanvasPipe* pipe);

};

/* extremly private!! :)  */
void glame_canvas_port_destroy_all(GnomeCanvas* canvas);
void glame_canvas_pipe_destroy_all(GnomeCanvas* canvas);
void glame_canvas_filter_destroy_all(GnomeCanvas* canvas);


/* public */
GtkType glame_canvas_pipe_get_type(void);

gboolean glame_canvas_pipe_show_properties(GlameCanvasPipe* pipe);
void glame_canvas_pipe_hide_properties(GlameCanvasPipe* pipe);

void glame_canvas_remove_unparented_filters(filter_t *parent);

/* global hash functions */

GlameCanvasFilter* glame_canvas_find_filter(filter_t* f);
GlameCanvasPipe* glame_canvas_find_pipe(filter_pipe_t* f);
GlameCanvasPort* glame_canvas_find_port(filter_port_t* f);


/* some access macros */

#define CANVAS_ITEM_CANVAS(i) GNOME_CANVAS(GNOME_CANVAS_ITEM(i)->canvas)
#define CANVAS_ITEM_GLAME_CANVAS(i) GLAME_CANVAS(GNOME_CANVAS_ITEM(i)->canvas)
#define CANVAS_ITEM_ROOT(i) gnome_canvas_root(GNOME_CANVAS(GNOME_CANVAS_ITEM(i)->canvas))
#define CANVAS_ITEM_NETWORK(i) (GLAME_CANVAS(GNOME_CANVAS_ITEM(i)->canvas)->net)

void glame_canvas_filter_expand_node(GlameCanvasFilter* filter);
#endif
	
