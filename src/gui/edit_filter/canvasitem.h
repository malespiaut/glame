/*
 * canvasitem.h
 *
 * $Id: canvasitem.h,v 1.2 2001/05/04 15:40:37 xwolf Exp $
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

typedef struct _GlameCanvasFilter GlameCanvasFilterClass;
typedef struct _GlameCanvasPort GlameCanvasPortClass;
typedef struct _GlameCanvasPipe GlameCanvasPipeClass;

/* GTK Type defines */

/* for GlameCanvasFilter */
#define GLAME_CANVAS_FILTER_TYPE              (glame_canvas_filter_get_type())
#define GLAME_CANVAS_FILTER(object)           (GTK_CHECK_CAST((object), GLAME_CANVAS_FILTER_TYPE, GlameCanvasFilter))
#define GLAME_CANVAS_FILTER_CLASS(object)     (GTK_CHECK_CLASS_CAST((object), GLAME_CANVAS_FILTER_TYPE, GlameCanvasFilterClass))
#define GLAME_IS_CANVAS_FILTER(object)        (GTK_CHECK_TYPE((object), GLAME_CANVAS_FILTER_TYPE))
#define GLAME_IS_CANVAS_FILTER_CLASS(object)  (GTK_CHECK_CLASS_TYPE((object), GLAME_CANVAS_FILTER_TYPE))
#define GLAME_CANVAS_FILTER_GET_CLASS(object) ((GlameCanvasFilterClass*) (((GtkObject*) (obj))->klass))

/* for GlameCanvasPort */
#define GLAME_CANVAS_PORT_TYPE              (glame_canvas_port_get_type())
#define GLAME_CANVAS_PORT(object)           (GTK_CHECK_CAST((object), GLAME_CANVAS_PORT_TYPE, GlameCanvasPort))
#define GLAME_CANVAS_PORT_CLASS(object)     (GTK_CHECK_CLASS_CAST((object), GLAME_CANVAS_PORT_TYPE, GlameCanvasPortClass))
#define GLAME_IS_CANVAS_PORT(object)        (GTK_CHECK_TYPE((object), GLAME_CANVAS_PORT_TYPE))
#define GLAME_IS_CANVAS_PORT_CLASS(object)  (GTK_CHECK_CLASS_TYPE((object), GLAME_CANVAS_PORT_TYPE))
#define GLAME_CANVAS_PORT_GET_CLASS(object) ((GlameCanvasPortClass*) (((GtkObject*) (obj))->klass))

/* for GlameCanvasPipe */
#define GLAME_CANVAS_PIPE_TYPE              (glame_canvas_pipe_get_type())
#define GLAME_CANVAS_PIPE(object)           (GTK_CHECK_CAST((object), GLAME_CANVAS_PIPE_TYPE, GlameCanvasPipe))
#define GLAME_CANVAS_PIPE_CLASS(object)     (GTK_CHECK_CLASS_CAST((object), GLAME_CANVAS_PIPE_TYPE, GlameCanvasPipeClass))
#define GLAME_IS_CANVAS_PIPE(object)        (GTK_CHECK_TYPE((object), GLAME_CANVAS_PIPE_TYPE))
#define GLAME_IS_CANVAS_PIPE_CLASS(object)  (GTK_CHECK_CLASS_TYPE((object), GLAME_CANVAS_PIPE_TYPE))
#define GLAME_CANVAS_PIPE_GET_CLASS(object) ((GlameCanvasPipeClass*) (((GtkObject*) (obj))->klass))


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
	double x, y;
	GnomeCanvasText *label;
	GnomeCanvasRect *labelBox;

	/* private */
	gboolean immutable : FALSE;
	gboolean undeletable : FALSE;
	gboolean connecting : FALSE;
	gboolean dragging : FALSE;
	
	double last_x, last_y;
	gint timeout_id : 0;
	GList *property_texts : NULL;

	GlameCanvasFilter **pprev_filter_hash;
	GlameCanvasFilter *next_filter_hash;
};

/*
 *
HASH(GCfilter, GlameCanvasFilter, 8,
	(GCfilter->filter == key ),
	((long)key/4),
	((long)GCfilter->filter/4),
	filter_t * key)

	GlameCanvasFilter* hash_find_GCfilter(filter_t*);
	void hash_add_GCFilter(GlameCanvasFilter*);

 */

struct _GlameCanvasFilterClass {
	GnomeCanvasGroupClass parent_class;

	void (* moved) (GlameCanvasFilter *filter,
			double x,
			double y);
};

/* I hope these have descriptive names */

/* public */
GtkType * glame_canvas_filter_get_type(void);

void glame_canvas_filter_set_immutable(GlameCanvasFilter* filter, gboolean immutable);
gboolean glame_canvas_filter_is_immutable(GlameCanvasFilter *filter);
void glame_canvas_filter_set_undeletable(GlameCanvasFilter* filter, gboolean undeletable);
gboolean glame_canvas_filter_is_undeletable(GlameCanvasFilter *filter);

void glame_canvas_filter_move(GlameCanvasFilter* filter, double dx, double dy);
void glame_canvas_filter_redraw(GlameCanvasFilter *filter);
void glame_canvas_filter_show_properties(GlameCanvasFilter * filter);
void glame_canvas_filter_hide_properties(GlameCanvasFilter * filter);

filter_paramdb_t * glame_canvas_filter_get_paramdb(GlameCanvasFilter *filter);


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
	
	GList * property_texts;
};

struct _GlameCanvasPortClass {
	GnomeCanvasRectClass parent_class;

	void (* connections_changed) (GlameCanvasPort *port);
	void (* moved) (GlameCanvasPort *port, double x, double y);
};

/* public */
GtkType * glame_canvas_port_get_type(void);
gboolean glame_canvas_port_is_external(GlameCanvasPort* port);
void glame_canvas_port_show_properties(GlameCanvasPort* port);
void glame_canvas_port_hide_properties(GlameCanvasPort* port);
void glame_canvas_port_redraw(GlameCanvasPort* port);
void glame_canvas_port_set_external(GlameCanvasPort* port, gboolean external);

filter_paramdb_t* glame_canvas_port_get_paramdb(GlameCanvasPort* port);

/* private */
double glame_canvas_port_get_connection_y_offset(GlameCanvasPort *port, GlameCanvasPipe* pipe);
void glame_canvas_port_move(GlameCanvasPort, double x, double y);


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

	GList *property_texts;
	GnomeCanvasPoints *points;
	GnomeCanvasLine *lineStart;
	GnomeCanvasLine *lineMid;
	GnomeCanvasLine *lineEnd;
	GnomeCanvasEllipse *circle;
};


struct _GlameCanvasPipeClass {
	GnomeCanvasGroupClass parent_class;

	void (* deleted) (GlameCanvasPipe * pipe);
};

/* public */
GtkType * glame_canvas_pipe_get_type(void);

void glame_canvas_pipe_show_properties(GlameCanvasPipe* pipe);
void glame_canvas_pipe_hide_properties(GlameCanvasPipe* pipe);


filter_paramdb_t* glame_canvas_pipe_get_paramdb(GlameCanvasPipe* pipe);



#endif
	
