#ifndef _GLAME_CANVAS_H
#define _GLAME_CANVAS_H


/*
 * canvas.h
 *
 * $Id: canvas.h,v 1.11 2001/03/02 10:28:13 xwolf Exp $
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
	double dy,last_y;
        int begin_id, end_id;
	filter_pipe_t *pipe;
	GList * property_texts;
};



/* node canvas item  */
typedef struct _GlameCanvasItem GlameCanvasItem;

struct _GlameCanvasItem
{
	GnomeCanvasGroup parent_object;
	
	filter_t *filter;

	double last_x,last_y;
	GnomeCanvasItem * nameBox;
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





void canvas_item_edit_properties_cb(GtkWidget* m,GlameCanvasItem *item);
void canvas_item_delete_cb(GtkWidget* m,GlameCanvasItem* it);
void canvas_connection_edit_source_properties_cb(GtkWidget* bla, GlameConnection* conn);
void canvas_connection_edit_dest_properties_cb(GtkWidget* bla, GlameConnection* conn);
void canvas_item_show_properties(GnomeCanvasItem * item);
void canvas_item_delete_property_list(gpointer item, gpointer bla);
void canvas_item_hide_properties(GnomeCanvasItem * item);
gint canvas_item_node_selected(GnomeCanvasItem*item, GdkEvent *event, gpointer data);
int canvas_add_filter_by_name(char *name);
void network_draw_error(gui_network *net);
void network_error_reset(gui_network *net);
void network_play(GtkWidget *button,gui_network*net);
void network_play(GtkWidget *button,gui_network*net);
void network_pause(GtkWidget *button,gui_network*net);
void network_stop(GtkWidget *button,gui_network*net);
void canvas_add_filter_by_name_cb(GtkWidget*wid, char* name);
gint root_event(GnomeCanvas *canv,GdkEvent*event,gpointer data);
GtkWidget * canvas_new_from_network(gui_network* net);
gint canvas_input_port_event_cb(GnomeCanvasItem*item,GdkEvent* event, gpointer data);
int canvas_port_is_inside(GlameCanvasPort* gport, double x, double y);
void canvas_port_calculate_docking_coords(GlameCanvasPort* port,double *x, double *y, int id);
gint canvas_connection_update_points(GlameConnection *connection);
gint canvas_connection_do_connect(GlameConnection* connection);
gint canvas_output_port_motion(GnomeCanvasItem *pitem,GdkEvent *event, gpointer data);
void canvas_outout_port_reorder_connections(GlameCanvasPort* port);
void canvas_input_port_reorder_connections(GlameCanvasPort* port);
void canvas_connection_destroy_cb(GtkWidget*bla,GlameConnection* conn);
gint canvas_connection_select(GnomeCanvasItem* item, GdkEvent *event,gpointer data);
gint canvas_output_port_event_cb(GnomeCanvasItem*item,GdkEvent* event, gpointer data);
gint handle_events(GnomeCanvasItem* item,GdkEvent *event, gpointer data);
void canvas_item_create_ports(GnomeCanvasGroup* grp,filter_t *f);
GtkObject* canvas_add_node_from_filter(GnomeCanvas *canvas, filter_t *filter,double x, double y);
void canvas_connection_connect_from_pipe(GlameConnection *c);
int canvas_connection_connect(GlameConnection *c);
void canvas_input_port_update_connections(GlameCanvasPort*p, gdouble x, gdouble y);
void canvas_output_port_update_connections(GlameCanvasPort*p, gdouble x, gdouble y);
void canvas_item_edit_properties(filter_paramdb_t *pdb, const char *caption);
void canvas_item_destroy(GlameCanvasItem* it);
void canvas_connection_destroy(GlameConnection* connection);
void canvas_register_as_cb(gchar* name, gpointer data);
void changeString(GtkEditable *wid, char ** returnbuffer);
void set_file_selection_filter(GnomeFileEntry* entry, const char * filter);
void canvas_save_as(GtkWidget*w,void*blu);
void register_filternetwork_cb(GtkWidget*bla,void*blu);
void canvas_update_scroll_region(GlameCanvas* canv);
void canvas_load_network(GtkWidget *bla, void *blu);
void canvas_load_scheme(GtkWidget*bla,void*blu);
void canvas_port_redirect(GtkWidget*bla,GlameCanvasPort *blu);
void update_string(GtkListItem* item,char ** returnbuffer);
void update_entry_text(GtkListItem* item,GtkEntry* entry);
void canvas_item_redirect_parameters(GtkWidget *bla, GlameCanvasItem *item);
void canvas_item_show_description(GtkWidget* wid,GlameCanvasItem* it);
GlameCanvas* draw_network(filter_t *filter);
void draw_network_cb(GtkWidget *bla, GlameCanvasItem *item);
#endif

	
	
