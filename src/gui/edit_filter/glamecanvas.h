/*
 * glamecanvas.h
 *
 * $Id: glamecanvas.h,v 1.22 2003/05/18 19:08:38 xwolf Exp $
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

#ifndef _GLAME_CANVAS_H
#define _GLAME_CANVAS_H
G_BEGIN_DECLS

#include <gnome.h>
#include "glmid.h"
#include "canvasitem.h"

/* type forwards */

struct _GlameCanvas;
struct _GlameCanvasClass;

typedef struct _GlameCanvas GlameCanvas;
typedef struct _GlameCanvasClass GlameCanvasClass;

struct _GlameCanvasGroup;
struct _GlameCanvasGroupClass;

typedef struct _GlameCanvasGroup GlameCanvasGroup;
typedef struct _GlameCanvasGroupClass GlameCanvasGroupClass;


/* GTK Type defines */

#define GLAME_CANVAS_TYPE              (glame_canvas_get_type())
#define GLAME_CANVAS(object)           (GTK_CHECK_CAST((object), GLAME_CANVAS_TYPE, GlameCanvas))
#define GLAME_CANVAS_CLASS(object)     (GTK_CHECK_CLASS_CAST((object), GLAME_CANVAS_TYPE, GlameCanvasClass))
#define GLAME_IS_CANVAS(object)        (GTK_CHECK_TYPE((object), GLAME_CANVAS_TYPE))
#define GLAME_IS_CANVAS_CLASS(object)  (GTK_CHECK_CLASS_TYPE((object), GLAME_CANVAS_TYPE))
#define GLAME_CANVAS_GET_CLASS(object) ((GlameCanvasClass*) (((GtkObject*) (obj))->klass))

GtkType glame_canvas_get_type(void);

struct _GlameCanvas {
	GnomeCanvas parent_object;

	/* public */
	filter_t *net;

	/* private */
	gboolean openedUp;
	gboolean paused;
	gfloat font_size;
	GList * selectedItems;
	plugin_t* last;
	filter_t* clipBoard;
	GlameCanvas **pprev_gcanvas_hash;
	GlameCanvas *next_gcanvas_hash;
};

struct _GlameCanvasClass {
	GnomeCanvasClass parent_class;
};


/* creates a canvas from an existing filternetwork 
 * if network==NULL a new filternetwork will allocated
 */
GlameCanvas* glame_canvas_new(filter_t * network);

GlameCanvasFilter* glame_canvas_add_filter(GlameCanvas* canvas, filter_t *filter);
GlameCanvasFilter* glame_canvas_add_filter_by_name(GlameCanvas * canvas, const char *name);
GlameCanvasFilter* glame_canvas_add_filter_by_plugin(GlameCanvas* canvas, plugin_t *plug);

/* new signal marshallers >:-O   */

typedef void (*GtkSignal_NONE__DOUBLE_DOUBLE)(gpointer,double,double,gpointer);
void glame_canvas_marshal_NONE__DOUBLE_DOUBLE(GtkObject*,
					      GtkSignalFunc,
					      gpointer,
					      GtkArg*);
GdkPixbuf*  glame_gui_get_icon_from_filter(filter_t* t);

char* glame_gui_get_font(GlameCanvas* canv);

void glame_canvas_execute(GlameCanvas* canv);
filter_t *glame_canvas_collapse_selection(GlameCanvas* canv);
void glame_canvas_expand_selection(GlameCanvas* canv);
void glame_canvas_register(GlameCanvas* canv, const char* filtername, const char* category);
void glame_canvas_save(GlameCanvas* canv, const char* filename, const char* filtername, const char* category);
void glame_canvas_property_dialog(GlameCanvas* canv);
void glame_canvas_set_zoom(GlameCanvas* canv, double pixelPerPoint);
double glame_canvas_get_zoom(GlameCanvas* canv);
void glame_canvas_view_all(GlameCanvas* canv);

GList* glame_canvas_find_items_in_region(GlameCanvas *canv, gdouble x1,gdouble y1,gdouble x2,gdouble y2);
/* a refresh, adds missing/new nodes, too */
void glame_canvas_redraw(GlameCanvas *canv);

/* a full redraw, kills all gui and regenerates it,
   use with care, destroys grouping/selection */
void glame_canvas_full_redraw(GlameCanvas *canv);

GlameCanvas* glame_canvas_find_canvas(filter_t *net);
void glame_canvas_draw_errors(GlameCanvas *canv);
void glame_canvas_reset_errors(GlameCanvas *canv);

void glame_canvas_copy_selected(GlameCanvas* canv);
void glame_canvas_paste_selection(GlameCanvas* canv);
void glame_canvas_select_item(GlameCanvas* canv, GlameCanvasFilter* filter);
void glame_canvas_select_add(GlameCanvas* canv, GlameCanvasFilter* filter);
void glame_canvas_select_exclusive(GlameCanvas* canv, GlameCanvasFilter* filter);
void glame_canvas_select_clear(GlameCanvas* canv);
void glame_canvas_select_unselect(GlameCanvas* canv, GlameCanvasFilter* filter);
void glame_canvas_reroute_selection(GlameCanvas *canv);
//GList* glame_canvas_select_get(GlameCanvas* canv);

#define GLAME_CANVAS_GROUP_TYPE              (glame_canvas_group_get_type())
#define GLAME_CANVAS_GROUP(object)           (GTK_CHECK_CAST((object), GLAME_CANVAS_GROUP_TYPE, GlameCanvasGroup))
#define GLAME_CANVAS_GROUP_CLASS(object)     (GTK_CHECK_CLASS_CAST((object), GLAME_CANVAS_GROUP_TYPE, GlameCanvasGroupClass))
#define GLAME_IS_CANVAS_GROUP(object)        (GTK_CHECK_TYPE((object), GLAME_CANVAS_GROUP_TYPE))
#define GLAME_IS_CANVAS_GROUP_CLASS(object)  (GTK_CHECK_CLASS_TYPE((object), GLAME_CANVAS_GROUP_TYPE))
#define GLAME_CANVAS_GROUP_GET_CLASS(object) ((GlameCanvasGroupClass*) (((GtkObject*) (obj))->klass))

/* save me from getting finger sores */
#define GCI(item) GNOME_CANVAS_ITEM(item)
#define GTO(object) GTK_OBJECT(object)

GtkType glame_canvas_group_get_type(void);
void glame_canvas_group_add_item(GlameCanvasGroup* glameGroup, GlameCanvasFilter* gItem);
void glame_canvas_group_set_item(GlameCanvasGroup* glameGroup, GlameCanvasFilter* gItem);
void glame_canvas_group_raise(GlameCanvasGroup* group);
enum {GROUP_TYPE_UNION, GROUP_TYPE_SHARING};
struct _GlameCanvasGroup {
	GnomeCanvasGroup parent_object;

	/* public */

	/* private */
	gint type;
	guint id;
  //	GList* children;   // these are GlameCanvasFilter*
  //	GList* groups;
};

struct _GlameCanvasGroupClass {
	GnomeCanvasGroupClass parent_class;
};

GList* glame_canvas_get_selected_items(GlameCanvas* canvas);
guint glame_canvas_group_root_id(GlameCanvasGroup* group);
void glame_canvas_group_delete(GlameCanvasGroup* group);
void glame_canvas_group_select(GlameCanvasGroup* group);
void glame_canvas_group_unselect(GlameCanvasGroup* group);
void glame_canvas_group_selected(GlameCanvas* canvas);
void glame_canvas_ungroup_selected(GlameCanvas* canvas);
void glame_canvas_add_last(GlameCanvas* canvas);
void glame_canvas_group_item_moved_cb(GlameCanvasFilter* item,
				      double x, double y,
				      GlameCanvasGroup* group);

G_END_DECLS

#endif
