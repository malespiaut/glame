#ifndef _GLAME_CANVAS_H
#define _GLAME_CANVAS_H

#include <gnome.h>
#include <libgnomeui/gnome-canvas.h>
//#include "filter.h"
#include "gui.h"






/* node canvas item  */
typedef struct _GlameCanvasItem GlameCanvasItem;

struct _GlameCanvasItem
{
	GnomeCanvasGroup parent_object;
	
	gui_filter *filter;

};

typedef struct _GlameCanvasItemClass GlameCanvasItemClass;
struct _GlameCanvasItemClass
{
	GnomeCanvasGroupClass parent_class;
} ;



GtkType glame_canvas_item_get_type(void);
GnomeCanvasItem* glame_canvas_item_new(GnomeCanvasGroup * group,
				       gui_filter * gfilter,
				       gdouble world_x,
				       gdouble world_y);

#define GLAME_TYPE_CANVAS_ITEM        (glame_canvas_item_get_type())
#define GLAME_CANVAS_ITEM(object)     (GTK_CHECK_CAST ((object), GLAME_TYPE_CANVAS_ITEM, GlameCanvasItem))
#define GLAME_CANVAS_ITEM_CLASS(c)    (GTK_CHECK_CLASS_CAST ((c), GLAME_TYPE_CANVAS_ITEM, GlameCanvasItemClass))
#define GLAME_IS_CANVAS_ITEM(object)      (GTK_CHECK_TYPE ((object), GLAME_TYPE_CANVAS_ITEM))
#define GLAME_IS_CANVAS_ITEM_CLASS(c) (GTK_CHECK_CLASS_TYPE ((c), GLAME_TYPE_CANVAS_ITEM))
#define GLAME_CANVAS_ITEM_GET_CLASS(obj)  ((GlameCanvasItemClass*) (((GtkObject*) (obj))->klass))





#endif
	
	
