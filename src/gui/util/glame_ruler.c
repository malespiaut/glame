/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#include "glame_ruler.h"

enum {
  ARG_0,
  ARG_LOWER,
  ARG_UPPER,
  ARG_POSITION,
  ARG_MAX_SIZE
};

static void glame_ruler_class_init    (GlameRulerClass *klass);
static void glame_ruler_init          (GlameRuler      *ruler);
static void glame_ruler_realize       (GtkWidget      *widget);
static void glame_ruler_unrealize     (GtkWidget      *widget);
static void glame_ruler_size_allocate (GtkWidget      *widget,
				      GtkAllocation  *allocation);
static gint glame_ruler_expose        (GtkWidget      *widget,
				      GdkEventExpose *event);
static void glame_ruler_make_pixmap   (GlameRuler      *ruler);
static void glame_ruler_set_arg       (GtkObject      *object,
				      GtkArg         *arg,
				      guint           arg_id);
static void glame_ruler_get_arg       (GtkObject      *object,
				      GtkArg         *arg,
				      guint           arg_id);

static GtkWidgetClass *parent_class;

static gchar *
glame_ruler_metric_pixels_translate (gdouble value)
{
  return g_strdup_printf ("%d", (long) value);
}

static const GlameRulerMetric glame_ruler_metric_pixels =
{
  "Pixels", "Pi", 1.0,
  { 1, 2, 5, 10, 20, 25, 50, 75, 100, 150,
    200, 250, 500, 1000, 1500, 2000, 2500, 5000, 7500, 10000 },
  { 1, 5, 10, 50, 100 },
  glame_ruler_metric_pixels_translate
};

GtkType
glame_ruler_get_type (void)
{
  static GtkType ruler2_type = 0;

  if (!ruler2_type)
    {
      static const GtkTypeInfo ruler2_info =
      {
	"GlameRuler",
	sizeof (GlameRuler),
	sizeof (GlameRulerClass),
	(GtkClassInitFunc) glame_ruler_class_init,
	(GtkObjectInitFunc) glame_ruler_init,
	/* reserved_1 */ NULL,
        /* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      ruler2_type = gtk_type_unique (GTK_TYPE_WIDGET, &ruler2_info);
    }

  return ruler2_type;
}

static void
glame_ruler_class_init (GlameRulerClass *class)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  object_class = (GtkObjectClass*) class;
  widget_class = (GtkWidgetClass*) class;

  parent_class = gtk_type_class (GTK_TYPE_WIDGET);
  
  object_class->set_arg = glame_ruler_set_arg;
  object_class->get_arg = glame_ruler_get_arg;

  widget_class->realize = glame_ruler_realize;
  widget_class->unrealize = glame_ruler_unrealize;
  widget_class->size_allocate = glame_ruler_size_allocate;
  widget_class->expose_event = glame_ruler_expose;

  class->draw_ticks = NULL;
  class->draw_pos = NULL;
  class->get_stride = NULL;

  gtk_object_add_arg_type ("GlameRuler::lower", GTK_TYPE_FLOAT,
			   GTK_ARG_READWRITE, ARG_LOWER);
  gtk_object_add_arg_type ("GlameRuler::upper", GTK_TYPE_FLOAT,
			   GTK_ARG_READWRITE, ARG_UPPER);
  gtk_object_add_arg_type ("GlameRuler::position", GTK_TYPE_FLOAT,
			   GTK_ARG_READWRITE, ARG_POSITION);
  gtk_object_add_arg_type ("GlameRuler::max_size", GTK_TYPE_FLOAT,
			   GTK_ARG_READWRITE, ARG_MAX_SIZE);
}

static void
glame_ruler_init (GlameRuler *ruler)
{
  ruler->backing_store = NULL;
  ruler->non_gr_exp_gc = NULL;
  ruler->xsrc = 0;
  ruler->ysrc = 0;
  ruler->slider_size = 0;
  ruler->lower = 0;
  ruler->upper = 0;
  ruler->position = 0;
  ruler->max_size = 0;

  glame_ruler_set_metric (ruler, (GlameRulerMetric*) &glame_ruler_metric_pixels);
}

static void
glame_ruler_set_arg (GtkObject  *object,
		    GtkArg     *arg,
		    guint       arg_id)
{
  GlameRuler *ruler = GLAME_RULER (object);

  switch (arg_id)
    {
    case ARG_LOWER:
      glame_ruler_set_range (ruler, GTK_VALUE_FLOAT (*arg), ruler->upper,
			    ruler->position, ruler->max_size);
      break;
    case ARG_UPPER:
      glame_ruler_set_range (ruler, ruler->lower, GTK_VALUE_FLOAT (*arg),
			    ruler->position, ruler->max_size);
      break;
    case ARG_POSITION:
      glame_ruler_set_range (ruler, ruler->lower, ruler->upper,
			    GTK_VALUE_FLOAT (*arg), ruler->max_size);
      break;
    case ARG_MAX_SIZE:
      glame_ruler_set_range (ruler, ruler->lower, ruler->upper,
			    ruler->position,  GTK_VALUE_FLOAT (*arg));
      break;
    }
}

static void
glame_ruler_get_arg (GtkObject  *object,
		    GtkArg     *arg,
		    guint       arg_id)
{
  GlameRuler *ruler = GLAME_RULER (object);
  
  switch (arg_id)
    {
    case ARG_LOWER:
      GTK_VALUE_FLOAT (*arg) = ruler->lower;
      break;
    case ARG_UPPER:
      GTK_VALUE_FLOAT (*arg) = ruler->upper;
      break;
    case ARG_POSITION:
      GTK_VALUE_FLOAT (*arg) = ruler->position;
      break;
    case ARG_MAX_SIZE:
      GTK_VALUE_FLOAT (*arg) = ruler->max_size;
      break;
    default:
      arg->type = GTK_TYPE_INVALID;
      break;
    }
}

void
glame_ruler_set_metric (GlameRuler       *ruler,
		       GlameRulerMetric *metric)
{
  g_return_if_fail (ruler != NULL);
  g_return_if_fail (GLAME_IS_RULER (ruler));

  ruler->metric = metric;

  if (GTK_WIDGET_DRAWABLE (ruler))
    gtk_widget_queue_draw (GTK_WIDGET (ruler));
}

void
glame_ruler_set_range (GlameRuler *ruler,
		      gdouble    lower,
		      gdouble    upper,
		      gdouble    position,
		      gdouble    max_size)
{
  g_return_if_fail (ruler != NULL);
  g_return_if_fail (GLAME_IS_RULER (ruler));

  ruler->lower = lower;
  ruler->upper = upper;
  ruler->position = position;
  ruler->max_size = max_size;

  if (GTK_WIDGET_DRAWABLE (ruler))
    gtk_widget_queue_draw (GTK_WIDGET (ruler));
}

void
glame_ruler_draw_ticks (GlameRuler *ruler)
{
  g_return_if_fail (ruler != NULL);
  g_return_if_fail (GLAME_IS_RULER (ruler));

  if (GLAME_RULER_GET_CLASS (ruler)->draw_ticks)
    GLAME_RULER_GET_CLASS (ruler)->draw_ticks (ruler);
}

gdouble
glame_ruler_get_stride (GlameRuler *ruler)
{
  g_return_if_fail (ruler != NULL);
  g_return_if_fail (GLAME_IS_RULER (ruler));

  if (GLAME_RULER_GET_CLASS (ruler)->get_stride)
    return GLAME_RULER_GET_CLASS (ruler)->get_stride (ruler);

  return 0.0;
}

void
glame_ruler_draw_pos (GlameRuler *ruler)
{
  g_return_if_fail (ruler != NULL);
  g_return_if_fail (GLAME_IS_RULER (ruler));

  if (GLAME_RULER_GET_CLASS (ruler)->draw_pos)
     GLAME_RULER_GET_CLASS (ruler)->draw_pos (ruler);
}


static void
glame_ruler_realize (GtkWidget *widget)
{
  GlameRuler *ruler;
  GdkWindowAttr attributes;
  gint attributes_mask;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GLAME_IS_RULER (widget));

  ruler = GLAME_RULER (widget);
  GTK_WIDGET_SET_FLAGS (ruler, GTK_REALIZED);

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);
  attributes.event_mask = gtk_widget_get_events (widget);
  attributes.event_mask |= (GDK_EXPOSURE_MASK |
			    GDK_POINTER_MOTION_MASK |
			    GDK_POINTER_MOTION_HINT_MASK);

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

  widget->window = gdk_window_new (gtk_widget_get_parent_window (widget), &attributes, attributes_mask);
  gdk_window_set_user_data (widget->window, ruler);

  widget->style = gtk_style_attach (widget->style, widget->window);
  gtk_style_set_background (widget->style, widget->window, GTK_STATE_ACTIVE);

  glame_ruler_make_pixmap (ruler);
}

static void
glame_ruler_unrealize (GtkWidget *widget)
{
  GlameRuler *ruler;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GLAME_IS_RULER (widget));

  ruler = GLAME_RULER (widget);

  if (ruler->backing_store)
    gdk_pixmap_unref (ruler->backing_store);
  if (ruler->non_gr_exp_gc)
    gdk_gc_destroy (ruler->non_gr_exp_gc);

  ruler->backing_store = NULL;
  ruler->non_gr_exp_gc = NULL;

  if (GTK_WIDGET_CLASS (parent_class)->unrealize)
    (* GTK_WIDGET_CLASS (parent_class)->unrealize) (widget);
}

static void
glame_ruler_size_allocate (GtkWidget     *widget,
			  GtkAllocation *allocation)
{
  GlameRuler *ruler;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GLAME_IS_RULER (widget));

  ruler = GLAME_RULER (widget);
  widget->allocation = *allocation;

  if (GTK_WIDGET_REALIZED (widget))
    {
      gdk_window_move_resize (widget->window,
			      allocation->x, allocation->y,
			      allocation->width, allocation->height);

      glame_ruler_make_pixmap (ruler);
    }
}

static gint
glame_ruler_expose (GtkWidget      *widget,
		   GdkEventExpose *event)
{
  GlameRuler *ruler;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (GLAME_IS_RULER (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if (GTK_WIDGET_DRAWABLE (widget))
    {
      ruler = GLAME_RULER (widget);

      glame_ruler_draw_ticks (ruler);
      
      gdk_draw_pixmap (widget->window,
		       ruler->non_gr_exp_gc,
		       ruler->backing_store,
		       0, 0, 0, 0,
		       widget->allocation.width,
		       widget->allocation.height);
      
      glame_ruler_draw_pos (ruler);
    }

  return FALSE;
}

static void
glame_ruler_make_pixmap (GlameRuler *ruler)
{
  GtkWidget *widget;
  gint width;
  gint height;

  widget = GTK_WIDGET (ruler);

  if (ruler->backing_store)
    {
      gdk_window_get_size (ruler->backing_store, &width, &height);
      if ((width == widget->allocation.width) &&
	  (height == widget->allocation.height))
	return;

      gdk_pixmap_unref (ruler->backing_store);
    }

  ruler->backing_store = gdk_pixmap_new (widget->window,
					 widget->allocation.width,
					 widget->allocation.height,
					 -1);

  ruler->xsrc = 0;
  ruler->ysrc = 0;

  if (!ruler->non_gr_exp_gc)
    {
      ruler->non_gr_exp_gc = gdk_gc_new (widget->window);
      gdk_gc_set_exposures (ruler->non_gr_exp_gc, FALSE);
    }
}
