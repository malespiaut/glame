/*
 * From GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * Copyright (C) 2001 Richard Guenther
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

#include <math.h>
#include <stdio.h>
#include <string.h>
#include "glame_hruler.h"


#define RULER_HEIGHT          14
#define MINIMUM_INCR          5


static void glame_hruler_class_init    (GlameHRulerClass *klass);
static void glame_hruler_init          (GlameHRuler      *hruler);
static gint glame_hruler_motion_notify (GtkWidget       *widget,
				       GdkEventMotion  *event);
static void glame_hruler_draw_ticks    (GlameRuler       *ruler);
static void glame_hruler_draw_pos      (GlameRuler       *ruler);
static gdouble glame_hruler_get_stride (GlameRuler       *ruler);


GtkType
glame_hruler_get_type (void)
{
  static GtkType hruler2_type = 0;

  if (!hruler2_type)
    {
      static const GtkTypeInfo hruler2_info =
      {
	"GlameHRuler",
	sizeof (GlameHRuler),
	sizeof (GlameHRulerClass),
	(GtkClassInitFunc) glame_hruler_class_init,
	(GtkObjectInitFunc) glame_hruler_init,
	/* reserved_1 */ NULL,
        /* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      hruler2_type = gtk_type_unique (GLAME_TYPE_RULER, &hruler2_info);
    }

  return hruler2_type;
}

static void
glame_hruler_class_init (GlameHRulerClass *klass)
{
  GtkWidgetClass *widget_class;
  GlameRulerClass *ruler_class;

  widget_class = (GtkWidgetClass*) klass;
  ruler_class = (GlameRulerClass*) klass;

  widget_class->motion_notify_event = glame_hruler_motion_notify;

  ruler_class->draw_ticks = glame_hruler_draw_ticks;
  ruler_class->draw_pos = glame_hruler_draw_pos;
  ruler_class->get_stride = glame_hruler_get_stride;
}

static void
glame_hruler_init (GlameHRuler *hruler)
{
  GtkWidget *widget;

  widget = GTK_WIDGET (hruler);
  widget->requisition.width = GTK_STYLE(widget->style)->xthickness * 2 + 1;
  widget->requisition.height = GTK_STYLE(widget->style)->ythickness * 2 + RULER_HEIGHT;
}


GtkWidget*
glame_hruler_new (void)
{
  return GTK_WIDGET (gtk_type_new (GLAME_TYPE_HRULER));
}

static gint
glame_hruler_motion_notify (GtkWidget      *widget,
			   GdkEventMotion *event)
{
  GlameRuler *ruler;
  gint x;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (GLAME_IS_HRULER (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  ruler = GLAME_RULER (widget);

  if (event->is_hint)
    gdk_window_get_pointer (widget->window, &x, NULL, NULL);
  else
    x = event->x;

  ruler->position = ruler->lower + ((ruler->upper - ruler->lower) * x) / widget->allocation.width;

  /*  Make sure the ruler has been allocated already  */
  if (ruler->backing_store != NULL)
    glame_ruler_draw_pos (ruler);

  return FALSE;
}

static void
glame_hruler_draw_ticks (GlameRuler *ruler)
{
  GtkWidget *widget;
  GdkGC *gc, *bg_gc;
  GdkFont *font;
  gint i;
  gint width, height;
  gint xthickness;
  gint ythickness;
  gint length, ideal_length;
  gdouble lower, upper;		/* Upper and lower limits, in ruler units */
  gdouble increment;		/* Number of pixels per unit */
  gint scale;			/* Number of units per major unit */
  gdouble subd_incr;
  gdouble start, end, cur;
  gchar *unit_str;
  gint digit_height;
  gint digit_offset;
  gint text_width;
  gint pos;
#if 0
  PangoLayout *layout;
  PangoRectangle logical_rect, ink_rect;
#endif

  g_return_if_fail (ruler != NULL);
  g_return_if_fail (GLAME_IS_HRULER (ruler));

  if (!GTK_WIDGET_DRAWABLE (ruler)) 
    return;

  widget = GTK_WIDGET (ruler);

  gc = widget->style->fg_gc[GTK_STATE_NORMAL];
  bg_gc = widget->style->bg_gc[GTK_STATE_NORMAL];
  font = gtk_style_get_font(widget->style);

  xthickness = GTK_STYLE(widget->style)->xthickness;
  ythickness = GTK_STYLE(widget->style)->ythickness;

#if 0
  layout = gtk_widget_create_pango_layout (widget, "012456789");
  pango_layout_get_extents (layout, &ink_rect, &logical_rect);
  
  digit_height = PANGO_PIXELS (ink_rect.height) + 1;
  digit_offset = ink_rect.y;
#endif
  digit_height = 10 /* FIXME */;
  digit_offset = 2 /* FIXME */;

  width = widget->allocation.width;
  height = widget->allocation.height - ythickness * 2;
   
  gtk_paint_box (widget->style, ruler->backing_store,
		 GTK_STATE_NORMAL, GTK_SHADOW_OUT, 
		 NULL, widget, "hruler",
		 0, 0, 
		 widget->allocation.width, widget->allocation.height);
  
  
  gdk_draw_line (ruler->backing_store, gc,
		 xthickness,
		 height + ythickness,
		 widget->allocation.width - xthickness,
		 height + ythickness);

  upper = ruler->upper / ruler->metric->pixels_per_unit;
  lower = ruler->lower / ruler->metric->pixels_per_unit;

  if ((upper - lower) == 0) 
    return;
  increment = (gdouble) width / (upper - lower);

  /* determine the scale
   *  We calculate the text size as for the vruler instead of using
   *  text_width = gdk_string_width(font, unit_str), so that the result
   *  for the scale looks consistent with an accompanying vruler
   */
  scale = ceil (ruler->max_size / ruler->metric->pixels_per_unit);
  unit_str = ruler->metric->translate (scale);
  text_width = strlen (unit_str) * digit_height + 1;
  g_free (unit_str);

  for (scale = 0; scale < ruler->metric->nr_ruler_scale; scale++)
    if (ruler->metric->ruler_scale[scale] * fabs(increment) > 2 * text_width)
      break;

  if (scale == ruler->metric->nr_ruler_scale)
    scale = ruler->metric->nr_ruler_scale - 1;

  /* drawing starts here */
  length = 0;
  for (i = ruler->metric->nr_subdivide - 1; i >= 0; i--)
    {
      subd_incr = (gdouble) ruler->metric->ruler_scale[scale] / 
	          (gdouble) ruler->metric->subdivide[i];
      if (subd_incr * fabs(increment) <= MINIMUM_INCR) 
	continue;

      /* Calculate the length of the tickmarks. Make sure that
       * this length increases for each set of ticks
       */
      ideal_length = height / (i + 1) - 1;
      if (ideal_length > ++length)
	length = ideal_length;

      if (lower < upper)
	{
	  start = floor (lower / subd_incr) * subd_incr;
	  end   = ceil  (upper / subd_incr) * subd_incr;
	}
      else
	{
	  start = floor (upper / subd_incr) * subd_incr;
	  end   = ceil  (lower / subd_incr) * subd_incr;
	}

  
      for (cur = start; cur <= end; cur += subd_incr)
	{
	  pos = GLAME_RULER_ROUND ((cur - lower) * increment);

	  gdk_draw_line (ruler->backing_store, gc,
			 pos, height + ythickness, 
			 pos, height - length + ythickness);

	  /* draw label */
	  if (i == 0)
	    {
#if 0
	      unit_str = ruler->metric->translate (cur);
	      pango_layout_set_text (layout, unit_str, -1);
              g_free (unit_str);

	      pango_layout_get_extents (layout, &logical_rect, NULL);

	      gdk_draw_layout (ruler->backing_store, gc,
			       pos + 2, ythickness + PANGO_PIXELS (logical_rect.y - digit_offset),
			       layout);
#endif
	      unit_str = ruler->metric->translate (cur);
	      gdk_draw_string(ruler->backing_store, font, gc,
			      pos + 2, ythickness + font->ascent -1,
			      unit_str);
              g_free (unit_str);
	    }
	}
    }

#if 0
  gtk_object_unref (GTK_OBJECT (layout));
#endif
}

static void
glame_hruler_draw_pos (GlameRuler *ruler)
{
  GtkWidget *widget;
  GdkGC *gc;
  int i;
  gint x, y;
  gint width, height;
  gint bs_width, bs_height;
  gint xthickness;
  gint ythickness;
  gdouble increment;

  g_return_if_fail (ruler != NULL);
  g_return_if_fail (GLAME_IS_HRULER (ruler));

  if (GTK_WIDGET_DRAWABLE (ruler))
    {
      widget = GTK_WIDGET (ruler);

      gc = widget->style->fg_gc[GTK_STATE_NORMAL];
      xthickness = GTK_STYLE(widget->style)->xthickness;
      ythickness = GTK_STYLE(widget->style)->ythickness;
      width = widget->allocation.width;
      height = widget->allocation.height - ythickness * 2;

      bs_width = height / 2;
      bs_width |= 1;  /* make sure it's odd */
      bs_height = bs_width / 2 + 1;

      if ((bs_width > 0) && (bs_height > 0))
	{
	  /*  If a backing store exists, restore the ruler  */
	  if (ruler->backing_store && ruler->non_gr_exp_gc)
	    gdk_draw_pixmap (ruler->widget.window,
			     ruler->non_gr_exp_gc,
			     ruler->backing_store,
			     ruler->xsrc, ruler->ysrc,
			     ruler->xsrc, ruler->ysrc,
			     bs_width, bs_height);

	  increment = (gdouble) width / (ruler->upper - ruler->lower);

	  x = GLAME_RULER_ROUND ((ruler->position - ruler->lower) * increment) + (xthickness - bs_width) / 2 - 1;
	  y = (height + bs_height) / 2 + ythickness;

	  for (i = 0; i < bs_height; i++)
	    gdk_draw_line (widget->window, gc,
			   x + i, y + i,
			   x + bs_width - 1 - i, y + i);


	  ruler->xsrc = x;
	  ruler->ysrc = y;
	}
    }
}

static gdouble
glame_hruler_get_stride (GlameRuler *ruler)
{
  GtkWidget *widget;
  GdkFont *font;
  gint width;
  gint ythickness;
  gdouble lower, upper;		/* Upper and lower limits, in ruler units */
  gdouble increment;		/* Number of pixels per unit */
  gint scale;			/* Number of units per major unit */
  gdouble subd_incr;
  gchar *unit_str;
  gint digit_height;
  gint text_width;
#if 0
  PangoLayout *layout;
  PangoRectangle logical_rect, ink_rect;
#endif

  if (!ruler || !GLAME_IS_HRULER (ruler))
    return 0.0;

  if (!GTK_WIDGET_DRAWABLE (ruler)) 
    return 0.0;

  widget = GTK_WIDGET (ruler);

  font = gtk_style_get_font(widget->style);

  ythickness = GTK_STYLE(GTK_OBJECT_GET_CLASS(widget->style))->ythickness;

#if 0
  layout = gtk_widget_create_pango_layout (widget, "012456789");
  pango_layout_get_extents (layout, &ink_rect, &logical_rect);
  
  digit_height = PANGO_PIXELS (ink_rect.height) + 1;
#endif
  digit_height = 10 /* FIXME */;

  width = widget->allocation.width;
   
  upper = ruler->upper / ruler->metric->pixels_per_unit;
  lower = ruler->lower / ruler->metric->pixels_per_unit;

  if ((upper - lower) == 0) 
    return 0.0;
  increment = (gdouble) width / (upper - lower);

  /* determine the scale
   *  We calculate the text size as for the vruler instead of using
   *  text_width = gdk_string_width(font, unit_str), so that the result
   *  for the scale looks consistent with an accompanying vruler
   */
  scale = ceil (ruler->max_size / ruler->metric->pixels_per_unit);
  unit_str = ruler->metric->translate (scale);
  text_width = strlen (unit_str) * digit_height + 1;
  g_free (unit_str);

  for (scale = 0; scale < ruler->metric->nr_ruler_scale; scale++)
    if (ruler->metric->ruler_scale[scale] * fabs(increment) > 2 * text_width)
      break;

  if (scale == ruler->metric->nr_ruler_scale)
    scale = ruler->metric->nr_ruler_scale - 1;

  subd_incr = (gdouble) ruler->metric->ruler_scale[scale] / 
	      (gdouble) ruler->metric->subdivide[0];
  if (subd_incr * fabs(increment) <= MINIMUM_INCR) 
    return 0.0;

  return subd_incr * increment;
}
