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

#ifndef __GLAME_RULER_H__
#define __GLAME_RULER_H__


#include <gdk/gdk.h>
#include <gtk/gtkwidget.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


typedef struct _GlameRuler        GlameRuler;
typedef struct _GlameRulerClass   GlameRulerClass;
typedef struct _GlameRulerMetric  GlameRulerMetric;

#define GLAME_RULER_ROUND(x)        ((int) (x + 0.500000001))

#define GLAME_TYPE_RULER            (glame_ruler_get_type ())
#define GLAME_RULER(obj)            (GTK_CHECK_CAST ((obj), GLAME_TYPE_RULER, GlameRuler))
#define GLAME_RULER_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GLAME_TYPE_RULER, GlameRulerClass))
#define GLAME_IS_RULER(obj)         (GTK_CHECK_TYPE ((obj), GLAME_TYPE_RULER))
#define GLAME_IS_RULER_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GLAME_TYPE_RULER))
#define GLAME_RULER_GET_CLASS(obj)  ((GlameRulerClass *)( GTK_OBJECT_GET_CLASS((GtkObject *)(obj))))


/* All distances below are in 1/72nd's of an inch. (According to
 * Adobe that's a point, but points are really 1/72.27 in.)
 */
struct _GlameRuler
{
  GtkWidget widget;

  GdkPixmap *backing_store;
  GdkGC *non_gr_exp_gc;
  const GlameRulerMetric *metric;
  gint xsrc, ysrc;
  gint slider_size;

  /* The upper limit of the ruler (in points) */
  gdouble lower;
  /* The lower limit of the ruler */
  gdouble upper;
  /* The position of the mark on the ruler */
  gdouble position;
  /* The maximum size of the ruler */
  gdouble max_size;
};

struct _GlameRulerClass
{
  GtkWidgetClass parent_class;

  void    (* draw_ticks) (GlameRuler *ruler);
  void    (* draw_pos)   (GlameRuler *ruler);
  gdouble (* get_stride) (GlameRuler *ruler);
};

struct _GlameRulerMetric
{
  gchar  *metric_name;
  gchar  *abbrev;
  /* This should be points_per_unit. This is the size of the unit
   * in 1/72nd's of an inch and has nothing to do with screen pixels */
  gdouble pixels_per_unit;
  gint nr_ruler_scale;
  gdouble *ruler_scale;
  gint nr_subdivide;
  gint *subdivide;
  gchar* (*translate) (gdouble value);
};


GtkType glame_ruler_get_type   (void);
void    glame_ruler_set_metric (GlameRuler       *ruler,
			        const GlameRulerMetric *metric);
void    glame_ruler_set_range  (GlameRuler       *ruler,
			       gdouble          lower,
			       gdouble          upper,
			       gdouble          position,
			       gdouble          max_size);
void    glame_ruler_draw_ticks (GlameRuler       *ruler);
void    glame_ruler_draw_pos   (GlameRuler       *ruler);
gdouble glame_ruler_get_stride (GlameRuler       *ruler);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GLAME_RULER_H__ */
