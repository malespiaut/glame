#ifndef _GTK_KNOB_H
#define _GTK_KNOB_H

/*
 * gtknob.h
 *
 * $Id: gtknob.h,v 1.4 2002/04/10 19:57:33 ochonpaul Exp $
 *
 * Copyright (C) 2000 timecop@japan.co.jp
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtkwidget.h>


#define GTK_TYPE_KNOB          (gtk_knob_get_type ())
#define GTK_KNOB(obj)          (GTK_CHECK_CAST ((obj), GTK_TYPE_KNOB, GtkKnob))
#define GTK_KNOB_CLASS(klass)  (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_KNOB, GtkKnobClass))
#define GTK_IS_KNOB(obj)       (GTK_CHECK_TYPE (obj, GTK_TYPE_KNOB))
typedef struct _GtkKnob GtkKnob;
typedef struct _GtkKnobClass GtkKnobClass;

struct _GtkKnob {
    GtkWidget widget;
    GdkPixmap *pixmap;
    guint8 button;
    gint16 x_click_point;
    gint16 y_click_point;

    gint value;
    gint old_value;

    GtkAdjustment *adjustment;

    gchar show_min[8];
    gchar show_max[8];
    gchar show_val[8];
    GdkFont *font;
    GdkGC *gc;
   
  };

struct _GtkKnobClass {
    GtkWidgetClass parent_class;
};


#ifdef __cplusplus
extern "C" {
#endif


guint          gtk_knob_get_type(void);
GtkWidget     *gtk_knob_new(GtkAdjustment * adj);
void           gtk_knob_set_adjustment(GtkKnob *knob, GtkAdjustment *adj);
GtkAdjustment *gtk_knob_get_adjustment(GtkKnob *knob);
void           gtk_knob_glade_register();


#ifdef __cplusplus
}
#endif

#endif				/* __GTK_KNOB_H__ */
