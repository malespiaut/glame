#ifndef _GTK_KNOB_H
#define _GTK_KNOB_H

/*
 * gtknob.h
 *
 * $Id: gtknob.h,v 1.7 2002/04/25 21:17:34 richi Exp $
 *
 * Copyright (C) 2000 timecop@japan.co.jp
 * Copyright (C) 2002 Richard Guenther, Laurent Georget
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
#define GTK_KNOB_MAX_TICKS     8
typedef struct _GtkKnob GtkKnob;
typedef struct _GtkKnobClass GtkKnobClass;
typedef gchar *(*GtkKnobFormatter)(gfloat, gpointer);

struct _GtkKnob {
	GtkWidget widget;
	GdkPixmap *pixmap;
	GtkAdjustment *adjustment;

	guint8 button;
	gint16 x_click_point;
	gint16 y_click_point;

	gint value;
	gint old_value;

	/* special ticks */
	gfloat ticks[GTK_KNOB_MAX_TICKS];
	gint nr_ticks;

	/* formatter */
	GtkKnobFormatter formatter;
	gpointer formatter_data;
	gchar *min_cache;
	gchar *max_cache;
	gchar *val_cache;

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
void           gtk_knob_set_formatter(GtkKnob *knob, GtkKnobFormatter f,
                                      gpointer data);
void           gtk_knob_add_tick(GtkKnob *knob, gfloat tick);

void           gtk_knob_glade_register();


#ifdef __cplusplus
}
#endif



#endif				/* __GTK_KNOB_H__ */
