/*
 * gtkwavedraw.h
 *
 * $Id: gtkwavedraw.h,v 1.1 2000/04/09 21:10:59 navratil Exp $
 *
 * Copyright (C) 2000 Joe Navratil
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


#ifndef __GTK_WAVE_DRAW_H__
#define __GTK_WAVE_DRAW_H__

#include <gdk/gdk.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_TYPE_WAVE_DRAW            (gtk_wave_draw_get_type ())
#define GTK_WAVE_DRAW(obj)            (GTK_CHECK_CAST ((obj), GTK_TYPE_WAVE_DRAW, GtkWaveDraw))
#define GTK_WAVE_DRAW_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_WAVE_DRAW, GtkWaveDrawClass))
#define GTK_IS_WAVE_DRAW(obj)         (GTK_CHECK_TYPE ((obj), GTK_TYPE_WAVE_DRAW))
#define GTK_IS_WAVE_DRAW_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_WAVE_DRAW))

typedef struct _GtkWaveDraw       GtkWaveDraw;
typedef struct _GtkWaveDrawClass  GtkWaveDrawClass;

struct _GtkWaveDraw
{
  GtkWidget widget;

  GdkPixmap *pixmap;              /* The background pixmap for the widget */
  GdkGC     *wavedraw_gc;         /* Generic GC, we're going to play with its
				   * foreground color to suit our needs */  

  gfloat **data;                  /* Data to be graphed */
  glong  **marker;                /* Any/all "saved points" for the data, 
				   * same major index as the data */
  glong  *n_samples;              /* Number of samples in each set of data */
  glong  *start;                  /* Starting point for each set of data */

  gint   n_waves;                 /* Number of waves we're displaying */

  glong  n_samples_current;       /* Number of samples currently on display */
  glong  start_current;           /* Starting position of current display */
  gint   n_points_per_pixel;      /* Maximum number of points to 
				   * display per pixel */
  
  gint   dirty;                   /* Dirty bit -- used to determine if the
				   * data has changed since the last time we
				   * drew it to the backbuffer */
};

struct _GtkWaveDrawClass
{
  GtkWidgetClass parent_class;

  void (* pressed)  (GtkWaveDraw *wavedraw);
  void (* released) (GtkWaveDraw *wavedraw);
  void (* motion)   (GtkWaveDraw *wavedraw,
		     gint        xdelta,
		     gint        ydelta);
  
};


GtkWidget* gtk_wave_draw_new        (void);
GtkType    gtk_wave_draw_get_type   (void);
void       gtk_wave_draw_zoom       (GtkWaveDraw *wavedraw, 
				     glong start, glong stop);
gint       gtk_wave_draw_add_wave   (GtkWaveDraw *wavedraw,
				     gfloat *data,
				     glong  n_samples,
				     glong  start);



#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_WAVE_DRAW_H__ */
