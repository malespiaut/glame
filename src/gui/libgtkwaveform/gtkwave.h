/* gtkwave.h: Gtk+ stub widget for displaying audio
 * Copyright (c) 2000 David A. Bartold
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

#ifndef __GTK_WAVE_H__
#define __GTK_WAVE_H__


#include <gtk/gtk.h>
#include "gtkwaveview.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_TYPE_WAVE            (gtk_wave_get_type ())
#define GTK_WAVE(obj)            (GTK_CHECK_CAST ((obj), GTK_TYPE_WAVE, GtkWave))
#define GTK_WAVE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_WAVE, GtkWaveClass))
#define GTK_IS_WAVE(obj)         (GTK_CHECK_TYPE ((obj), GTK_TYPE_WAVE))
#define GTK_IS_WAVE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_WAVE))


typedef struct _GtkWave      GtkWave;
typedef struct _GtkWaveClass GtkWaveClass;

struct _GtkWave
{
  GtkVBox         parent;

  GtkWidget      *view;
};

struct _GtkWaveClass
{
  GtkVBoxClass    parent_class;
};


GtkType       gtk_wave_get_type        (void);
GtkWidget    *gtk_wave_new             (void);
GtkWaveView  *gtk_wave_get_view        (GtkWave       *wave);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_WAVE_H__ */
