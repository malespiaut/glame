/* gtkwave.c: Gtk+ stub widget for displaying audio
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

#include <gtk/gtk.h>
#include "gtkwave.h"

static GtkVBoxClass *parent_class;

static void gtk_wave_class_init (GtkWaveClass *klass);
static void gtk_wave_init       (GtkWave      *wave);

GtkType
gtk_wave_get_type (void)
{
  static GtkType wave_type = 0;
  
  if (!wave_type)
    {
      static const GtkTypeInfo wave_info =
      {
        "GtkWave",
        sizeof (GtkWave),
        sizeof (GtkWaveClass),
        (GtkClassInitFunc) gtk_wave_class_init,
        (GtkObjectInitFunc) gtk_wave_init,
        /* reserved_1 */ NULL,
        /* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL
      };
      
      wave_type = gtk_type_unique (GTK_TYPE_VBOX, &wave_info);
    }
  
  return wave_type;
}


GtkWidget *
gtk_wave_new (void)
{
  GtkWidget *widget;

  widget = GTK_WIDGET (gtk_type_new (GTK_TYPE_WAVE));

  GTK_BOX (widget)->homogeneous = FALSE;
  GTK_BOX (widget)->spacing = 0;

  return widget;
}


static void
gtk_wave_class_init (GtkWaveClass *klass)
{
  GtkObjectClass *object_class = GTK_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
  GtkVBoxClass   *vbox_class = GTK_VBOX_CLASS (klass);

  parent_class = gtk_type_class (GTK_TYPE_VBOX);
}


static void
gtk_wave_init (GtkWave *wave)
{
  wave->view = gtk_wave_view_new ();
  gtk_box_pack_start (GTK_BOX (wave), wave->view, TRUE, TRUE, 0);
  gtk_widget_show_all (wave->view);
}


GtkWaveView *
gtk_wave_get_view (GtkWave *wave)
{
  if (wave->view == NULL)
    return NULL;

  return GTK_WAVE_VIEW (wave->view);
}
