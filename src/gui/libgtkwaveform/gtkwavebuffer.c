/* gtkwavebuffer.c: interface class for read-only wave data
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

#include <glib.h>
#include "gtkwavebuffer.h"
#include <gtk/gtksignal.h>


static void gtk_wave_buffer_class_init      (GtkWaveBufferClass *klass);
static void gtk_wave_buffer_init            (GtkWaveBuffer      *wavebuffer);


GtkType
gtk_wave_buffer_get_type (void)
{
  static GtkType wavebuffer_type = 0;

  if (!wavebuffer_type)
    {
      static const GtkTypeInfo wavebuffer_info =
      {
        "GtkWaveBuffer",
        sizeof (GtkWaveBuffer),
        sizeof (GtkWaveBufferClass),
        (GtkClassInitFunc) gtk_wave_buffer_class_init,
        (GtkObjectInitFunc) gtk_wave_buffer_init,
        /* reserved_1 */ NULL,
        /* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL
      };

      wavebuffer_type = gtk_type_unique (GTK_TYPE_OBJECT, &wavebuffer_info);
    }

  return wavebuffer_type;
}


static void
gtk_wave_buffer_class_init (GtkWaveBufferClass *klass)
{
  GtkObjectClass *object_class;
  GtkType type;

  type = gtk_wave_buffer_get_type ();

  object_class = (GtkObjectClass *) klass;

  klass->get_datatype = NULL;
  klass->get_length = NULL;
  klass->get_num_channels = NULL;
  klass->get_samples = NULL;

  modified_signal =
    gtk_signal_new ("modified",
                    GTK_RUN_FIRST,
                    type,
                    GTK_SIGNAL_OFFSET (GtkWaveBufferClass, modified),
                    gtk_marshal_NONE__POINTER,
                    GTK_TYPE_NONE,
                    1, GTK_TYPE_POINTER);

  insert_data_signal =
    gtk_signal_new ("insert_data",
                    GTK_RUN_FIRST,
                    type,
                    GTK_SIGNAL_OFFSET (GtkWaveBufferClass, insert_data),
                    gtk_marshal_NONE__POINTER,
                    GTK_TYPE_NONE,
                    1, GTK_TYPE_POINTER);

  delete_data_signal =
    gtk_signal_new ("delete_data",
                    GTK_RUN_FIRST,
                    type,
                    GTK_SIGNAL_OFFSET (GtkWaveBufferClass, delete_data),
                    gtk_marshal_NONE__POINTER,
                    GTK_TYPE_NONE,
                    1, GTK_TYPE_POINTER);

  /*  gtk_object_class_add_signals (object_class, &modified_signal, 1);
  gtk_object_class_add_signals (object_class, &insert_data_signal, 1);
  gtk_object_class_add_signals (object_class, &delete_data_signal, 1);
  */
  klass->modified = NULL;
  klass->insert_data = NULL;
  klass->delete_data = NULL;
}

static void
gtk_wave_buffer_init (GtkWaveBuffer *wavebuffer)
{
}


guint32
gtk_wave_buffer_get_num_channels (GtkWaveBuffer *wavebuffer)
{
  return GTK_WAVE_BUFFER_CLASS (G_OBJECT_GET_CLASS (wavebuffer))->get_num_channels (wavebuffer);
}

guint32
gtk_wave_buffer_get_rate (GtkWaveBuffer *wavebuffer)
{
  return GTK_WAVE_BUFFER_CLASS (G_OBJECT_GET_CLASS (wavebuffer))->get_rate (wavebuffer);
}

guint32
gtk_wave_buffer_get_length (GtkWaveBuffer *wavebuffer)
{
  return GTK_WAVE_BUFFER_CLASS (G_OBJECT_GET_CLASS (wavebuffer))->get_length (wavebuffer);
}

GWavefileType
gtk_wave_buffer_get_datatype (GtkWaveBuffer *wavebuffer)
{
  return GTK_WAVE_BUFFER_CLASS (G_OBJECT_GET_CLASS (wavebuffer))->get_datatype (wavebuffer);
}

void
gtk_wave_buffer_get_samples (GtkWaveBuffer *wavebuffer,
                             guint32        start,
                             guint32        length,
                             guint32        channel_mask,
                             gpointer       data)
{
  GTK_WAVE_BUFFER_CLASS (G_OBJECT_GET_CLASS (wavebuffer))->get_samples (wavebuffer, start, length, channel_mask, data);
}
