/* gtkeditablewavebuffer.c: Interface for editable wavebuffer.
 * Copyright (c) 2000 David A. Bartold
 * Copyright (C) 2000, 2001 Richard Guenther
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
#include <gtk/gtksignal.h>
#include <gtk/gtkmain.h>
#include "gtkeditablewavebuffer.h"


static void gtk_editable_wave_buffer_class_init      (GtkEditableWaveBufferClass *klass);
static void gtk_editable_wave_buffer_init            (GtkEditableWaveBuffer      *editable_wave_buffer);

guint modified_signal;
guint insert_data_signal;
guint delete_data_signal;


GtkType
gtk_editable_wave_buffer_get_type (void)
{
  static GtkType editable_wave_buffer_type = 0;

  if (!editable_wave_buffer_type)
    {
      static const GtkTypeInfo editable_wave_buffer_info =
      {
        "GtkEditableWaveBuffer",
        sizeof (GtkEditableWaveBuffer),
        sizeof (GtkEditableWaveBufferClass),
        (GtkClassInitFunc) gtk_editable_wave_buffer_class_init,
        (GtkObjectInitFunc) gtk_editable_wave_buffer_init,
        /* reserved_1 */ NULL,
        /* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL
      };

      editable_wave_buffer_type = gtk_type_unique (GTK_TYPE_WAVE_BUFFER, &editable_wave_buffer_info);
    }

  return editable_wave_buffer_type;
}


static void
gtk_editable_wave_buffer_real_destroy (GtkObject *object)
{
  GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (object);

  if (editable->modified)
    {
      gtk_idle_remove (editable->modified_idle);
      editable->modified = FALSE;
    }
}


static void
gtk_editable_wave_buffer_class_init (GtkEditableWaveBufferClass *klass)
{
  GtkObjectClass *object_class;
  GtkWaveBufferClass *wavebuffer_class;

  object_class = (GtkObjectClass *) klass;
  wavebuffer_class = (GtkWaveBufferClass *) klass;

  object_class->destroy = gtk_editable_wave_buffer_real_destroy;

  wavebuffer_class->get_datatype = NULL;
  wavebuffer_class->get_length = NULL;
  wavebuffer_class->get_num_channels = NULL;
  wavebuffer_class->get_samples = NULL;
  klass->set_samples = NULL;
  klass->insert = NULL;
  klass->del = NULL;
}


static void
gtk_editable_wave_buffer_init (GtkEditableWaveBuffer *editable)
{
}


static gint
gtk_editable_wave_buffer_emit_modified (GtkEditableWaveBuffer *editable)
{
  gtk_signal_emit (GTK_OBJECT (editable), modified_signal, &editable->modified_range);
  editable->modified = FALSE;

  return 0;
}


void
gtk_editable_wave_buffer_queue_modified (GtkEditableWaveBuffer *editable, guint32 start, guint32 length)
{
  if (editable->modified)
    {
      g_range_add (&editable->modified_range, start, length);
    }
  else
    {
      g_range_set (&editable->modified_range, start, length);
      editable->modified = TRUE;
      editable->modified_idle =
        gtk_idle_add ((GtkFunction) &gtk_editable_wave_buffer_emit_modified,
                      editable);
    }
}


gint
gtk_editable_wave_buffer_set_samples (GtkEditableWaveBuffer *editable,
                                      guint32 start,
                                      guint32 length,
                                      guint32 channel_mask,
                                      gpointer data)
{
  return GTK_EDITABLE_WAVE_BUFFER_CLASS (G_OBJECT_GET_CLASS (editable))->set_samples (editable, start, length, channel_mask, data);
}


gint
gtk_editable_wave_buffer_insert (GtkEditableWaveBuffer *editable,
                                 guint32 start,
                                 guint32 length)
{
  return GTK_EDITABLE_WAVE_BUFFER_CLASS (G_OBJECT_GET_CLASS (editable))->insert (editable, start, length);
}


gint
gtk_editable_wave_buffer_delete (GtkEditableWaveBuffer *editable,
                                 guint32 start,
                                 guint32 length)
{
  return GTK_EDITABLE_WAVE_BUFFER_CLASS (G_OBJECT_GET_CLASS (editable))->del (editable, start, length);
}
