/* gtkeditablewavebuffer.h: Interface for editable wavebuffer.
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

#ifndef __GTK_EDITABLE_WAVE_BUFFER_H__
#define __GTK_EDITABLE_WAVE_BUFFER_H__

#include "grange.h"
#include "gtkwavebuffer.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_TYPE_EDITABLE_WAVE_BUFFER            (gtk_editable_wave_buffer_get_type ())
#define GTK_EDITABLE_WAVE_BUFFER(obj)            (GTK_CHECK_CAST ((obj), GTK_TYPE_EDITABLE_WAVE_BUFFER, GtkEditableWaveBuffer))
#define GTK_EDITABLE_WAVE_BUFFER_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_EDITABLE_WAVE_BUFFER, GtkEditableWaveBufferClass))
#define GTK_IS_EDITABLE_WAVE_BUFFER(obj)         (GTK_CHECK_TYPE ((obj), GTK_TYPE_EDITABLE_WAVE_BUFFER))
#define GTK_IS_EDITABLE_WAVE_BUFFER_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_EDITABLE_WAVE_BUFFER))


typedef struct _GtkEditableWaveBuffer         GtkEditableWaveBuffer;
typedef struct _GtkEditableWaveBufferClass    GtkEditableWaveBufferClass;

struct _GtkEditableWaveBuffer
{
  GtkWaveBuffer object;

  gboolean  modified;
  GRange    modified_range;
  guint     modified_idle;
};

struct _GtkEditableWaveBufferClass
{
  GtkWaveBufferClass object_class;

  /* Pure abstract functions. */
  gint (*set_samples)   (GtkEditableWaveBuffer *editable_wave_buffer,
                         guint32                start,
                         guint32                length,
                         guint32                channel_mask,
                         gpointer               data);
  gint (*insert)        (GtkEditableWaveBuffer *editable_wave_buffer,
                         guint32                start,
                         guint32                length);
  gint (*delete)        (GtkEditableWaveBuffer *editable_wave_buffer,
                         guint32                start,
                         guint32                length);
};

GtkType gtk_editable_wave_buffer_get_type       (void);
void    gtk_editable_wave_buffer_queue_modified (GtkEditableWaveBuffer *editable,
                                                 guint32                start,
                                                 guint32                length);
gint    gtk_editable_wave_buffer_set_samples    (GtkEditableWaveBuffer *editable_wave_buffer,
                                                 guint32                start,
                                                 guint32                length,
                                                 guint32                channel_mask,
                                                 gpointer               data);
gint    gtk_editable_wave_buffer_insert         (GtkEditableWaveBuffer *editable_wave_buffer,
                                                 guint32                pos,
                                                 guint32                length);
gint    gtk_editable_wave_buffer_delete         (GtkEditableWaveBuffer *editable_wave_buffer,
                                                 guint32                start,
                                                 guint32                length);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_EDITABLE_WAVE_BUFFER_H__ */

