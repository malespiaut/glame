/* gtkwavebuffer.h: interface class for read-only wave data
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

#ifndef __GTK_WAVE_BUFFER_H__
#define __GTK_WAVE_BUFFER_H__

#include <gtk/gtkobject.h>
#include "gwavefile.h"
#include "grange.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Fix old gtk versions. */
#ifndef G_OBJECT_GET_CLASS
#define G_OBJECT_GET_CLASS(object) ((GTK_OBJECT(object))->klass)
#endif


#define GTK_TYPE_WAVE_BUFFER            (gtk_wave_buffer_get_type ())
#define GTK_WAVE_BUFFER(obj)            (GTK_CHECK_CAST ((obj), GTK_TYPE_WAVE_BUFFER, GtkWaveBuffer))
#define GTK_WAVE_BUFFER_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_WAVE_BUFFER, GtkWaveBufferClass))
#define GTK_IS_WAVE_BUFFER(obj)         (GTK_CHECK_TYPE ((obj), GTK_TYPE_WAVE_BUFFER))
#define GTK_IS_WAVE_BUFFER_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_WAVE_BUFFER))


typedef struct _GtkWaveBuffer         GtkWaveBuffer;
typedef struct _GtkWaveBufferClass    GtkWaveBufferClass;

struct _GtkWaveBuffer
{
  GtkObject object;
};

struct _GtkWaveBufferClass
{
  GtkObjectClass object_class;

  /* Signals */
  void           (*modified)      (GtkObject *object,
                                   GRange    *range);
  void           (*insert_data)   (GtkObject *object,
                                   GRange    *range);
  void           (*delete_data)   (GtkObject *object,
                                   GRange    *range);

  /* Pure abstract functions. */
  guint32        (*get_rate)          (GtkWaveBuffer *wavebuffer);
  GWavefileType  (*get_datatype)      (GtkWaveBuffer *wavebuffer);
  guint32        (*get_length)        (GtkWaveBuffer *wavebuffer);
  guint32        (*get_num_channels)  (GtkWaveBuffer *wavebuffer);
  void           (*get_samples)       (GtkWaveBuffer *wavebuffer,
                                       guint32        start,
                                       guint32        length,
                                       guint32        channel_mask,
                                       gpointer       data);
};

extern guint   modified_signal;
extern guint   insert_data_signal;
extern guint   delete_data_signal;

GtkType         gtk_wave_buffer_get_type         (void);

guint32         gtk_wave_buffer_get_rate         (GtkWaveBuffer *wavebuffer);
guint32         gtk_wave_buffer_get_num_channels (GtkWaveBuffer *wavebuffer);
guint32         gtk_wave_buffer_get_length       (GtkWaveBuffer *wavebuffer);
GWavefileType   gtk_wave_buffer_get_datatype     (GtkWaveBuffer *wavebuffer);
void            gtk_wave_buffer_get_samples      (GtkWaveBuffer *wavebuffer,
                                                  guint32        start,
                                                  guint32        length,
                                                  guint32        channel_mask,
                                                  gpointer       data);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_WAVE_BUFFER_H__ */

