/* gwavefile.h: read wave data directly from a .WAV file
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

#ifndef __G_WAVEFILE_H__
#define __G_WAVEFILE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <glib.h>
#include <stdio.h>
#include "gtkwavebuffer.h"

typedef struct _GWavefile GWavefile;
typedef enum   _GWavefileType         GWavefileType;

enum _GWavefileType
{
  G_WAVEFILE_TYPE_NULL,
  G_WAVEFILE_TYPE_S32, G_WAVEFILE_TYPE_U32,
  G_WAVEFILE_TYPE_S16, G_WAVEFILE_TYPE_U16,
  G_WAVEFILE_TYPE_S8, G_WAVEFILE_TYPE_U8,
  G_WAVEFILE_TYPE_F4, G_WAVEFILE_TYPE_F8
};

struct _GWavefile
{
  FILE          *file;

  guint32        rate;
  guint32        n_channels;
  guint32        length;
  guint32        width;
  GWavefileType  datatype;
  guint32        data_offset;
};


GWavefile      *g_wavefile_open             (gchar     *file);
void            g_wavefile_close            (GWavefile *wave);
GWavefileType   g_wavefile_get_datatype     (GWavefile *wave);
guint32         g_wavefile_get_length       (GWavefile *wave);
guint32         g_wavefile_get_num_channels (GWavefile *wave);
guint32         g_wavefile_get_rate         (GWavefile *wave);
void            g_wavefile_read             (GWavefile *wave,
                                             guint32    offset,
                                             guint32    length,
                                             gpointer   buf);

/* Utility functions. */
static inline gint16
double_to_s16 (gdouble d)
{
  if (d >= 1.0)
    return 32767;
  else if (d <= -1.0)
    return -32768;

  return (gint16) ((d + 1.0) * 32767.5 - 32768.0);
}

static inline gint32
double_to_s32 (gdouble d)
{
  if (d >= 1.0)
    return 0x7fffffff /* 2147483647 */;
  else if (d <= -1.0)
    return 0x80000000 /* -2147483648 */;

  return (gint32) ((d + 1.0) * 2147483647.5 - 2147483648.0);
}

static inline gdouble
s32_to_double (gint32 i)
{
  return (((double) i) + 2147483648.0) / 2147483647.5 - 1.0;
}

static inline gdouble
s16_to_double (gint16 i)
{
  return (((double) i) + 32768.0) / 32767.5 - 1.0;
}


guint32 g_wavefile_type_width   (GWavefileType dtype);
void    g_wavefile_type_convert (guint32       n_channels,
                                 guint32       length,
                                 GWavefileType out_type,
                                 gpointer      out,
                                 GWavefileType in_type,
                                 gpointer      in);
void    g_wavefile_type_clear   (guint32       n_channels,
                                 guint32       length,
                                 GWavefileType type,
                                 gpointer      out);


#ifdef __cplusplus
}
#endif

#endif /* __G_WAVEFILE_H__ */
