/* gwavefile.c: read wave data directly from a .WAV file
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
#include <stdio.h>
#include "gtkwavebuffer.h"
#include "gwavefile.h"


GWavefile      *g_wavefile_open             (gchar     *file);
void            g_wavefile_close            (GWavefile *wave);
GWavefileType   g_wavefile_get_datatype  (GWavefile *wave);
guint32         g_wavefile_get_length       (GWavefile *wave);
guint32         g_wavefile_get_num_channels (GWavefile *wave);
guint32         g_wavefile_get_rate         (GWavefile *wave);
void            g_wavefile_read             (GWavefile *wave,
                                             guint32    offset,
                                             guint32    length,
                                             gpointer   buf);

#define WAVEID(a,b,c,d) (a | ((b) << 8) | ((c) << 16) | ((d) << 24))

static guint32
get_32 (FILE *in)
{
  guint32 v;

  v =  fgetc (in);
  v |= fgetc (in) << 8;
  v |= fgetc (in) << 16;
  v |= fgetc (in) << 24;

  return v;
}

static guint16
get_16 (FILE *in)
{
  guint32 v;

  v =  fgetc (in);
  v |= fgetc (in) << 8;

  return v;
}

GWavefile *
g_wavefile_open (gchar *file)
{
  GWavefile *wave;
  FILE *f;
  guint32 status;
  guint32 file_size, chunk_size, pos, id, rate;
  guint32 found_fmt, found_data;

  guint32 data_length, data_offset;
  guint32 fmt_tag, fmt_channels, fmt_blockalign, fmt_bits;
  GWavefileType fmt_datatype;

  status = 0;
  f = fopen (file, "r");
  if (f == NULL)
    return NULL;

  if (get_32 (f) != WAVEID ('R', 'I', 'F', 'F'))
    status = 1;

  if (status == 0)
    {
      file_size = get_32 (f);

      if (status == 0)
        if (get_32 (f) != WAVEID ('W', 'A', 'V', 'E'))
          status = 1;
    }

  if (status == 0)
    {
      file_size += 8;

      pos = 12;
      found_fmt = 0;
      found_data = 0;
      while (pos < file_size)
        {
          id = get_32 (f);
          chunk_size = get_32 (f);

          switch (id)
            {
              case WAVEID ('f', 'm', 't', ' '):
                if (found_fmt == 1)
                   status = 1;
                else
                  {
                    fmt_tag = get_16 (f);
                    fmt_channels = get_16 (f);
                    rate = get_32 (f);
                    get_32 (f);
                    fmt_blockalign = get_16 (f);
                    fmt_bits = get_16 (f);
                    found_fmt = 1;
                  }
                break;

              case WAVEID ('d', 'a', 't', 'a'):
                if (found_data == 1)
                   status = 1;
                else
                  {
                    data_length = chunk_size;
                    data_offset = pos + 8;  
                    found_data = 1;
                  }
                break;
            }

          pos += chunk_size + 8;
          fseek (f, pos, SEEK_SET);
        }

      if (found_fmt == 0 || found_data == 0)
        status = 1;
    }

  if (status == 0)
    {
      if (fmt_bits == 8)
        fmt_datatype = G_WAVEFILE_TYPE_U8;
      else if (fmt_bits == 16)
        fmt_datatype = G_WAVEFILE_TYPE_S16;
      else
        status = 1;
    }

  if (status == 0)
    status = ferror (f);

  if (status == 0)
    {
      wave = g_new (GWavefile, 1);
      wave->file = f;
      wave->datatype = fmt_datatype;
      wave->rate = rate;
      wave->n_channels = fmt_channels;
      wave->width = g_wavefile_type_width (wave->datatype) * wave->n_channels;
      wave->length = data_length / wave->width;
      wave->data_offset = data_offset;

      return wave;
    }

  if (f != NULL)
    fclose (f);

  return NULL;
}

void
g_wavefile_close (GWavefile *wave)
{
  fclose (wave->file);
  g_free (wave);
}

GWavefileType
g_wavefile_get_datatype (GWavefile *wave)
{
  return wave->datatype;
}

guint32
g_wavefile_get_length (GWavefile *wave)
{
  return wave->length;
}

guint32
g_wavefile_get_num_channels (GWavefile *wave)
{
  return wave->n_channels;
}

guint32
g_wavefile_get_rate (GWavefile *wave)
{
  return wave->rate;
}

void
g_wavefile_read (GWavefile *wave, guint32 offset, guint32 length, gpointer buf)
{
  g_return_if_fail (offset + length <= wave->length);

  fseek (wave->file, wave->data_offset + offset * wave->width, SEEK_SET);
  fread (buf, wave->width, length, wave->file);

#if G_BYTE_ORDER == G_BIG_ENDIAN
  {
    gint32 count;
    gint32 data_width;
    gint32 i;

    count = length * wave->n_channels;
    data_width = g_wavefile_type_width (wave->datatype);
    switch (data_width)
      {
      case 2:
        for (i = 0; i < count; i++)
          ((guint16*) buf)[i] = GUINT16_FROM_LE (((guint16*) buf)[i]);
        break;

      case 4:
        for (i = 0; i < count; i++)
          ((guint32*) buf)[i] = GUINT32_FROM_LE (((guint32*) buf)[i]);
        break;

      case 8:
        for (i = 0; i < count; i++)
          {
            guint32 a, b;

            a = ((guint32*) buf)[i << 1];
            b = ((guint32*) buf)[(i << 1) + 1];
            ((guint32*) buf)[i << 1] = GUINT32_FROM_LE (b);
            ((guint32*) buf)[(i << 1) + 1] = GUINT32_FROM_LE (a);
          }
        break;
      }
  }
#endif
}


guint32
g_wavefile_type_width (GWavefileType dtype)
{
  switch (dtype)
    {
    case G_WAVEFILE_TYPE_F8:
      return 8;

    case G_WAVEFILE_TYPE_S32: case G_WAVEFILE_TYPE_U32: case G_WAVEFILE_TYPE_F4:
      return 4;

    case G_WAVEFILE_TYPE_S16: case G_WAVEFILE_TYPE_U16:
      return 2;

    case G_WAVEFILE_TYPE_S8: case G_WAVEFILE_TYPE_U8:
      return 1;
    }

  return 0;
}

void
g_wavefile_type_convert (guint32       n_channels,
                              guint32       length,
                              GWavefileType out_type,
                              gpointer      out,
                              GWavefileType in_type,
                              gpointer      in)
{
  gint32 *s32;
  guint32 *u32;
  gint16 *s16;
  guint16 *u16;
  guint8 *u8;
  gint8 *s8;
  gdouble *f8;
  gfloat *f4;

  guint32 i, frames;

  frames = length * n_channels;

  if (in_type == out_type)
    {
      guint32 width;

      width = g_wavefile_type_width (in_type);
      memcpy (out, in, frames * width);
      return;
    }

  switch (out_type)
    {
    case G_WAVEFILE_TYPE_F8:
      f8 = (gdouble*) out;

      switch (in_type)
        {
          case G_WAVEFILE_TYPE_F4:
            f4 = (gfloat*) in;
            for (i = 0; i < frames; i++)
              *f8++ = *f4++;
            break;

          case G_WAVEFILE_TYPE_U32:
            u32 = (guint32*) in;
            for (i = 0; i < frames; i++)
              *f8++ = s32_to_double ((*u32++) ^ 0x80000000);
            break;

          case G_WAVEFILE_TYPE_S32:
            s32 = (gint32*) in;
            for (i = 0; i < frames; i++)
              *f8++ = s32_to_double (*s32++);
            break;

          case G_WAVEFILE_TYPE_U16:
            u16 = (guint16*) in;
            for (i = 0; i < frames; i++)
              *f8++ = s16_to_double ((*u16++) ^ 0x8000);
            break;

          case G_WAVEFILE_TYPE_S16:
            s16 = (gint16*) in;
            for (i = 0; i < frames; i++)
              *f8++ = s16_to_double (*s16++);
            break;

          case G_WAVEFILE_TYPE_U8:
            u8 = (guint8*) in;
            for (i = 0; i < frames; i++)
              *f8++ = s16_to_double (((*u8++) << 8) ^ 0x8000);
            break;

          case G_WAVEFILE_TYPE_S8:
            s8 = (gint8*) in;
            for (i = 0; i < frames; i++)
              *f8++ = s16_to_double ((*s8++) << 8);
            break;
        }
      break;

    case G_WAVEFILE_TYPE_F4:
      f4 = (gfloat*) out;

      switch (in_type)
        {
          case G_WAVEFILE_TYPE_F8:
            f8 = (gdouble*) in;
            for (i = 0; i < frames; i++)
              *f4++ = *f8++;
            break;

          case G_WAVEFILE_TYPE_U32:
            u32 = (guint32*) in;
            for (i = 0; i < frames; i++)
              *f4++ = s32_to_double ((*u32++) ^ 0x80000000);
            break;

          case G_WAVEFILE_TYPE_S32:
            s32 = (gint32*) in;
            for (i = 0; i < frames; i++)
              *f4++ = s32_to_double (*s32++);
            break;

          case G_WAVEFILE_TYPE_U16:
            u16 = (guint16*) in;
            for (i = 0; i < frames; i++)
              *f4++ = s16_to_double ((*u16++) ^ 0x8000);
            break;

          case G_WAVEFILE_TYPE_S16:
            s16 = (gint16*) in;
            for (i = 0; i < frames; i++)
              *f4++ = s16_to_double (*s16++);
            break;

          case G_WAVEFILE_TYPE_U8:
            u8 = (guint8*) in;
            for (i = 0; i < frames; i++)
              *f4++ = s16_to_double (((*u8++) << 8) ^ 0x8000);
            break;

          case G_WAVEFILE_TYPE_S8:
            s8 = (gint8*) in;
            for (i = 0; i < frames; i++)
              *f4++ = s16_to_double ((*s8++) << 8);
            break;
        }
      break;

    case G_WAVEFILE_TYPE_S32:
      s32 = (gint32*) out;

      switch (in_type)
        {
        case G_WAVEFILE_TYPE_F8:
          f8 = (gdouble*) in;
          for (i = 0; i < frames; i++)
            *s32++ = double_to_s32 (*f8++);
          break;

        case G_WAVEFILE_TYPE_F4:
          f4 = (gfloat*) in;
          for (i = 0; i < frames; i++)
            *s32++ = double_to_s32 (*f4++);
          break;

        case G_WAVEFILE_TYPE_U32:
          u32 = (guint32*) in;
          for (i = 0; i < frames; i++)
            *s32++ = (*u32++) ^ 0x80000000;
          break;

        case G_WAVEFILE_TYPE_S16:
          u16 = (guint16*) in;
          for (i = 0; i < frames; i++)
            *s32++ = (*u16++) << 16;
          break;

        case G_WAVEFILE_TYPE_U16:
          u16 = (guint16*) in;
          for (i = 0; i < frames; i++)
            *s32++ = ((*u16++) << 16) ^ 0x80000000;
          break;

        case G_WAVEFILE_TYPE_S8:
          s8 = (gint8*) in;
          for (i = 0; i < frames; i++)
            *s32++ = (*s8++) << 24;
          break;

        case G_WAVEFILE_TYPE_U8:
          u8 = (guint8*) in;
          for (i = 0; i < frames; i++)
            *s32++ = ((*u8++) << 24) ^ 0x8000000;
          break;

        }
        break;

    case G_WAVEFILE_TYPE_U32:
      u32 = (guint32*) out;

      switch (in_type)
        {
        case G_WAVEFILE_TYPE_S32:
          s32 = (gint32*) in;
          for (i = 0; i < frames; i++)
            *u32++ = (*s32++) ^ 0x80000000;
          break;

        default:
          g_wavefile_type_convert (n_channels, length, G_WAVEFILE_TYPE_S32, out, in_type, in);
          g_wavefile_type_convert (n_channels, length, out_type, out, G_WAVEFILE_TYPE_S32, out);
        }
      break;

    case G_WAVEFILE_TYPE_S16:
      s16 = (gint16*) out;

      switch (in_type)
        {
        case G_WAVEFILE_TYPE_F8:
          f8 = (gdouble*) in;
          for (i = 0; i < frames; i++)
            *s16++ = double_to_s16 (*f8++);
          break;

        case G_WAVEFILE_TYPE_F4:
          f4 = (gfloat*) in;
          for (i = 0; i < frames; i++)
            *s16++ = double_to_s16 (*f4++);
          break;

        case G_WAVEFILE_TYPE_S32:
          s32 = (gint32*) in;
          for (i = 0; i < frames; i++)
            *s16++ = (*s32++) >> 16;
          break;

        case G_WAVEFILE_TYPE_U32:
          u32 = (guint32*) in;
          for (i = 0; i < frames; i++)
            *s16++ = ((*u32++) >> 16) ^ 0x8000;
          break;

        case G_WAVEFILE_TYPE_U16:
          u16 = (guint16*) in;
          for (i = 0; i < frames; i++)
            *s16++ = (*u16++) ^ 0x8000;
          break;

        case G_WAVEFILE_TYPE_S8:
          s8 = (gint8*) in;
          for (i = 0; i < frames; i++)
            *s16++ = (*s8++) << 8;
          break;

        case G_WAVEFILE_TYPE_U8:
          u8 = (guint8*) in;
          for (i = 0; i < frames; i++)
            *s16++ = ((*u8++) << 8) ^ 0x8000;
          break;
        }
        break;

    case G_WAVEFILE_TYPE_U16:
      u16 = (guint16*) out;

      switch (in_type)
        {
        case G_WAVEFILE_TYPE_S16:
          s16 = (gint16*) in;
          for (i = 0; i < frames; i++)
            *u16++ = (*s16++) ^ 0x80000000;
          break;

        default:
          g_wavefile_type_convert (n_channels, length, G_WAVEFILE_TYPE_S16, out, in_type, in);
          g_wavefile_type_convert (n_channels, length, out_type, out, G_WAVEFILE_TYPE_S16, out);
        }
      break;

    case G_WAVEFILE_TYPE_S8:
      s8 = (gint8*) out;

      switch (in_type)
        {
        case G_WAVEFILE_TYPE_F8:
          f8 = (gdouble*) in;
          for (i = 0; i < frames; i++)
            *s8++ = double_to_s16 (*f8++) >> 8;
          break;

        case G_WAVEFILE_TYPE_F4:
          f4 = (gfloat*) in;
          for (i = 0; i < frames; i++)
            *s8++ = double_to_s16 (*f4++) >> 8;
          break;

        case G_WAVEFILE_TYPE_S32:
          s32 = (gint32*) in;
          for (i = 0; i < frames; i++)
            *s8++ = (*s32++) >> 24;
          break;

        case G_WAVEFILE_TYPE_U32:
          u32 = (guint32*) in;
          for (i = 0; i < frames; i++)
            *s8++ = ((*u32++) >> 24) ^ 0x80;
          break;

        case G_WAVEFILE_TYPE_S16:
          u16 = (guint16*) in;
          for (i = 0; i < frames; i++)
            *s8++ = (*u16++) >> 8;
          break;

        case G_WAVEFILE_TYPE_U16:
          u16 = (guint16*) in;
          for (i = 0; i < frames; i++)
            *s8++ = ((*u16++) >> 8) ^ 0x80;
          break;

        case G_WAVEFILE_TYPE_U8:
          u8 = (guint8*) in;
          for (i = 0; i < frames; i++)
            *s8++ = (*u8++) ^ 0x80;
          break;
        }
      break;

    case G_WAVEFILE_TYPE_U8:
      u8 = (guint8*) out;

      switch (in_type)
        {
        case G_WAVEFILE_TYPE_S8:
          s8 = (gint8*) in;
          for (i = 0; i < frames; i++)
            *u8++ = (*s8++) ^ 0x80000000;
          break;

        default:
          g_wavefile_type_convert (n_channels, length, G_WAVEFILE_TYPE_S8, out, in_type, in);
          g_wavefile_type_convert (n_channels, length, out_type, out, G_WAVEFILE_TYPE_S8, out);
          break;
        }
      break;
    }
}

void
g_wavefile_type_clear (guint32       n_channels,
                       guint32       length,
                       GWavefileType type,
                       gpointer      out)
{
  guint32 i, frames;
  gint8 *s8;
  gint16 *s16;
  gint32 *s32;
  
  frames = n_channels * length;

  switch (type)
    {
    case G_WAVEFILE_TYPE_U32:
      s32 = (gint32*) out;
      for (i = 0; i < frames; i++)
        *s32++ = 0x80000000L;
      break;

    case G_WAVEFILE_TYPE_U16:
      s16 = (gint16*) out;
      for (i = 0; i < frames; i++)
        *s16++ = 0x8000;
      break;

    case G_WAVEFILE_TYPE_U8:
      s8 = (gint8*) out;
      for (i = 0; i < frames; i++)
        *s8++ = 0x80;
      break;

    default:
      memset (out, 0, g_wavefile_type_width (type) * frames);
    }
}
