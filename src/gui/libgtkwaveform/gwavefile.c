/*
 * gwavefile.c: wave utilities
 *
 * Copyright (c) 2000, 2001 David A. Bartold, Richard Guenther
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
#include <string.h>
#include "gwavefile.h"


guint32
g_wavefile_type_width (GWavefileType dtype)
{
  switch (dtype)
    {
    case G_WAVEFILE_TYPE_F8: case G_WAVEFILE_TYPE_F8NI:
      return 8;

    case G_WAVEFILE_TYPE_S32: case G_WAVEFILE_TYPE_U32: case G_WAVEFILE_TYPE_F4: case G_WAVEFILE_TYPE_F4NI:
      return 4;

    case G_WAVEFILE_TYPE_S16: case G_WAVEFILE_TYPE_U16:
      return 2;

    case G_WAVEFILE_TYPE_S8: case G_WAVEFILE_TYPE_U8:
      return 1;

    default:
      break;
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

  guint32 i, j, frames;

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

	  default:
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

	  default:
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

        default:
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

	case G_WAVEFILE_TYPE_F8NI: {
	  gint16 *s16temp = s16;
	  f8 = (gdouble*) in;
	  for (j=0; j<n_channels; j++) {
	    s16temp = s16;
	    for (i=0; i<length; i++) {
	      *s16temp = double_to_s16(*f8++);
	      s16temp += n_channels;
	    }
	    s16++;
	  }
	  break;
	}

        case G_WAVEFILE_TYPE_F4:
          f4 = (gfloat*) in;
          for (i = 0; i < frames; i++)
            *s16++ = double_to_s16 (*f4++);
          break;

	case G_WAVEFILE_TYPE_F4NI: {
	  gint16 *s16temp = s16;
	  f4 = (gfloat*) in;
	  for (j=0; j<n_channels; j++) {
	    s16temp = s16;
	    for (i=0; i<length; i++) {
	      *s16temp = double_to_s16(*f4++);
	      s16temp += n_channels;
	    }
	    s16++;
	  }
	  break;
	}

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

	default:
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

	default:
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

    default:
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
