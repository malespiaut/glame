/* grange.h: Simple range type
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

#ifndef __G_RANGE_H__
#define __G_RANGE_H__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _GRange GRange;

struct _GRange
{
  gint32 left;
  gint32 right;
};

void g_range_set (GRange *range, gint32 pos, gint32 length);
void g_range_add (GRange *range, gint32 pos, gint32 length);

#ifdef __cplusplus
}
#endif

#endif /* __G_RANGE_H__ */
