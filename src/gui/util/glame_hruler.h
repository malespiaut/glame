/*
 * From GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * Copyright (C) 2001 Richard Guenther
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

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#ifndef __GLAME_HRULER_H__
#define __GLAME_HRULER_H__


#include <gdk/gdk.h>
#include "glame_ruler.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


typedef struct _GlameHRuler       GlameHRuler;
typedef struct _GlameHRulerClass  GlameHRulerClass;

#define GLAME_TYPE_HRULER	    (glame_hruler_get_type ())
#define GLAME_HRULER(obj)            (GTK_CHECK_CAST ((obj), GLAME_TYPE_HRULER, GlameHRuler))
#define GLAME_HRULER_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GLAME_TYPE_HRULER, GlameHRulerClass))
#define GLAME_IS_HRULER(obj)         (GTK_CHECK_TYPE ((obj), GLAME_TYPE_HRULER))
#define GLAME_IS_HRULER_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GLAME_TYPE_HRULER))
#define GLAME_HRULER_GET_CLASS(obj)  ((GlameHRulerClass *)(((GtkObject *)(obj))->klass))


struct _GlameHRuler
{
  GlameRuler ruler; 
};

struct _GlameHRulerClass
{
  GlameRulerClass parent_class;
};


GtkType    glame_hruler_get_type (void);
GtkWidget* glame_hruler_new      (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GLAME_HRULER_H__ */
