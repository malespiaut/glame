/*
 * $Id: gtkswapfilebuffer.h,v 1.3 2001/03/12 09:41:51 richi Exp $
 *
 * Copyright (c) 2000 Richard Guenther
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

#ifndef __GTK_SWAPFILE_BUFFER_H__
#define __GTK_SWAPFILE_BUFFER_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gdk/gdk.h>
#include "swapfile.h"
#include "gtkwavebuffer.h"
#include "gtkeditablewavebuffer.h"
#include "gwavefile.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_TYPE_SWAPFILE_BUFFER            (gtk_swapfile_buffer_get_type ())
#define GTK_SWAPFILE_BUFFER(obj)            (GTK_CHECK_CAST ((obj), GTK_TYPE_SWAPFILE_BUFFER, GtkSwapfileBuffer))
#define GTK_SWAPFILE_BUFFER_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_SWAPFILE_BUFFER, GtkSwapfileBufferClass))
#define GTK_IS_SWAPFILE_BUFFER(obj)         (GTK_CHECK_TYPE ((obj), GTK_TYPE_SWAPFILE_BUFFER))
#define GTK_IS_SWAPFILE_BUFFER_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SWAPFILE_BUFFER))


typedef struct _GtkSwapfileBuffer      GtkSwapfileBuffer;
typedef struct _GtkSwapfileBufferClass GtkSwapfileBufferClass;


struct _GtkSwapfileBuffer
{
	GtkEditableWaveBuffer   object;

	int rate;
	int nrtracks;
        long *fname;
	swfd_t *fd;
	struct sw_stat *stat;
};

struct _GtkSwapfileBufferClass
{
  GtkEditableWaveBufferClass object_class;
};


#define GTK_SWAPFILE_BUFFER_MAX_TRACKS 16

GtkType    gtk_swapfile_buffer_get_type (void);
GtkObject *gtk_swapfile_buffer_new(int nrtracks, int rate, ...);
GtkObject *gtk_swapfile_buffer_new_va(int nrtracks, int rate, va_list va);
GtkObject *gtk_swapfile_buffer_new_a(int nrtracks, int rate, long *names);

int        gtk_swapfile_buffer_get_filenames (GtkSwapfileBuffer *buffer,
					      long **names);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_SWAPFILE_BUFFER_H__ */
