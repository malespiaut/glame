/*
 * $Id: gtkswapfilebuffer.h,v 1.9 2001/08/07 09:14:17 richi Exp $
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
#include "gpsm.h"
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

	gpsm_grp_t *item;
	long size;

	int nrtracks;
	gpsm_swfile_t **swfile;
	glsig_handler_t **handler;
	swfd_t *fd;
};

struct _GtkSwapfileBufferClass
{
  GtkEditableWaveBufferClass object_class;
};


GtkType      gtk_swapfile_buffer_get_type (void);
GtkObject   *gtk_swapfile_buffer_new(gpsm_grp_t *item);

gpsm_grp_t  *gtk_swapfile_buffer_get_item(GtkSwapfileBuffer *buffer);
int          gtk_swapfile_buffer_get_swfiles(GtkSwapfileBuffer *buffer,
					     gpsm_swfile_t ***files);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_SWAPFILE_BUFFER_H__ */
