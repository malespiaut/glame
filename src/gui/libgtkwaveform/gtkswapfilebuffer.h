/*
 * $Id: gtkswapfilebuffer.h,v 1.11 2003/05/25 13:37:27 richi Exp $
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

/* Returns the original item the swapfile buffer was created with. */
gpsm_grp_t  *gtk_swapfile_buffer_get_item(GtkSwapfileBuffer *buffer);

/* Returns the active tracks in a gpsm group with properties like the
 * one returned by gpsm_collect_swfiles(). Vertical positions are
 * corresponding to the track number.
 * You need to free the returned group by yourself. */
gpsm_grp_t  *gtk_swapfile_buffer_get_active(GtkSwapfileBuffer *buffer,
					    guint32            sel_mask);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_SWAPFILE_BUFFER_H__ */
