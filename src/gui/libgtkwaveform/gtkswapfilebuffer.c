/*
 * $Id: gtkswapfilebuffer.c,v 1.1 2000/12/22 10:47:42 richi Exp $
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

#include <glib.h>
#include <gtk/gtk.h>
#include "gtkswapfilebuffer.h"


static void gtk_swapfile_buffer_class_init          (GtkSwapfileBufferClass *klass);
static void gtk_swapfile_buffer_init                (GtkSwapfileBuffer      *swapfile);
static guint32 gtk_swapfile_buffer_get_rate         (GtkWaveBuffer      *wavebuffer);
static GWavefileType gtk_swapfile_buffer_get_datatype  (GtkWaveBuffer  *wavebuffer);
static guint32 gtk_swapfile_buffer_get_length       (GtkWaveBuffer      *wavebuffer);
static guint32 gtk_swapfile_buffer_get_num_channels (GtkWaveBuffer      *wavebuffer);
static void gtk_swapfile_buffer_get_samples         (GtkWaveBuffer      *wavebuffer,
                                                      guint32             start,
                                                      guint32             length,
                                                      guint32             channel_mask,
                                                      gpointer            data);



GtkType
gtk_swapfile_buffer_get_type (void)
{
	static GtkType swapfile_type = 0;

	if (!swapfile_type) {
		static const GtkTypeInfo swapfile_info =
		{
			"GtkSwapfileBuffer",
			sizeof (GtkSwapfileBuffer),
			sizeof (GtkSwapfileBufferClass),
			(GtkClassInitFunc) gtk_swapfile_buffer_class_init,
			(GtkObjectInitFunc) gtk_swapfile_buffer_init,
			/* reserved_1 */ NULL,
			/* reserved_2 */ NULL,
			(GtkClassInitFunc) NULL
		};

		swapfile_type = gtk_type_unique (GTK_TYPE_WAVE_BUFFER,
						 &swapfile_info);
	}

	return swapfile_type;
}

static void
gtk_swapfile_buffer_finalize (GtkObject *obj)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (obj);

	sw_close(swapfile->fd);
}

static void
gtk_swapfile_buffer_class_init (GtkSwapfileBufferClass *klass)
{
	GtkObjectClass *object_class;
	GtkWaveBufferClass *wavebuffer_class;

	object_class = (GtkObjectClass *) klass;
	wavebuffer_class = (GtkWaveBufferClass *) klass;

	object_class->finalize = gtk_swapfile_buffer_finalize;

	wavebuffer_class->get_rate = gtk_swapfile_buffer_get_rate;
	wavebuffer_class->get_datatype = gtk_swapfile_buffer_get_datatype;
	wavebuffer_class->get_length = gtk_swapfile_buffer_get_length;
	wavebuffer_class->get_num_channels = gtk_swapfile_buffer_get_num_channels;
	wavebuffer_class->get_samples = gtk_swapfile_buffer_get_samples;
}

static void
gtk_swapfile_buffer_init (GtkSwapfileBuffer *swapfile)
{
	/* nothing. */
}

static guint32 gtk_swapfile_buffer_get_rate (GtkWaveBuffer *wavebuffer)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (wavebuffer);

	return swapfile->rate;
}

static GWavefileType
gtk_swapfile_buffer_get_datatype (GtkWaveBuffer *wavebuffer)
{
#ifdef SAMPLE_FLOAT
	return G_WAVEFILE_TYPE_F4;
#else
	return G_WAVEFILE_TYPE_F8;
#endif
}

static guint32
gtk_swapfile_buffer_get_length (GtkWaveBuffer *wavebuffer)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (wavebuffer);

	return swapfile->stat.size / sizeof(SAMPLE);
}

static guint32
gtk_swapfile_buffer_get_num_channels (GtkWaveBuffer *wavebuffer)
{
	return 1;
}

static void
gtk_swapfile_buffer_get_samples (GtkWaveBuffer *wavebuffer,
                                  guint32        start,
                                  guint32        length,
                                  guint32        channel_mask,
                                  gpointer       data)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (wavebuffer);
	off_t pos;
	ssize_t cnt;

	if (!(channel_mask & 1))
		return;
	/* FIXME - error checks. */
	pos = sw_lseek(swapfile->fd, start*sizeof(SAMPLE), SEEK_SET);
	cnt = sw_read(swapfile->fd, data, length*sizeof(SAMPLE));
}

GtkObject *
gtk_swapfile_buffer_new (long name, int rate)
{
	GtkSwapfileBuffer *swapfile;
	swfd_t fd;
	struct sw_stat sbuf;

	if ((fd = sw_open(name, O_RDONLY, TXN_NONE)) == -1)
		return NULL;
	if (sw_fstat(fd, &sbuf) == -1) {
		sw_close(fd);
		return NULL;
	}
	swapfile = gtk_type_new (GTK_TYPE_SWAPFILE_BUFFER);
	swapfile->fd = fd;
	swapfile->stat = sbuf;
	swapfile->rate = rate;

	return GTK_OBJECT(swapfile);
}
