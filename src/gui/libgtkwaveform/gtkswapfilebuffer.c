/*
 * $Id: gtkswapfilebuffer.c,v 1.4 2001/03/16 09:55:42 richi Exp $
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
#include "glame_types.h"


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
static gint gtk_swapfile_buffer_set_samples         (GtkEditableWaveBuffer 
*editable,
                                                          guint32 start,
                                                          guint32 length,
                                                          guint32 channel_mask,
                                                          gpointer data);
static gint gtk_swapfile_buffer_insert              (GtkEditableWaveBuffer 
*editable,
                                                          guint32 start,
                                                          guint32 length);
static gint gtk_swapfile_buffer_delete              (GtkEditableWaveBuffer 
*editable,
                                                      guint32 start,
                                                      guint32 length);



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

		swapfile_type = gtk_type_unique (GTK_TYPE_EDITABLE_WAVE_BUFFER,
						 &swapfile_info);
	}

	return swapfile_type;
}

static void
gtk_swapfile_buffer_finalize (GtkObject *obj)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (obj);
	int i;

	for (i=0; i<swapfile->nrtracks; i++) {
		sw_close(swapfile->fd[i]);
	}
	free(swapfile->fname);
	free(swapfile->fd);
	free(swapfile->stat);
}

static void
gtk_swapfile_buffer_class_init (GtkSwapfileBufferClass *klass)
{
	GtkObjectClass *object_class;
	GtkWaveBufferClass *wavebuffer_class;
	GtkEditableWaveBufferClass *editable_class;

	object_class = (GtkObjectClass *) klass;
	wavebuffer_class = (GtkWaveBufferClass *) klass;
	editable_class = (GtkEditableWaveBufferClass *) klass;

	object_class->finalize = gtk_swapfile_buffer_finalize;

	wavebuffer_class->get_rate = gtk_swapfile_buffer_get_rate;
	wavebuffer_class->get_datatype = gtk_swapfile_buffer_get_datatype;
	wavebuffer_class->get_length = gtk_swapfile_buffer_get_length;
	wavebuffer_class->get_num_channels = gtk_swapfile_buffer_get_num_channels;
	wavebuffer_class->get_samples = gtk_swapfile_buffer_get_samples;
	editable_class->set_samples = gtk_swapfile_buffer_set_samples;
	editable_class->insert = gtk_swapfile_buffer_insert;
	editable_class->delete = gtk_swapfile_buffer_delete;
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
	return G_WAVEFILE_TYPE_F4NI;
#else
	return G_WAVEFILE_TYPE_F8NI;
#endif
}

static guint32
gtk_swapfile_buffer_get_length (GtkWaveBuffer *wavebuffer)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (wavebuffer);

	return swapfile->stat[0].size / SAMPLE_SIZE;
}

static guint32
gtk_swapfile_buffer_get_num_channels (GtkWaveBuffer *wavebuffer)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (wavebuffer);

	return swapfile->nrtracks;
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
	int i;

	for (i=0; i<swapfile->nrtracks; i++) {
		if (!(channel_mask & (1<<i))) {
			data += length*SAMPLE_SIZE;
			continue;
		}
		/* FIXME - error checks. */
		pos = sw_lseek(swapfile->fd[i], start*SAMPLE_SIZE, SEEK_SET);
		cnt = sw_read(swapfile->fd[i], data, length*SAMPLE_SIZE);
		data += length*SAMPLE_SIZE;
	}
}

static gint
gtk_swapfile_buffer_set_samples (GtkEditableWaveBuffer *editable,
				 guint32 start,
				 guint32 length,
				 guint32 channel_mask,
				 gpointer data)
{
  /* Just notification. */
  gtk_editable_wave_buffer_queue_modified (editable, start, length);

  return 0;
}

static gint
gtk_swapfile_buffer_insert (GtkEditableWaveBuffer *editable,
                                 guint32 start,
                                 guint32 length)
{
  GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (editable);
  GRange range;
  int i;

  /* Re-stat the swapfile. */
  for (i=0; i<swapfile->nrtracks; i++)
	  sw_fstat(swapfile->fd[i], &swapfile->stat[i]);

  /* Just notification. */
  g_range_set (&range, start, length);
  gtk_signal_emit (GTK_OBJECT (editable), insert_data_signal, &range);
  gtk_editable_wave_buffer_queue_modified (editable, start,
					   swapfile->stat[0].size/SAMPLE_SIZE - start);

  return 0;
}

/* Note that this is just a notification, but not operation itself. */
static gint
gtk_swapfile_buffer_delete (GtkEditableWaveBuffer *editable,
			    guint32 start,
			    guint32 length)
{
  GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (editable);
  GRange range;
  int i;

  /* Re-stat the swapfile. */
  for (i=0; i<swapfile->nrtracks; i++)
	  sw_fstat(swapfile->fd[i], &swapfile->stat[i]);

  /* Just pass on the change. */
  g_range_set (&range, start, length);
  gtk_signal_emit (GTK_OBJECT (editable), delete_data_signal, &range);
  gtk_editable_wave_buffer_queue_modified (editable, start,
					   swapfile->stat[0].size/SAMPLE_SIZE - start);

  return 0;
}

GtkObject *gtk_swapfile_buffer_new_a(int nrtracks, int rate, long *names)
{
	GtkSwapfileBuffer *swapfile;
	swfd_t *fd;
	struct sw_stat *sbuf;
	long *name;
	int i, j;

	/* Well, 16 is already more than libgtkwaveform supports (2)... */
	if (nrtracks <=0 || nrtracks > 16)
		return NULL;

	/* No duplicate files! */
	for (i=0; i<nrtracks; i++)
		for (j=0; j<nrtracks; j++)
			if (i!=j && names[i] == names[j])
				return NULL;

	fd = calloc(nrtracks, sizeof(swfd_t));
	sbuf = calloc(nrtracks, sizeof(struct sw_stat));
	name = calloc(nrtracks, sizeof(long));

	for (i=0; i<nrtracks; i++) {
		name[i] = names[i];
		if ((fd[i] = sw_open(name[i], O_RDONLY, TXN_NONE)) == -1
		    || sw_fstat(fd[i], &sbuf[i]) == -1
		    || sbuf[i].size != sbuf[0].size)
			goto err;
	}
	swapfile = gtk_type_new (GTK_TYPE_SWAPFILE_BUFFER);
	swapfile->nrtracks = nrtracks;
	swapfile->rate = rate;
	swapfile->fname = name;
	swapfile->fd = fd;
	swapfile->stat = sbuf;

	return GTK_OBJECT(swapfile);
 err:
	for (i=0; i<nrtracks; i++)
		if (fd[i] != 0)
			sw_close(fd[i]);
	free(fd);
	free(name);
	free(sbuf);
	return NULL;
}
GtkObject *gtk_swapfile_buffer_new_va(int nrtracks, int rate, va_list va)
{
	long names[16];
	int i;

	if (nrtracks <=0 || nrtracks > 16)
		return NULL;

	for (i=0; i<nrtracks; i++)
		names[i] = va_arg(va, long);

	return gtk_swapfile_buffer_new_a(nrtracks, rate, names);
}
GtkObject *gtk_swapfile_buffer_new(int nrtracks, int rate, ...)
{
	long names[16];
	va_list va;
	int i;

	if (nrtracks <=0 || nrtracks > 16)
		return NULL;

	va_start(va, rate);
	for (i=0; i<nrtracks; i++)
		names[i] = va_arg(va, long);
	va_end(va);

	return gtk_swapfile_buffer_new_a(nrtracks, rate, names);
}

int gtk_swapfile_buffer_get_filenames(GtkSwapfileBuffer *buffer, long **names)
{
        *names = buffer->fname;
	return buffer->nrtracks;
}
