/*
 * $Id: gtkswapfilebuffer.c,v 1.5 2001/04/06 09:23:15 richi Exp $
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
		glsig_delete_handler(swapfile->handler[i]);
	}
	free(swapfile->swfile);
	free(swapfile->fd);
	free(swapfile->handler);
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

	return gpsm_swfile_samplerate(swapfile->swfile[0]);
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

	return gpsm_item_hsize(swapfile->swfile[0]);
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
	DERROR("You should not call this(?)");
	return 0;
}

static gint
gtk_swapfile_buffer_insert (GtkEditableWaveBuffer *editable,
                                 guint32 start,
                                 guint32 length)
{
	DERROR("You should not call this(?)");
	return 0;
}

/* Note that this is just a notification, but not operation itself. */
static gint
gtk_swapfile_buffer_delete (GtkEditableWaveBuffer *editable,
			    guint32 start,
			    guint32 length)
{
	DERROR("You should not call this(?)");
	return 0;
}


static void handle_swfile(glsig_handler_t *handler, long sig, va_list va)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(glsig_handler_private(handler));
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER(swapfile);

	switch (sig) {
	case GPSM_SIG_SWFILE_INSERT: {
		gpsm_swfile_t *item;
		long position, size;
		GRange range;

		GLSIGH_GETARGS3(va, item, position, size);

		/* Notify the wave buffer, raise gtk insert signal. */
		g_range_set(&range, position, size);
		gtk_signal_emit(GTK_OBJECT(editable), insert_data_signal,
				&range);
		gtk_editable_wave_buffer_queue_modified(editable, position, gpsm_item_hsize(item) - position);

		break;
	}
	case GPSM_SIG_SWFILE_CUT: {
		gpsm_swfile_t *item;
		long position, size;
		GRange range;

		GLSIGH_GETARGS3(va, item, position, size);

		/* Notify the wave buffer, raise gtk insert signal. */
		g_range_set(&range, position, size);
		gtk_signal_emit(GTK_OBJECT(editable), delete_data_signal,
				&range);
		gtk_editable_wave_buffer_queue_modified(editable, position, gpsm_item_hsize(item) - position);

		break;
	}
	case GPSM_SIG_SWFILE_CHANGED: {
		gpsm_swfile_t *item;
		long position, size;

		GLSIGH_GETARGS3(va, item, position, size);

		/* Notify the wave buffer. */
		gtk_editable_wave_buffer_queue_modified(editable, position, size);

		break;
	}
	case GPSM_SIG_ITEM_DESTROY: {
		DPRINTF("FIXME - kill the window.\n");
		break;
	}
	default:
		DPRINTF("Unhandled signal %li\n", sig);
	}
}
GtkObject *gtk_swapfile_buffer_new(gpsm_item_t *item)
{
	GtkSwapfileBuffer *swapfile;
	swfd_t *fd;
	gpsm_swfile_t **swfile;
	gpsm_item_t *it;
	glsig_handler_t **handler;
	int nrtracks = 0;
	int rate = -1;
	long size = -1;
	int i;

	/* First obtain information about the to be displayed channels.
	 * Ensure theyre equal sized and rated. */
	if (GPSM_ITEM_IS_GRP(item)) {
		gpsm_grp_foreach_item(item, it) {
			gpsm_item_t *it2;
			if (!GPSM_ITEM_IS_SWFILE(it))
				return NULL;
			if (rate == -1)
				rate = gpsm_swfile_samplerate(it);
			if (size == -1)
				size = gpsm_item_hsize(it);
			if (gpsm_swfile_samplerate(it) != rate
			    || gpsm_item_hsize(it) != size)
				return NULL;
			gpsm_grp_foreach_item(item, it2)
				if (GPSM_ITEM_IS_SWFILE(it2)
				    && it != it2
				    && gpsm_swfile_filename(it) == gpsm_swfile_filename(it2))
					return NULL;
			nrtracks++;
		}
	} else {
		nrtracks = 1;
		rate = gpsm_swfile_samplerate(item);
		size = gpsm_item_hsize(item);
	}

	fd = calloc(nrtracks, sizeof(swfd_t));
	swfile = calloc(nrtracks, sizeof(gpsm_swfile_t *));
	handler = calloc(nrtracks, sizeof(glsig_handler_t *));

	
	if (GPSM_ITEM_IS_GRP(item)) {
		i = 0;
		gpsm_grp_foreach_item(item, it) {
			swfile[i] = (gpsm_swfile_t *)it;
			if ((fd[i] = sw_open(gpsm_swfile_filename(it), O_RDONLY,
					     TXN_NONE)) == -1)
				goto err;
			i++;
		}
	} else {
		swfile[0] = (gpsm_swfile_t *)item;
		if ((fd[0] = sw_open(gpsm_swfile_filename(item), O_RDONLY,
				     TXN_NONE)) == -1)
			goto err;
	}

	swapfile = gtk_type_new (GTK_TYPE_SWAPFILE_BUFFER);
	swapfile->nrtracks = nrtracks;
	swapfile->swfile = swfile;
	swapfile->handler = handler;
	swapfile->fd = fd;
	for (i=0; i<nrtracks; i++) {
		handler[i] = glsig_add_handler(gpsm_item_emitter(swfile[i]),
					       GPSM_SIG_SWFILE_INSERT|GPSM_SIG_SWFILE_CUT|GPSM_SIG_SWFILE_CHANGED|GPSM_SIG_ITEM_DESTROY,
					       handle_swfile, swapfile);
	}

	return GTK_OBJECT(swapfile);
 err:
	for (i=0; i<nrtracks; i++)
		if (fd[i] != 0)
			sw_close(fd[i]);
	free(fd);
	free(swfile);
	free(handler);
	return NULL;
}

int gtk_swapfile_buffer_get_swfiles(GtkSwapfileBuffer *buffer,
				    gpsm_swfile_t ***files)
{
	*files = buffer->swfile;
	return buffer->nrtracks;
}

