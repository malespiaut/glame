/*
 * $Id: gtkswapfilebuffer.c,v 1.23 2005/01/16 15:48:00 richi Exp $
 *
 * Copyright (c) 2000, 2001, 2002, 2004 Richard Guenther
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

#include <sys/param.h>
#include <string.h>
#include <glib.h>
#include <gtk/gtk.h>
#include "gtkswapfilebuffer.h"
#include "glame_types.h"
#include "glsimd.h"


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
gtk_swapfile_buffer_finalize (GObject *obj)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (obj);
	int i;

	for (i=0; i<swapfile->nrtracks; i++) {
		sw_close(swapfile->fd[i]);
		if (swapfile->handler[i])
			glsig_delete_handler(swapfile->handler[i]);
	}
	if (swapfile->handler[swapfile->nrtracks])
		glsig_delete_handler(swapfile->handler[swapfile->nrtracks]);
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
	GObjectClass *gobject_class;
	  
	object_class = (GtkObjectClass *) klass;
	wavebuffer_class = (GtkWaveBufferClass *) klass;
	editable_class = (GtkEditableWaveBufferClass *) klass;

	gobject_class = G_OBJECT_CLASS (klass);
	gobject_class->finalize = gtk_swapfile_buffer_finalize;

	wavebuffer_class->get_rate = gtk_swapfile_buffer_get_rate;
	wavebuffer_class->get_datatype = gtk_swapfile_buffer_get_datatype;
	wavebuffer_class->get_length = gtk_swapfile_buffer_get_length;
	wavebuffer_class->get_num_channels = gtk_swapfile_buffer_get_num_channels;
	wavebuffer_class->get_samples = gtk_swapfile_buffer_get_samples;
	editable_class->set_samples = gtk_swapfile_buffer_set_samples;
	editable_class->insert = gtk_swapfile_buffer_insert;
	editable_class->del = gtk_swapfile_buffer_delete;
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
	/* Use glame native format to avoid another copy. */
	return G_WAVEFILE_TYPE_F4NI;
}

static guint32
gtk_swapfile_buffer_get_length (GtkWaveBuffer *wavebuffer)
{
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER (wavebuffer);

	return gpsm_item_hsize(swapfile->item);
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
	ssize_t cnt, lcnt;
	int i;

	for (i=0; i<swapfile->nrtracks; i++) {
		if (!(channel_mask & (1<<i)))
			goto next;

		/* Running position inside the actual swapfile. */
		if (GPSM_ITEM_IS_GRP(swapfile->item))
			pos = start - gpsm_item_hposition(swapfile->swfile[i]);
		else
			pos = start;
		cnt = length;

		/* Pre-zeroing */
		if (pos < 0) {
			lcnt = MIN(-pos, cnt);
			memset(data, 0, lcnt*SAMPLE_SIZE);
			pos += lcnt;
			if ((cnt -= lcnt) == 0)
				goto next;
		}

		/* The data - FIXME - error checks(?) */
		if (pos < gpsm_item_hsize(swapfile->swfile[i])) {
			lcnt = MIN(gpsm_item_hsize(swapfile->swfile[i])-pos, cnt);
			sw_lseek(swapfile->fd[i], pos*SAMPLE_SIZE, SEEK_SET);
			sw_read(swapfile->fd[i], data+(length-cnt)*SAMPLE_SIZE, lcnt*SAMPLE_SIZE);
			pos += lcnt;
			if ((cnt -= lcnt) == 0)
				goto next;
		}

		/* Post-zeroing */
		if (cnt > 0)
			memset(data+(length-cnt)*SAMPLE_SIZE, 0, cnt*SAMPLE_SIZE);

	next:
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

		GLSIGH_GETARGS3(va, item, position, size);
		if (GPSM_ITEM_IS_GRP(swapfile->item))
			position += gpsm_item_hposition(item);

		/* Notify the wave buffer. */
		gtk_editable_wave_buffer_queue_modified(editable, position, gpsm_item_hsize(item) - position);

		break;
	}
	case GPSM_SIG_SWFILE_CUT: {
		gpsm_swfile_t *item;
		long position, size;

		GLSIGH_GETARGS3(va, item, position, size);
		if (GPSM_ITEM_IS_GRP(swapfile->item))
			position += gpsm_item_hposition(item);

		/* Notify the wave buffer. */
		gtk_editable_wave_buffer_queue_modified(editable, position, gpsm_item_hsize(item) - position);

		break;
	}
	case GPSM_SIG_SWFILE_CHANGED: {
		/* We get this signal raised from the childs. */
		gpsm_swfile_t *item;
		long position, size;

		GLSIGH_GETARGS3(va, item, position, size);
		if (GPSM_ITEM_IS_GRP(swapfile->item))
			position += gpsm_item_hposition(item);

		/* Notify the wave buffer. */
		gtk_editable_wave_buffer_queue_modified(editable, position, size);

		break;
	}
	case GPSM_SIG_ITEM_CHANGED: {
		/* We get this signal raised from the "root" item only. */
		gpsm_item_t *item;
		GRange range;

		GLSIGH_GETARGS1(va, item);

		/* Did we have a size change? Simulate delete/insert
		 * events (we dont know what happened exactly). */
		if (swapfile->size < gpsm_item_hsize(item)) {
			g_range_set(&range, swapfile->size,
				    gpsm_item_hsize(item) - swapfile->size);
			gtk_signal_emit(GTK_OBJECT(editable), insert_data_signal,
					&range);
		} else if (swapfile->size > gpsm_item_hsize(item)) {
			g_range_set(&range, gpsm_item_hsize(item),
				    swapfile->size - gpsm_item_hsize(item));
			gtk_signal_emit(GTK_OBJECT(editable), delete_data_signal,
					&range);
		}
		swapfile->size = gpsm_item_hsize(item);

		/* Invalidate the whole cache. */
		gtk_editable_wave_buffer_queue_modified(editable, 0, swapfile->size);

		break;
	}
	case GPSM_SIG_ITEM_DESTROY: {
		int i;
		for (i=0; i<=swapfile->nrtracks; i++)
			if (swapfile->handler[i] == handler)
				swapfile->handler[i] = NULL;
		break;
	}
	default:
		DPRINTF("Unhandled signal %li\n", sig);
	}
}
GtkObject *gtk_swapfile_buffer_new(gpsm_grp_t *item)
{
	GtkSwapfileBuffer *swapfile;
	swfd_t *fd;
	gpsm_swfile_t **swfile;
	gpsm_item_t *it;
	glsig_handler_t **handler;
	int nrtracks = 0;
	int rate = -1;
	int i;

	/* Sanity first. */
	if (!item || !GPSM_ITEM_IS_GRP(item))
		return NULL;

	/* First obtain information about the to be displayed channels.
	 * Ensure theyre equal sized and rated. */
	gpsm_grp_foreach_item(item, it) {
		gpsm_item_t *it2;
		if (rate == -1)
			rate = gpsm_swfile_samplerate(it);
		if (gpsm_swfile_samplerate(it) != rate)
			return NULL;
		gpsm_grp_foreach_item(item, it2)
			if (it != it2
			    && (gpsm_swfile_filename(it) == gpsm_swfile_filename(it2)
				|| gpsm_item_vposition(it) == gpsm_item_vposition(it2)))
				return NULL;
		nrtracks++;
	}
	if (nrtracks == 0)
		return NULL;

	fd = (swfd_t *)calloc(nrtracks, sizeof(swfd_t));
	swfile = (gpsm_swfile_t **)calloc(nrtracks, sizeof(gpsm_swfile_t *));
	handler = (glsig_handler_t **)calloc(nrtracks+1, sizeof(glsig_handler_t *));

	i = 0;
	gpsm_grp_foreach_item(item, it) {
		swfile[i] = (gpsm_swfile_t *)it;
		if ((fd[i] = sw_open(gpsm_swfile_filename(it),
				     O_RDONLY)) == -1)
			goto err;
		i++;
	}

	swapfile = (GtkSwapfileBuffer *)gtk_type_new (GTK_TYPE_SWAPFILE_BUFFER);
	swapfile->item = item;
	swapfile->size = gpsm_item_hsize(item);
	swapfile->nrtracks = nrtracks;
	swapfile->swfile = swfile;
	swapfile->handler = handler;
	swapfile->fd = fd;
	for (i=0; i<nrtracks; i++) {
		handler[i] = glsig_add_handler(gpsm_item_emitter(swfile[i]),
					       GPSM_SIG_SWFILE_INSERT|GPSM_SIG_SWFILE_CUT|GPSM_SIG_SWFILE_CHANGED|GPSM_SIG_ITEM_DESTROY,
					       handle_swfile, swapfile);
	}
	handler[nrtracks] = glsig_add_handler(gpsm_item_emitter(item),
					      GPSM_SIG_ITEM_CHANGED|GPSM_SIG_ITEM_DESTROY,
					      handle_swfile, swapfile);

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

gpsm_grp_t *gtk_swapfile_buffer_get_item(GtkSwapfileBuffer *buffer)
{
	return buffer->item;
}

gpsm_grp_t  *gtk_swapfile_buffer_get_active(GtkSwapfileBuffer *buffer,
					    guint32 sel_mask)
{
	gpsm_grp_t *grp;
	int i;

	grp = gpsm_newgrp("(unnamed)");
	for (i=0; i<buffer->nrtracks; ++i) {
		if (!(sel_mask & 1U<<i))
			continue;
		gpsm_item_place(grp, (gpsm_item_t *)gpsm_swfile_link(buffer->swfile[i]),
				0, i);
	}

	return grp;
}

