/*
 * gwave.c
 * $Id: gwave.c,v 1.1 2002/03/25 21:45:18 richi Exp $
 *
 * Copyright (C) 2001 Richard Guenther
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gwave.h"


static void gwave_destroy(GtkObject *gwave)
{
	GtkObjectClass* parent_class;
	parent_class = gtk_type_class(gtk_object_get_type());
	GTK_OBJECT_CLASS(parent_class)->destroy(gwave);
}

static void gwave_class_init(GWaveClass *class)
{
	GtkObjectClass *object_class;
	static guint invalidated_signal;
	static guint modified_signal;
	static guint insert_data_signal;
	static guint delete_data_signal;

	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = gwave_destroy;

	invalidated_signal =
		gtk_signal_new ("invalidated", GTK_RUN_FIRST,
				g_wave_get_type(),
				GTK_SIGNAL_OFFSET(GWaveClass, invalidated),
				gtk_marshal_NONE__NONE,
				GTK_TYPE_NONE,
				0);
	gtk_object_class_add_signals(object_class, &invalidated_signal, 1);

	modified_signal =
		gtk_signal_new ("modified", GTK_RUN_FIRST,
				g_wave_get_type(),
				GTK_SIGNAL_OFFSET(GWaveClass, modified),
				gtk_marshal_NONE__POINTER,
				GTK_TYPE_NONE,
				1, GTK_TYPE_POINTER);
	gtk_object_class_add_signals(object_class, &modified_signal, 1);

	insert_data_signal =
		gtk_signal_new ("insert_data", GTK_RUN_FIRST,
				g_wave_get_type(),
				GTK_SIGNAL_OFFSET(GWaveClass, insert_data),
				gtk_marshal_NONE__POINTER,
				GTK_TYPE_NONE,
				1, GTK_TYPE_POINTER);
	gtk_object_class_add_signals(object_class, &insert_data_signal, 1);

	delete_data_signal =
		gtk_signal_new ("delete_data", GTK_RUN_FIRST,
				g_wave_get_type(),
				GTK_SIGNAL_OFFSET(GWaveClass, delete_data),
				gtk_marshal_NONE__POINTER,
				GTK_TYPE_NONE,
				1, GTK_TYPE_POINTER);
	gtk_object_class_add_signals(object_class, &delete_data_signal, 1);

	/* signals */
	class->invalidated = NULL;
	class->modified = NULL;
	class->insert_data = NULL;
	class->delete_data = NULL;

	/* to be implemented by a derived class */
	class->get_rate = NULL;
	class->get_datatype = NULL;
	class->get_length = NULL;
	class->get_num_channels = NULL;
	class->get_samples = NULL;
}

static void gwave_init(GWave *gwave)
{
	/* nothing */
}

GtkType gwave_get_type(void)
{
	static GtkType gwave_type = 0;
	
	if (!gwave_type){
		GtkTypeInfo gwave_info = {
			"GWave",
			sizeof(GWave),
			sizeof(GWaveClass),
			(GtkClassInitFunc)gwave_class_init,
			(GtkObjectInitFunc)gwave_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		gwave_type = gtk_type_unique(
			gtk_object_get_type(), &gwave_info);
		gtk_type_set_chunk_alloc(gwave_type, 8);
	}

	return gwave_type;
}


/*
 * Virtual functions.
 */

guint32 g_wave_get_rate(GWave *wave)
{
	return G_WAVE_CLASS(G_OBJECT_GET_CLASS(wave))->get_rate(wave);
}

guint32 g_wave_get_num_channels(GWave *wave)
{
	return G_WAVE_CLASS(G_OBJECT_GET_CLASS(wave))->get_num_channels(wave);
}

guint32 g_wave_get_length(GWave *wave)
{
	return G_WAVE_CLASS(G_OBJECT_GET_CLASS(wave))->get_length(wave);
}

guint32 g_wave_get_datatype(GWave *wave)
{
	return G_WAVE_CLASS(G_OBJECT_GET_CLASS(wave))->get_datatype(wave);
}

void g_wave_get_samples(GWave *wave, guint32 start, guint32 length,
	 		guint32 channel_mask, gpointer data)
{
	G_WAVE_CLASS(G_OBJECT_GET_CLASS(wave))->get_samples(wave, start, length, channel_mask, data);
}
