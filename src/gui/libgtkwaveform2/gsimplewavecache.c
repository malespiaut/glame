/*
 * gsimplewavecache.c
 * $Id: gsimplewavecache.c,v 1.1 2002/03/25 21:45:18 richi Exp $
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

#include "gsimplewavecache.h"


static void gsimplewavecache_destroy(GtkObject *gsimplewavecache)
{
	GnomeAppClass* parent_class;
	parent_class = gtk_type_class(g_wave_cache_get_type());
	GTK_OBJECT_CLASS(parent_class)->destroy(gsimplewavecache);
}

static void gsimplewavecache_class_init(GSimpleWaveCacheClass *class)
{
	GtkObjectClass *object_class;

	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = gsimplewavecache_destroy;
}

static void gsimplewavecache_init(GSimpleWaveCache *gsimplewavecache)
{
	gsimplewavecache->wave = NULL;
	gsimplewavecache->type = G_WAVE_TYPE_NULL;
}

GtkType gsimplewavecache_get_type(void)
{
	static GtkType gsimplewavecache_type = 0;
	
	if (!gsimplewavecache_type){
		GtkTypeInfo gsimplewavecache_info = {
			"GSimpleWaveCache",
			sizeof(GSimpleWaveCache),
			sizeof(GSimpleWaveCacheClass),
			(GtkClassInitFunc)gsimplewavecache_class_init,
			(GtkObjectInitFunc)gsimplewavecache_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		gsimplewavecache_type = gtk_type_unique(
			g_wave_cache_get_type(), &gsimplewavecache_info);
		gtk_type_set_chunk_alloc(gsimplewavecache_type, 8);
	}

	return gsimplewavecache_type;
}


/*
 * API and virtual function implementatios.
 */

GSimpleWaveCache *g_simple_wave_cache_new(GtkEditableWaveBuffer *wave)
{
	GSimpleWaveCache *wave;
	GWavefileType type;

	if (!wave)
		return NULL;

	if (!(wavecache = G_SIMPLE_WAVE_CACHE(
		gtk_type_new(g_simple_wave_cache_get_type()))))
		return NULL;
	wavecache->wave = wave;
	type = gtk_wavebuffer_get_type(GTK_WAVE_BUFFER(wave));
	switch (type) {
	G_WAVEFILE_TYPE_S16:
		wavecache->type = G_WAVE_TYPE_S16_I;
		break;
	G_WAVEFILE_TYPE_F4:
		wavecache->type = G_WAVE_TYPE_FLOAT_I;
		break;
	G_WAVEFILE_TYPE_F4NI:
		wavecache->type = G_WAVE_TYPE_FLOAT_NI;
		break;
	default:
		wavecache->type = G_WAVE_TYPE_NULL;
		break;
	}

	return wavecache;
}



static void get_rate(GWave *wave)
{
	GSimpleWaveCache *swave = G_SIMPLE_WAVE_CACHE(wave);
	return gtk_wavebuffer_get_rate(swave->wave);
}

static void get_datatype(GWave *wave)
{
	return G_SIMPLE_WAVE_CACHE(wave)->type;
}

static void get_length(GWave *wave)
{
	GSimpleWaveCache *swave = G_SIMPLE_WAVE_CACHE(wave);
	return gtk_wavebuffer_get_length(swave->wave);
}

static void get_num_channels(GWave *wave)
{
	GSimpleWaveCache *swave = G_SIMPLE_WAVE_CACHE(wave);
	return gtk_wavebuffer_get_num_channels(swave->wave);
}

static void get_samples(GWave *wave,
			guint32 start, guint32 length,
			guint32 channel_mask, gpointer data)
{
	GSimpleWaveCache *swave = G_SIMPLE_WAVE_CACHE(wave);

	/* same type as wave object? wrap to original method */
	if (swave->wave->get_datatype(swave->wave) == swave->type)
		return gtk_wavebuffer_get_samples(
			swave->wave, start, length, channel_mask, data);

	/* FIXME - raw data, but type converted */
}

static void set_datatype(GWaveCache *wavecache, GWaveType type)
{
	G_SIMPLE_WAVE_CACHE(wavecache)->type = type;
}

static void get_samples_sample(GWaveCache *wavecache,
			       guint32 start, guint32 length,
			       guint32 nr_samples, guint32 channel_mask,
			       gpointer data)
{
	/* FIXME - get type converted data and resample it */
}
