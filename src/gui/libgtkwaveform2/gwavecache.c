/*
 * gwavecache.c
 * $Id: gwavecache.c,v 1.2 2004/10/23 13:09:26 richi Exp $
 *
 * Copyright (C) 2001, 2002 Richard Guenther
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

#include "gwavecache.h"


static void gwavecache_destroy(GtkObject *gwavecache)
{
	GWaveClass* parent_class;
	parent_class = gtk_type_class(g_wave_get_type());
	GTK_OBJECT_CLASS(parent_class)->destroy(gwavecache);
}

static void gwavecache_class_init(GWaveCacheClass *class)
{
	GtkObjectClass *object_class;

	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = gwavecache_destroy;

	/* to be implemented by a derived class */
	class->set_datatype = NULL;
	class->get_samples_sample = NULL;
}

static void gwavecache_init(GWaveCache *gwavecache)
{
	/* nothing */
}

GtkType gwavecache_get_type(void)
{
	static GtkType gwavecache_type = 0;
	
	if (!gwavecache_type){
		GtkTypeInfo gwavecache_info = {
			"GWaveCache",
			sizeof(GWaveCache),
			sizeof(GWaveCacheClass),
			(GtkClassInitFunc)gwavecache_class_init,
			(GtkObjectInitFunc)gwavecache_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		gwavecache_type = gtk_type_unique(
			g_wave_get_type(), &gwavecache_info);
		gtk_type_set_chunk_alloc(gwavecache_type, 8);
	}

	return gwavecache_type;
}


/*
 * Virtual functions.
 */

void g_wave_cache_set_datatype(GWaveCache *wavecache, GWaveType type)
{
	return wavecache->set_datatype(wavecache, type);
}

void g_wave_cache_get_samples_sample(GWaveCache *wavecache,
                                     guint32 start, guint32 length,
                                     guint32 nr_samples,
                                     guint32 channel_mask,
                                     gpointer data)
{
	return wavecache->get_samples_sample(wavecache, start, length,
					     nr_samples, channel_mask, data);
}
