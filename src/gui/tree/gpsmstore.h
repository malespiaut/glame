/*
 * gpsmstore.h
 *
 * Copyright (C) 2004 Richard Guenther
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

#ifndef _GPSM_STORE_H
#define _GPSM_STORE_H

#include <gtk/gtktreemodel.h>
#include <stdarg.h>
#include "gpsm.h"

#ifdef __cplusplus
extern "C" {
#endif				/* __cplusplus */

#define GLAME_TYPE_GPSM_STORE			(glame_gpsm_store_get_type ())
#define GLAME_GPSM_STORE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GLAME_TYPE_GPSM_STORE, GlameGpsmStore))
#define GLAME_GPSM_STORE_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GLAME_TYPE_GPSM_STORE, GlameGpsmStoreClass))
#define GLAME_IS_GPSM_STORE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GLAME_TYPE_GPSM_STORE))
#define GLAME_IS_GPSM_STORE_CLASS(klass)		(G_TYPE_CHECK_CLASS_TYPE ((klass), GLAME_TYPE_GPSM_STORE))
#define GLAME_GPSM_STORE_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), GLAME_TYPE_GPSM_STORE, GlameGpsmStoreClass))

	typedef struct _GlameGpsmStore GlameGpsmStore;
	typedef struct _GlameGpsmStoreClass GlameGpsmStoreClass;

	struct _GlameGpsmStore {
		GObject parent;

		gpsm_grp_t *root;
	};

	struct _GlameGpsmStoreClass {
		GObjectClass parent_class;

		/* Padding for future expansion */
		void (*_gtk_reserved1) (void);
		void (*_gtk_reserved2) (void);
		void (*_gtk_reserved3) (void);
		void (*_gtk_reserved4) (void);
	};

/* gpsm store has static columns data:
 * 0: gpsm item label
 * 1: gpsm item hsize
 */
#define GPSM_STORE_LABEL 0
#define GPSM_STORE_SIZE  1
#define GPSM_STORE_COL_N 2

	GType glame_gpsm_store_get_type(void);
	GlameGpsmStore *glame_gpsm_store_new(gpsm_grp_t * root);

	gpsm_item_t *glame_gpsm_store_get_item(GtkTreeIter * iter);

#ifdef __cplusplus
}
#endif				/* __cplusplus */
#endif				/* _GPSM_STORE_H */
