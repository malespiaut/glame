#ifndef _CANVAS_TYPES_H
#define _CANVAS_TYPES_H

/*
 * canvas_types.h
 *
 * $Id: canvas_types.h,v 1.6 2001/06/27 07:45:03 richi Exp $
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

#include <gnome.h>
#include "gpsm.h"


/* Types
 */

struct _TimelineCanvasClass;
struct _TimelineCanvas;
typedef struct _TimelineCanvasClass TimelineCanvasClass;
typedef struct _TimelineCanvas TimelineCanvas;

struct _TimelineCanvasItemClass;
struct _TimelineCanvasItem;
typedef struct _TimelineCanvasItemClass TimelineCanvasItemClass;
typedef struct _TimelineCanvasItem TimelineCanvasItem;

struct _TimelineCanvasGroupClass;
struct _TimelineCanvasGroup;
typedef struct _TimelineCanvasGroupClass TimelineCanvasGroupClass;
typedef struct _TimelineCanvasGroup TimelineCanvasGroup;

struct _TimelineCanvasFileClass;
struct _TimelineCanvasFile;
typedef struct _TimelineCanvasFileClass TimelineCanvasFileClass;
typedef struct _TimelineCanvasFile TimelineCanvasFile;


/* GTK Type defines
 */

/* TimelineCanvas */
#define TIMELINE_CANVAS_TYPE (timeline_canvas_get_type())
#define TIMELINE_CANVAS(object) (GTK_CHECK_CAST((object), TIMELINE_CANVAS_TYPE, TimelineCanvas))
#define TIMELINE_CANVAS_CLASS(object) (GTK_CHECK_CLASS_CAST((object), TIMELINE_CANVAS_TYPE, TimelineCanvasClass))
#define TIMELINE_IS_CANVAS(object) (GTK_CHECK_TYPE((object), TIMELINE_CANVAS_TYPE))
#define TIMELINE_IS_CANVAS_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), TIMELINE_CANVAS_TYPE))
#define TIMELINE_CANVAS_GET_CLASS(object) ((TimelineCanvasClass*) (((GtkObject*) (obj))->klass))

/* TimelineCanvasItem */
#define TIMELINE_CANVAS_ITEM_TYPE (timeline_canvas_item_get_type())
#define TIMELINE_CANVAS_ITEM(object) (GTK_CHECK_CAST((object), TIMELINE_CANVAS_ITEM_TYPE, TimelineCanvasItem))
#define TIMELINE_CANVAS_ITEM_CLASS(object) (GTK_CHECK_CLASS_CAST((object), TIMELINE_CANVAS_ITEM_TYPE, TimelineCanvasItemClass))
#define TIMELINE_IS_CANVAS_ITEM(object) (GTK_CHECK_TYPE((object), TIMELINE_CANVAS_ITEM_TYPE))
#define TIMELINE_IS_CANVAS_ITEM_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), TIMELINE_CANVAS_ITEM_TYPE))
#define TIMELINE_CANVAS_ITEM_GET_CLASS(object) ((TimelineCanvasItemClass*) (((GtkObject*) (obj))->klass))

/* TimelineCanvasGroup */
#define TIMELINE_CANVAS_GROUP_TYPE (timeline_canvas_group_get_type())
#define TIMELINE_CANVAS_GROUP(object) (GTK_CHECK_CAST((object), TIMELINE_CANVAS_GROUP_TYPE, TimelineCanvasGroup))
#define TIMELINE_CANVAS_GROUP_CLASS(object) (GTK_CHECK_CLASS_CAST((object), TIMELINE_CANVAS_GROUP_TYPE, TimelineCanvasGroupClass))
#define TIMELINE_IS_CANVAS_GROUP(object) (GTK_CHECK_TYPE((object), TIMELINE_CANVAS_GROUP_TYPE))
#define TIMELINE_IS_CANVAS_GROUP_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), TIMELINE_CANVAS_GROUP_TYPE))
#define TIMELINE_CANVAS_GROUP_GET_CLASS(object) ((TimelineCanvasGroupClass*) (((GtkObject*) (obj))->klass))

/* TimelineCanvasFile */
#define TIMELINE_CANVAS_FILE_TYPE (timeline_canvas_file_get_type())
#define TIMELINE_CANVAS_FILE(object) (GTK_CHECK_CAST((object), TIMELINE_CANVAS_FILE_TYPE, TimelineCanvasFile))
#define TIMELINE_CANVAS_FILE_CLASS(object) (GTK_CHECK_CLASS_CAST((object), TIMELINE_CANVAS_FILE_TYPE, TimelineCanvasFileClass))
#define TIMELINE_IS_CANVAS_FILE(object) (GTK_CHECK_TYPE((object), TIMELINE_CANVAS_FILE_TYPE))
#define TIMELINE_IS_CANVAS_FILE_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), TIMELINE_CANVAS_FILE_TYPE))
#define TIMELINE_CANVAS_FILE_GET_CLASS(object) ((TimelineCanvasFileClass*) (((GtkObject*) (obj))->klass))


/* Cool helpers - less to type */
#define TIMELINE_CI_GPSM(object) (TIMELINE_CANVAS_ITEM(object)->item)
#define TIMELINE_CI_GROUP(object) (GNOME_CANVAS_GROUP(GNOME_CANVAS_ITEM(object)->parent))
#define TIMELINE_CI_ROOT(object) (gnome_canvas_root(GNOME_CANVAS_ITEM(object)->canvas))
#define TIMELINE_CI_ROOTI(object) GNOME_CANVAS_ITEM(gnome_canvas_root(GNOME_CANVAS_ITEM(object)->canvas))

/* The actual data structures/functions
 */


/* TimelineCanvas
 */

struct _TimelineCanvasClass {
	GnomeCanvasClass parent_class;

};

struct _TimelineCanvas {
	GnomeCanvas parent_object;

	glsig_handler_t *gpsm_handler0;
	glsig_handler_t *gpsm_handler1;
	gpsm_grp_t *root;

	/* HACK(s) */
	GtkRuler *ruler;
};


/* TimelineCanvasItem
 */

struct _TimelineCanvasItemClass {
	GnomeCanvasGroupClass parent_class;

};

struct _TimelineCanvasItem {
	GnomeCanvasGroup parent_object;

	glsig_handler_t *gpsm_handler;
	gpsm_item_t *item;
};


/* TimelineCanvasGroup
 */

struct _TimelineCanvasGroupClass {
	TimelineCanvasItemClass parent_class;

};

struct _TimelineCanvasGroup {
	TimelineCanvasItem parent_object;

	GnomeCanvasItem *rect;
	GnomeCanvasItem *text;
};


/* TimelineCanvasFile
 */

struct _TimelineCanvasFileClass {
	TimelineCanvasItemClass parent_class;

};

struct _TimelineCanvasFile {
	TimelineCanvasItem parent_object;

	GnomeCanvasItem *rect;
	GnomeCanvasItem *text;
};



/* Public functions.
 */


GtkType timeline_canvas_get_type(void);

TimelineCanvas *timeline_canvas_new(gpsm_grp_t *root);

void timeline_canvas_scale(TimelineCanvas *canvas, double scale);


GtkType timeline_canvas_item_get_type(void);

void timeline_canvas_item_gpsm2w(long hposition, long vposition,
				 long hsize, long vsize,
				 int rate,
				 double *x1, double *y1,
				 double *x2, double *y2);

void timeline_canvas_item_w2gpsm(long *hposition, long *vposition,
				 long *hsize, long *vsize,
				 int rate,
				 double x1, double y1,
				 double x2, double y2);


GtkType timeline_canvas_group_get_type(void);

TimelineCanvasGroup *timeline_canvas_group_new(GnomeCanvasGroup *group,
					       gpsm_grp_t *grp);

void timeline_canvas_group_update(TimelineCanvasGroup *group);

void timeline_canvas_group_highlight(TimelineCanvasGroup *item, gboolean lite);


GtkType timeline_canvas_file_get_type(void);

TimelineCanvasFile *timeline_canvas_file_new(GnomeCanvasGroup *group,
					     gpsm_swfile_t *file);

void timeline_canvas_file_update(TimelineCanvasFile *file);

void timeline_canvas_file_highlight(TimelineCanvasFile *item, gboolean lite);


/* Find the Timeline Canvas Item associated with item beyond grp. Only
 * one level is searched, returns NULL, if not found. */
GnomeCanvasItem *timeline_canvas_find_gpsm_item(GnomeCanvasGroup *grp,
						gpsm_item_t *item);


#endif
