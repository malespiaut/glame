#ifndef _CANVAS_TYPES_H
#define _CANVAS_TYPES_H

/*
 * canvas_types.h
 *
 * $Id: canvas_types.h,v 1.1 2001/05/18 09:35:51 richi Exp $
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

struct _TimelineCanvasGroupClass;
struct _TimelineCanvasFileClass;
typedef struct _TimelineCanvasGroupClass TimelineCanvasGroupClass;
typedef struct _TimelineCanvasFileClass TimelineCanvasFileClass;

struct _TimelineCanvasGroup;
struct _TimelineCanvasFile;
typedef struct _TimelineCanvasGroup TimelineCanvasGroup;
typedef struct _TimelineCanvasFile TimelineCanvasFile;


/* GTK Type defines
 */

/* TimelineCanvasGroup */
#define TIMELINE_CANVAS_GROUP_TYPE (timeline_canvas_group_get_type())
#define TIMELINE_CANVAS_GROUP(object) (GTK_CHECK_CAST((object), TIMELINE_CANVAS_GROUP_TYPE, TimelineCanvasGroup))
#define TIMELINE_CANVAS_GROUP_CLASS(object) (GTK_CHECK_CLASS_CAST((object), TIMELINE_CANVAS_GROUP_TYPE, TimelineCanvasGroupClass))
#define TIMELINE_IS_CANVAS_GROUP(object) (GTK_CHECK_TYPE((object), TIMELINE_CANVAS_GROUP_TYPE))
#define TIMELINE_IS_CANVAS_GROUP_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), TIMELINE_CANVAS_GROUP_TYPE))
#define TIMELINE_CANVAS_GROUP_GET_CLASS(object) ((TimelineCanvasFilterClass*) (((GtkObject*) (obj))->klass))

/* TimelineCanvasFile */
#define TIMELINE_CANVAS_FILE_TYPE (timeline_canvas_file_get_type())
#define TIMELINE_CANVAS_FILE(object) (GTK_CHECK_CAST((object), TIMELINE_CANVAS_FILE_TYPE, TimelineCanvasFile))
#define TIMELINE_CANVAS_FILE_CLASS(object) (GTK_CHECK_CLASS_CAST((object), TIMELINE_CANVAS_FILE_TYPE, TimelineCanvasFileClass))
#define TIMELINE_IS_CANVAS_FILE(object) (GTK_CHECK_TYPE((object), TIMELINE_CANVAS_FILE_TYPE))
#define TIMELINE_IS_CANVAS_FILE_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), TIMELINE_CANVAS_FILE_TYPE))
#define TIMELINE_CANVAS_FILE_GET_CLASS(object) ((TimelineCanvasFilterClass*) (((GtkObject*) (obj))->klass))



/* The actual data structures/functions
 */


/* TimelineCanvasGroup
 */

struct _TimelineCanvasGroupClass {
	GnomeCanvasGroupClass parent_class;

};

struct _TimelineCanvasGroup {
	GnomeCanvasGroup parent_object;

	glsig_handler_t *gpsm_handler;
};


/* TimelineCanvasFile
 */

struct _TimelineCanvasFileClass {
	GnomeCanvasItemClass parent_class;

};

struct _TimelineCanvasFile {
	GnomeCanvasItem parent_object;

	glsig_handler_t *gpsm_handler;
};



/* Public functions.
 */

GtkType timeline_canvas_group_get_type(void);

TimelineCanvasGroup *timeline_canvas_group_new(GnomeCanvasGroup *group,
					       gpsm_grp_t *grp);


GtkType timeline_canvas_file_get_type(void);

TimelineCanvasFile *timeline_canvas_file_new(GnomeCanvasGroup *group,
					     gpsm_swfile_t *file);


#endif
