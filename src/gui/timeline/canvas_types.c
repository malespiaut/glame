/*
 * canvas_types.c
 *
 * $Id: canvas_types.c,v 1.1 2001/05/18 09:35:51 richi Exp $
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

#include "canvas_types.h"


/*
 * TimelineCanvasGroup
 */

static void timeline_canvas_group_destroy(GtkObject *object)
{
	TimelineCanvasGroupClass* parent_class;
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS_GROUP);
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
}

static void timeline_canvas_group_class_init(TimelineCanvasGroupClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = timeline_canvas_group_destroy;
}

static void timeline_canvas_group_init(TimelineCanvasGroup *grp)
{
}

GtkType timeline_canvas_group_get_type(void)
{
	static GtkType timeline_canvas_group_type = 0;
	
	if (!timeline_canvas_group_type){
		GtkTypeInfo timeline_canvas_group_info = {
			"TimelineCanvasGroup",
			sizeof(TimelineCanvasGroup),
			sizeof(TimelineCanvasGroupClass),
			(GtkClassInitFunc)timeline_canvas_group_class_init,
			(GtkObjectInitFunc)timeline_canvas_group_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		timeline_canvas_group_type = gtk_type_unique(
			GNOME_TYPE_CANVAS_GROUP, &timeline_canvas_group_info);
		gtk_type_set_chunk_alloc(timeline_canvas_group_type, 8);
	}

	return timeline_canvas_group_type;
}

TimelineCanvasGroup *timeline_canvas_group_new(GnomeCanvasGroup *group,
					       gpsm_grp_t *grp)
{
	TimelineCanvasGroup *item;

	item = TIMELINE_CANVAS_GROUP(
		gnome_canvas_item_new(group, TIMELINE_CANVAS_GROUP_TYPE, NULL));

	return item;
}



/*
 * TimelineCanvasFile
 */

static void timeline_canvas_file_destroy(GtkObject *object)
{
	GnomeCanvasItemClass* parent_class;
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS_ITEM);
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
}

static void timeline_canvas_file_class_init(TimelineCanvasFileClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = timeline_canvas_file_destroy;
}

static void timeline_canvas_file_init(TimelineCanvasFile *grp)
{
}

GtkType timeline_canvas_file_get_type(void)
{
	static GtkType timeline_canvas_file_type = 0;
	
	if (!timeline_canvas_file_type){
		GtkTypeInfo timeline_canvas_file_info = {
			"TimelineCanvasFile",
			sizeof(TimelineCanvasFile),
			sizeof(TimelineCanvasFileClass),
			(GtkClassInitFunc)timeline_canvas_file_class_init,
			(GtkObjectInitFunc)timeline_canvas_file_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		timeline_canvas_file_type = gtk_type_unique(
			GNOME_TYPE_CANVAS_ITEM, &timeline_canvas_file_info);
		gtk_type_set_chunk_alloc(timeline_canvas_file_type, 8);
	}

	return timeline_canvas_file_type;
}

TimelineCanvasFile *timeline_canvas_file_new(GnomeCanvasGroup *group,
					     gpsm_swfile_t *swfile)
{
	TimelineCanvasFile *item;

	item = TIMELINE_CANVAS_FILE(
		gnome_canvas_item_new(group, TIMELINE_CANVAS_FILE_TYPE, NULL));

	return item;
}
