/*
 * canvas_types.c
 *
 * $Id: canvas_types.c,v 1.3 2001/06/11 08:40:12 richi Exp $
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
 * Canvas helpers.
 */

/* Compute canvas coords from time[s] / track[nr] */
#define _HUNIT(x) (50.0*(x))
#define _VUNIT(y) (100.0*(y))
#define _VBORDER 10.0



/*
 * TimelineCanvas
 */

static void timeline_canvas_destroy(GtkObject *canvas)
{
	GnomeCanvas* parent_class;
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS);
	GTK_OBJECT_CLASS(parent_class)->destroy(canvas);
	if (TIMELINE_CANVAS(canvas)->gpsm_handler)
		glsig_delete_handler(TIMELINE_CANVAS(canvas)->gpsm_handler);
}

static void timeline_canvas_class_init(TimelineCanvasClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = timeline_canvas_destroy;
}

static void timeline_canvas_init(TimelineCanvas *canvas)
{
	canvas->gpsm_handler = NULL;
	canvas->root = NULL;
}

GtkType timeline_canvas_get_type(void)
{
	static GtkType timeline_canvas_type = 0;
	
	if (!timeline_canvas_type){
		GtkTypeInfo timeline_canvas_info = {
			"TimelineCanvas",
			sizeof(TimelineCanvas),
			sizeof(TimelineCanvasClass),
			(GtkClassInitFunc)timeline_canvas_class_init,
			(GtkObjectInitFunc)timeline_canvas_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		timeline_canvas_type = gtk_type_unique(
			GNOME_TYPE_CANVAS, &timeline_canvas_info);
		gtk_type_set_chunk_alloc(timeline_canvas_type, 8);
	}

	return timeline_canvas_type;
}

static void timeline_canvas_handle_root(glsig_handler_t *handler, long sig, va_list va)
{
	TimelineCanvas *canvas = TIMELINE_CANVAS(glsig_handler_private(handler));
	gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas), 0.0, 0.0,
				       _HUNIT(gpsm_item_hsize(canvas->root)/44100.0/*FIXME*/),
				       _VUNIT(gpsm_item_vsize(canvas->root)));
}

TimelineCanvas *timeline_canvas_new(gpsm_grp_t *root)
{
	TimelineCanvas *canvas;

	canvas = TIMELINE_CANVAS(gtk_type_new(timeline_canvas_get_type()));
	canvas->root = root;

	/* Register handler to adjust canvas and invoke it one time. */
	canvas->gpsm_handler = glsig_add_handler(gpsm_item_emitter(root), GPSM_SIG_ITEM_CHANGED, timeline_canvas_handle_root, canvas);
	glsig_handler_exec(canvas->gpsm_handler, GPSM_SIG_ITEM_CHANGED, root);

	return canvas;
}



/*
 * TimelineCanvasItem
 */

static void timeline_canvas_item_destroy(GtkObject *item)
{
	GnomeCanvasGroupClass* parent_class;
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS_GROUP);
	GTK_OBJECT_CLASS(parent_class)->destroy(item);
	if (TIMELINE_CANVAS_ITEM(item)->gpsm_handler)
		glsig_delete_handler(TIMELINE_CANVAS_ITEM(item)->gpsm_handler);
}

static void timeline_canvas_item_class_init(TimelineCanvasItemClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = timeline_canvas_item_destroy;
}

static void timeline_canvas_item_init(TimelineCanvasItem *item)
{
	item->gpsm_handler = NULL;
	item->item = NULL;
}

GtkType timeline_canvas_item_get_type(void)
{
	static GtkType timeline_canvas_item_type = 0;
	
	if (!timeline_canvas_item_type){
		GtkTypeInfo timeline_canvas_item_info = {
			"TimelineCanvasItem",
			sizeof(TimelineCanvasItem),
			sizeof(TimelineCanvasItemClass),
			(GtkClassInitFunc)timeline_canvas_item_class_init,
			(GtkObjectInitFunc)timeline_canvas_item_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		timeline_canvas_item_type = gtk_type_unique(
			GNOME_TYPE_CANVAS_GROUP, &timeline_canvas_item_info);
		gtk_type_set_chunk_alloc(timeline_canvas_item_type, 8);
	}

	return timeline_canvas_item_type;
}



/*
 * TimelineCanvasGroup
 */

static void timeline_canvas_group_destroy(GtkObject *group)
{
	TimelineCanvasItemClass* parent_class;
	parent_class = gtk_type_class(TIMELINE_CANVAS_ITEM_TYPE);
	GTK_OBJECT_CLASS(parent_class)->destroy(group);
}

static void timeline_canvas_group_class_init(TimelineCanvasGroupClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = timeline_canvas_group_destroy;
}

static void timeline_canvas_group_init(TimelineCanvasGroup *grp)
{
	grp->level = 0;
	grp->rect = NULL;
	grp->text = NULL;
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
			TIMELINE_CANVAS_ITEM_TYPE,
			&timeline_canvas_group_info);
		gtk_type_set_chunk_alloc(timeline_canvas_group_type, 8);
	}

	return timeline_canvas_group_type;
}

void timeline_canvas_group_update(TimelineCanvasGroup *group)
{
	TimelineCanvasItem *item = TIMELINE_CANVAS_ITEM(group);
	double x1, y1, x2, y2;

	/* Update the rect hsize. */
	x2 = _HUNIT(gpsm_item_hsize(item->item)/44100.0/*FIXME*/);
	y2 = _VUNIT(gpsm_item_vsize(item->item)) - _VBORDER*group->level*2.0;
	gnome_canvas_item_set(group->rect,
			      "x2", x2, "y2", y2, NULL);

	/* Update the label. */
	gnome_canvas_item_set(group->text,
			      "text", gpsm_item_label(item->item), NULL);

	/* Move the file. */
	x1 = _HUNIT(gpsm_item_hposition(item->item)/44100.0/*FIXME*/);
	y1 = _VUNIT(gpsm_item_vposition(item->item)) + _VBORDER*group->level;
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(group),
			      "x", x1,
			      "y", y1,
			      NULL);

	gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(item));
}

TimelineCanvasGroup *timeline_canvas_group_new(GnomeCanvasGroup *group,
					       gpsm_grp_t *grp)
{
	TimelineCanvasGroup *item;
	double x1, x2, y1, y2;
	int level;

	if (TIMELINE_IS_CANVAS_GROUP(group))
		level = TIMELINE_CANVAS_GROUP(group)->level + 1;
	else
		level = 0;

	x1 = _HUNIT(gpsm_item_hposition(grp)/44100.0/*FIXME*/);
	y1 = _VUNIT(gpsm_item_vposition(grp)) + _VBORDER*level;
	//gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(group), &x1, &y1);
	DPRINTF("grp item at %.3f %.3f (level %i)\n",
		(float)x1, (float)y1, level);
	item = TIMELINE_CANVAS_GROUP(
		gnome_canvas_item_new(group, TIMELINE_CANVAS_GROUP_TYPE,
				      "x", x1, "y", y1,
				      NULL));
	TIMELINE_CANVAS_ITEM(item)->item = (gpsm_item_t *)grp;
	item->level = level;


	x1 = 0.0;
	y1 = 0.0;
	x2 = _HUNIT(gpsm_item_hsize(grp)/44100.0/*FIXME*/);
	y2 = _VUNIT(gpsm_item_vsize(grp)) - _VBORDER*level*2.0;
	//gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(item), &x1, &y1);
	//gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(item), &x2, &y2);
	DPRINTF("grp rect is %.3f %.3f - %.3f %.3f\n",
		(float)x1, (float)y1, (float)x2, (float)y2);
	item->rect = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1", x1,
			      "y1", y1,
			      "x2", x2,
			      "y2", y2,
			      "outline_color", "black",
			      "width_units", MAX(1.0, 4.0-level),
			      "fill_color", NULL,
			      NULL);

	item->text = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_text_get_type(),
			      "x", x1+5.0, "y", y1+5.0,
			      "anchor", GTK_ANCHOR_NORTH_WEST,
			      "fill_color", "black",
			      "font", "-adobe-helvetica-medium-r-normal-*-12-120-*-*-p-*-iso8859-1",
			      "text", gpsm_item_label(grp),
			      NULL);

	return item;
}



/*
 * TimelineCanvasFile
 */

static void timeline_canvas_file_destroy(GtkObject *file)
{
	TimelineCanvasItemClass* parent_class;
	parent_class = gtk_type_class(TIMELINE_CANVAS_ITEM_TYPE);
	GTK_OBJECT_CLASS(parent_class)->destroy(file);
}

static void timeline_canvas_file_class_init(TimelineCanvasFileClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = timeline_canvas_file_destroy;
}

static void timeline_canvas_file_init(TimelineCanvasFile *file)
{
	file->rect = NULL;
	file->text = NULL;
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
			TIMELINE_CANVAS_ITEM_TYPE, &timeline_canvas_file_info);
		gtk_type_set_chunk_alloc(timeline_canvas_file_type, 8);
	}

	return timeline_canvas_file_type;
}

void timeline_canvas_file_update(TimelineCanvasFile *file)
{
	TimelineCanvasItem *item = TIMELINE_CANVAS_ITEM(file);
	int level;
	double x1, y1, x2;

	/* Get the level. */
	level = 1;
	if (TIMELINE_IS_CANVAS_GROUP(GNOME_CANVAS_ITEM(item)->parent))
		level = TIMELINE_CANVAS_GROUP(GNOME_CANVAS_ITEM(item)->parent)->level+1;

	/* Update the rect hsize. */
	x2 = _HUNIT(gpsm_item_hsize(item->item)/(double)gpsm_swfile_samplerate(item->item));
	gnome_canvas_item_set(file->rect, "x2", x2, NULL);

	/* Update the label. */
	gnome_canvas_item_set(file->text,
			      "text", gpsm_item_label(item->item), NULL);

	/* Move the file. */
	x1 = _HUNIT(gpsm_item_hposition(item->item)/(double)gpsm_swfile_samplerate(item->item));
	y1 = _VUNIT(gpsm_item_vposition(item->item)) + _VBORDER*level;
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(file),
			      "x", x1,
			      "y", y1,
			      NULL);

	gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(item));
}

TimelineCanvasFile *timeline_canvas_file_new(GnomeCanvasGroup *group,
					     gpsm_swfile_t *swfile)
{
	TimelineCanvasFile *item;
	double x1, x2, y1, y2;
	int level;

	if (TIMELINE_IS_CANVAS_GROUP(group))
		level = TIMELINE_CANVAS_GROUP(group)->level + 1;
	else
		level = 1;

	x1 = _HUNIT(gpsm_item_hposition(swfile)/(double)gpsm_swfile_samplerate(swfile));
	y1 = _VUNIT(gpsm_item_vposition(swfile)) + _VBORDER*level;
	//gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(group), &x1, &y1);
	DPRINTF("swfile item at %.3f %.3f (level %i)\n",
		(float)x1, (float)y1, level);
	item = TIMELINE_CANVAS_FILE(
		gnome_canvas_item_new(group, TIMELINE_CANVAS_FILE_TYPE,
				      "x", x1, "y", y1, NULL));
	TIMELINE_CANVAS_ITEM(item)->item = (gpsm_item_t *)swfile;

	x1 = 0.0;
	y1 = 0.0;
	x2 = _HUNIT(gpsm_item_hsize(swfile)/(double)gpsm_swfile_samplerate(swfile));
	y2 = _VUNIT(1) - 2.0*_VBORDER*level;
	//gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(item), &x1, &y1);
	//gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(item), &x2, &y2);
	DPRINTF("swfile rect is %.3f %.3f - %.3f %.3f\n",
		(float)x1, (float)y1, (float)x2, (float)y2);
	item->rect = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1", x1,
			      "y1", y1,
			      "x2", x2,
			      "y2", y2,
			      "outline_color", "black",
			      "width_units", 1.0,
			      "fill_color_rgba", 0xffffff00,
			      NULL);

	item->text = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_text_get_type(),
			      "x", x1+5.0, "y", y1+5.0,
			      "anchor", GTK_ANCHOR_NORTH_WEST,
			      "fill_color", "black",
			      "font", "-adobe-helvetica-medium-r-normal-*-12-120-*-*-p-*-iso8859-1",
			      "text", gpsm_item_label(swfile),
			      NULL);

	return item;
}



/*
 * Utils tightly coupled with the canvas items.
 */

GnomeCanvasItem *timeline_canvas_find_gpsm_item(GnomeCanvasGroup *grp,
						gpsm_item_t *item)
{
	GList *litem;

	litem = grp->item_list;
	while (litem) {
		GnomeCanvasItem *citem = GNOME_CANVAS_ITEM(litem->data);
		if (TIMELINE_IS_CANVAS_ITEM(citem)
		    && (TIMELINE_CANVAS_ITEM(citem)->item == item))
			return citem;
		litem = g_list_next(litem);
	}
	return NULL;
}
