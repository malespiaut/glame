/*
 * timeline_canvas.c
 *
 * $Id: timeline_canvas.c,v 1.9 2004/10/23 13:09:26 richi Exp $
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

#include "timeline_canvas.h"


/*
 * Canvas helpers.
 */

/* Compute canvas coords from time[s] / track[nr] */
#define _HUNIT(x) (50.0*(x))
#define _HUNIT1(x) ((x)/50.0)
#define _VUNIT(y) (100.0*(y))
#define _VUNIT1(y) ((y)/100.0)
#define _VBORDER 20.0



/*
 * TimelineCanvas
 */

static void timeline_canvas_destroy(GtkObject *canvas)
{
	GnomeCanvas* parent_class;
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS);
	GTK_OBJECT_CLASS(parent_class)->destroy(canvas);
	if (TIMELINE_CANVAS(canvas)->gpsm_handler0)
		glsig_delete_handler(TIMELINE_CANVAS(canvas)->gpsm_handler0);
	if (TIMELINE_CANVAS(canvas)->gpsm_handler1)
		glsig_delete_handler(TIMELINE_CANVAS(canvas)->gpsm_handler1);
}

static void timeline_canvas_class_init(TimelineCanvasClass *klass)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(klass);
	object_class->destroy = timeline_canvas_destroy;
}

static void timeline_canvas_init(TimelineCanvas *canvas)
{
	canvas->gpsm_handler0 = NULL;
	canvas->gpsm_handler1 = NULL;
	canvas->root = NULL;
	canvas->active_item = NULL;
	canvas->active_group = NULL;
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
	}

	return timeline_canvas_type;
}

static void timeline_canvas_handle_root(glsig_handler_t *handler, long sig, va_list va)
{
	TimelineCanvas *canvas = TIMELINE_CANVAS(glsig_handler_private(handler));
	GnomeCanvasItem *root = GNOME_CANVAS_ITEM(
		gnome_canvas_root(GNOME_CANVAS(canvas)));
	double scale, size;

	if (root->object.flags & GNOME_CANVAS_ITEM_AFFINE_FULL) {
		scale = root->xform[0];
	} else
		scale = 1.0;

	/* calculate size so hpos 0 is aligned with the window left */
	size = scale*_HUNIT(gpsm_item_hsize(canvas->root)/44100.0/*FIXME*/);
	if (GTK_WIDGET_REALIZED(canvas))
		size = MAX(size, GTK_WIDGET(canvas)->allocation.width);
	gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas), 0.0, 0.0, size,
				       _VUNIT(gpsm_item_vsize(canvas->root)));
}

static void on_canvas_realize(GtkWidget *widget, gpointer *data)
{
	TimelineCanvas *canvas = TIMELINE_CANVAS(widget);
        glsig_handler_exec(canvas->gpsm_handler0, GPSM_SIG_ITEM_CHANGED,
                           canvas->root);
}

TimelineCanvas *timeline_canvas_new(gpsm_grp_t *root)
{
	TimelineCanvas *canvas;

	canvas = TIMELINE_CANVAS(gtk_type_new(timeline_canvas_get_type()));
	canvas->root = root;

	/* Register handlers to adjust visible canvas region on realization
	 * and window resize. */
	gtk_signal_connect(GTK_OBJECT(canvas), "realize",
			   GTK_SIGNAL_FUNC(on_canvas_realize), NULL);
	gtk_signal_connect(GTK_OBJECT(canvas), "size_allocate",
			   GTK_SIGNAL_FUNC(on_canvas_realize), NULL);

	/* Register handler to adjust canvas and invoke it one time. */
	canvas->gpsm_handler0 = glsig_add_handler(gpsm_item_emitter(root), GPSM_SIG_ITEM_CHANGED, timeline_canvas_handle_root, canvas);
	glsig_handler_exec(canvas->gpsm_handler0, GPSM_SIG_ITEM_CHANGED, root);

	return canvas;
}

void timeline_canvas_scale(TimelineCanvas *canvas, double scale)
{
	double affine[6] = { scale, 0.0, 0.0, 1.0, 0.0, 0.0};
	gnome_canvas_item_affine_relative(
		GNOME_CANVAS_ITEM(gnome_canvas_root(GNOME_CANVAS(canvas))),
		affine);
	glsig_handler_exec(canvas->gpsm_handler0, GPSM_SIG_ITEM_CHANGED,
			   canvas->root);
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

static void timeline_canvas_item_class_init(TimelineCanvasItemClass *klass)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(klass);
	object_class->destroy = timeline_canvas_item_destroy;
	klass->update = NULL;
	klass->highlight = NULL;
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
	}

	return timeline_canvas_item_type;
}

void timeline_canvas_item_update(TimelineCanvasItem *item)
{
	TIMELINE_CANVAS_ITEM_CLASS(G_OBJECT_GET_CLASS(item))->update(item);
}

void timeline_canvas_item_highlight(TimelineCanvasItem *item, gboolean lite)
{
	TIMELINE_CANVAS_ITEM_CLASS(G_OBJECT_GET_CLASS(item))->highlight(item, lite);
}

void timeline_canvas_item_gpsm2w(long hposition, long vposition,
				 long hsize, long vsize,
				 int rate,
				 double *x1, double *y1,
				 double *x2, double *y2)
{
	*x1 = _HUNIT(hposition/(double)rate);
	*y1 = _VUNIT(vposition);
	*x2 = *x1 + _HUNIT(hsize/(double)rate);
	*y2 = *y1 + _VUNIT(vsize);
}

void timeline_canvas_item_w2gpsm(long *hposition, long *vposition,
				 long *hsize, long *vsize,
				 int rate,
				 double x1, double y1,
				 double x2, double y2)
{
	*hposition = _HUNIT1(x1*(double)rate);
	*vposition = _VUNIT1(y1) + 0.5;
	*hsize = _HUNIT1((x2-x1)*(double)rate);
	*vsize = _VUNIT1((y2-y1));
}




/*
 * TimelineCanvasGroup
 */

static void timeline_canvas_group_update(TimelineCanvasItem *item);
static void timeline_canvas_group_highlight(TimelineCanvasItem *item,
					    gboolean lite);

static void timeline_canvas_group_destroy(GtkObject *group)
{
	TimelineCanvasItemClass* parent_class;
	parent_class = gtk_type_class(TIMELINE_CANVAS_ITEM_TYPE);
	GTK_OBJECT_CLASS(parent_class)->destroy(group);
}

static void timeline_canvas_group_class_init(TimelineCanvasGroupClass *klass)
{
	GtkObjectClass *object_class;
	TimelineCanvasItemClass *item_class;
	object_class = GTK_OBJECT_CLASS(klass);
	object_class->destroy = timeline_canvas_group_destroy;
	item_class = TIMELINE_CANVAS_ITEM_CLASS(klass);
	item_class->update = timeline_canvas_group_update;
	item_class->highlight = timeline_canvas_group_highlight;
}

static void timeline_canvas_group_init(TimelineCanvasGroup *grp)
{
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
	}

	return timeline_canvas_group_type;
}

static void timeline_canvas_group_update(TimelineCanvasItem *item)
{
	TimelineCanvasGroup *group = TIMELINE_CANVAS_GROUP(item);
	double x1, y1, x2, y2;
	gpsm_item_t *it;

	/* Update the rect hsize. */
	x2 = _HUNIT(gpsm_item_hsize(item->item)/44100.0/*FIXME*/);
	y2 = _VUNIT(gpsm_item_vsize(item->item));
	gnome_canvas_item_set(group->rect,
			      "x2", x2, "y2", y2, NULL);

	/* Update the label. */
	gnome_canvas_item_set(group->text,
			      "text", gpsm_item_label(item->item), NULL);

	/* Move the file. */
	x1 = _HUNIT(gpsm_item_hposition(item->item)/44100.0/*FIXME*/);
	y1 = _VUNIT(gpsm_item_vposition(item->item));
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(group),
			      "x", x1,
			      "y", y1,
			      NULL);

	gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(item));
	gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(group->text)); /* doenst seem to handle position change only -- GNOME bug */

	/* Somehow we need to trigger updates on all childs. Ugh. */
	gpsm_grp_foreach_item(item->item, it) {
		GnomeCanvasItem *child;
		child = timeline_canvas_find_gpsm_item(GNOME_CANVAS_GROUP(group), it);
		if (!child) {
			DPRINTF("FUCK\n");
			continue;
		}
		timeline_canvas_item_update(TIMELINE_CANVAS_ITEM(child));
	}
}

TimelineCanvasGroup *timeline_canvas_group_new(GnomeCanvasGroup *group,
					       gpsm_grp_t *grp)
{
	TimelineCanvasGroup *item;
	double x1, x2, y1, y2;

	x1 = _HUNIT(gpsm_item_hposition(grp)/44100.0/*FIXME*/);
	y1 = _VUNIT(gpsm_item_vposition(grp));
	item = TIMELINE_CANVAS_GROUP(
		gnome_canvas_item_new(group, TIMELINE_CANVAS_GROUP_TYPE,
				      "x", x1, "y", y1,
				      NULL));
	TIMELINE_CANVAS_ITEM(item)->item = (gpsm_item_t *)grp;


	x1 = 0.0;
	y1 = 0.0;
	x2 = _HUNIT(gpsm_item_hsize(grp)/44100.0/*FIXME*/);
	y2 = _VUNIT(gpsm_item_vsize(grp));
	item->rect = gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1", x1,
			      "y1", y1,
			      "x2", x2,
			      "y2", y2,
			      "outline_color", "black",
			      "width_units", 4.0,
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

static void timeline_canvas_group_highlight(TimelineCanvasItem *item, gboolean lite)
{
	TimelineCanvasGroup *group = TIMELINE_CANVAS_GROUP(item);
	gnome_canvas_item_set(group->rect,
			      "outline_color", lite ? "blue" : "black",
			      NULL);
	gnome_canvas_item_raise_to_top(group->rect);
	gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(item));
}



/*
 * TimelineCanvasFile
 */

static void timeline_canvas_file_update(TimelineCanvasItem *item);
static void timeline_canvas_file_highlight(TimelineCanvasItem *item,
					   gboolean lite);

static void timeline_canvas_file_destroy(GtkObject *file)
{
	TimelineCanvasItemClass* parent_class;
	parent_class = gtk_type_class(TIMELINE_CANVAS_ITEM_TYPE);
	GTK_OBJECT_CLASS(parent_class)->destroy(file);
}

static void timeline_canvas_file_class_init(TimelineCanvasFileClass *klass)
{
	GtkObjectClass *object_class;
	TimelineCanvasItemClass *item_class;
	object_class = GTK_OBJECT_CLASS(klass);
	object_class->destroy = timeline_canvas_file_destroy;
	item_class = TIMELINE_CANVAS_ITEM_CLASS(klass);
	item_class->update = timeline_canvas_file_update;
	item_class->highlight = timeline_canvas_file_highlight;
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
	}

	return timeline_canvas_file_type;
}

static void timeline_canvas_file_update(TimelineCanvasItem *item)
{
	TimelineCanvasFile *file = TIMELINE_CANVAS_FILE(item);
	double x1, y1, x2;

	/* Update the rect hsize. */
	x2 = _HUNIT(gpsm_item_hsize(item->item)/(double)gpsm_swfile_samplerate(item->item));
	gnome_canvas_item_set(file->rect, "x2", x2, NULL);

	/* Update the label. */
	gnome_canvas_item_set(file->text,
			      "text", gpsm_item_label(item->item), NULL);

	/* Move the file. */
	x1 = _HUNIT(gpsm_item_hposition(item->item)/(double)gpsm_swfile_samplerate(item->item));
	y1 = _VUNIT(gpsm_item_vposition(item->item)) + _VBORDER;
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(file),
			      "x", x1,
			      "y", y1,
			      NULL);

	gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(item));
	gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(file->text)); /* doenst seem to handle position change only -- GNOME bug */
}

TimelineCanvasFile *timeline_canvas_file_new(GnomeCanvasGroup *group,
					     gpsm_swfile_t *swfile)
{
	TimelineCanvasFile *item;
	double x1, x2, y1, y2;

	x1 = _HUNIT(gpsm_item_hposition(swfile)/(double)gpsm_swfile_samplerate(swfile));
	y1 = _VUNIT(gpsm_item_vposition(swfile)) + _VBORDER;
	item = TIMELINE_CANVAS_FILE(
		gnome_canvas_item_new(group, TIMELINE_CANVAS_FILE_TYPE,
				      "x", x1, "y", y1, NULL));
	TIMELINE_CANVAS_ITEM(item)->item = (gpsm_item_t *)swfile;

	x1 = 0.0;
	y1 = 0.0;
	x2 = _HUNIT(gpsm_item_hsize(swfile)/(double)gpsm_swfile_samplerate(swfile));
	y2 = _VUNIT(1) - 2.0*_VBORDER;
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

static void timeline_canvas_file_highlight(TimelineCanvasItem *item, gboolean lite)
{
	TimelineCanvasFile *file = TIMELINE_CANVAS_FILE(item);
	gnome_canvas_item_set(file->rect,
			      "outline_color", lite ? "yellow" : "black",
			      NULL);
	gnome_canvas_item_request_update(GNOME_CANVAS_ITEM(item));
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
