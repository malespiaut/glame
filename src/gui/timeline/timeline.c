/*
 * timeline.c
 * $Id: timeline.c,v 1.3 2001/06/11 08:40:12 richi Exp $
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
#include "canvas_types.h"
#include "timeline.h"


/*
 * GPSM signal handlers - the "backend" of the timeline.
 */

static void handle_grp_add_item(GnomeCanvasGroup *group, gpsm_item_t *item);
static void handle_file(glsig_handler_t *handler, long sig, va_list va);
static void handle_group(glsig_handler_t *handler, long sig, va_list va);

static void handle_group(glsig_handler_t *handler, long sig, va_list va)
{
	switch (sig) {
	case GPSM_SIG_ITEM_CHANGED: {
		TimelineCanvasGroup *group;
		gpsm_grp_t *grp;

		group = TIMELINE_CANVAS_GROUP(glsig_handler_private(handler));
		GLSIGH_GETARGS1(va, grp);

		timeline_canvas_group_update(group);
		break;
	}
	case GPSM_SIG_GRP_REMOVEITEM: {
		TimelineCanvasGroup *group;
		GnomeCanvasItem *citem;
		gpsm_grp_t *grp;
		gpsm_item_t *item;

		group = TIMELINE_CANVAS_GROUP(glsig_handler_private(handler));
		GLSIGH_GETARGS2(va, grp, item);
		if (TIMELINE_CANVAS_ITEM(group)->item != grp)
			DPRINTF("FUCK!\n");

		citem = timeline_canvas_find_gpsm_item(GNOME_CANVAS_GROUP(group), item);
		if (!citem)
			DPRINTF("FUCK2\n");
		gtk_object_destroy(GTK_OBJECT(citem));

		break;
	}
	case GPSM_SIG_GRP_NEWITEM: {
		TimelineCanvasGroup *group;
		gpsm_grp_t *grp;
		gpsm_item_t *item;

		group = TIMELINE_CANVAS_GROUP(glsig_handler_private(handler));
		GLSIGH_GETARGS2(va, grp, item);

		handle_grp_add_item(GNOME_CANVAS_GROUP(group), item);

		break;
	}
	default:
		DPRINTF("Unhandled signal %li\n", sig);
	}
}

static void handle_file(glsig_handler_t *handler, long sig, va_list va)
{
	switch (sig) {
	case GPSM_SIG_ITEM_CHANGED: {
		TimelineCanvasFile *file;
		gpsm_swfile_t *swfile;

		file = TIMELINE_CANVAS_FILE(glsig_handler_private(handler));
		GLSIGH_GETARGS1(va, swfile);

		timeline_canvas_file_update(file);
		break;
	}
	default:
		DPRINTF("Unhandled signal %li\n", sig);
	}
}

static void handle_root(glsig_handler_t *handler, long sig, va_list va)
{
	switch (sig) {
	case GPSM_SIG_GRP_REMOVEITEM: {
		TimelineCanvas *canvas;
		GnomeCanvasItem *citem;
		gpsm_grp_t *root;
		gpsm_item_t *item;

		canvas = TIMELINE_CANVAS(glsig_handler_private(handler));
		GLSIGH_GETARGS2(va, root, item);
		if (canvas->root != root)
			DPRINTF("FUCK!\n");

		citem = timeline_canvas_find_gpsm_item(
			gnome_canvas_root(GNOME_CANVAS(canvas)), item);
		if (!citem)
			DPRINTF("FUCK2\n");
		gtk_object_destroy(GTK_OBJECT(citem));

		break;
	}
	case GPSM_SIG_GRP_NEWITEM: {
		TimelineCanvas *canvas;
		gpsm_grp_t *root;
		gpsm_item_t *item;

		canvas = TIMELINE_CANVAS(glsig_handler_private(handler));
		GLSIGH_GETARGS2(va, root, item);

		handle_grp_add_item(GNOME_CANVAS_GROUP(
			gnome_canvas_root(GNOME_CANVAS(canvas))), item);

		break;
	}
	default:
		DPRINTF("Unhandled signal %li\n", sig);
	}
}

static void handle_grp_add_item(GnomeCanvasGroup *group, gpsm_item_t *item)
{
	if (GPSM_ITEM_IS_GRP(item)) {
		TimelineCanvasGroup *grp;
		gpsm_item_t *it;

		DPRINTF("Adding group %s\n", gpsm_item_label(item));
		grp = timeline_canvas_group_new(group, (gpsm_grp_t *)item);
		/* FIXME: add handlers (gpsm) */
		TIMELINE_CANVAS_ITEM(grp)->gpsm_handler = glsig_add_handler(
			gpsm_item_emitter(item), GPSM_SIG_GRP_NEWITEM
			|GPSM_SIG_GRP_REMOVEITEM|GPSM_SIG_ITEM_CHANGED,
			handle_group, grp);

		/* Recurse. */
		gpsm_grp_foreach_item(item, it)
			handle_grp_add_item(GNOME_CANVAS_GROUP(grp), it);

	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		TimelineCanvasFile *file;

		DPRINTF("Adding file %s\n", gpsm_item_label(item));
		file = timeline_canvas_file_new(group, (gpsm_swfile_t *)item);
		/* FIXME: add handlers (gpsm) */
		TIMELINE_CANVAS_ITEM(file)->gpsm_handler = glsig_add_handler(
			gpsm_item_emitter(item), GPSM_SIG_ITEM_CHANGED,
			handle_file, file);
	}
}




/*
 * Public API for the timeline.
 */

GtkWidget *glame_timeline_new(gpsm_grp_t *root)
{
	GtkWidget *window, *canvas;
	gpsm_item_t *item;

	if (!root || !GPSM_ITEM_IS_GRP(root))
		return NULL;

	window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(window),
				       GTK_POLICY_ALWAYS,
				       GTK_POLICY_ALWAYS);
	gtk_widget_push_visual(gdk_rgb_get_visual());
	gtk_widget_push_colormap(gdk_rgb_get_cmap());
	canvas = GTK_WIDGET(timeline_canvas_new(root));
	gtk_widget_pop_colormap();
	gtk_widget_pop_visual();
	gtk_container_add(GTK_CONTAINER(window), canvas);
	gtk_widget_show(canvas);
	gtk_widget_show(window);

	/* Add all existing childs of root. */
	gpsm_grp_foreach_item(root, item)
		handle_grp_add_item(gnome_canvas_root(GNOME_CANVAS(canvas)), item);

	/* Add handler for item additions/removal in root group. */
	TIMELINE_CANVAS(canvas)->gpsm_handler = glsig_add_handler(
		gpsm_item_emitter(root), GPSM_SIG_GRP_NEWITEM
		|GPSM_SIG_GRP_REMOVEITEM, handle_root, canvas);

	return window;
}

GtkWidget *glame_timeline_new_with_window(const char *caption,
					  gpsm_grp_t *root)
{
	GtkWidget *timeline, *window, *toolbar;

	if (!root || !GPSM_ITEM_IS_GRP(root))
		return NULL;

	/* Create the timeline canvas. */
	timeline = glame_timeline_new(root);
	if (!timeline)
		return NULL;

	/* Create the window, with canvas, toolbar, etc. */
	window = gnome_app_new(caption, _(caption));
	gnome_app_set_contents(GNOME_APP(window), timeline);
	toolbar = gtk_toolbar_new(GTK_ORIENTATION_VERTICAL, GTK_TOOLBAR_ICONS);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Close", "Close", "Close",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CLOSE),
				NULL, NULL);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Help", "Help", "Help",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_HELP),
				NULL, NULL);
	gnome_app_add_toolbar(GNOME_APP(window), GTK_TOOLBAR(toolbar),
			      "timeline::toolbar",
			      GNOME_DOCK_ITEM_BEH_EXCLUSIVE|GNOME_DOCK_ITEM_BEH_NEVER_FLOATING,
			      GNOME_DOCK_RIGHT, 0, 0, 0);

	return window;
}
