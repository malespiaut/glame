/*
 * timeline.c
 * $Id: timeline.c,v 1.9 2001/06/27 09:19:12 richi Exp $
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
#include "glscript.h"
#include "glame_accelerator.h"
#include "util/glame_gui_utils.h"
#include "waveeditgui.h"
#include "canvas_types.h"
#include "timeline.h"


/* GUI is single threaded, so we may have some global state.
 */

static TimelineCanvas *active_timeline = NULL;


/*
 * Gtk signal handlers for the timeline / the canvas items.
 */

static void handle_timeline_enter(GtkWidget *canvas, GdkEventCrossing *event,
				  TimelineCanvas *timeline)
{
	if (event->type == GDK_ENTER_NOTIFY) {
		DPRINTF("Entered timeline for %s\n",
			gpsm_item_label(timeline->root));
		active_timeline = timeline;
	}
}

static void handle_timeline_destroy(GtkWidget *canvas,
				    TimelineCanvas *timeline)
{
	active_timeline = NULL;
}

static void update_ruler_position(TimelineCanvas *canvas, long hposition)
{
	gtk_ruler_set_range(canvas->ruler,
			    canvas->ruler->lower,
			    canvas->ruler->upper,
			    hposition/44100.0,
			    canvas->ruler->max_size);
}

static gboolean file_event(TimelineCanvasFile* file, GdkEvent* event,
			   gpointer data)
{
	TimelineCanvas *canvas = TIMELINE_CI_CANVAS(file);
	/* Move, Drag'n'Drop state machine globals. */
	static GnomeCanvasItem *dnd_rect = NULL;
	static TimelineCanvasFile *dnd_file = NULL;
	static double dnd_start_x, dnd_start_y;

	switch(event->type){
	case GDK_ENTER_NOTIFY: {
		DPRINTF("Entered item %s / group %s\n",
			gpsm_item_label(TIMELINE_CANVAS_ITEM(file)->item),
			gpsm_item_label(gpsm_item_parent(TIMELINE_CI_GPSM(file))));
		if (canvas->active_item)
			timeline_canvas_item_highlight(canvas->active_item,
						       FALSE);
		if (canvas->active_group) {
			timeline_canvas_item_highlight(TIMELINE_CANVAS_ITEM(canvas->active_group),
						       FALSE);
			canvas->active_group = NULL;
		}
		if (TIMELINE_IS_CANVAS_GROUP(TIMELINE_CI_GROUP(file))) {
			canvas->active_group = TIMELINE_CANVAS_GROUP(TIMELINE_CI_GROUP(file));
			timeline_canvas_item_highlight(TIMELINE_CANVAS_ITEM(canvas->active_group),
						       TRUE);
		}
		canvas->active_item = TIMELINE_CANVAS_ITEM(file);
		timeline_canvas_item_highlight(TIMELINE_CANVAS_ITEM(file), TRUE);
		break;
	}
	case GDK_2BUTTON_PRESS: {
		DPRINTF("Waveedit for %s (%p)\n",
			gpsm_item_label(TIMELINE_CANVAS_ITEM(file)->item),
			TIMELINE_CANVAS_ITEM(file)->item);
		if(event->button.button==1)
			gtk_widget_show(GTK_WIDGET(glame_waveedit_gui_new(gpsm_item_label(TIMELINE_CANVAS_ITEM(file)->item),TIMELINE_CANVAS_ITEM(file)->item)));
		break;
	}
	case GDK_MOTION_NOTIFY: {
		/* Move, Drag'n'Drop state machine.
		 */

		GdkEventMotion *mevent = (GdkEventMotion *)event;
		double x1, x2, y1, y2, scale;

		/* Not an interesting event. */
		if (!(mevent->state & GDK_BUTTON1_MASK))
			break;

		/* CAVEAT: the event coordinates dont seem to honour
		 *         the canvas root groups affine (zoom) - we
		 *         have to explicitly handle it. Ugh. */
		{
			GnomeCanvasItem *root = TIMELINE_CI_ROOTI(file);
			if (root->object.flags & GNOME_CANVAS_ITEM_AFFINE_FULL)
				scale = root->xform[0];
			else
				scale = 1.0;
			mevent->x /= scale;
		}

		/* We need file and the files rect params anyway. */
		if (!dnd_file)
			dnd_file = file;
		timeline_canvas_item_gpsm2w(
			gpsm_item_hposition(TIMELINE_CI_GPSM(dnd_file)),
			gpsm_item_vposition(TIMELINE_CI_GPSM(dnd_file)),
			gpsm_item_hsize(TIMELINE_CI_GPSM(dnd_file)),
			gpsm_item_vsize(TIMELINE_CI_GPSM(dnd_file)),
			gpsm_swfile_samplerate(TIMELINE_CI_GPSM(dnd_file)),
			&x1, &y1, &x2, &y2);

		/* We may need to startup the machine (i.e
		 * create the temporary canvas object(s)). */
		if (!dnd_rect) {
			dnd_start_x = mevent->x;
			dnd_start_y = mevent->y;
			DPRINTF("D&D start pos %.3f %.3f\n",
				dnd_start_x, dnd_start_y);
			dnd_rect = gnome_canvas_item_new(
				TIMELINE_CI_GROUP(file),
				gnome_canvas_rect_get_type(),
				"x1", x1,
				"y1", y1,
				"x2", x2,
				"y2", y2,
				"outline_color", "red",
				"width_units", 3.0,
				"fill_color", NULL,
				NULL);
			gnome_canvas_item_raise_to_top(dnd_rect);
			gnome_canvas_item_show(dnd_rect);

		} else {
			double dx, dy;
			long hposition, dummy;
			/* Just move the rect (horizontally). */
			dx = mevent->x - dnd_start_x;
			dy = mevent->y - dnd_start_y;
			if (x1+dx < 0.0)
				dx = -x1;
			gnome_canvas_item_set(dnd_rect,
					      "x1", x1+dx,
					      "x2", x2+dx, NULL);
			gnome_canvas_item_request_update(dnd_rect);
			DPRINTF("D&D moved by %.1f, %.1f\n", dx, dy);
			timeline_canvas_item_w2gpsm(
				&hposition, &dummy,
				&dummy, &dummy,
				gpsm_swfile_samplerate(TIMELINE_CI_GPSM(dnd_file)),
				x1+dx, 0.0, 0.0, 0.0);
			update_ruler_position(active_timeline, hposition);
		}
	}
	case GDK_BUTTON_RELEASE: {
		/* Move, Drag'n'Drop state machine. Possible finish.
		 */

		GdkEventButton *bevent = (GdkEventButton *)event;

		/* Not an interesting event. */
		if (bevent->button != 1)
			break;

		if (dnd_rect) {
			double x1, x2, y1, y2;
			long hposition, dummy;

			gtk_object_get(GTK_OBJECT(dnd_rect),
				       "x1", &x1, "x2", &x2,
				       "y1", &y1, "y2", &y2,
				       NULL);
			timeline_canvas_item_w2gpsm(
				&hposition, &dummy,
				&dummy, &dummy,
				gpsm_swfile_samplerate(TIMELINE_CI_GPSM(dnd_file)),
				x1, y1, x2, y2);
			DPRINTF("D&D cleanup (drop rect %.3f %.3f - %.3f %.3f) to %li (original %li)\n",
				x1, y1, x2, y2,
				hposition, gpsm_item_hposition(TIMELINE_CI_GPSM(dnd_file)));
			TIMELINE_CI_GPSM(dnd_file)->hposition = hposition;
			glsig_emit(gpsm_item_emitter(TIMELINE_CI_GPSM(dnd_file)), GPSM_SIG_ITEM_CHANGED, TIMELINE_CI_GPSM(dnd_file));

			gtk_object_destroy(GTK_OBJECT(dnd_rect));
			dnd_file = NULL;
			dnd_rect = NULL;
		}
	}
	default:
		break;
	}
	return FALSE;
}

static gboolean group_event(TimelineCanvasGroup* grp, GdkEvent* event,
			    gpointer data)
{
	switch(event->type){
	default:
		break;
	}
	return FALSE;
}

static gboolean root_event(TimelineCanvas* canvas, GdkEvent* event,
			   gpointer data)
{
	switch(event->type){
#if 0
	case GDK_MOTION_NOTIFY: {
	long hpos, dummy;
	double scale;
	GnomeCanvasItem *root = gnome_canvas_root(GNOME_CANVAS(canvas));
	if (root->object.flags & GNOME_CANVAS_ITEM_AFFINE_FULL)
		scale = root->xform[0];
	else
		scale = 1.0;
	timeline_canvas_item_w2gpsm(&hpos, &dummy, &dummy, &dummy,
				    44100,
				    ((GdkEventMotion *)event)->x,
				    0.0, 0.0, 0.0);
	gtk_ruler_set_range(canvas->ruler,
			    canvas->ruler->lower,
			    canvas->ruler->upper,
			    canvas->ruler->lower + hpos/44100.0/scale,
			    canvas->ruler->max_size);
	}
#endif
	default:
		break;
	}
	return FALSE;
}



/*
 * Menu / toolbar handlers.
 */

static void zoom_in_cb(GtkWidget *widget, TimelineCanvas *timeline)
{
	timeline_canvas_scale(timeline, 2.0);
}

static void zoom_out_cb(GtkWidget *widget, TimelineCanvas *timeline)
{
	timeline_canvas_scale(timeline, 0.5);
}

static void close_cb(GtkWidget *widget, GtkWidget *timeline)
{
	gtk_widget_destroy(timeline);
	active_timeline = NULL;
}

static void help_cb(GtkWidget *widget, void *foo)
{
	gnome_help_goto(NULL, "info:glame#The_Timeline");
}

static void ruler_update_cb(GtkAdjustment *adjustment, TimelineCanvas *canvas)
{
	long hpos, hsize, dummy;
	double scale;
	GnomeCanvasItem *root = GNOME_CANVAS_ITEM(gnome_canvas_root(GNOME_CANVAS(canvas)));
	if (root->object.flags & GNOME_CANVAS_ITEM_AFFINE_FULL)
		scale = root->xform[0];
	else
		scale = 1.0;
	timeline_canvas_item_w2gpsm(&hpos, &dummy, &hsize, &dummy,
				    44100,
				    adjustment->value, 0.0,
				    adjustment->value + adjustment->page_size,
				    0.0);
	gtk_ruler_set_range(canvas->ruler,
			    hpos/44100.0/scale,
			    (hpos + hsize)/44100.0/scale,
			    hpos/44100.0/scale /* FIXME - track mouse */,
			    (hpos + hsize)/44100.0/scale);
}


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

		timeline_canvas_item_update(TIMELINE_CANVAS_ITEM(group));
		break;
	}
	case GPSM_SIG_GRP_REMOVEITEM: {
		TimelineCanvasGroup *group;
		TimelineCanvas *canvas;
		GnomeCanvasItem *citem;
		gpsm_grp_t *grp;
		gpsm_item_t *item;

		group = TIMELINE_CANVAS_GROUP(glsig_handler_private(handler));
		GLSIGH_GETARGS2(va, grp, item);
		if (TIMELINE_CANVAS_ITEM(group)->item != (gpsm_item_t *)grp)
			DPRINTF("FUCK!\n");
		canvas = TIMELINE_CI_CANVAS(group);

		citem = timeline_canvas_find_gpsm_item(GNOME_CANVAS_GROUP(group), item);
		if (!citem)
			DPRINTF("FUCK2\n");
		if ((void *)citem == (void *)canvas->active_item)
			canvas->active_item = NULL;
		if ((void *)citem == (void *)canvas->active_group)
			canvas->active_group = NULL;
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

		timeline_canvas_item_update(TIMELINE_CANVAS_ITEM(file));
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
		if ((void *)citem == (void *)canvas->active_item)
			canvas->active_item = NULL;
		if ((void *)citem == (void *)canvas->active_group)
			canvas->active_group = NULL;
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
		gtk_signal_connect(GTK_OBJECT(grp),"event",group_event,NULL);

	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		TimelineCanvasFile *file;

		DPRINTF("Adding file %s\n", gpsm_item_label(item));
		file = timeline_canvas_file_new(group, (gpsm_swfile_t *)item);
		/* FIXME: add handlers (gpsm) */
		TIMELINE_CANVAS_ITEM(file)->gpsm_handler = glsig_add_handler(
			gpsm_item_emitter(item), GPSM_SIG_ITEM_CHANGED,
			handle_file, file);
		gtk_signal_connect(GTK_OBJECT(file),"event",file_event,NULL);
	}
}




/*
 * Public API for the timeline.
 */

static SCM gls_timeline_root()
{
	if (!active_timeline)
		return SCM_BOOL_F;
	return gpsmitem2scm(active_timeline->root);
}

static SCM gls_timeline_active_group()
{
	if (!active_timeline || !active_timeline->active_group)
		return SCM_BOOL_F;
	return gpsmitem2scm(TIMELINE_CANVAS_ITEM(active_timeline->active_group)->item);
}

static SCM gls_timeline_active_item()
{
	if (!active_timeline || !active_timeline->active_item)
		return SCM_BOOL_F;
	return gpsmitem2scm(active_timeline->active_item->item);
}

void glame_timeline_init()
{
	gh_new_procedure0_0("timeline-root",
			    gls_timeline_root);
	gh_new_procedure0_0("timeline-active-group",
			    gls_timeline_active_group);
	gh_new_procedure0_0("timeline-active-item",
			    gls_timeline_active_item);
}

GtkWidget *glame_timeline_new(gpsm_grp_t *root)
{
	GtkWidget *window, *canvas;
	gpsm_item_t *item;

	if (!root || !GPSM_ITEM_IS_GRP(root))
		return NULL;

	window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(window),
				       GTK_POLICY_ALWAYS,
				       GTK_POLICY_AUTOMATIC);
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
	TIMELINE_CANVAS(canvas)->gpsm_handler1 = glsig_add_handler(
		gpsm_item_emitter(root), GPSM_SIG_GRP_NEWITEM
		|GPSM_SIG_GRP_REMOVEITEM, handle_root, canvas);

	/* Add handler for enter events to track the active timeline. */
	gtk_signal_connect(GTK_OBJECT(canvas),"event", root_event, NULL);
	gtk_signal_connect(GTK_OBJECT(canvas), "enter_notify_event",
			   handle_timeline_enter, canvas);
	gtk_signal_connect(GTK_OBJECT(canvas), "destroy",
			   handle_timeline_destroy, canvas);

	return window;
}

GtkWidget *glame_timeline_new_with_window(const char *caption,
					  gpsm_grp_t *root)
{
	GtkWidget *timeline, *window, *toolbar, *vbox, *ruler;
	TimelineCanvas *canvas;

	if (!root || !GPSM_ITEM_IS_GRP(root))
		return NULL;

	/* Create the timeline canvas. */
	timeline = glame_timeline_new(root);
	if (!timeline)
		return NULL;
	canvas = TIMELINE_CANVAS(GTK_BIN(timeline)->child);

	/* Create the window, with canvas, toolbar, etc. */
	window = gnome_app_new(caption, _(caption));
	gtk_window_set_default_size(GTK_WINDOW(window), 500, 300);
	vbox = gtk_vbox_new(FALSE, 5);
	ruler = gtk_hruler_new();
	canvas->ruler = GTK_RULER(ruler);
	gtk_ruler_set_metric(GTK_RULER(ruler), GTK_PIXELS);
	gtk_ruler_set_range(GTK_RULER(ruler), 0.0, 100.0, 0.0, 1000.0);
	gtk_box_pack_start(GTK_BOX(vbox), ruler, FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(vbox), timeline);
	gtk_widget_show(ruler);
	gtk_widget_show(timeline);
	gtk_widget_show(vbox);
	gtk_signal_connect(GTK_OBJECT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(timeline))), "changed",
			   ruler_update_cb, canvas);
	gtk_signal_connect(GTK_OBJECT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(timeline))), "value_changed",
			   ruler_update_cb, canvas);
	gnome_app_set_contents(GNOME_APP(window), vbox);
	toolbar = gtk_toolbar_new(GTK_ORIENTATION_VERTICAL, GTK_TOOLBAR_ICONS);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Zoom in", "Zooms in","foo",
				glame_load_icon_widget("zoom_in.png",24,24),
				zoom_in_cb,TIMELINE_CANVAS(GTK_BIN(timeline)->child));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Zoom out", "Zooms out","foo",
				glame_load_icon_widget("zoom_out.png",24,24),
				zoom_out_cb,TIMELINE_CANVAS(GTK_BIN(timeline)->child));
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Close", "Close", "Close",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CLOSE),
				close_cb, window);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Help", "Help", "Help",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_HELP),
				help_cb, NULL);
	gnome_app_add_toolbar(GNOME_APP(window), GTK_TOOLBAR(toolbar),
			      "timeline::toolbar",
			      GNOME_DOCK_ITEM_BEH_EXCLUSIVE|GNOME_DOCK_ITEM_BEH_NEVER_FLOATING,
			      GNOME_DOCK_RIGHT, 0, 0, 0);

	/* Add accelerator handler. */
	glame_accel_install(GTK_WIDGET(window), "timeline", NULL);

	return window;
}
