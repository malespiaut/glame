/*
 * timeline.c
 *
 * $Id: timeline.c,v 1.24 2004/10/23 13:09:26 richi Exp $
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

#include <gnome.h>
#include "glscript.h"
#include "glame_accelerator.h"
#include "util/glame_gui_utils.h"
#include "util/glame_hruler.h"
#include "waveeditgui.h"
#include "timeline.h"


/* GUI is single threaded, so we may have some global state.
 */

static TimelineGui *active_timeline = NULL;


/* Ruler metric - time based.
 */

static gchar *time_metric_translate(gdouble value)
{
  gint minutes;
  gdouble seconds;

  minutes = ((gint) (value / 60.0));
  seconds = value - (((gint)value) / 60) * 60;

  return g_strdup_printf ("%02d:%02.3f", minutes, seconds);
}

static const GlameRulerMetric time_metric = {
  "Time Metric", "time", 1.0,
  24, (gdouble[24]){ 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0,
		     5.0, 15.0, 30.0, 45.0, 60.0, 90.0, 120.0,
		     180.0, 240.0, 360.0, 480.0, 720.0, 960.0,
		     1440.0, 1920.0, 2880.0, 3840.0 },
  5, (gint[5]){ 1, 5, 10, 30, 60 },
  time_metric_translate
};


/*
 * Gtk signal handlers for the timeline / the canvas items.
 */

static void handle_timeline_enter(GtkWidget *canvas, GdkEventCrossing *event,
				  TimelineGui *timeline)
{
	if (event->type == GDK_ENTER_NOTIFY) {
		DPRINTF("Entered timeline for %s\n",
			gpsm_item_label(timeline->canvas->root));
		active_timeline = timeline;
	}
}

static void handle_timeline_destroy(GtkWidget *canvas,
				    TimelineGui *timeline)
{
	active_timeline = NULL;
}

static void update_ruler_position(TimelineGui *timeline, long hposition)
{
	glame_ruler_set_range(timeline->ruler,
			      timeline->ruler->lower,
			      timeline->ruler->upper,
			      hposition/44100.0,
			      timeline->ruler->max_size);
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

		/* Not an interesting event - but update ruler. */
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

		if (!(mevent->state & GDK_BUTTON1_MASK)) {
			long hposition, dummy;
			timeline_canvas_item_w2gpsm(
				&hposition, &dummy,
				&dummy, &dummy,
				44100/* FIXME */,
				mevent->x, 0.0, 0.0, 0.0);
			update_ruler_position(active_timeline, hposition);
			break;
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
			double dx, dy, ddummy;
			long hposition, vposition, idy, dummy;
			/* Just move the rect (horizontally). */
			dx = mevent->x - dnd_start_x;
			dy = mevent->y - dnd_start_y;
#if 0 /* WTF is that?? [richi] */
			if (x1+dx < 0.0)
				dx = -x1;
			if (y1+dy < 0.0)
				dy = -y1;
#endif
			timeline_canvas_item_w2gpsm(
				&hposition, &vposition,
				&dummy, &dummy,
				gpsm_swfile_samplerate(TIMELINE_CI_GPSM(dnd_file)),
				x1+dx, y1+dy, 0.0, 0.0);
			if (gpsm_item_can_place(
				/* FIXME -- does not do cross-group moving
				 * canvas->active_group, */
				gpsm_item_parent(TIMELINE_CI_GPSM(dnd_file)),
				TIMELINE_CI_GPSM(dnd_file),
				hposition, vposition)) {
				DPRINTF("Can place at %li, %li\n",
					hposition, vposition);
				timeline_canvas_item_w2gpsm(
					&dummy, &idy,&dummy,&dummy,
					gpsm_swfile_samplerate(TIMELINE_CI_GPSM(dnd_file)),
					0.0,dy,0.0,0.0);
				timeline_canvas_item_gpsm2w(
					0,idy,0,0,
					gpsm_swfile_samplerate(TIMELINE_CI_GPSM(dnd_file)),
					&ddummy,&dy,&ddummy,&ddummy);
				gnome_canvas_item_set(dnd_rect,
						      "x1", x1+dx,
						      "x2", x2+dx, 
						      "y1", y1+dy,
						      "y2", y2+dy, NULL);
				gnome_canvas_item_raise_to_top(dnd_rect);
				gnome_canvas_item_request_update(dnd_rect);
				DPRINTF("D&D moved by %.1f, %.1f\n", dx, dy);

				update_ruler_position(active_timeline, hposition);
			} else
				DPRINTF("Cannot place at %li, %li\n",
					hposition, vposition);
		}
		break;
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
			long hposition, vposition, dummy;

			gtk_object_get(GTK_OBJECT(dnd_rect),
				       "x1", &x1, "x2", &x2,
				       "y1", &y1, "y2", &y2,
				       NULL);
			timeline_canvas_item_w2gpsm(
				&hposition, &vposition,
				&dummy, &dummy,
				gpsm_swfile_samplerate(TIMELINE_CI_GPSM(dnd_file)),
				x1, y1, x2, y2);
			DPRINTF("D&D cleanup (drop rect %.3f %.3f - %.3f %.3f) to %li (original %li)\n",
				x1, y1, x2, y2,
				hposition, gpsm_item_hposition(TIMELINE_CI_GPSM(dnd_file)));
			gpsm_item_place(
				//canvas->active_group,
				gpsm_item_parent(TIMELINE_CI_GPSM(dnd_file)),
				TIMELINE_CI_GPSM(dnd_file),
				hposition, vposition);//gpsm_item_vposition(TIMELINE_CI_GPSM(dnd_file)));

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
	glame_ruler_set_range(canvas->ruler,
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

static void zoom_in_cb(GtkWidget *widget, TimelineGui *timeline)
{
	timeline_canvas_scale(timeline->canvas, 2.0);
}

static void zoom_out_cb(GtkWidget *widget, TimelineGui *timeline)
{
	timeline_canvas_scale(timeline->canvas, 0.5);
}

static void close_cb(GtkWidget *widget, TimelineGui *timeline)
{
	gtk_widget_destroy(GTK_WIDGET(timeline));
	active_timeline = NULL;
}

static void help_cb(GtkWidget *widget, void *foo)
{
	glame_help_goto(NULL, "info:glame#The_Timeline");
}

static void ruler_update_cb(GtkAdjustment *adjustment, TimelineGui *timeline)
{
	long hpos, hsize, dummy;
	double scale;
	GnomeCanvasItem *root = GNOME_CANVAS_ITEM(gnome_canvas_root(GNOME_CANVAS(timeline->canvas)));
	if (root->object.flags & GNOME_CANVAS_ITEM_AFFINE_FULL)
		scale = root->xform[0];
	else
		scale = 1.0;
	timeline_canvas_item_w2gpsm(&hpos, &dummy, &hsize, &dummy,
				    44100,
				    adjustment->value, 0.0,
				    adjustment->value + adjustment->page_size,
				    0.0);
	glame_ruler_set_range(timeline->ruler,
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
		TimelineGui *timeline;
		GnomeCanvasItem *citem;
		gpsm_grp_t *root;
		gpsm_item_t *item;

		timeline = TIMELINE_GUI(glsig_handler_private(handler));
		GLSIGH_GETARGS2(va, root, item);
		if (timeline->canvas->root != root)
			DPRINTF("FUCK!\n");

		citem = timeline_canvas_find_gpsm_item(
			gnome_canvas_root(GNOME_CANVAS(timeline->canvas)), item);
		if (!citem)
			DPRINTF("FUCK2\n");
		if ((void *)citem == (void *)timeline->canvas->active_item)
			timeline->canvas->active_item = NULL;
		if ((void *)citem == (void *)timeline->canvas->active_group)
			timeline->canvas->active_group = NULL;
		gtk_object_destroy(GTK_OBJECT(citem));

		break;
	}
	case GPSM_SIG_GRP_NEWITEM: {
		TimelineGui *timeline;
		gpsm_grp_t *root;
		gpsm_item_t *item;

		timeline = TIMELINE_GUI(glsig_handler_private(handler));
		GLSIGH_GETARGS2(va, root, item);

		handle_grp_add_item(GNOME_CANVAS_GROUP(
			gnome_canvas_root(GNOME_CANVAS(timeline->canvas))), item);

		break;
	}
	case GPSM_SIG_ITEM_DESTROY: {
		TimelineGui *timeline;

		timeline = TIMELINE_GUI(glsig_handler_private(handler));
		gtk_widget_destroy(GTK_WIDGET(timeline));
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
		gtk_signal_connect(GTK_OBJECT(grp), "event",
				   (GtkSignalFunc)group_event,NULL);

	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		TimelineCanvasFile *file;

		DPRINTF("Adding file %s\n", gpsm_item_label(item));
		file = timeline_canvas_file_new(group, (gpsm_swfile_t *)item);
		/* FIXME: add handlers (gpsm) */
		TIMELINE_CANVAS_ITEM(file)->gpsm_handler = glsig_add_handler(
			gpsm_item_emitter(item), GPSM_SIG_ITEM_CHANGED,
			handle_file, file);
		gtk_signal_connect(GTK_OBJECT(file), "event",
				   (GtkSignalFunc)file_event, NULL);
	}
}




/*
 * Public API for the timeline.
 */

static SCM gls_timeline_root()
{
	if (!active_timeline)
		return SCM_BOOL_F;
	return gpsmitem2scm((gpsm_item_t *)active_timeline->canvas->root);
}

static SCM gls_timeline_active_group()
{
	if (!active_timeline || !active_timeline->canvas->active_group)
		return SCM_BOOL_F;
	return gpsmitem2scm(TIMELINE_CANVAS_ITEM(active_timeline->canvas->active_group)->item);
}

static SCM gls_timeline_active_item()
{
	if (!active_timeline || !active_timeline->canvas->active_item)
		return SCM_BOOL_F;
	return gpsmitem2scm(active_timeline->canvas->active_item->item);
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

static void timeline_gui_destroy(GtkObject *timeline)
{
	GnomeAppClass* parent_class;
	parent_class = gtk_type_class(gnome_app_get_type());
	GTK_OBJECT_CLASS(parent_class)->destroy(timeline);
}

static void timeline_gui_class_init(TimelineGuiClass *klass)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(klass);
	object_class->destroy = timeline_gui_destroy;
}

static void timeline_gui_init(TimelineGui *timeline)
{
	timeline->canvas = NULL;
}

GtkType timeline_gui_get_type(void)
{
	static GtkType timeline_gui_type = 0;
	
	if (!timeline_gui_type){
		GtkTypeInfo timeline_gui_info = {
			"TimelineGui",
			sizeof(TimelineGui),
			sizeof(TimelineGuiClass),
			(GtkClassInitFunc)timeline_gui_class_init,
			(GtkObjectInitFunc)timeline_gui_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		timeline_gui_type = gtk_type_unique(
			gnome_app_get_type(), &timeline_gui_info);
	}

	return timeline_gui_type;
}

GtkWidget *glame_timeline_new_with_window(const char *caption,
					  gpsm_grp_t *root)
{
	GtkWidget *swindow, *toolbar, *vbox;
	TimelineGui *window;
	gpsm_item_t *item;

	if (!root || !GPSM_ITEM_IS_GRP(root))
		return NULL;

	window = TIMELINE_GUI(gtk_type_new(timeline_gui_get_type()));
	gnome_app_construct(GNOME_APP(window), "glame0.7", _(caption));


	/* Construct a scrolled window and embed the
	 * TimelineCanvas inside it.
	 */

	swindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(swindow),
				       GTK_POLICY_ALWAYS,
				       GTK_POLICY_AUTOMATIC);
	gtk_widget_push_visual(gdk_rgb_get_visual());
	gtk_widget_push_colormap(gdk_rgb_get_cmap());
	window->canvas = timeline_canvas_new(root);
	gtk_widget_pop_colormap();
	gtk_widget_pop_visual();
	gtk_container_add(GTK_CONTAINER(swindow), GTK_WIDGET(window->canvas));
	gtk_widget_show(GTK_WIDGET(window->canvas));
	gtk_widget_show(swindow);

	/* Add all existing childs of root. */
	gpsm_grp_foreach_item(root, item)
		handle_grp_add_item(gnome_canvas_root(GNOME_CANVAS(window->canvas)),
				    item);

	/* Add handler for item additions/removal in root group. */
	TIMELINE_CANVAS(window->canvas)->gpsm_handler1 = glsig_add_handler(
		gpsm_item_emitter(root), GPSM_SIG_GRP_NEWITEM
		|GPSM_SIG_GRP_REMOVEITEM|GPSM_SIG_ITEM_DESTROY,
		handle_root, window);

	/* Add handler for enter events to track the active timeline. */
	gtk_signal_connect(GTK_OBJECT(window->canvas),"event",
			   (GtkSignalFunc)root_event, NULL);
	gtk_signal_connect(GTK_OBJECT(window->canvas), "enter_notify_event",
			   (GtkSignalFunc)handle_timeline_enter, window);
	gtk_signal_connect(GTK_OBJECT(window->canvas), "destroy",
			   (GtkSignalFunc)handle_timeline_destroy, window);


	/* Construct the toplevel window with ruler and toolbar
	 * where we embed the scrolled window with the canvas.
	 */

	gtk_window_set_default_size(GTK_WINDOW(window), 500, 300);
	vbox = gtk_vbox_new(FALSE, 5);
	window->ruler = GLAME_RULER(glame_hruler_new());
	glame_ruler_set_metric(window->ruler, &time_metric);
	glame_ruler_set_range(window->ruler, 0.0, 100.0, 0.0, 1000.0);
	gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(window->ruler),
			   FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(vbox), swindow);
	gtk_widget_show(GTK_WIDGET(window->ruler));
	gtk_widget_show(swindow);
	gtk_widget_show(vbox);
	gtk_signal_connect(GTK_OBJECT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(swindow))), "changed",
			   (GtkSignalFunc)ruler_update_cb, window);
	gtk_signal_connect(GTK_OBJECT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(swindow))), "value_changed",
			   (GtkSignalFunc)ruler_update_cb, window);
	gnome_app_set_contents(GNOME_APP(window), vbox);
	toolbar = gtk_toolbar_new();
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Zoom in", "Zooms in","foo",
				glame_load_icon_widget("zoom_in.png",24,24),
				GTK_SIGNAL_FUNC(zoom_in_cb), window);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
				"Zoom out", "Zooms out","foo",
				glame_load_icon_widget("zoom_out.png",24,24),
				GTK_SIGNAL_FUNC(zoom_out_cb), window);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),
				 "Close", "Close", 
				 GNOME_STOCK_PIXMAP_CLOSE,
				 GTK_SIGNAL_FUNC(close_cb), window,-1);
	gtk_toolbar_insert_stock(GTK_TOOLBAR(toolbar),
				 "Help", "Help", 
				 GNOME_STOCK_PIXMAP_HELP,
				 GTK_SIGNAL_FUNC(help_cb), NULL,-1);
	gnome_app_add_toolbar(GNOME_APP(window), GTK_TOOLBAR(toolbar),
			      "timeline::toolbar",
			      BONOBO_DOCK_ITEM_BEH_EXCLUSIVE|BONOBO_DOCK_ITEM_BEH_NEVER_FLOATING,
			      BONOBO_DOCK_RIGHT, 0, 0, 0);

	/* Add accelerator handler. */
	glame_accel_install(GTK_WIDGET(window), "timeline", NULL);

	return GTK_WIDGET(window);
}
