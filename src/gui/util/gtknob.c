/*
 * gtknob.c
 *
 * $Id: gtknob.c,v 1.2 2002/02/24 18:30:30 richi Exp $
 *
 * Copyright (C) 2000 timecop@japan.co.jp
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

#include <stdio.h>
#include <gtk/gtk.h>
#include "gtknob.h"

/* statically include knob pixmap */
#include "gtknob.xpm"

static void gtk_knob_class_init(GtkKnobClass * klass);
static void gtk_knob_init(GtkKnob * knob);

enum {
    ARG_0,
    ARG_ADJUSTMENT
};

/* GtkObject functions */
static void gtk_knob_destroy(GtkObject * object);
static void gtk_knob_set_arg(GtkObject * object, GtkArg * arg,
			     guint arg_id);
static void gtk_knob_get_arg(GtkObject * object, GtkArg * arg,
			     guint arg_id);

/* GtkWidget functions */

static void gtk_knob_realize(GtkWidget * widget);
static void gtk_knob_unrealize(GtkWidget * widget);
static void gtk_knob_size_request(GtkWidget * widget,
				  GtkRequisition * requisition);
static void gtk_knob_size_allocate(GtkWidget * widget,
				   GtkAllocation * allocation);

/* events */
static gint gtk_knob_button_press(GtkWidget * widget,
				  GdkEventButton * event);
static gint gtk_knob_button_release(GtkWidget * widget,
				    GdkEventButton * event);
static gint gtk_knob_motion_notify(GtkWidget * widget,
				   GdkEventMotion * event);
/* end events */

static void gtk_knob_draw(GtkWidget * widget, GdkRectangle * area);
static void gtk_knob_draw_focus(GtkWidget * widget);

static gint gtk_knob_expose(GtkWidget * widget, GdkEventExpose * event);
static gint gtk_knob_focus_in(GtkWidget * widget, GdkEventFocus * event);
static gint gtk_knob_focus_out(GtkWidget * widget, GdkEventFocus * event);

/* adjustment hacks */
static void gtk_knob_set_adjustment(GtkKnob * knob,
				    GtkAdjustment * adjustment);
static void gtk_knob_adjustment_value_changed(GtkAdjustment * adjustment,
					      gpointer data);

/* GtkKnob-specific functions */

static void gtk_knob_paint(GtkKnob * knob, GdkRectangle * area);

static GtkWidgetClass *parent_class = NULL;

guint gtk_knob_get_type(void)
{
    static guint knob_type = 0;

    if (!knob_type) {
	static const GtkTypeInfo knob_info = {
	    "GtkKnob",
	    sizeof(GtkKnob),
	    sizeof(GtkKnobClass),
	    (GtkClassInitFunc) gtk_knob_class_init,
	    (GtkObjectInitFunc) gtk_knob_init,
	    /* reserved_1 */ NULL,
	    /* reserved_2 */ NULL,
	    (GtkClassInitFunc) NULL,
	};

	knob_type = gtk_type_unique(gtk_widget_get_type(), &knob_info);
    }

    return knob_type;
}

static void gtk_knob_class_init(GtkKnobClass * klass)
{
    GtkObjectClass *object_class;
    GtkWidgetClass *widget_class;

    object_class = (GtkObjectClass *) klass;
    widget_class = (GtkWidgetClass *) klass;

    parent_class = gtk_type_class(gtk_widget_get_type());

    gtk_object_add_arg_type("GtkKnob::adjustment",
			    GTK_TYPE_ADJUSTMENT,
			    GTK_ARG_READWRITE | GTK_ARG_CONSTRUCT,
			    ARG_ADJUSTMENT);

    object_class->destroy = gtk_knob_destroy;
    object_class->set_arg = gtk_knob_set_arg;
    object_class->get_arg = gtk_knob_get_arg;

    widget_class->realize = gtk_knob_realize;
    widget_class->unrealize = gtk_knob_unrealize;

    widget_class->size_request = gtk_knob_size_request;
    widget_class->size_allocate = gtk_knob_size_allocate;

    widget_class->button_press_event = gtk_knob_button_press;
    widget_class->button_release_event = gtk_knob_button_release;
    widget_class->motion_notify_event = gtk_knob_motion_notify;
    widget_class->expose_event = gtk_knob_expose;

    widget_class->draw = gtk_knob_draw;
    widget_class->draw_focus = gtk_knob_draw_focus;

    widget_class->focus_in_event = gtk_knob_focus_in;
    widget_class->focus_out_event = gtk_knob_focus_out;
}

static void gtk_knob_set_arg(GtkObject * object, GtkArg * arg,
			     guint arg_id)
{
    GtkKnob *knob;

    knob = GTK_KNOB(object);

    switch (arg_id) {
    case ARG_ADJUSTMENT:
	gtk_knob_set_adjustment(GTK_KNOB(knob), GTK_VALUE_POINTER(*arg));
	break;
    default:
	break;
    }
}

static void gtk_knob_get_arg(GtkObject * object, GtkArg * arg,
			     guint arg_id)
{
    GtkKnob *knob;

    knob = GTK_KNOB(object);

    switch (arg_id) {
    case ARG_ADJUSTMENT:
	GTK_VALUE_POINTER(*arg) = GTK_RANGE(knob);
	break;
    default:
	arg->type = GTK_TYPE_INVALID;
	break;
    }
}



static void gtk_knob_init(GtkKnob * knob)
{
    GTK_WIDGET_SET_FLAGS(GTK_WIDGET(knob), GTK_CAN_FOCUS);

    /* set knob to 0 initially */
    knob->value = 0;
    knob->old_value = 0;
    knob->button = 0;
    knob->x_click_point = -1;
    knob->y_click_point = -1;
    knob->adjustment = NULL;
}

GtkWidget *gtk_knob_new(GtkAdjustment * adj)
{
    GtkWidget *knob;

    knob = gtk_widget_new(GTK_TYPE_KNOB, "adjustment", adj, NULL);
    GTK_KNOB(knob)->value = (adj->value - adj->lower) / (adj->upper - adj->lower) * 53;

    return knob;
}

/* GtkObject functions */
static void gtk_knob_destroy(GtkObject * object)
{
    GtkKnob *knob;

    g_return_if_fail(object != NULL);
    g_return_if_fail(GTK_IS_KNOB(object));

    knob = GTK_KNOB(object);

    if (knob->adjustment)
	gtk_signal_disconnect_by_data(GTK_OBJECT(knob->adjustment),
				      (gpointer) knob);

    /* set value back to 0 */
    knob->value = 0;
    knob->old_value = 0;
    knob->button = 0;
    knob->x_click_point = -1;
    knob->y_click_point = -1;

    /* Chain up */
    if (GTK_OBJECT_CLASS(parent_class)->destroy)
	(*GTK_OBJECT_CLASS(parent_class)->destroy) (object);
}

/* GtkWidget functions */

static gint gtk_knob_button_press(GtkWidget * widget,
				  GdkEventButton * event)
{
    GtkKnob *knob;

    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_KNOB(widget), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    knob = GTK_KNOB(widget);

    if (knob->button == 0) {
	gtk_grab_add(widget);

	knob->button = event->button;
	knob->x_click_point = event->x;
	knob->y_click_point = event->y;
	knob->old_value = knob->value;
    }

    return FALSE;
}

static gint gtk_knob_button_release(GtkWidget * widget,
				    GdkEventButton * event)
{
    GtkKnob *knob;

    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_KNOB(widget), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    knob = GTK_KNOB(widget);

    if (knob->button == event->button) {
	gtk_grab_remove(widget);

	knob->button = 0;
	knob->x_click_point = -1;
	knob->y_click_point = -1;
	knob->old_value = knob->value;
    }
    return FALSE;
}

static gint gtk_knob_motion_notify(GtkWidget * widget,
				   GdkEventMotion * event)
{
    GtkKnob *knob;
    GdkModifierType mods;
    GdkRectangle rect;
    static gint oldvalue;
    gint x, y;
    gint temp;

    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_KNOB(widget), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    knob = GTK_KNOB(widget);

    if (knob->button != 0) {
	gdk_window_get_pointer(widget->window, &x, &y, &mods);

	temp = knob->old_value + (-(y - knob->y_click_point));
	if (temp < 0)
	    temp = 0;
	if (temp > 53)
	    temp = 53;

	knob->value = temp;

	if (knob->value != oldvalue) {
	    rect.x = 0;
	    rect.y = 0;
	    rect.width = widget->allocation.width;
	    rect.height = widget->allocation.height;
	    gtk_knob_draw(widget, &rect);
	    knob->adjustment->value =
		(knob->value / 53.0) * (knob->adjustment->upper - knob->adjustment->lower) + knob->adjustment->lower;
	    gtk_signal_emit_by_name(GTK_OBJECT(knob->adjustment),
				    "value_changed");

	    /* printf("adjustment: %f : %f : %f\n", knob->adjustment->upper,
	       knob->adjustment->lower, knob->adjustment->value); */

	    oldvalue = knob->value;
	}
    }


    return FALSE;
}

static void gtk_knob_realize(GtkWidget * widget)
{
    GdkWindowAttr attributes;
    gint attributes_mask;
    GtkKnob *knob;

    g_return_if_fail(widget != NULL);
    g_return_if_fail(GTK_IS_KNOB(widget));

    knob = GTK_KNOB(widget);

    /* Set realized flag */
    GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED);

    /* Main widget window */
    attributes.window_type = GDK_WINDOW_CHILD;
    attributes.x = widget->allocation.x;
    attributes.y = widget->allocation.y;
    attributes.width = widget->allocation.width;
    attributes.height = widget->allocation.height;
    attributes.wclass = GDK_INPUT_OUTPUT;
    attributes.visual = gtk_widget_get_visual(widget);
    attributes.colormap = gtk_widget_get_colormap(widget);
    attributes.event_mask = attributes.event_mask = GDK_ALL_EVENTS_MASK;

    attributes_mask =
	GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

    widget->window = gdk_window_new(gtk_widget_get_parent_window(widget),
				    &attributes, attributes_mask);
    gdk_window_set_user_data(widget->window, widget);

    /* the knob pixmap */
    knob->pixmap =
	gdk_pixmap_create_from_xpm_d(widget->window, NULL, NULL, knob_xpm);

    /* Style */
    widget->style = gtk_style_attach(widget->style, widget->window);
}

static void gtk_knob_unrealize(GtkWidget * widget)
{
    GtkKnob *knob;

    g_return_if_fail(widget != NULL);
    g_return_if_fail(GTK_IS_KNOB(widget));

    knob = GTK_KNOB(widget);

    /* Hide all windows */
    if (GTK_WIDGET_MAPPED(widget))
	gtk_widget_unmap(widget);

    GTK_WIDGET_UNSET_FLAGS(widget, GTK_MAPPED);

    /* delete the pixmap */
    gdk_pixmap_unref(knob->pixmap);

    /* This destroys widget->window and unsets the realized flag
     */
    if (GTK_WIDGET_CLASS(parent_class)->unrealize)
	(*GTK_WIDGET_CLASS(parent_class)->unrealize) (widget);
}

static void
gtk_knob_size_request(GtkWidget * widget, GtkRequisition * requisition)
{
    g_return_if_fail(widget != NULL);
    g_return_if_fail(GTK_IS_KNOB(widget));

    requisition->width = 30;
    requisition->height = 30;
}

static void
gtk_knob_size_allocate(GtkWidget * widget, GtkAllocation * allocation)
{
    GtkKnob *knob;

    g_return_if_fail(widget != NULL);
    g_return_if_fail(GTK_IS_KNOB(widget));

    knob = GTK_KNOB(widget);

    widget->allocation = *allocation;
    if (GTK_WIDGET_REALIZED(widget)) {
	gdk_window_move_resize(widget->window,
			       allocation->x,
			       allocation->y,
			       allocation->width, allocation->height);
    }
}

static void gtk_knob_draw(GtkWidget * widget, GdkRectangle * area)
{
    GtkKnob *knob;

    g_return_if_fail(widget != NULL);
    g_return_if_fail(GTK_IS_KNOB(widget));

    knob = GTK_KNOB(widget);

    gtk_knob_paint(knob, area);
}

static void gtk_knob_draw_focus(GtkWidget * widget)
{
    GdkRectangle rect;
    GtkKnob *knob;

    g_return_if_fail(widget != NULL);
    g_return_if_fail(GTK_IS_KNOB(widget));

    knob = GTK_KNOB(widget);

    rect.x = 0;
    rect.y = 0;
    rect.width = widget->allocation.width;
    rect.height = widget->allocation.height;

    if (GTK_WIDGET_DRAWABLE(knob))
	gtk_knob_paint(knob, &rect);
}

static gint gtk_knob_expose(GtkWidget * widget, GdkEventExpose * event)
{
    if (event->window == widget->window)
	gtk_knob_paint(GTK_KNOB(widget), &event->area);
    else
	g_assert_not_reached();

    return TRUE;
}

static gint gtk_knob_focus_in(GtkWidget * widget, GdkEventFocus * event)
{
    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_KNOB(widget), FALSE);

    GTK_WIDGET_SET_FLAGS(widget, GTK_HAS_FOCUS);
    gtk_widget_draw_focus(widget);

    return FALSE;
}

static gint gtk_knob_focus_out(GtkWidget * widget, GdkEventFocus * event)
{
    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_KNOB(widget), FALSE);

    GTK_WIDGET_UNSET_FLAGS(widget, GTK_HAS_FOCUS);
    gtk_widget_draw_focus(widget);

    return FALSE;
}

static void gtk_knob_paint(GtkKnob * knob, GdkRectangle * area)
{
    GtkWidget *widget;

    g_return_if_fail(knob != NULL);
    g_return_if_fail(GTK_IS_KNOB(knob));

    widget = GTK_WIDGET(knob);

    if (!GTK_WIDGET_DRAWABLE(widget))
	return;

    /* draw the knob pixmap */
    gdk_draw_pixmap(widget->window, widget->style->black_gc, knob->pixmap,
		    knob->value * 31, 0, 0, 0, 30, 30);

    if (GTK_WIDGET_HAS_FOCUS(widget)) {
	gtk_paint_focus(widget->style, widget->window,
			area, widget, "knob",
			widget->allocation.x, widget->allocation.y,
			widget->allocation.width - 1,
			widget->allocation.height - 1);
    }
}

void gtk_knob_set_adjustment(GtkKnob * knob, GtkAdjustment * adjustment)
{
    g_return_if_fail(knob != NULL);
    g_return_if_fail(GTK_IS_KNOB(knob));

    if (!adjustment)
	adjustment =
	    (GtkAdjustment *) gtk_adjustment_new(0.0, 0.0, 0.0, 0.0, 0.0,
						 0.0);
    else
	g_return_if_fail(GTK_IS_ADJUSTMENT(adjustment));

    if (knob->adjustment != adjustment) {
	if (knob->adjustment) {
	    gtk_signal_disconnect_by_data(GTK_OBJECT(knob->adjustment),
					  (gpointer) knob);
	    gtk_object_unref(GTK_OBJECT(knob->adjustment));
	}
	knob->adjustment = adjustment;
	gtk_object_ref(GTK_OBJECT(adjustment));
	gtk_object_sink(GTK_OBJECT(adjustment));

	gtk_signal_connect(GTK_OBJECT(adjustment), "value_changed",
			   (GtkSignalFunc)
			   gtk_knob_adjustment_value_changed,
			   (gpointer) knob);
    }
}

static void gtk_knob_adjustment_value_changed(GtkAdjustment * adjustment,
					      gpointer data)
{
    GtkKnob *knob;
    GtkWidget *widget;
    GdkRectangle rect;
    gint temp;

    g_return_if_fail(adjustment != NULL);
    g_return_if_fail(data != NULL);

    knob = GTK_KNOB(data);
    widget = GTK_WIDGET(data);

    temp = (adjustment->value - adjustment->lower) / (adjustment->upper - adjustment->lower) * 53;
    if (knob->value != temp) {
	knob->value = temp;
	rect.x = 0;
	rect.y = 0;
	rect.width = widget->allocation.width;
	rect.height = widget->allocation.height;
	gtk_knob_draw(widget, &rect);
    }
}

