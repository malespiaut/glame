/*
 * gtknob.c
 *
 * $Id: gtknob.c,v 1.17 2003/05/26 21:13:39 richi Exp $
 *
 * Copyright (C) 2000 timecop@japan.co.jp
 * Copyright (C) 2002 Richard Guenther, Laurent Georget
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details .

 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <gtk/gtk.h>
#ifdef HAVE_LIBGLADE
#include <glade/glade.h>
#include <glade/glade-build.h>
#endif
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

static void gtk_knob_draw(GtkWidget * widget, GdkRectangle * area);
static void gtk_knob_draw_focus(GtkWidget * widget);

static gint gtk_knob_expose(GtkWidget * widget, GdkEventExpose * event);
static gint gtk_knob_focus_in(GtkWidget * widget, GdkEventFocus * event);
static gint gtk_knob_focus_out(GtkWidget * widget, GdkEventFocus * event);

/* adjustment hacks */
static void gtk_knob_adjustment_value_changed(GtkAdjustment * adjustment,
					      gpointer data);



/* pixel / tick calculations */
static gint gtk_knob_value_to_frame(GtkKnob *knob, gfloat value)
{
	return (value - knob->adjustment->lower) / (knob->adjustment->upper - knob->adjustment->lower) * 53.0;
}
static gfloat gtk_knob_frame_to_value(GtkKnob *knob, gint frame)
{
	int i;
	for (i=0; i<knob->nr_ticks; i++)
		if (gtk_knob_value_to_frame(knob, knob->ticks[i]) == frame)
			return knob->ticks[i];
	return (frame / 53.0) * (knob->adjustment->upper - knob->adjustment->lower) + knob->adjustment->lower;
}


/* GtkKnob-specific functions */

static void gtk_knob_paint(GtkKnob * knob, GdkRectangle * area);

static GtkWidgetClass *parent_class = NULL;


static void gtk_knob_class_init(GtkKnobClass * klass)
{
    GtkObjectClass *object_class;
    GtkWidgetClass *widget_class;

    object_class = (GtkObjectClass *) klass;
    widget_class = (GtkWidgetClass *) klass;

    parent_class = GTK_OBJECT_CLASS(gtk_type_class(gtk_widget_get_type()));

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
    //FIXME
    //    widget_class->draw = gtk_knob_draw;
    //    widget_class->draw_focus = gtk_knob_draw_focus;

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
	gtk_knob_set_adjustment(GTK_KNOB(knob), GTK_ADJUSTMENT(GTK_VALUE_POINTER(*arg)));
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

static gchar *gtk_knob_standard_formatter(gfloat val, gpointer data)
{
	char buf[16];
	snprintf(buf, 16, "%.1f", val);
	return g_strdup(buf);
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
    knob->nr_ticks = 0;
    knob->formatter = gtk_knob_standard_formatter;
    knob->min_cache = NULL;
    knob->max_cache = NULL;
    knob->val_cache = NULL;
    knob->font = NULL;
    knob->gc = NULL;
}


/* GtkObject functions */
static void gtk_knob_destroy(GtkObject * object)
{
    GtkKnob *knob;

    g_return_if_fail(object != NULL);
    g_return_if_fail(GTK_IS_KNOB(object));

    knob = GTK_KNOB(object);

    if (knob->adjustment) {
	gtk_signal_disconnect_by_data(GTK_OBJECT(knob->adjustment),
				      (gpointer) knob);
	g_object_unref(knob->adjustment);
	knob->adjustment = NULL;
    }

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

    if (event->button == 1) {
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
    gint temp, i;

    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_KNOB(widget), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    knob = GTK_KNOB(widget);

    if (knob->button != 0) {
	gdk_window_get_pointer(widget->window, &x, &y, &mods);

	temp = knob->old_value + (-(y - knob->y_click_point));
	/* adjust temp for crossed ticks to which we stick a while */
	if (temp > knob->old_value) {
		for (i=0; i<knob->nr_ticks; i++) {
			if (gtk_knob_frame_to_value(knob, knob->old_value) > knob->ticks[i])
				continue;
			if (gtk_knob_frame_to_value(knob, temp) > knob->ticks[i]) {
				temp -= 5;
				if (gtk_knob_frame_to_value(knob, temp) < knob->ticks[i])
					temp = gtk_knob_value_to_frame(knob, knob->ticks[i]);
			}
		}
	} else if (temp < knob->old_value) {
		for (i=knob->nr_ticks-1; i>=0; i--) {
			if (gtk_knob_frame_to_value(knob, knob->old_value) < knob->ticks[i])
				continue;
			if (gtk_knob_frame_to_value(knob, temp) < knob->ticks[i]) {
				temp += 5;
				if (gtk_knob_frame_to_value(knob, temp) > knob->ticks[i])
					temp = gtk_knob_value_to_frame(knob, knob->ticks[i]);
			}
		}
	}
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
	    gtk_adjustment_set_value(knob->adjustment,
				     gtk_knob_frame_to_value(knob, knob->value));
	    oldvalue = knob->value;
	    gtk_knob_draw(widget, &rect);
	}
    }

    return FALSE;
}

static void gtk_knob_realize(GtkWidget * widget)
{
    GdkWindowAttr attributes;
    gint attributes_mask;
    GtkKnob *knob;
    static GdkColormap *colormap = NULL;
    GdkColor foreground,background;
    
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
    if(knob->font == NULL) {
	knob->font = gdk_font_load("-schumacher-clean-medium-r-normal-*-*-100-*-*-c-*-iso646.1991-irv" );
	if (knob->font == NULL) {puts("Font not found.");} /* FIXME : find a default font? */
    }
    if(colormap==NULL) colormap = gdk_colormap_get_system();
    knob->gc = gdk_gc_new(widget->window);
    if(gdk_color_parse("blue",&foreground)) {
        gdk_color_alloc(colormap,&foreground);
        gdk_gc_set_foreground(knob->gc,&foreground);
    }
    if(gdk_color_parse("white",&background)) {
        gdk_color_alloc(colormap,&background);
        /* gdk_gc_set_background(knob->gc,&background); */
	gdk_window_set_background(widget->window,&background);  
    }  
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

    requisition->width = 60;
    requisition->height = 50;
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
    //FIXME    gtk_widget_draw_focus(widget);

    return FALSE;
}

static gint gtk_knob_focus_out(GtkWidget * widget, GdkEventFocus * event)
{
    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_KNOB(widget), FALSE);

    GTK_WIDGET_UNSET_FLAGS(widget, GTK_HAS_FOCUS);
    //FIXME gtk_widget_draw_focus(widget);

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
		    knob->value * 31, 0, 15, 10, 30, 30);

    if (GTK_WIDGET_HAS_FOCUS(widget)) {
	    gtk_paint_focus(widget->style, widget->window,GTK_STATE_NORMAL,
			area, widget, "knob",
			widget->allocation.x, widget->allocation.y,
			widget->allocation.width - 1,
			widget->allocation.height - 1);
    }
    /* draw the min/max and value strings */
    if (!knob->min_cache)
	    knob->min_cache = knob->formatter(knob->adjustment->lower, knob->formatter_data);
    gdk_draw_string(widget->window,
		    knob->font,
		    knob->gc,
		    0,47,
		    knob->min_cache);
    if (!knob->max_cache)
	    knob->max_cache = knob->formatter(knob->adjustment->upper, knob->formatter_data);
    gdk_draw_string(widget->window,
		    knob->font,
		    knob->gc,
		    30,47,
		    knob->max_cache);
    if (!knob->val_cache)
      knob->val_cache = knob->formatter(knob->adjustment->value, knob->formatter_data);
    
    gdk_draw_rectangle (widget->window,widget->style->white_gc,TRUE,0,0,60,11);/* overlay  previous grabbed value*/
                                             
    gdk_draw_string(widget->window,
		    knob->font,
		    knob->gc,
		    18,10,
		    knob->val_cache);
}

static void gtk_knob_adjustment_value_changed(GtkAdjustment * adjustment,
					      gpointer data)
{
    GtkKnob *knob;
    GtkWidget *widget;
    GdkRectangle rect;
    gint temp;
    /* int nb; */
    g_return_if_fail(adjustment != NULL);
    g_return_if_fail(data != NULL);
    
    knob = GTK_KNOB(data);
    widget = GTK_WIDGET(data);

    /* update displayed value */
    if (knob->val_cache)
	    g_free(knob->val_cache);
    knob->val_cache = knob->formatter(adjustment->value, knob->formatter_data);
    temp = gtk_knob_value_to_frame(knob, adjustment->value);
    if (knob->value != temp) {
	knob->value = temp;
	
	rect.x = 0;
	rect.y = 0;
	rect.width = widget->allocation.width;
	rect.height = widget->allocation.height;
	
	gtk_knob_draw(widget, &rect);
    }
}


#ifdef HAVE_LIBGLADE
#include "glscript.h"

static gchar *gtk_knob_scheme_formatter(gfloat value, char *code)
{
	SCM format_s, res_s;
	char *res;
	int len;

	format_s = glame_gh_safe_eval_str(code);
	if (!gh_procedure_p(format_s))
		return g_strdup("Error");
	res_s = gh_call1(format_s, gh_double2scm(value));
	if (!gh_string_p(res_s))
		return g_strdup("Error");
	res = gh_scm2newstr(res_s, &len);
	return res;
}

static GtkWidget *gtk_knob_glade_new(GladeXML *xml, GladeWidgetInfo *info)
{
	GtkWidget *knob;
	GtkObject *adj;
	GList *tmp;
	float value = 0.0, lower = 0.0, upper = 1.0, tick;
	char *formatter = NULL;

	for (tmp = info->attributes; tmp; tmp = tmp->next) {
		GladeAttribute *attr = tmp->data;
		if (strcmp(attr->name, "value") == 0)
			sscanf(attr->value, "%f", &value);
		if (strcmp(attr->name, "lower") == 0)
			sscanf(attr->value, "%f", &lower);
		if (strcmp(attr->name, "upper") == 0)
			sscanf(attr->value, "%f", &upper);
		if (strcmp(attr->name, "formatter") == 0)
			formatter = attr->value;
	}
	adj = gtk_adjustment_new(value, lower, upper, 0.1, 0.1, 0.0);
	knob = gtk_knob_new(GTK_ADJUSTMENT(adj));
	if (formatter)
		gtk_knob_set_formatter(GTK_KNOB(knob), (GtkKnobFormatter)gtk_knob_scheme_formatter, g_strdup(formatter));
	for (tmp = info->attributes; tmp; tmp = tmp->next) {
		GladeAttribute *attr = tmp->data;
		if (strcmp(attr->name, "tick") == 0) {
			sscanf(attr->value, "%f", &tick);
			gtk_knob_add_tick(GTK_KNOB(knob), tick);
		}
	}

	return knob;
}
#endif



/*
 * Public interface.
 */

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

GtkWidget *gtk_knob_new(GtkAdjustment * adj)
{
    GtkWidget *knob;
    knob = gtk_widget_new(GTK_TYPE_KNOB, "adjustment", adj, NULL);
    GTK_KNOB(knob)->value = gtk_knob_value_to_frame(GTK_KNOB(knob), adj->value);
    return knob;
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

GtkAdjustment *gtk_knob_get_adjustment(GtkKnob *knob)
{
    return knob->adjustment;
}

void gtk_knob_set_formatter(GtkKnob *knob, GtkKnobFormatter f, gpointer data)
{
	knob->formatter = f;
	knob->formatter_data = data;
	if (knob->min_cache)
		g_free(knob->min_cache);
	if (knob->max_cache)
		g_free(knob->max_cache);
	if (knob->val_cache)
		g_free(knob->val_cache);
	knob->min_cache = NULL;
	knob->max_cache = NULL;
	knob->val_cache = NULL;
}

void gtk_knob_add_tick(GtkKnob *knob, gfloat tick)
{
	int i, j;
	if (knob->nr_ticks >= GTK_KNOB_MAX_TICKS)
		return;
	/* insert ticks sorted */
	for (i=0; i<knob->nr_ticks; i++)
		if (knob->ticks[i] > tick)
			break;
	for (j=knob->nr_ticks-1; j>=i; j--)
		knob->ticks[j+1] = knob->ticks[j];
	knob->ticks[i] = tick;
	knob->nr_ticks++;
}


#ifdef HAVE_LIBGLADE
void gtk_knob_glade_register()
{
	static GladeWidgetBuildData widgets[] = {
		{ "GtkKnob", gtk_knob_glade_new, NULL },
		{ NULL, NULL, NULL }
	};
	glade_register_widgets(widgets);
}
#else
void gtk_knob_glade_register()
{
}
#endif

