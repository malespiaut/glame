/*
 * glame_curve.c
 *
 * $Id: glame_curve.c,v 1.6 2001/09/25 11:23:36 xwolf Exp $
 *
 * Copyright (C) 2001 Johannes Hirche
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

#include <stdio.h>
#include "util.h"
#include "glame_curve.h"

#define RADIUS          3 

static guint curve_changed_signal = 0;

static void glame_curve_class_init(GlameCurveClass* klass)
{
	/* nothing to be done here, no signals... */
	GtkObjectClass *object_class;
  
	object_class = (GtkObjectClass *) klass;
  
	curve_changed_signal =
		gtk_signal_new ("curve_changed", GTK_RUN_FIRST, object_class->type,
				GTK_SIGNAL_OFFSET (GlameCurveClass, curve_changed),
				gtk_marshal_NONE__NONE, GTK_TYPE_NONE, 0);
	gtk_object_class_add_signals (object_class, &curve_changed_signal, 1);

}

static gint glame_curve_event(GtkWidget* widget, GdkEvent* event, GlameCurve* gcurve)
{
	GdkEventMotion* mevent;
	static gboolean changed = FALSE;
	switch(event->type){
	case GDK_MOTION_NOTIFY:
		mevent = (GdkEventMotion *) event;
		if((mevent->state & GDK_BUTTON1_MASK)||
		   (mevent->state & GDK_BUTTON2_MASK)||
		   (mevent->state & GDK_BUTTON3_MASK))
			changed = TRUE;
		/* FIXME this is not really necessary, could emit another signal here, curve_motion or something */
		break;
	case GDK_BUTTON_PRESS:
		changed = TRUE;
		break;
	case GDK_BUTTON_RELEASE:
		if(changed){
			gtk_signal_emit (GTK_OBJECT (gcurve), curve_changed_signal);
			changed = FALSE;
		}
		break;
	default:
		break;
	}
	return FALSE;
	
}
	

static void glame_curve_init(GlameCurve* gcurve)
{
	GtkCurve* curve=GTK_CURVE(gcurve);
	
	curve->cursor_type = GDK_TOP_LEFT_ARROW;
	curve->pixmap = NULL;
	curve->curve_type = GTK_CURVE_TYPE_SPLINE;
	curve->height = 0;
	curve->grab_point = -1;
	
	curve->num_points = 0;
	curve->point = 0;
	
	curve->num_ctlpoints = 0;
	curve->ctlpoint = NULL;
	
	curve->min_x = 0.0;
	curve->max_x = 1.0;
	curve->min_y = 0.0;
	curve->max_y = 1.0;
	gtk_signal_connect(GTK_OBJECT(gcurve),"event",
			   (GtkSignalFunc) glame_curve_event, curve);
}
 
GtkWidget* glame_curve_new(void)
{
	return GTK_WIDGET( gtk_type_new(glame_curve_get_type()));
}

GtkType glame_curve_get_type(void)
{
	static guint curve_type = 0;
	
	if(!curve_type){
		GtkTypeInfo curve_info = {
			"GlameCurve",
			sizeof(GlameCurve),
			sizeof(GlameCurveClass),
			(GtkClassInitFunc)glame_curve_class_init,
			(GtkObjectInitFunc)glame_curve_init,
			NULL,
			NULL
		};
		
		curve_type = gtk_type_unique(gtk_curve_get_type(),&curve_info);
	}
	
	return curve_type;
}


    
void glame_curve_set_control_vector(GlameCurve* curve,
				    int num_points,
				    gfloat (*ctl_points)[2])
{
	int width;
	GtkCurve* gCurve = GTK_CURVE(curve);
	for(width=0;width<num_points;width++){
		fprintf(stderr,"set: %f %f\n",ctl_points[width][0],ctl_points[width][1]);
	}

	gCurve->num_ctlpoints = num_points;
	if(gCurve->ctlpoint)
		g_free(gCurve->ctlpoint);
	gCurve->ctlpoint = malloc(gCurve->num_ctlpoints*sizeof(gfloat[2]));
	memcpy(gCurve->ctlpoint,ctl_points,gCurve->num_ctlpoints*sizeof(gfloat[2]));
	if(gCurve->num_points){
		g_free(gCurve->point);
		gCurve->point=NULL;
		gCurve->num_points=0;
	}
	for(width=0;width<num_points;width++){
		fprintf(stderr,"set: %f %f\n",ctl_points[width][0],ctl_points[width][1]);
	}
	/* FIXME */
	//glame_curve_redraw(gCurve);
	//	gtk_curve_set_curve_type(gCurve,GTK_CURVE_TYPE_SPLINE);//gCurve->curve_type);
	gtk_widget_queue_draw(GTK_WIDGET(gCurve));
}

void glame_curve_get_control_vector(GlameCurve* curve,
				    int* num_points,
				    gfloat (*(*ctl_points))[2])
{
	int width;
	GtkCurve* gCurve = GTK_CURVE(curve);
	*num_points = gCurve->num_ctlpoints;
	DPRINTF("Controlpoints: %d\n",*num_points);
	*ctl_points = malloc(gCurve->num_ctlpoints*2*sizeof(gfloat));
	memcpy(*ctl_points,gCurve->ctlpoint,gCurve->num_ctlpoints*2*sizeof(gfloat));
	for(width=0;width<*num_points;width++){
		DPRINTF("get: %f %f\n",
			(*ctl_points)[width][0],(*ctl_points)[width][1]);
	}
}


