#ifndef _GLAME_TIMELINE_H
#define _GLAME_TIMELINE_H

/*
 * timeline.h
 * $Id: timeline.h,v 1.3 2001/07/06 12:14:53 mag Exp $
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

#include <gtk/gtkwidget.h>
#include "gpsm.h"
#include "timeline_canvas.h"


struct _TimelineGuiClass;
struct _TimelineGui;
typedef struct _TimelineGuiClass TimelineGuiClass;
typedef struct _TimelineGui TimelineGui;

#define TIMELINE_GUI_TYPE (timeline_gui_get_type())
#define TIMELINE_GUI(object) (GTK_CHECK_CAST((object), TIMELINE_GUI_TYPE, TimelineGui))
#define TIMELINE_GUI_CLASS(object) (GTK_CHECK_CLASS_CAST((object), TIMELINE_GUI_TYPE, TimelineGuiClass))
#define IS_TIMELINE_GUI(object) (GTK_CHECK_TYPE((object), TIMELINE_GUI_TYPE))
#define IS_TIMELINE_GUI_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), TIMELINE_GUI_TYPE))
#define TIMELINE_GUI_GET_CLASS(object) ((TimelineGuiClass*) (((GtkObject*) (obj))->klass))

struct _TimelineGuiClass {
	GnomeAppClass parent_class;

};

struct _TimelineGui {
	GnomeApp parent_object;

	TimelineCanvas *canvas;
	GtkRuler *ruler;
};



/* Inits the timeline subsystem. */
void glame_timeline_init();


GtkType timeline_gui_get_type();


/* Create a new timeline view of the provided gpsm group embedded
 * into a fully fledged window. */
GtkWidget *glame_timeline_new_with_window(const char *caption,
					  gpsm_grp_t *root);


#endif
