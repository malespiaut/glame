#ifndef _FILTEREDITGUI_H
#define _FILTEREDITGUI_H

/*
 * filtereditgui.h
 *
 * $Id: filtereditgui.h,v 1.12 2005/03/30 17:12:59 xwolf Exp $
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
 *
 */
G_BEGIN_DECLS

#include <gnome.h>
#include "edit_filter/glamecanvas.h"
#include "edit_filter/canvasitem.h"
#include "filter.h"


struct _FiltereditGuiClass;
struct _FiltereditGui;
typedef struct _FiltereditGuiClass FiltereditGuiClass;
typedef struct _FiltereditGui FiltereditGui;

#define FILTEREDIT_GUI_TYPE (filteredit_gui_get_type())
#define FILTEREDIT_GUI(object) (GTK_CHECK_CAST((object), FILTEREDIT_GUI_TYPE, FiltereditGui))
#define FILTEREDIT_GUI_CLASS(object) (GTK_CHECK_CLASS_CAST((object), FILTEREDIT_GUI_TYPE, FiltereditGuiClass))
#define IS_FILTEREDIT_GUI(object) (GTK_CHECK_TYPE((object), FILTEREDIT_GUI_TYPE))
#define IS_FILTEREDIT_GUI_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), FILTEREDIT_GUI_TYPE))
#define FILTEREDIT_GUI_GET_CLASS(object) ((FiltereditGuiClass*) (((GtkObject*) (obj))->klass))

struct _FiltereditGuiClass {
	GnomeAppClass parent_class;

};

struct _FiltereditGui {
	GnomeApp parent_object;

	int deleted;

	GlameCanvas *canvas;
	GtkToolbar *toolbar;
	GtkWidget *startbutton,*stopbutton;
	int pm_playing;
};



GtkType filteredit_gui_get_type(void);

GtkWidget *glame_filtereditgui_new(filter_t *net, int prot);

void glame_load_network(GtkWidget *foo, gpointer bla);

void glame_filtereditgui_draw_error(GlameCanvas* canv);

void glame_filtereditgui_reset_error(GlameCanvas* canv);

void glame_filtereditgui_init(void);

G_END_DECLS

#endif
