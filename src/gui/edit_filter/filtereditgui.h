#ifndef _FILTEREDITGUI_H
#define _FILTEREDITGUI_H

/*
 * filtereditgui.h
 *
 * $Id: filtereditgui.h,v 1.5 2001/06/19 12:09:01 richi Exp $
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

#include <gnome.h>
#include "edit_filter/glamecanvas.h"
#include "edit_filter/canvasitem.h"


GtkWidget *glame_filtereditgui_new(filter_t *net);

GtkWidget *glame_filtereditgui_new_cb(GtkObject* ignore, filter_t *net);

void glame_load_network(GtkWidget *foo, gpointer bla);

void glame_filtereditgui_draw_error(GlameCanvas* canv);

void glame_filtereditgui_reset_error(GlameCanvas* canv);

void glame_filtereditgui_init(void);


#endif
