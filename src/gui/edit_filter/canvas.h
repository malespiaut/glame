#ifndef _GLAME_CANVAS_H
#define _GLAME_CANVAS_H

/*
 * canvas.h
 *
 * $Id: canvas.h,v 1.17 2001/03/27 21:04:20 xwolf Exp $
 *
 * Copyright (C) 2000 Johannes Hirche
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

#include "glame_gui_utils.h"

gint canvas_item_node_selected(GnomeCanvasItem*item, GdkEvent *event, gpointer data);
gint handle_events(GnomeCanvasItem* item,GdkEvent *event, gpointer data);
void canvas_item_create_ports(GnomeCanvasGroup* grp,filter_t *f);
GtkWidget * canvas_new_from_network(gui_network* net);
void canvas_item_redraw(GlameCanvasItem* item);
GlameCanvas* draw_network(filter_t *filter);



#endif
