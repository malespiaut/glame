#ifndef _SWAPFILEGUI_H
#define _SWAPFILEGUI_H

/*
 * swapfilegui.h
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

#include <gnome.h>
#include "gpsm.h"

/* The swapfile widget is one of the different view types of
 * the GLAME project structure management tree.
 */


/* Creates a view of the specified GPSM group (you usually want to
 * pass gpsm_root() here). The GPSM subsystem needs to be initialized
 * before you can create a swapfile gui.
 * Returns a swapfile gui widget (not shown) or NULL on error. Deletion
 * of the widget works as usual. */
GtkWidget *glame_swapfile_widget_new(gpsm_grp_t *root);


#endif
