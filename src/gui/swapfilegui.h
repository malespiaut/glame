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

#ifndef _SWAPFILEGUI_H
#define _SWAPFILEGUI_H

#include <gnome.h>

/* The swapfile widget is responsible for opening and closing the
 * swapfile, i.e. it gets constructed out of a swapfile.
 * As there can be only one swapfile open at a time, only one
 * swapfile widget can be constructed (i.e. there is a global
 * containing the widget).
 */

/* Opens the swapfile and creates the tree widget out of the information
 * stored in file 0 in xml format. FIXME: policy wrt inconsistencies
 * of xml/swapfile state, not existing swapfile, etc.
 * Returns the swapfile gui widget (not shown). */
GtkWidget *glame_swapfile_gui_new(const char *swapfile);

/* Destroys the swapfile widget (so you can create another one) and
 * updates the xml representation of the meta information in swapfile
 * 0 and closes the swapfile. */
void glame_swapfile_gui_destroy();


/* Adds a new toplevel group (aka project). */
void glame_swapfile_gui_add_toplevel_group(const char *name);


#endif
