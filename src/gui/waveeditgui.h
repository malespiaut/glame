/*
 * waveeditgui.h
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

#ifndef _WAVEEDITGUI_H
#define _WAVEEDITGUI_H

#include <gtk/gtk.h>
#include "gtkwaveview.h"
#include "gtkswapfilebuffer.h"
#include "gpsm.h"


/* Create a new waveedit widget out of a gpsm-item which can be
 * either a gpsm-grp or a gpsm-swfile. All gpsm-grp members have
 * to be gpsm-swfiles of the same hposition and hsize. */
GtkWidget *glame_waveedit_gui_new(const char *title, gpsm_item_t *item);

/* Delete temporary (clipboard) files. */
void glame_waveedit_cleanup();


#endif
