#ifndef _GLAME_TIMELINE_H
#define _GLAME_TIMELINE_H

/*
 * timeline.h
 * $Id: timeline.h,v 1.1 2001/05/18 09:35:51 richi Exp $
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


/* Create a new timeline widget with root as contents. Returns
 * a widget on success, NULL on error. */
GtkWidget *glame_timeline_new(gpsm_grp_t *root);

/* Create a new timeline view of the provided gpsm group embedded
 * into a fully fledged window. */
GtkWidget *glame_timeline_new_with_window(const char *caption,
					  gpsm_grp_t *root);


#endif
