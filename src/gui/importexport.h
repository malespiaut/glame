#ifndef _IMPORTEXPORT_H
#define _IMPORTEXPORT_H

/*
 * importexport.h
 * $Id: importexport.h,v 1.1 2001/12/07 10:36:47 richi Exp $
 *
 * Copyright (C) 2001 Alexander Ehlert
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include "gpsm.h"


gpsm_item_t *glame_import_dialog(GtkWindow *parent);

int glame_export_dialog(gpsm_item_t *item, GtkWindow *parent);


#endif
