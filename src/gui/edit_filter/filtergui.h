#ifndef _FILTERGUI_H
#define _FILTERGUI_H

/*
 * filtergui.h
 *
 * $Id: filtergui.h,v 1.2 2001/06/19 12:09:01 richi Exp $
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

#include "util/glame_gui_utils.h"


/* Create a new filter network. The data is contained in the 
 * gui_network struct which contains the filter_t * as well.
 * If the filter_t creation succeded a editing window is created.
 */
gui_network* gui_network_new();

#endif
