#ifndef _GLAME_DND_H
#define _GLAME_DND_H

/*
 * glame_dnd.h
 *
 * $Id: glame_dnd.h,v 1.2 2003/04/11 20:10:21 richi Exp $
 *
 * Copyright (C) 2002 Johannes Hirche
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
 * * This file (will) have all the visible gui functions 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gnome.h>
#include <libgnomecanvas/gnome-canvas.h>
#include <gtk/gtk.h>

enum {
	GLAME_DND_TYPE_STRING,
	GLAME_DND_TYPE_STRING_SCM,
	GLAME_DND_TYPE_STRING_SCM_NETWORK,
	GLAME_DND_TYPE_POINTER_FILTER_T,
	GLAME_DND_TYPE_SCM_OBJ,
	
} GlameDNDTargetTypeIDs;
	
/* these are GtkTargetEntries */

#define GLAME_DND_TARGET_STRING { "GlameDNDTypeString", 0, GLAME_DND_TYPE_STRING }
#define GLAME_DND_TARGET_STRING_SCM { "GlameDNDTypeStringScheme", 0, GLAME_DND_TYPE_STRING_SCM }
#define GLAME_DND_TARGET_STRING_SCM_NETWORK { "GlameDNDTypeStringSchemeNetwork",0, GLAME_DND_TYPE_STRING_SCM_NETWORK }
#define GLAME_DND_TARGET_POINTER_FILTER_T { "GlameDNDTypePointerFilterT", 0, GLAME_DND_TYPE_POINTER_FILTER_T }
#define GLAME_DND_TARGET_SCM_OBJ { "GlameDNDTypeScmObj", 0, GLAME_DND_TYPE_SCM_OBJ }


#endif
