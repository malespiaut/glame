#ifndef _GLTREE_H
#define _GLTREE_H

/*
 * gltree.h
 *
 * $Id: gltree.h,v 1.3 2004/04/05 20:15:57 ochonpaul Exp $
 *
 * Copyright (C) 2003 Johannes Hirche, Richard Guenther
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

#include <gtk/gtk.h>
#include "gpsm.h"


enum {
	//	PROJECT,
	//	KNOB,
	INFO,
	GPSM_ITEM,
	N_ITEMS
};
	

class glTree{
public:
	GtkTreeStore *store;
	gpsm_grp_t *root ;
	GtkWidget *tree;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
 public:
	glTree(){
		store = NULL;
		root=NULL;
	}
	glTree(gpsm_grp_t * newroot);
	
	

};



#endif
