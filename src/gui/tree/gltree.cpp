/*
 * gltree.cpp
 *
 * $Id: gltree.cpp,v 1.2 2003/05/25 21:48:02 richi Exp $
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include "gltree.h"


glTree::glTree(gpsm_grp_t * newroot)
{
	root=newroot;
	store = gtk_tree_store_new(N_ITEMS,
				   //				   G_TYPE_OBJECT,
				   G_TYPE_STRING);
	GtkTreeIter iter,iter2;
	
	gpsm_item_t *item;
	gpsm_grp_foreach_item(newroot,item){
		gtk_tree_store_append(store, &iter,NULL);
		gtk_tree_store_set(store, &iter,INFO,strdup(item->label),-1);
		if(GPSM_ITEM_IS_GRP(item)){
			gpsm_item_t *it;
			gpsm_grp_foreach_item(item,it){
				gtk_tree_store_append(store,&iter2, &iter);
				gtk_tree_store_set(store,&iter2,INFO,strdup(it->label),-1);
			}
		}
	}
		
		tree = gtk_tree_view_new_with_model (GTK_TREE_MODEL (store));
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("label",
							   renderer,
							   "text", INFO,
							   NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);
}
				   
	
