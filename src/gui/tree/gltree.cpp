/*
 * gltree.cpp
 *
 * $Id: gltree.cpp,v 1.3 2004/04/05 20:17:54 ochonpaul Exp $
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
#include "waveeditgui.h"

static void edit_wave_cb(GtkTreeView * treeview,
			 GtkTreePath * path,
			 GtkTreeViewColumn * col, gpointer userdata);

glTree::glTree(gpsm_grp_t * newroot)
{
	root = newroot;
	store = gtk_tree_store_new(N_ITEMS,
				   //                              G_TYPE_OBJECT,
				   G_TYPE_STRING, G_TYPE_POINTER);
	GtkTreeIter iter, iter2;
	gpsm_item_t *item;

	gpsm_grp_foreach_item(newroot, item) {
		gtk_tree_store_append(store, &iter, NULL);
		gtk_tree_store_set(store, &iter, INFO, strdup(item->label),
				   -1);
		gtk_tree_store_set(store, &iter, GPSM_ITEM, item, -1);
		if (GPSM_ITEM_IS_GRP(item)) {
			gpsm_item_t *it;
			gpsm_grp_foreach_item(item, it) {
				gtk_tree_store_append(store, &iter2,
						      &iter);
				gtk_tree_store_set(store, &iter2, INFO,
						   strdup(it->label), -1);
				gtk_tree_store_set(store, &iter2,
						   GPSM_ITEM, it, -1);
			}
		}
	}

	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes("label",
							  renderer,
							  "text", INFO,
							  NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);


	g_signal_connect(tree, "row-activated", G_CALLBACK(edit_wave_cb),
			 NULL);



}


static void
edit_wave_cb(GtkTreeView * treeview,
	     GtkTreePath * path,
	     GtkTreeViewColumn * col, gpointer userdata)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	gchar *label;
	gpsm_item_t *gpsm_item;
	WaveeditGui *we;

	model = gtk_tree_view_get_model(treeview);

	if (gtk_tree_model_get_iter(model, &iter, path)) {
		gtk_tree_model_get(model, &iter, INFO, &label, GPSM_ITEM,
				   &gpsm_item, -1);
		// g_print("You selected  %s\n", label);
		// g_print("You selected  %s\n", gpsm_item->label);
		we = glame_waveedit_gui_new(label, gpsm_item);
		if (!we) {
			gnome_dialog_run_and_close(GNOME_DIALOG
						   (gnome_error_dialog
						    (_
						     ("Cannot open wave editor"))));
			return;
		}
		gtk_quit_add_destroy(1, GTK_OBJECT(we));
		gtk_widget_show_all(GTK_WIDGET(we));
		// deselect_all(active_swapfilegui); 
	}
}
