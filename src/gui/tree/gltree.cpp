/*
 * gltree.cpp
 *
 * $Id: gltree.cpp,v 1.5 2004/04/18 20:51:18 ochonpaul Exp $
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
#include "util/glame_gui_utils.h"
#include "swapfile.h"
static gboolean click_cb(GtkWidget * treeview, GdkEventButton * event,
			 gpointer userdata);
static void edit_wave_cb(GtkTreeView * treeview, GtkTreePath * path,
			 GtkTreeViewColumn * col, gpointer userdata);
static void edit_wave_from_menu_cb(GtkWidget * menuitem, gpointer treeview);
static void timeline_cb(GtkTreeView * treeview, GtkTreePath * path,
			GtkTreeViewColumn * col, gpointer userdata);
static void delete_cb(GtkWidget * menuitem, gpointer treeview);
static void file_property_cb(GtkWidget * menuitem, gpointer treeview);
static void group_property_cb(GtkWidget * menuitem, gpointer treeview);


glTree::glTree(gpsm_grp_t * newroot)
{
	root = newroot;
	store = gtk_tree_store_new(N_ITEMS,
				   //                              G_TYPE_OBJECT,
				   G_TYPE_STRING, G_TYPE_POINTER);
	GtkTreeIter iter, iter2;
	gpsm_item_t *item;

	// FIXME: need to go deeper in the tree
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
	// g_object_set(renderer, "editable", TRUE, NULL);
// 	g_signal_connect(renderer, "edited", (GCallback) item_info_edited_callback, store);

	column = gtk_tree_view_column_new_with_attributes("label",
							  renderer,
							  "text", INFO,
							  NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);


	// g_signal_connect(tree, "row-activated", G_CALLBACK(edit_wave_cb),
//                       NULL);

	g_signal_connect(tree, "button-press-event",
			 GCallback(click_cb), NULL);

}



void
view_swfile_popup_menu(GtkWidget * treeview, GdkEventButton * event,
		gpointer userdata)
{
	GtkWidget *menu, *menuitem;
	
	menu = gtk_menu_new();

	menuitem = gtk_menu_item_new_with_label(_("Edit"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) edit_wave_from_menu_cb, treeview);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Track Properties..."));
	g_signal_connect(menuitem, "activate",
			 (GCallback) file_property_cb, treeview);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Timeline"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) timeline_cb, treeview);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	// menuitem = gtk_separator_menu_item_new(void);
	// gtk_menu_shell_append(GTK_MENU_SHELL(menu), gtk_separator_menu_item_new(void));

	menuitem = gtk_menu_item_new_with_label(_("Delete"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) delete_cb, treeview);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	gtk_widget_show_all(menu);

	/* Note: event can be NULL here when called from view_onPopupMenu;
	 *  gdk_event_get_time() accepts a NULL argument */
	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
		       (event != NULL) ? event->button : 0,
		       gdk_event_get_time((GdkEvent *) event));

}

void
view_grp_popup_menu(GtkWidget * treeview, GdkEventButton * event,
		gpointer userdata)
{
	GtkWidget *menu, *menuitem;
	
	menu = gtk_menu_new();

	menuitem = gtk_menu_item_new_with_label(_("Edit"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) edit_wave_from_menu_cb, treeview);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Group Properties..."));
	g_signal_connect(menuitem, "activate",
			 (GCallback)group_property_cb,
			 treeview);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Timeline"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) timeline_cb, treeview);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	// menuitem = gtk_separator_menu_item_new(void);
	// gtk_menu_shell_append(GTK_MENU_SHELL(menu), gtk_separator_menu_item_new(void));

	menuitem = gtk_menu_item_new_with_label(_("Delete"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) delete_cb, treeview);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	gtk_widget_show_all(menu);

	/* Note: event can be NULL here when called from view_onPopupMenu;
	 *  gdk_event_get_time() accepts a NULL argument */
	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL,
		       (event != NULL) ? event->button : 0,
		       gdk_event_get_time((GdkEvent *) event));

}


static gboolean
click_cb(GtkWidget * treeview, GdkEventButton * event, gpointer userdata)
{

	GtkTreeIter iter;
	GtkTreeModel *model;
	GtkTreePath *path;
	gpsm_item_t *item;
	GtkTreeSelection *selection;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

	if (gtk_tree_selection_count_selected_rows(selection) <= 1) {
		/* Get tree path for row that was clicked */
		gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(treeview),
					      (int) event->x,
					      (int) event->y, &path, NULL,
					      NULL, NULL);
	}
	// Select right clicked row 
	if (gtk_tree_selection_count_selected_rows(selection) <= 1) {
		{
			gtk_tree_selection_unselect_all(selection);
			gtk_tree_selection_select_path(selection, path);
		}
	}
	// double click
	if (event->type == GDK_2BUTTON_PRESS && event->button == 1) {
		edit_wave_cb(GTK_TREE_VIEW(treeview), path, NULL, NULL);
		return TRUE;
	}
	// single click with the right mouse button 
	if (event->type == GDK_BUTTON_PRESS && event->button == 3) {
		g_print("Single right click on the tree view.\n");

		model = gtk_tree_view_get_model(GTK_TREE_VIEW(treeview));

		if (gtk_tree_model_get_iter(model, &iter, path)) {
			gtk_tree_model_get(model, &iter, GPSM_ITEM,
					   &item, -1);
			if (GPSM_ITEM_IS_SWFILE(item))	view_swfile_popup_menu(treeview, event, NULL);
			else if (GPSM_ITEM_IS_GRP(item)) view_grp_popup_menu(treeview, event, NULL);
			gtk_tree_path_free(path);
			return TRUE;
		}

		return FALSE;
	}
	return FALSE;
}


static void
edit_wave_cb(GtkTreeView * treeview, GtkTreePath * path,
	     GtkTreeViewColumn * col, gpointer userdata)
{
	GtkTreeSelection *selection;
	GtkTreeIter iter;
	GtkTreeModel *model;
	gchar *label;
	gpsm_item_t *item;
	WaveeditGui *we;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
		gtk_tree_model_get(model, &iter, INFO, &label, GPSM_ITEM,
				   &item, -1);

		we = glame_waveedit_gui_new(label, item);
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


static void edit_wave_from_menu_cb(GtkWidget * menuitem, gpointer treeview)
{

	edit_wave_cb(GTK_TREE_VIEW(treeview), NULL, NULL, NULL);

}




static void timeline_cb(GtkTreeView * treeview, GtkTreePath * path,
			GtkTreeViewColumn * col, gpointer userdata)
{
	g_print("in timeline cb");
}



static void delete_cb(GtkWidget * menuitem, gpointer treeview)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	gpsm_item_t *item;
	gpsm_grp_t *deleted;
	GtkTreeSelection *selection;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
		gtk_tree_model_get(model, &iter, GPSM_ITEM, &item, -1);
		// deselect_all(active_swapfilegui); 
		if (!
		    (deleted =
		     gpsm_find_grp_label(gpsm_root(), NULL,
					 GPSM_GRP_DELETED_LABEL))) {
			deleted = gpsm_newgrp(GPSM_GRP_DELETED_LABEL);
			gpsm_item_place(gpsm_root(),
					(gpsm_item_t *) deleted, 0,
					GPSM_GRP_DELETED_VPOS);
		} else if ((gpsm_item_t *) deleted == item) {
			gpsm_item_destroy((gpsm_item_t *) deleted);
			return;
		}
		gpsm_item_place(deleted, item,
				0, gpsm_item_vsize(deleted) + 1);
		
		if (!gtk_tree_store_remove(GTK_TREE_STORE(model), &iter)) {
			DPRINTF("Remove failed.");
		}
	}


}


static void file_property_cb(GtkWidget * menuitem, gpointer treeview)
{
	GtkWidget *dialog, *vbox;
	char f_name[1024];
	double f_pos;
	long f_rate;
	GtkTreeIter iter;
	GtkTreeModel *model;
	gpsm_item_t *item;
	GtkTreeSelection *selection;
	GtkTreePath *path;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);
	
	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
	  gtk_tree_model_get(model, &iter, GPSM_ITEM, &item, -1);
	  strncpy(f_name, gpsm_item_label(item), 1024);
	f_pos = gpsm_swfile_position(item);
	f_rate = gpsm_swfile_samplerate(item);

	dialog = GTK_WIDGET(gtk_type_new(gnome_dialog_get_type()));
	// gnome_dialog_set_parent(GNOME_DIALOG(dialog),
// 				GTK_WINDOW(active_swapfilegui->app));
	gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);
	gnome_dialog_append_button(
		GNOME_DIALOG(dialog), GNOME_STOCK_BUTTON_OK);
	gnome_dialog_append_button(
		GNOME_DIALOG(dialog), GNOME_STOCK_BUTTON_CANCEL);
	gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

	vbox = GNOME_DIALOG(dialog)->vbox;

	/* file name */
	create_label_edit_pair(vbox, "Track name:",
			       "swapfile::fileprop::name", f_name);

	/* file samplerate */
	create_label_long_pair(vbox, "Samplerate:", &f_rate, 1, 96000);

	/* file position */
	create_label_double_pair(vbox, "Position:", &f_pos, -M_PI, M_PI);

	if (gnome_dialog_run_and_close(GNOME_DIALOG(dialog)) == 1)
		return;

	// update file and gtk tree
	gpsm_item_set_label(item, f_name);
	gpsm_swfile_set((gpsm_swfile_t *)item,
			f_rate, f_pos);
	gtk_tree_store_set (GTK_TREE_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW(treeview))), &iter,
			    INFO, f_name,-1); 
	}
}


static void group_property_cb(GtkWidget * menuitem, gpointer treeview)
{
	GtkWidget *dialog, *vbox;
	char g_name[1024];
	GtkTreeIter iter;
	GtkTreeModel *model;
	gpsm_item_t *item;
	GtkTreeSelection *selection;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);
	
	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
	  gtk_tree_model_get(model, &iter, GPSM_ITEM, &item, -1);
	  strncpy(g_name, gpsm_item_label(item), 1024);

	  dialog = GTK_WIDGET(gtk_type_new(gnome_dialog_get_type()));
	// gnome_dialog_set_parent(GNOME_DIALOG(dialog),
// 				GTK_WINDOW(active_swapfilegui->app));
	gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);
	gnome_dialog_append_button(
		GNOME_DIALOG(dialog), GNOME_STOCK_BUTTON_OK);
	gnome_dialog_append_button(
		GNOME_DIALOG(dialog), GNOME_STOCK_BUTTON_CANCEL);
	gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);

	vbox = GNOME_DIALOG(dialog)->vbox;

	/* group name */
	create_label_edit_pair(vbox, "Group name:",
			       "swapfile::groupprop::name", g_name);

	if (gnome_dialog_run_and_close(GNOME_DIALOG(dialog)) == 1)
		return;

	/* update group and gtk_tree*/
	gpsm_item_set_label(item, g_name);
	gtk_tree_store_set (GTK_TREE_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW(treeview))), &iter,
			    INFO, g_name,-1); 


	}
}
