/*
 * gltree.cpp
 *
 * $Id: gltree.cpp,v 1.15 2004/05/31 21:28:43 richi Exp $
 *
 * Copyright (C) 2003, 2004 Johannes Hirche, Richard Guenther, Laurent Georget
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
// #include "swapfile.h"
#include "glscript.h"
#include "importexport.h"
#include "clipboard.h"

/* static members of glTree.  */
GtkTreeView *glTree::tree = NULL;
GlameGpsmStore *glTree::store = NULL;
GtkTreeIter glTree::iter;

static gboolean click_cb(GtkWidget * treeview, GdkEventButton * event,
			 gpointer userdata);
static void edit_wave_cb(GtkWidget *, gpointer);
static void timeline_cb(GtkWidget *, gpointer);
static void delete_cb(GtkWidget * menuitem, gpointer treeview);
static void file_property_cb(GtkWidget * menuitem, gpointer treeview);
static void group_property_cb(GtkWidget * menuitem, gpointer treeview);
static void addgroup_cb(GtkWidget * menuitem, gpointer treeview);
static void addfile_cb(GtkWidget *menu, gpointer treeview);
static void addstereo_cb(GtkWidget *menu,  gpointer treeview);
static void addclipboard_cb(GtkWidget * menu, gpointer treeview);
static void mergeparent_cb(GtkWidget *menu, gpointer which);
static void copyselected_cb(GtkWidget *menu, gpointer which);
static void import_cb(GtkWidget *menu, gpointer treeview);
static void export_cb(GtkWidget *bla, gpointer treeview); 


glTree::glTree(gpsm_grp_t * newroot)
{
	GtkTreeSelection *selection;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;

	// only ever have one of us
	if (tree)
		abort();

	root = newroot;
	store = glame_gpsm_store_new((gpsm_item_t *)newroot);
	tree = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
	
	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes("label",
							  renderer,
							  "text", glTree::INFO,
							  NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	// single selections (for now)
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

	// don't show the header(s)
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(tree), FALSE);

	g_signal_connect(tree, "button-press-event",
			 GCallback(click_cb), NULL);
}

/* Menu event - Apply operation. */
static void applyop_cb(GtkWidget *bla, plugin_t *plugin)
{
	gpsmop_func_t operation;
	GtkTreeIter *iter = &glTree::iter;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	gpsm_item_t *item;

	gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);

	if (!(operation = (gpsmop_func_t)plugin_query(plugin, PLUGIN_GPSMOP))) {
		DPRINTF("No such operation %s\n", plugin_name(plugin));
		return;
	}
	DPRINTF("Executing operation %s on %s [%li, %li[\n",
		plugin_name(plugin), gpsm_item_label(item),
		(long)0, (long)gpsm_item_hsize(item));

	if (operation(item, 0, gpsm_item_hsize(item)) == -1)
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog(_("Error executing"))));

	DPRINTF("%s finished.\n", plugin_name(plugin));
	// deselect_all(active_);
}


static int choose_ops(plugin_t *plugin)
{
	/* Only use filters, hide Import */
	if (!plugin_query(plugin, PLUGIN_GPSMOP)
	    || strcmp(plugin_name(plugin), "import") == 0)
		return 0;

	return 1;
}


void
view_swfile_popup_menu(GtkWidget * treeview, GdkEventButton * event,
		       gpointer which)
{
        GtkWidget *menu, *menuitem, *op_menu;
	
	menu = gtk_menu_new();

	menuitem = gtk_menu_item_new_with_label(_("Edit"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) edit_wave_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Track Properties..."));
	g_signal_connect(menuitem, "activate",
			 (GCallback) file_property_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Timeline"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) timeline_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu),menuitem );

	menuitem = gtk_menu_item_new_with_label(_("Delete"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) delete_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	
	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu),menuitem );

	menuitem = gtk_menu_item_new_with_label(_("Apply Operation"));
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	op_menu = GTK_WIDGET(glame_gui_build_plugin_menu(choose_ops, applyop_cb));
	gtk_menu_item_set_submenu (GTK_MENU_ITEM(menuitem) , op_menu);

	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu),menuitem );

	menuitem = gtk_menu_item_new_with_label(_("Export..."));
	g_signal_connect(menuitem, "activate",
			 (GCallback) export_cb, which);
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
		    gpointer which)
{
	GtkWidget *menu, *menuitem, *op_menu;
	
	menu = gtk_menu_new();

	menuitem = gtk_menu_item_new_with_label(_("Edit"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) edit_wave_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Group Properties..."));
	g_signal_connect(menuitem, "activate",
			 (GCallback)group_property_cb,
			 which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Timeline"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) timeline_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	
	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu),menuitem );

	menuitem = gtk_menu_item_new_with_label(_("Delete"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) delete_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Add group"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) addgroup_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Add mono track"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) addfile_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Add stereo track"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) addstereo_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Add clipboard"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) addclipboard_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);	
	
	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu),menuitem );

	menuitem = gtk_menu_item_new_with_label(_("Merge with parent"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) mergeparent_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);	

	menuitem = gtk_menu_item_new_with_label(_("Copy selected"));
	g_signal_connect(menuitem, "activate",
			 (GCallback) copyselected_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);	

	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu),menuitem );
	
	menuitem = gtk_menu_item_new_with_label(_("Apply Operation"));
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	op_menu = GTK_WIDGET(glame_gui_build_plugin_menu(choose_ops, applyop_cb));
	gtk_menu_item_set_submenu (GTK_MENU_ITEM(menuitem) , op_menu);

	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu),menuitem );
	
	menuitem = gtk_menu_item_new_with_label(_("Import..."));
	g_signal_connect(menuitem, "activate",
			 (GCallback) import_cb, which);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	menuitem = gtk_menu_item_new_with_label(_("Export..."));
	g_signal_connect(menuitem, "activate",
			 (GCallback) export_cb, which);
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
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	GtkTreePath *path;
	gpsm_item_t *item = NULL;
	GtkTreeSelection *selection;

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));

	/* Get tree path for row that was clicked */
	gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(treeview),
				      (int) event->x, (int) event->y,
				      &path, NULL, NULL, NULL);
	if (!path)
		return FALSE;

	if (gtk_tree_model_get_iter(model, &glTree::iter, path))
		gtk_tree_model_get(model, &glTree::iter, glTree::GPSM_ITEM, &item, -1);
	gtk_tree_path_free(path);
	if (!item)
		return FALSE;

	// double click
	if (event->type == GDK_2BUTTON_PRESS && event->button == 1) {
		edit_wave_cb(NULL, &glTree::iter);
		return TRUE;
	}
	// single click with the right mouse button 
	else if (event->type == GDK_BUTTON_PRESS && event->button == 3) {
		if (GPSM_ITEM_IS_SWFILE(item))
			view_swfile_popup_menu(treeview, event, &glTree::iter);
		else if (GPSM_ITEM_IS_GRP(item))
		        view_grp_popup_menu(treeview, event, &glTree::iter);
		return TRUE;
	}

	return FALSE;
}


static void
edit_wave_cb(GtkWidget *widget, gpointer which)
{
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	gchar *label;
	gpsm_item_t *item;
	WaveeditGui *we;

	gtk_tree_model_get(model, iter, glTree::INFO, &label,
			   glTree::GPSM_ITEM, &item, -1);

	we = glame_waveedit_gui_new(label, item);
	if (!we) {
	       gnome_dialog_run_and_close(
		      GNOME_DIALOG(gnome_error_dialog(_("Cannot open wave editor"))));
	       return;
	}
	gtk_quit_add_destroy(1, GTK_OBJECT(we));
	gtk_widget_show_all(GTK_WIDGET(we));
}


static void timeline_cb(GtkWidget *widget, gpointer which)
{
	DPRINTF("in timeline cb");
}



static void delete_cb(GtkWidget * menuitem, gpointer which)
{
        GtkTreeIter *iter = (GtkTreeIter *)which, iter2;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	gpsm_item_t *item;
	gpsm_grp_t *deleted;
	gboolean     valid;
	gchar *comp;

	gtk_tree_model_get(model, iter,
			   glTree::GPSM_ITEM, &item, -1);

	deleted = gpsm_find_grp_label(gpsm_root(), NULL,
				      GPSM_GRP_DELETED_LABEL);
	if ((deleted))  {
	    // Search for [deleted]  iter 
	    valid = gtk_tree_model_get_iter_first(model, &iter2);
	    while (valid)
	      {			
		gtk_tree_model_get(model, &iter2, glTree::INFO, &comp, -1);
		if (!g_ascii_strncasecmp (GPSM_GRP_DELETED_LABEL, comp,8)) break;
		valid = gtk_tree_model_iter_next(model, &iter2);
	      }

	    if ((gpsm_item_t *) deleted == item) {
	      gpsm_item_destroy((gpsm_item_t *) deleted);
	      //gtk_tree_store_remove(GTK_TREE_STORE(model), &iter2);
	      return;
	    }
	  }
	  else if (!(deleted))  {
	    deleted = gpsm_newgrp(GPSM_GRP_DELETED_LABEL);
	    gpsm_item_place(gpsm_root(), (gpsm_item_t *) deleted, 0,
			    GPSM_GRP_DELETED_VPOS);
	    //gtk_tree_store_append(GTK_TREE_STORE(model), &iter2, NULL);
	    //gtk_tree_store_set (GTK_TREE_STORE(model), &iter2,
	    //			glTree::INFO,GPSM_GRP_DELETED_LABEL ,glTree::GPSM_ITEM,(gpsm_item_t *) deleted,-1); 
	  
	  } 
	  gpsm_item_place(deleted, item,
			  0, gpsm_item_vsize(deleted) + 1);
	  
}


static void file_property_cb(GtkWidget * menuitem, gpointer which)
{
	GtkWidget *dialog, *vbox;
	char f_name[1024];
	double f_pos;
	long f_rate;
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	gpsm_item_t *item;
	
        gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);
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
}


static void group_property_cb(GtkWidget * menuitem, gpointer which)
{
	GtkWidget *dialog, *vbox;
	char g_name[1024];
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	gpsm_item_t *item;

        gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);
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
}

static void addgroup_cb(GtkWidget *menu, gpointer which)
{
	gpsm_grp_t *grp;
	gpsm_item_t *item;
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	
	gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);
	if (!GPSM_ITEM_IS_GRP(item)
	    || !gpsm_grp_is_vbox((gpsm_grp_t *)item))
		return;

	// Create new gpsm group. 
	grp = gpsm_newgrp(_("Unnamed group"));
	gpsm_vbox_insert((gpsm_grp_t *)item, (gpsm_item_t *)grp,
			 0, gpsm_item_vsize(item));
	
	/* Find out which widget it got and open an edit field. */
	// grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)grp);
// 	if (grpw)
// 		edit_tree_label(grpw);
// 	deselect_all(active_swapfilegui);

}



/* Append an empty mono wave (without group) to the current vbox. */
static void addfile_cb(GtkWidget *menu, gpointer which)
{
	gpsm_swfile_t *swfile;
	gpsm_item_t *item;
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);


        gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);
	if (!GPSM_ITEM_IS_GRP(item)
	      || !gpsm_grp_is_vbox((gpsm_grp_t *)item))
		return;
	  
	  // Create new gpsm swfile and insert it. 
	swfile = gpsm_newswfile(_("Unnamed track"));
	gpsm_vbox_insert((gpsm_grp_t *)item,
			 (gpsm_item_t *)swfile,
			 0, gpsm_item_vsize(item));

// 	/* Find out which widget it got and open an edit field. */
// 	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)swfile);
// 	if (grpw)
// 		edit_tree_label(grpw);
// 	deselect_all(active_swapfilegui);

}


/* Append an empty stereo wave (with group) to the current vbox. */
static void addstereo_cb(GtkWidget *menu,  gpointer which)
{
        gpsm_swfile_t *left, *right ;
	gpsm_grp_t *grp;
	gpsm_item_t *item;
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);

	gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);
	  if (!GPSM_ITEM_IS_GRP(item)
	      || !gpsm_grp_is_vbox((gpsm_grp_t *)item))
	    return;
	
	  /* Create new group and two gpsm swfiles and insert it. */
	  grp = gpsm_newgrp(_("Unnamed stereo track "));
	  left = gpsm_newswfile(_("left"));
	  gpsm_swfile_set_position(left, FILTER_PIPEPOS_LEFT);
	  right = gpsm_newswfile(_("right"));
	  gpsm_swfile_set_position(right, FILTER_PIPEPOS_RIGHT);
	  gpsm_vbox_insert(grp, (gpsm_item_t *)left, 0, 0);
	  gpsm_vbox_insert(grp, (gpsm_item_t *)right, 0, 1);
	  gpsm_vbox_insert((gpsm_grp_t *)item,
			   (gpsm_item_t *)grp,
			   0, gpsm_item_vsize(item));
		
// 	/* Find out which widget it got and open an edit field. */
// 	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)grp);
// 	if (grpw)
// 		edit_tree_label(grpw);
// 	deselect_all(active_swapfilegui);
// 
}



static void addclipboard_cb(GtkWidget * menu, gpointer which)
{
	gpsm_grp_t *grp;
	gpsm_item_t *item;
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);

	gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);

	if (!GPSM_ITEM_IS_GRP(item)
	    || !gpsm_grp_is_vbox((gpsm_grp_t *) item))
	  return;

	/* Create new gpsm group. */
	if (!(grp = clipboard_get())) {
	  gnome_dialog_run_and_close(GNOME_DIALOG
				     (gnome_error_dialog(_("Clipboard is empty"))));
	  return;
	}
		gpsm_vbox_insert((gpsm_grp_t *) item, (gpsm_item_t *) grp,
				 0, gpsm_item_vsize(item));

		/* Find out which widget it got and open an edit field. */
		// grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)grp);
		// if (grpw)
//              edit_tree_label(grpw);
		// deselect_all(active_swapfilegui);

}

/* Move all items in this group one level up in the tree
 * (and delete the group itself). */
static void mergeparent_cb(GtkWidget *menu, gpointer which)
{
	struct glame_list_head *dummy;
	gpsm_grp_t *group, *parent;
	gpsm_item_t *i, *item;
	long group_hpos, group_vpos;
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	
 	gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);
	if (!GPSM_ITEM_IS_GRP(item))
	    return;
	  // deselect_all(active_swapfilegui);
	  
	  group = (gpsm_grp_t *)item;
	  parent = gpsm_item_parent(group);
	  group_hpos = gpsm_item_hposition(group);
	  group_vpos = gpsm_item_vposition(group);
	  gpsm_item_remove((gpsm_item_t *)group);

	  gpsm_grp_safe_foreach_item(group, dummy , i) {
	    long hpos, vpos;
	    hpos = gpsm_item_hposition(i) + group_hpos;
	    vpos = gpsm_item_vposition(i) + group_vpos;
	    gpsm_item_remove(i);
	    gpsm_item_place(parent, i, hpos, vpos);
	  }
	  gpsm_item_destroy((gpsm_item_t *)group);
}


/* Copy (COW, new swapfiles) the selected item as child of the
 * current item. */
static void copyselected_cb(GtkWidget *menu, gpointer which)
{
        gpsm_item_t *selected_item, *dest_item;
	GtkTreeIter *dest_iter = (GtkTreeIter *)which, selected_iter;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);
	GtkTreeSelection *selection;

	gtk_tree_model_get(model, dest_iter, glTree::GPSM_ITEM, &dest_item, -1);
	if (!GPSM_ITEM_IS_GRP(dest_item))
		return;

	selection = gtk_tree_view_get_selection(glTree::tree);
	if (!gtk_tree_selection_get_selected(selection, &model, &selected_iter))
	  return;
	gtk_tree_model_get(model, &selected_iter, glTree::GPSM_ITEM, &selected_item, -1);
	
	gpsm_item_t *copy;

	/* Dont allow copying myself into myself. */
	if (selected_item == dest_item)
	  return;

	if (GPSM_ITEM_IS_SWFILE(selected_item))
	  copy = (gpsm_item_t *)gpsm_swfile_cow((gpsm_swfile_t *)selected_item);
	else if (GPSM_ITEM_IS_GRP(selected_item))
	  copy = (gpsm_item_t *)gpsm_grp_cow((gpsm_grp_t *)selected_item);
	else
	  return;
	if (gpsm_grp_is_vbox((gpsm_grp_t *)dest_item))
	  gpsm_vbox_insert((gpsm_grp_t *)dest_item, copy,
			   0, gpsm_item_vsize(dest_item));
	else if (gpsm_grp_is_hbox((gpsm_grp_t *)dest_item))
	  gpsm_hbox_insert((gpsm_grp_t *)dest_item, copy,
			   gpsm_item_hsize(dest_item), 0);
	else {
	  gnome_dialog_run_and_close(GNOME_DIALOG(
						  gnome_error_dialog(_("Cannot place item into irregular group"))));
	  gpsm_item_destroy(copy);
	  
	}
}


static void import_cb(GtkWidget *menu, gpointer which)
{
	gpsm_item_t *imported;
	gpsm_item_t *item;
	GtkTreeIter *iter = (GtkTreeIter *)which;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);;
	const gchar *label;

	imported = glame_import_dialog(GTK_WINDOW(glTree::tree));
	if (!imported)
		return;
	
	gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);
	if (!GPSM_ITEM_IS_GRP(item)
	    || !gpsm_grp_is_vbox((gpsm_grp_t *)item))
	    return;
	label = gpsm_item_label (imported); //item label to update treestore
	if (gpsm_grp_is_vbox((gpsm_grp_t *)item)
	    && gpsm_vbox_insert((gpsm_grp_t *)item, imported,
				0, gpsm_item_vsize(item)) == 0){
	  return;
	}
	else if (gpsm_grp_is_hbox((gpsm_grp_t *)item)
		 && gpsm_hbox_insert((gpsm_grp_t *)item, imported,
				     gpsm_item_hsize(item), 0) == 0){
	  return;
	}


	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(("Cannot place imported wave"))));
		
	gpsm_item_destroy(imported);
}



static void export_cb(GtkWidget *menu, gpointer which)
{
	GtkTreeIter *iter = (GtkTreeIter *)which;
        gpsm_item_t *item;
	GtkTreeModel *model = GTK_TREE_MODEL(glTree::store);;

	gtk_tree_model_get(model, iter, glTree::GPSM_ITEM, &item, -1);
	gnome_dialog_run_and_close(glame_export_dialog(item, GTK_WINDOW(glTree::tree)));
}
