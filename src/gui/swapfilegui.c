/*
 * swapfilegui.c
 *
 * $Id: swapfilegui.c,v 1.75 2002/01/07 23:02:35 richi Exp $
 * 
 * Copyright (C) 2001 Richard Guenther, Johannes Hirche, Alexander Ehlert
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

#include <stdio.h>
#include <errno.h>
#include <gnome.h>
#include <xmlmemory.h>
#include <parser.h>
#include <libintl.h>
#include "glame_types.h"
#include "swapfile.h"
#include "gltreeitem.h"
#include "waveeditgui.h"
#include "filter.h"
#include "util/glame_gui_utils.h"
#include "clipboard.h"
#include "timeline/timeline.h"
#include "glscript.h"
#include "network_utils.h"
#include "importexport.h"
#include "swapfilegui.h"


/* GUI is single threaded, so we may have some global state.
 */

static SwapfileGui *active_swapfilegui = NULL;


/* Forward declarations. */
static int click_cb(GtkWidget *item, GdkEventButton *event,
		    gpointer data);
static void copyselected_cb(GtkWidget *menu, GlameTreeItem *item);
static void linkselected_cb(GtkWidget *menu, GlameTreeItem *item);
static void mergeparent_cb(GtkWidget *menu, GlameTreeItem *item);
static void flatten_cb(GtkWidget *menu, GlameTreeItem *item);
static void addgroup_cb(GtkWidget *menu, GlameTreeItem *item);
static void addclipboard_cb(GtkWidget *menu, GlameTreeItem *item);
static void addfile_cb(GtkWidget *menu, GlameTreeItem *item);
static void addstereo_cb(GtkWidget *menu, GlameTreeItem *item);
static void edit_cb(GtkWidget *menu, GlameTreeItem *item);
static void timeline_cb(GtkWidget *menu, GlameTreeItem *item);
static void import_cb(GtkWidget *menu, GlameTreeItem *item);
static void export_cb(GtkWidget *menu, GlameTreeItem *item);
static void delete_cb(GtkWidget *menu, GlameTreeItem *item);
static void handle_grp(glsig_handler_t *handler, long sig, va_list va);
static void group_cb(GtkWidget *menu, GlameTreeItem *item);

static GnomeUIInfo dummy1_menu[] = {
	GNOMEUIINFO_END
};
static GnomeUIInfo dummy2_menu[] = {
	GNOMEUIINFO_END
};

static GnomeUIInfo group_menu_data[] = {
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Edit"), N_("edit"), edit_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Timeline"), N_("timeline"), timeline_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Delete"), N_("delete"), delete_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM(N_("Add group"), N_("addgroup"), addgroup_cb, NULL),
	GNOMEUIINFO_ITEM(N_("Add clipboard"), N_("addclipboard"), addclipboard_cb, NULL),
	GNOMEUIINFO_ITEM(N_("Add mono wave"), N_("addfile"), addfile_cb, NULL),
	GNOMEUIINFO_ITEM(N_("Add stereo wave"), N_("addstereo"), addstereo_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Link selected"), N_("link"), linkselected_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Copy selected"), N_("copy"), copyselected_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Merge with parent"), N_("import"), mergeparent_cb, NULL),
        GNOMEUIINFO_ITEM(N_("Flatten"), N_("flatten"), flatten_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE(N_("Apply operation"), dummy1_menu),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Import..."), N_("import"), import_cb, NULL),
	GNOMEUIINFO_ITEM(N_("Export..."), N_("Export swapfile tracks"), export_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_END
};
#define GROUP_MENU_ADDGROUP_INDEX 6
#define GROUP_MENU_ADDCLIPBOARD_INDEX 7
#define GROUP_MENU_ADDMONO_INDEX 8
#define GROUP_MENU_ADDSTEREO_INDEX 9
#define GROUP_MENU_APPLYOP_INDEX 16
#define GROUP_MENU_IMPORT_INDEX 18
static GnomeUIInfo file_menu_data[] = {
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Edit"), N_("edit"), edit_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Delete"), N_("delete"), delete_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE(N_("Apply operation"), dummy2_menu),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM(N_("Group"), N_("group"), group_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM(N_("Export..."), N_("Export swapfile tracks"), export_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_END
};
#define FILE_MENU_APPLYOP_INDEX 5


static void edit_tree_label_cb(GtkEntry* entry, GlameTreeItem* item)
{
	char *text = gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);
	/* Unlock accelerators. HACK(?) */
	gtk_signal_handler_unblock(GTK_OBJECT(active_swapfilegui->app),
				   active_swapfilegui->accel_handler);
	gtk_widget_destroy(GTK_WIDGET(entry));
	gtk_widget_show_all(item->hbox);
	if (text) {
		gpsm_item_set_label(item->item, text);
		g_free(text);
	}
}
void edit_tree_label(GlameTreeItem * item)
{
	GtkWidget *entry;

	/* Deselect item and replace GtkLabel with GtkEntry. */
	gtk_tree_item_deselect(GTK_TREE_ITEM(item));
	entry = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(entry), gpsm_item_label(item->item));
	gtk_signal_connect(GTK_OBJECT(entry), "activate",
			   (GtkSignalFunc)edit_tree_label_cb, item);
	gtk_container_add(GTK_CONTAINER(item->hbox), entry);
	gtk_widget_hide_all(item->hbox);
	gtk_widget_show(entry);
	gtk_widget_show(item->hbox);
	gtk_container_check_resize(GTK_CONTAINER(item));
	gtk_widget_grab_focus(GTK_WIDGET(entry));
	/* Block accelerators. HACK(?) */
	gtk_signal_handler_block(GTK_OBJECT(active_swapfilegui->app),
				 active_swapfilegui->accel_handler);
}

static void deselect_all(SwapfileGui *gui)
{
	GList *selected;

	if (!gui)
		return;

	while ((selected = GTK_TREE_SELECTION(gui->tree))) {
		GlameTreeItem *i = GLAME_TREE_ITEM(selected->data);
		gtk_tree_unselect_child(i->tree, GTK_WIDGET(i));
	}
}

/* Menu event - Apply operation. */
static void applyop_cb(GtkWidget *bla, plugin_t *plugin)
{
	gpsm_item_t *item = active_swapfilegui->active_item->item;
	int (*operation)(gpsm_item_t *, long, long);

	if (!(operation = plugin_query(plugin, PLUGIN_GPSMOP))) {
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
	deselect_all(active_swapfilegui);
}

/* Somehow only select "operations" */
static int choose_ops(plugin_t *plugin)
{
	/* Only use filters, hide Import */
	if (!plugin_query(plugin, PLUGIN_GPSMOP)
	    || strcmp(plugin_name(plugin), "import") == 0)
		return 0;

	return 1;
}

static int click_cb(GtkWidget *item, GdkEventButton *event,
		    gpointer data)
{
	GlameTreeItem *i = GLAME_TREE_ITEM(item);
	GtkWidget *menu, *op_menu;

	if (event->button == 1
	    && event->type == GDK_2BUTTON_PRESS) {
		edit_tree_label(i);
		return TRUE;
	}

	if (event->button != 3
	    || event->type != GDK_BUTTON_PRESS)
		return FALSE;

	if (GPSM_ITEM_IS_SWFILE(i->item)) {
		menu = gnome_popup_menu_new(file_menu_data);
		op_menu = GTK_WIDGET(glame_gui_build_plugin_menu(choose_ops, applyop_cb));
		gtk_widget_show(GTK_WIDGET(op_menu));
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_menu_data[FILE_MENU_APPLYOP_INDEX].widget), GTK_WIDGET(op_menu));

	} else if (GPSM_ITEM_IS_GRP(i->item)) {
		menu = gnome_popup_menu_new(group_menu_data);
		op_menu = GTK_WIDGET(glame_gui_build_plugin_menu(choose_ops, applyop_cb));
		gtk_widget_show(GTK_WIDGET(op_menu));
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(group_menu_data[GROUP_MENU_APPLYOP_INDEX].widget), GTK_WIDGET(op_menu));
		gtk_widget_set_sensitive(group_menu_data[GROUP_MENU_ADDGROUP_INDEX].widget,
					 gpsm_grp_is_vbox((gpsm_grp_t *)i->item) ? TRUE : FALSE);
		gtk_widget_set_sensitive(group_menu_data[GROUP_MENU_ADDCLIPBOARD_INDEX].widget,
					 gpsm_grp_is_vbox((gpsm_grp_t *)i->item) ? TRUE : FALSE);
		gtk_widget_set_sensitive(group_menu_data[GROUP_MENU_ADDMONO_INDEX].widget,
					 gpsm_grp_is_vbox((gpsm_grp_t *)i->item) ? TRUE : FALSE);
		gtk_widget_set_sensitive(group_menu_data[GROUP_MENU_ADDSTEREO_INDEX].widget,
					 gpsm_grp_is_vbox((gpsm_grp_t *)i->item) ? TRUE : FALSE);
		gtk_widget_set_sensitive(group_menu_data[GROUP_MENU_IMPORT_INDEX].widget,
					 gpsm_grp_is_vbox((gpsm_grp_t *)i->item) ? TRUE : FALSE);
        } else
		return TRUE;

	gnome_popup_menu_do_popup(menu, NULL, NULL, event, i);
	return TRUE;
}

/* Copy (COW, new swapfiles) all selected items as childs of the
 * current item. */
static void copyselected_cb(GtkWidget *menu, GlameTreeItem *item)
{
	GList *selected;

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;

	selected = GTK_TREE_SELECTION(glame_tree_item_parent(item));
	while (selected) {
		GlameTreeItem *i = GLAME_TREE_ITEM(selected->data);
		gpsm_item_t *copy;

		/* Dont allow copying myself into myself. */
		if (item->item == i->item)
			goto next;

		if (GPSM_ITEM_IS_SWFILE(i->item))
			copy = (gpsm_item_t *)gpsm_swfile_cow((gpsm_swfile_t *)i->item);
		else if (GPSM_ITEM_IS_GRP(i->item))
			copy = (gpsm_item_t *)gpsm_grp_cow((gpsm_grp_t *)i->item);
		else
			goto next;
		gpsm_item_place((gpsm_grp_t *)item->item, copy,
				0, gpsm_item_vsize(item->item));

	next:
		selected = g_list_next(selected);
	}

	deselect_all(active_swapfilegui);
}

/* Link (just new items, same swapfile name) all selected items as
 * childs of the current item. */
static void linkselected_cb(GtkWidget *menu, GlameTreeItem *item)
{
	GList *selected;

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;

	selected = GTK_TREE_SELECTION(glame_tree_item_parent(item));
	while (selected) {
		GlameTreeItem *i = GLAME_TREE_ITEM(selected->data);
		gpsm_item_t *copy;

		/* Dont allow copying myself into myself. */
		if (item->item == i->item)
			goto next;

		if (GPSM_ITEM_IS_SWFILE(i->item))
			copy = (gpsm_item_t *)gpsm_swfile_link((gpsm_swfile_t *)i->item);
		else if (GPSM_ITEM_IS_GRP(i->item))
			copy = (gpsm_item_t *)gpsm_grp_link((gpsm_grp_t *)i->item);
		else
			goto next;
		gpsm_item_place((gpsm_grp_t *)item->item, copy,
				0, gpsm_item_vsize(item->item));

	next:
		selected = g_list_next(selected);
	}
	deselect_all(active_swapfilegui);
}

/* Move all items in this group one level up in the tree
 * (and delete the group itself). */
static void mergeparent_cb(GtkWidget *menu, GlameTreeItem *item)
{
	struct glame_list_head *dummy;
	gpsm_grp_t *group, *parent;
	gpsm_item_t *i;
	long group_hpos, group_vpos;

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;
	deselect_all(active_swapfilegui);

	group = (gpsm_grp_t *)item->item;
	parent = gpsm_item_parent(group);
	group_hpos = gpsm_item_hposition(group);
	group_vpos = gpsm_item_vposition(group);
	gpsm_item_remove((gpsm_item_t *)group);

	gpsm_grp_safe_foreach_item(group, dummy, i) {
		long hpos, vpos;
		hpos = gpsm_item_hposition(i) + group_hpos;
		vpos = gpsm_item_vposition(i) + group_vpos;
		gpsm_item_remove(i);
		gpsm_item_place(parent, i, hpos, vpos);
	}
	gpsm_item_destroy((gpsm_item_t *)group);
}

/* Flatten the group using gpsm_flatten and replace it with the
 * flattened group. */
static void flatten_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_grp_t *group, *parent;
	gpsm_item_t *old;
	long hpos, vpos;

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;
	old = item->item;

	/* Flatten the active group. */
	if (!(group = gpsm_flatten(old))) {
	        DPRINTF("gpsm_flatten failed!?\n");
		return;
	}

	/* Destroy the active group and insert the flattened one. */
	parent = gpsm_item_parent(old);
	hpos = gpsm_item_hposition(old);
	vpos = gpsm_item_vposition(old);
	gpsm_item_destroy(old);
	gpsm_item_place(parent, (gpsm_item_t *)group, hpos, vpos);
	deselect_all(active_swapfilegui);
}

/* Append an empty mono wave (without group) to the current vbox. */
static void addfile_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_swfile_t *swfile;
	GlameTreeItem *grpw;

	if (!GPSM_ITEM_IS_GRP(item->item)
	    || !gpsm_grp_is_vbox((gpsm_grp_t *)item->item))
		return;

	/* Create new gpsm swfile and insert it. */
	swfile = gpsm_newswfile(_("Unnamed"));
	gpsm_vbox_insert((gpsm_grp_t *)item->item,
			 (gpsm_item_t *)swfile,
			 0, gpsm_item_vsize(item->item));

	/* Expand the parent widget. */
	gtk_tree_item_expand(GTK_TREE_ITEM(item));

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)swfile);
	if (grpw)
		edit_tree_label(grpw);
	deselect_all(active_swapfilegui);
}

/* Append an empty stereo wave (with group) to the current vbox. */
static void addstereo_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_swfile_t *left, *right;
	gpsm_grp_t *grp;
	GlameTreeItem *grpw;

	if (!GPSM_ITEM_IS_GRP(item->item)
	    || !gpsm_grp_is_vbox((gpsm_grp_t *)item->item))
		return;

	/* Create new group and two gpsm swfiles and insert it. */
	grp = gpsm_newgrp(_("Unnamed"));
	left = gpsm_newswfile(_("left"));
	gpsm_swfile_set_position(left, FILTER_PIPEPOS_LEFT);
	right = gpsm_newswfile(_("right"));
	gpsm_swfile_set_position(right, FILTER_PIPEPOS_RIGHT);
	gpsm_vbox_insert(grp, (gpsm_item_t *)left, 0, 0);
	gpsm_vbox_insert(grp, (gpsm_item_t *)right, 0, 1);
	gpsm_vbox_insert((gpsm_grp_t *)item->item,
			 (gpsm_item_t *)grp,
			 0, gpsm_item_vsize(item->item));

	/* Expand the parent widget. */
	gtk_tree_item_expand(GTK_TREE_ITEM(item));

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)grp);
	if (grpw)
		edit_tree_label(grpw);
	deselect_all(active_swapfilegui);
}

static void group_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_grp_t *grp, *parent;
	gpsm_item_t *it;
	long hpos, vpos;
	GlameTreeItem *grpw;
	GtkObject *tree;

	if (!GPSM_ITEM_IS_SWFILE(item->item))
		return;

	/* Create new gpsm group, move item into it and re-insert
	 * it at old item position. */
	parent = gpsm_item_parent(item->item);
	tree = GTK_OBJECT(item->tree);
	grp = gpsm_newgrp(gpsm_item_label(item->item));
	it = item->item;
	hpos = gpsm_item_hposition(it);
	vpos = gpsm_item_vposition(it);
	gpsm_item_remove(it);
	gpsm_item_place(grp, it, 0, 0);
	gpsm_item_place(parent, (gpsm_item_t *)grp, hpos, vpos);

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(tree), (gpsm_item_t *)grp);
	if (grpw)
		edit_tree_label(grpw);
	deselect_all(active_swapfilegui);
}

static void addgroup_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_grp_t *grp;
	GlameTreeItem *grpw;

	if (!GPSM_ITEM_IS_GRP(item->item)
	    || !gpsm_grp_is_vbox((gpsm_grp_t *)item->item))
		return;

	/* Create new gpsm group. */
	grp = gpsm_newgrp(_("Unnamed"));
	gpsm_vbox_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)grp,
			 0, gpsm_item_vsize(item->item));

	/* Expand the parent widget. */
	gtk_tree_item_expand(GTK_TREE_ITEM(item));

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)grp);
	if (grpw)
		edit_tree_label(grpw);
	deselect_all(active_swapfilegui);
}

static void addclipboard_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_grp_t *grp;
	GlameTreeItem *grpw;

	if (!GPSM_ITEM_IS_GRP(item->item)
	    || !gpsm_grp_is_vbox((gpsm_grp_t *)item->item))
		return;

	/* Create new gpsm group. */
	if (!(grp = clipboard_get())) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog(_("Clipboard is empty"))));
		return;
	}
	gpsm_vbox_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)grp,
			 0, gpsm_item_vsize(item->item));

	/* Expand the parent widget. */
	gtk_tree_item_expand(GTK_TREE_ITEM(item));

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)grp);
	if (grpw)
		edit_tree_label(grpw);
	deselect_all(active_swapfilegui);
}

static void delete_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_grp_t *deleted;

	deselect_all(active_swapfilegui);
	if (!(deleted = gpsm_find_grp_label(gpsm_root(), NULL, GPSM_GRP_DELETED_LABEL))) {
		deleted = gpsm_newgrp(GPSM_GRP_DELETED_LABEL);
		gpsm_item_place(gpsm_root(), (gpsm_item_t *)deleted,
				0, GPSM_GRP_DELETED_VPOS);
	} else if ((gpsm_item_t *)deleted == item->item) {
		gpsm_item_destroy((gpsm_item_t *)deleted);
		return;
	}
	gpsm_item_place(deleted, item->item,
			0, gpsm_item_vsize(deleted));
}

static void edit_cb(GtkWidget *menu, GlameTreeItem *item)
{
	WaveeditGui *we;
		
	we = glame_waveedit_gui_new(gpsm_item_label(item->item), item->item);
	if (!we) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog(_("Cannot open wave editor"))));
		return;
	}
	gtk_quit_add_destroy(1, GTK_OBJECT(we));
	gtk_widget_show_all(GTK_WIDGET(we));
	deselect_all(active_swapfilegui);
}

static void timeline_cb(GtkWidget *menu, GlameTreeItem *item)
{
	static int warning_shown = 0;
	GtkWidget *tl;
		
	tl = glame_timeline_new_with_window(gpsm_item_label(item->item),
					    (gpsm_grp_t *)item->item);
	if (!tl) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog(_("Cannot open timeline"))));
		return;
	}
	gtk_quit_add_destroy(1, GTK_OBJECT(tl));
	gtk_widget_show_all(tl);
	deselect_all(active_swapfilegui);
	if (!warning_shown) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_warning_dialog_parented(_("The timeline is highly experimental\nand may cause unexpected effects\nwithin other parts of GLAME.\nBe warned."), GTK_WINDOW(tl))));
		warning_shown = 1;
	}
}

void changeString_cb(GtkEditable *wid, char *returnbuffer)
{
	strncpy(returnbuffer, gtk_editable_get_chars(wid, 0, -1), 100);
}
static void export_cb(GtkWidget *menu, GlameTreeItem *item)
{
	glame_export_dialog(item->item, NULL);
	deselect_all(active_swapfilegui);
}

static void import_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_item_t *imported;

	imported = glame_import_dialog(NULL);
	if (!imported)
		return;

	if (gpsm_vbox_insert((gpsm_grp_t *)item->item, imported, 0, 0) == -1) {
		gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(_("Cannot place imported wave"))));
		gpsm_item_destroy(imported);
	}
}


/*
 * Drag & Drop state-machine.
 * FIXME: better grab/release the pointer
 */
extern GtkWidget *glame_appbar;
static void drag_start_stop_cb(GtkWidget *widget, GdkEventButton *event,
			       GlameTreeItem *item)
{
	static GlameTreeItem *drag_widget = NULL;
	static int mode = -1;
	static int cursor_type = -1;
	static GdkCursor *cursor = NULL;
	GdkEventButton *bevent = (GdkEventButton *)event;
	GdkEventCrossing *cevent = (GdkEventCrossing *)event;
	gpsm_item_t *source, *dest;
	gpsm_grp_t *parent;

	if (event->type == GDK_BUTTON_PRESS) {
		if (bevent->button != 1)
			return;

		/* drag&drop start */
		drag_widget = NULL;
		mode = -1;
		if (bevent->state & GDK_SHIFT_MASK) {
			DPRINTF("SHIFT modifier\n");
			mode = 1;
			gnome_appbar_push(GNOME_APPBAR(glame_appbar),
					  _("Drop into hbox"));
		} else if (bevent->state & GDK_CONTROL_MASK) {
			DPRINTF("CTRL modifier\n");
			mode = 2;
			gnome_appbar_push(GNOME_APPBAR(glame_appbar),
					  _("Drop into vbox"));
		} else {
			DPRINTF("illegal modifier\n");
			return; /* modifier not valid */
		}
		drag_widget = item;

	} else if (event->type == GDK_BUTTON_RELEASE) {
		if (bevent->button != 1)
			return;

		/* drag&drop end */
		if (!drag_widget)
			return; /* spurious event - ignore */
		gnome_appbar_pop(GNOME_APPBAR(glame_appbar));
		if (cursor) {
			cursor_type = -1;
			gdk_window_set_cursor(GTK_WIDGET(active_swapfilegui)->window, NULL);
			gdk_cursor_destroy(cursor);
			cursor = NULL;
		}
		if (drag_widget == item)
			return; /* nop */

		source = drag_widget->item;
		dest = item->item;
		DPRINTF("drag&drop: %s on %s\n",
			gpsm_item_label(drag_widget->item),
			gpsm_item_label(item->item));

		if (mode == 1) {
			/* Mode 1 - hbox insertion either before
			 * dropped item or at tail (if dropped
			 * on group). */
			if (GPSM_ITEM_IS_GRP(dest)) {
				if (gpsm_hbox_insert((gpsm_grp_t *)dest, source,
						     gpsm_item_hsize(dest), 0) == -1)
					DPRINTF("insertion failed\n");
			} else {
				if (gpsm_hbox_insert(gpsm_item_parent(dest), source,
						     gpsm_item_hposition(dest), 0) == -1)
					DPRINTF("insertion failed\n");
			}
		} else if (mode == 2) {
			/* Mode 2 - vbox insertion either before
			 * dropped item or at tail (if dropped
			 * on group). */
			if (GPSM_ITEM_IS_GRP(dest)) {
				if (gpsm_vbox_insert((gpsm_grp_t *)dest, source,
						     0, gpsm_item_vsize(dest)) == -1)
					DPRINTF("insertion failed\n");
			} else {
				if (gpsm_vbox_insert(gpsm_item_parent(dest), source,
						     0, gpsm_item_vposition(dest)) == -1)
					DPRINTF("insertion failed\n");
			}
		}

		drag_widget = NULL;
		mode = -1;

	} else if (event->type == GDK_LEAVE_NOTIFY
		   && ((GdkEventCrossing *)event)->mode == GDK_CROSSING_NORMAL) {
		/* active_swapfilegui->active_item = NULL; */

	} else if (event->type == GDK_ENTER_NOTIFY) {
		int ok = 1;
		active_swapfilegui->active_item = item;
		if ((!drag_widget || !(cevent->state & GDK_BUTTON1_MASK))
		     && cursor) {
			cursor_type = -1;
			gdk_window_set_cursor(GTK_WIDGET(active_swapfilegui)->window, NULL);
			gdk_cursor_destroy(cursor);
			cursor = NULL;
			drag_widget = NULL;
			mode = -1;
		}
		if (!drag_widget)
			return;

		if (GPSM_ITEM_IS_GRP(item->item))
			parent = (gpsm_grp_t *)item->item;
		else
			parent = gpsm_item_parent(item->item);
		if ((mode == 1 && !gpsm_grp_is_hbox(parent))
		    || (mode == 2 && !gpsm_grp_is_vbox(parent)))
			ok = 0;

		if (cursor_type == ok)
			return;

		cursor_type = ok;
		if (ok) {
			GdkCursor *c;
			c = gdk_cursor_new(GDK_HAND2);
			gdk_window_set_cursor(GTK_WIDGET(active_swapfilegui)->window, c);
			if (cursor)
				gdk_cursor_destroy(cursor);
			cursor = c;
		} else {
			GdkCursor *c;
			c = gdk_cursor_new(GDK_CIRCLE);
			gdk_window_set_cursor(GTK_WIDGET(active_swapfilegui)->window, c);
			if (cursor)
				gdk_cursor_destroy(cursor);
			cursor = c;
		}
	}
}


static void handle_swfile(glsig_handler_t *handler, long sig, va_list va)
{
	switch (sig) {
	case GPSM_SIG_ITEM_CHANGED: {
		GlameTreeItem *itemw = GLAME_TREE_ITEM(glsig_handler_private(handler));
		gpsm_item_t *item;

		GLSIGH_GETARGS1(va, item);
		if (itemw->item != item)
			DERROR("Wrong itemw->item");

		/* Update the item widget. */
		glame_tree_item_update(GLAME_TREE_ITEM(itemw));
		break;
	}
	default:
		DPRINTF("Unhandled signal in swfile handler (%li)\n", sig);
	}
}

static void handle_grp_add_treeitem(GtkObject *tree, gpsm_item_t *item)
{
	GlameTreeItem *itemw, *nextw;
	gpsm_item_t *next;

	if (!tree || !item
	    || !(GPSM_ITEM_IS_SWFILE(item) || GPSM_ITEM_IS_GRP(item)))
		return;

	/* Construct the item widget and register signal handlers
	 * to the new item. */
	itemw = GLAME_TREE_ITEM(glame_tree_item_new(item));
	if (GPSM_ITEM_IS_GRP(item)) {
		itemw->handler = glsig_add_handler(
			gpsm_item_emitter(item),
			GPSM_SIG_GRP_NEWITEM|GPSM_SIG_GRP_REMOVEITEM|GPSM_SIG_ITEM_CHANGED,
			handle_grp, itemw);
		/* drag&drop handlers */
		gtk_signal_connect(GTK_OBJECT(itemw), "button_press_event",
				   (GtkSignalFunc)drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "button_release_event",
				   (GtkSignalFunc)drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "enter_notify_event",
				   (GtkSignalFunc)drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "leave_notify_event",
				   (GtkSignalFunc)drag_start_stop_cb, itemw);
	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		itemw->handler = glsig_add_handler(
			gpsm_item_emitter(item), GPSM_SIG_ITEM_CHANGED,
			handle_swfile, itemw);
		/* drag&drop handlers */
		gtk_signal_connect(GTK_OBJECT(itemw), "button_press_event",
				   (GtkSignalFunc)drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "button_release_event",
				   (GtkSignalFunc)drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "enter_notify_event",
				   (GtkSignalFunc)drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "leave_notify_event",
				   (GtkSignalFunc)drag_start_stop_cb, itemw);
	}

	/* Register gtk handlers and append the item widget. */
	gtk_signal_connect_after(GTK_OBJECT(itemw), "button_press_event",
				 (GtkSignalFunc)click_cb,(gpointer)NULL);

	/* Look where we want to add the item (match gpsm list order). */
	if (!gpsm_item_parent(item)
	    || !(next = gpsm_grp_next(gpsm_item_parent(item), item))
	    || !(nextw = glame_tree_find_gpsm_item(tree, next)))
		glame_tree_append(tree, itemw);
	else
		glame_tree_insert(tree, itemw,
				  gtk_tree_child_position(
					  nextw->tree, GTK_WIDGET(nextw)));

	glame_tree_item_update(itemw);

	/* If item is a group we need to recurse down the items. */
	if (GPSM_ITEM_IS_GRP(item)) {
		gpsm_item_t *it;
		gpsm_grp_foreach_item(item, it)
			handle_grp_add_treeitem(GTK_OBJECT(itemw), it);
	}

	gtk_widget_show(GTK_WIDGET(itemw));
}
static void handle_grp(glsig_handler_t *handler, long sig, va_list va)
{
	switch (sig) {
	case GPSM_SIG_ITEM_CHANGED: {
		GlameTreeItem *itemw = GLAME_TREE_ITEM(glsig_handler_private(handler));
		gpsm_item_t *item;

		GLSIGH_GETARGS1(va, item);
		if (itemw->item != item)
			DERROR("Wrong itemw->item");

		/* Update the item widget. */
		glame_tree_item_update(GLAME_TREE_ITEM(itemw));
		break;
	}
	case GPSM_SIG_GRP_REMOVEITEM: {
		GtkObject *tree = GTK_OBJECT(glsig_handler_private(handler));
		GlameTreeItem *itemw;
		gpsm_grp_t *group;
		gpsm_item_t *item;

		GLSIGH_GETARGS2(va, group, item);

		/* Remove the item widget, if it is still there. */
		if ((itemw = glame_tree_find_gpsm_item(tree, item)))
			glame_tree_remove(GLAME_TREE_ITEM(itemw));

		/* Note, that our signal handler will be deleted by
		 * the widgets destroy method. (hopefully gtk is sane here) */

		break;
	}
	case GPSM_SIG_GRP_NEWITEM: {
		GtkObject *tree = GTK_OBJECT(glsig_handler_private(handler));
		gpsm_grp_t *group;
		gpsm_item_t *item;

		GLSIGH_GETARGS2(va, group, item);
		if (GLAME_IS_TREE_ITEM(tree)
		    && GLAME_TREE_ITEM(tree)->item != (gpsm_item_t *)group)
			DERROR("Wrong tree->item");

		/* Insert item (and subitems, if necessary). */
		handle_grp_add_treeitem(tree, item);

		break;
	}
	default:
		DPRINTF("Unhandled signal in grp handler (%li)\n", sig);
	}
}

static void handle_enterleave(GtkWidget *tree, GdkEventCrossing *event,
			      SwapfileGui *swapfile)
{
	if (event->type == GDK_ENTER_NOTIFY)
		active_swapfilegui = swapfile;
}


/*
 * Externally visible API and the SwapfileGui object (with guile access).
 */

static SCM gls_swapfilegui_root_item()
{
	if (!active_swapfilegui)
		return SCM_BOOL_F;
	return gpsmitem2scm((gpsm_item_t *)active_swapfilegui->root);
}

static SCM gls_swapfilegui_active_item()
{
	if (!active_swapfilegui)
		return SCM_BOOL_F;
	if (!active_swapfilegui->active_item)
		return gpsmitem2scm((gpsm_item_t *)active_swapfilegui->root);
	return gpsmitem2scm((gpsm_item_t *)active_swapfilegui->active_item->item);
}

static SCM gls_swapfilegui_selected_items()
{
	GList *selected;
	SCM s_items = SCM_LIST0;
	if (!active_swapfilegui)
		return s_items;
	selected = GTK_TREE_SELECTION(active_swapfilegui->tree);
	while (selected) {
		GlameTreeItem *i = GLAME_TREE_ITEM(selected->data);
		s_items = gh_cons(gpsmitem2scm(i->item), s_items);
		selected = g_list_next(selected);
	}
	return s_items;
}

void glame_swapfilegui_init()
{
	gh_new_procedure0_0("swapfilegui-root-item",
			    gls_swapfilegui_root_item);
	gh_new_procedure0_0("swapfilegui-active-item",
			    gls_swapfilegui_active_item);
	gh_new_procedure0_0("swapfilegui-selected-items",
			    gls_swapfilegui_selected_items);
}

static void swapfile_gui_destroy(GtkObject *object)
{
	SwapfileGui *swapfile = SWAPFILE_GUI(object);
	GtkEventBox* parent_class;
	parent_class = gtk_type_class(GTK_TYPE_EVENT_BOX);
	GTK_OBJECT_CLASS(parent_class)->destroy(GTK_OBJECT(swapfile));
	if (swapfile->gpsm_handler)
		glsig_delete_handler(swapfile->gpsm_handler);
}

static void swapfile_gui_class_init(SwapfileGuiClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = swapfile_gui_destroy;
}

static void swapfile_gui_init(SwapfileGui *swapfile)
{
	swapfile->gpsm_handler = NULL;
	swapfile->root = NULL;
	swapfile->tree = NULL;
	swapfile->active_item = NULL;
	swapfile->accel_handler = 0;
	swapfile->app = NULL;
}

GtkType swapfile_gui_get_type(void)
{
	static GtkType swapfile_gui_type = 0;
	
	if (!swapfile_gui_type){
		GtkTypeInfo swapfile_gui_info = {
			"SwapfileGui",
			sizeof(SwapfileGui),
			sizeof(SwapfileGuiClass),
			(GtkClassInitFunc)swapfile_gui_class_init,
			(GtkObjectInitFunc)swapfile_gui_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		swapfile_gui_type = gtk_type_unique(
			GTK_TYPE_EVENT_BOX, &swapfile_gui_info);
		gtk_type_set_chunk_alloc(swapfile_gui_type, 8);
	}

	return swapfile_gui_type;
}

SwapfileGui *glame_swapfile_widget_new(gpsm_grp_t *root)
{
	SwapfileGui *swapfile;
	gpsm_item_t *item;

	if (!root)
		return NULL;

	/* Create the toplevel objects. */
	swapfile = SWAPFILE_GUI(gtk_type_new(swapfile_gui_get_type()));
	swapfile->root = root;
        swapfile->tree = gtk_tree_new();
	gtk_container_add(GTK_CONTAINER(swapfile), swapfile->tree);
        gtk_tree_set_view_mode(GTK_TREE(swapfile->tree), GTK_TREE_VIEW_LINE);
        gtk_tree_set_view_lines(GTK_TREE(swapfile->tree), TRUE);
        gtk_tree_set_selection_mode(GTK_TREE(swapfile->tree),
				    GTK_SELECTION_MULTIPLE);
	/* SINGLE   -- single
	   BROWSE   -- single
	   MULTIPLE -- multiple
	   EXTENDED -- none
	*/

	/* Add the root group and cause "newitem" signals to be sent
	 * for each item. */
	swapfile->gpsm_handler = glsig_add_handler(gpsm_item_emitter(root),
			  GPSM_SIG_GRP_NEWITEM|GPSM_SIG_GRP_REMOVEITEM,
			  handle_grp, swapfile->tree);

	/* Track the active swapfilegui via enter/leave events. */
	gtk_signal_connect(GTK_OBJECT(swapfile), "enter_notify_event",
			   (GtkSignalFunc)handle_enterleave, swapfile);

	/* Add all existing childs of the root group to the tree. */
	gpsm_grp_foreach_item(root, item)
		handle_grp_add_treeitem(GTK_OBJECT(swapfile->tree), item);

	gtk_widget_show(swapfile->tree);
	active_swapfilegui = swapfile;
	return swapfile;
}
