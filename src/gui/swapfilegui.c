/*
 * swapfilegui.c
 *
 * $Id: swapfilegui.c,v 1.51 2001/07/03 09:33:39 richi Exp $
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
#include "swapfilegui.h"


/* GUI is single threaded, so we may have some global state.
 */

static SwapfileGui *active_swapfilegui = NULL;
static GlameTreeItem *active_swapfilegui_item = NULL;


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
        GNOMEUIINFO_ITEM("Edit", "edit", edit_cb, NULL),
        GNOMEUIINFO_ITEM("Timeline", "timeline", timeline_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Add group", "addgroup", addgroup_cb, NULL),
	GNOMEUIINFO_ITEM("Add clipboard", "addclipboard", addclipboard_cb, NULL),
	GNOMEUIINFO_ITEM("Add mono wave", "addfile", addfile_cb, NULL),
	GNOMEUIINFO_ITEM("Add stereo wave", "addstereo", addstereo_cb, NULL),
        GNOMEUIINFO_ITEM("Link selected", "link", linkselected_cb, NULL),
        GNOMEUIINFO_ITEM("Copy selected", "copy", copyselected_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Merge with parent", "import", mergeparent_cb, NULL),
        GNOMEUIINFO_ITEM("Flatten", "flatten", flatten_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Apply operation", dummy1_menu),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Import...", "import", import_cb, NULL),
	GNOMEUIINFO_ITEM("Export...", "Export swapfile tracks", export_cb, NULL),
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
        GNOMEUIINFO_ITEM("Edit", "edit", edit_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Apply operation", dummy2_menu),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Group", "group", group_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Export...", "Export swapfile tracks", export_cb, NULL),
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
	gtk_container_remove(GTK_CONTAINER(item), GTK_WIDGET(entry));
	gtk_widget_destroy(GTK_WIDGET(entry));
	if (text) {
		gpsm_item_set_label(item->item, text);
		g_free(text);
	}
}
void edit_tree_label(GlameTreeItem * item)
{
	GtkWidget *label;
	GtkWidget *entry;

	/* Deselect item and replace GtkLabel with GtkEntry. */
	gtk_tree_item_deselect(GTK_TREE_ITEM(item));
	label = GTK_WIDGET((g_list_first(gtk_container_children(GTK_CONTAINER(item))))->data);
	gtk_container_remove(GTK_CONTAINER(item), label);
	entry = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(entry), gpsm_item_label(item->item));
	gtk_signal_connect(GTK_OBJECT(entry), "activate", edit_tree_label_cb, item);
	gtk_widget_show(entry);
	gtk_container_add(GTK_CONTAINER(item), entry);
	gtk_container_check_resize(GTK_CONTAINER(item));
	gtk_widget_grab_focus(GTK_WIDGET(entry));
	/* Block accelerators. HACK(?) */
	gtk_signal_handler_block(GTK_OBJECT(active_swapfilegui->app),
				 active_swapfilegui->accel_handler);
}

static gpsm_item_t *actual_item = NULL;

/* Menu event - Apply operation. */
static void applyop_cb(GtkWidget *bla, plugin_t *plugin)
{
	gpsm_item_t *item = actual_item;
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
			gnome_error_dialog("Error executing")));

	DPRINTF("%s finished.\n", plugin_name(plugin));
}

/* Somehow only select "operations" */
static int choose_ops(plugin_t *plugin)
{
	/* Only use filters. */
	if (!(plugin_query(plugin, PLUGIN_GPSMOP)))
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

	if (event->button != 3)
		return FALSE;

	actual_item = i->item;

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
		gpsm_swfile_t *copy;

		if (!GPSM_ITEM_IS_SWFILE(i->item))
			goto next;
		copy = gpsm_swfile_cow((gpsm_swfile_t *)i->item);
		gpsm_grp_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)copy,
				-1, -1);

	next:
		selected = g_list_next(selected);
	}
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
		gpsm_swfile_t *copy;

		if (!GPSM_ITEM_IS_SWFILE(i->item))
			goto next;

		copy = gpsm_swfile_link((gpsm_swfile_t *)i->item);
		gpsm_grp_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)copy,
				-1, -1);

	next:
		selected = g_list_next(selected);
	}
}

/* Move all items in this group one level up in the tree
 * (and delete the group itself). */
static void mergeparent_cb(GtkWidget *menu, GlameTreeItem *item)
{
	struct list_head *dummy;
	gpsm_grp_t *group;
	gpsm_item_t *i;

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;
	group = (gpsm_grp_t *)item->item;

	gpsm_grp_safe_foreach_item(group, dummy, i) {
		long hpos, vpos;
		hpos = gpsm_item_hposition(i) + gpsm_item_hposition(group);
		vpos = gpsm_item_vposition(i) + gpsm_item_vposition(group);
		gpsm_item_remove(i);
		gpsm_grp_insert(gpsm_item_parent(group), i, hpos, vpos);
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
	gpsm_grp_insert(parent, (gpsm_item_t *)group, hpos, vpos);
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
	swfile = gpsm_newswfile("Unnamed");
	gpsm_vbox_insert((gpsm_grp_t *)item->item,
			 (gpsm_item_t *)swfile,
			 0, gpsm_item_vsize(item->item));

	/* Expand the parent widget. */
	gtk_tree_item_expand(GTK_TREE_ITEM(item));

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)swfile);
	if (grpw)
		edit_tree_label(grpw);
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
	grp = gpsm_newgrp("Unnamed");
	left = gpsm_newswfile("left");
	gpsm_swfile_set_position(left, FILTER_PIPEPOS_LEFT);
	right = gpsm_newswfile("right");
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
	gpsm_grp_insert(grp, it, 0, 0);
	gpsm_grp_insert(parent, (gpsm_item_t *)grp, hpos, vpos);

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(tree), (gpsm_item_t *)grp);
	if (grpw)
		edit_tree_label(grpw);
}

static void addgroup_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_grp_t *grp;
	GlameTreeItem *grpw;

	if (!GPSM_ITEM_IS_GRP(item->item)
	    || !gpsm_grp_is_vbox((gpsm_grp_t *)item->item))
		return;

	/* Create new gpsm group. */
	grp = gpsm_newgrp("Unnamed");
	gpsm_vbox_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)grp,
			 0, gpsm_item_vsize(item->item));

	/* Expand the parent widget. */
	gtk_tree_item_expand(GTK_TREE_ITEM(item));

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)grp);
	if (grpw)
		edit_tree_label(grpw);
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
			gnome_error_dialog("Clipboard is empty")));
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
}

static void delete_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_item_destroy(item->item);
}

static void edit_cb(GtkWidget *menu, GlameTreeItem *item)
{
	WaveeditGui *we;
		
	we = glame_waveedit_gui_new(gpsm_item_label(item->item), item->item);
	if (!we) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Cannot open wave editor")));
		return;
	}
	gtk_widget_show_all(GTK_WIDGET(we));
}

static void timeline_cb(GtkWidget *menu, GlameTreeItem *item)
{
	GtkWidget *tl;
		
	tl = glame_timeline_new_with_window(gpsm_item_label(item->item),
					    (gpsm_grp_t *)item->item);
	if (!tl) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Cannot open timeline")));
		return;
	}
	gtk_widget_show_all(tl);
}

void changeString_cb(GtkEditable *wid, char *returnbuffer)
{
	strncpy(returnbuffer, gtk_editable_get_chars(wid, 0, -1), 100);
}
static void export_cb(GtkWidget *menu, GlameTreeItem *item)
{
	GtkWidget * we;
	char *filename;
	filter_t *net, *swin, *writefile, *render;
	filter_paramdb_t *db;
	filter_param_t *param;
	filter_port_t *source, *dest;
	filter_pipe_t *pipe;
	gpsm_grp_t *grp;
	gpsm_item_t *it;
	float pos;

	/* Query the file name. */
	filename = alloca(256);
	we = glame_dialog_file_request("Export As...", "swapfilegui:exportas",
				       "Filename", NULL, filename);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(we)))
		return;

	/* Build temporary group out of flattened item->item. */
	if (!(grp = gpsm_flatten(item->item)))
		return;

	/* Build basic network. */
	net = filter_creat(NULL);
	writefile = net_add_plugin_by_name(net, "write_file");
	db = filter_paramdb(writefile);
	param = filterparamdb_get_param(db, "filename");
	if (filterparam_set(param, &filename) == -1)
		goto fail_cleanup;
	dest = filterportdb_get_port(filter_portdb(writefile), PORTNAME_IN); 

	render = net_add_plugin_by_name(net, "render");
	source = filterportdb_get_port(filter_portdb(render), PORTNAME_OUT);
	if (!(pipe = filterport_connect(source, dest)))
		goto fail_cleanup;
	pos = -1.57;
	filterparam_set(filterparamdb_get_param(filterpipe_sourceparamdb(pipe), "position"), &pos);
	if (!(pipe = filterport_connect(source, dest)))
		goto fail_cleanup;
	pos = 1.57;
	filterparam_set(filterparamdb_get_param(filterpipe_sourceparamdb(pipe), "position"), &pos);

	gpsm_grp_foreach_item(grp, it)
		if (!(swin = net_add_gpsm_input(net, (gpsm_swfile_t *)it,
						0, -1)))
			goto fail_cleanup;
	if (net_apply_node(net, render) == -1)
		goto fail_cleanup;

	net_prepare_bulk();
	if (filter_launch(net) == -1
	    || filter_start(net) == -1)
		goto fail_cleanup;
	filter_wait(net);
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)grp);
	net_restore_default();
	return;

 fail_cleanup:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create exporting network")));
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)grp);
	net_restore_default();
}

static void import_cb(GtkWidget *menu, GlameTreeItem *item)
{
	GtkWidget *dialog;
	plugin_t *p_swapfile_out;
	filter_t *net = NULL, *readfile, *swout;
	filter_port_t *source;
	filter_pipe_t *pipe;
	gint i, channels;
	char *filenamebuffer;
	gpsm_grp_t *group = NULL;
	GlameTreeItem *grpw;
	gpsm_item_t *it;

	filenamebuffer = alloca(256);

	/* Query the file name. */
	dialog = glame_dialog_file_request("Import audio file",
					   "swapfilegui:import",
					   "Filename", NULL, filenamebuffer);
	if(!gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;

	/* Setup core network. */
	net = filter_creat(NULL);
	if (!(readfile = filter_instantiate(plugin_get("read_file"))))
		return;
	if (filterparam_set(filterparamdb_get_param(filter_paramdb(readfile),
						    "filename"),
			    &filenamebuffer) == -1)
		goto fail_cleanup;
	source = filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT);
	filter_add_node(net, readfile, "readfile");

	if (!(p_swapfile_out = plugin_get("swapfile_out"))) {
		DPRINTF("swapfile_out not found\n");
		return;
	}

	/* Setup gpsm group. */
	group = gpsm_newgrp(g_basename(filenamebuffer));

	i = 0;
	do {
		char swfilename[256];
		snprintf(swfilename, 255, "track-%i", i);
		if (!(it = (gpsm_item_t *)gpsm_newswfile(swfilename)))
			goto fail_cleanup;
		gpsm_grp_insert(group, it, 0, i);
		if (!(swout = filter_instantiate(p_swapfile_out)))
			goto fail_cleanup;
		filter_add_node(net, swout, "swapfile_out");
		if (filterparam_set(filterparamdb_get_param(filter_paramdb(swout), "filename"),
				    &gpsm_swfile_filename(it)) == -1)
			goto fail_cleanup;
		if (!(pipe = filterport_connect(
			source, filterportdb_get_port(
				filter_portdb(swout), PORTNAME_IN)))) {
			DPRINTF("Connection failed for channel %d\n",i+1);
			gpsm_item_destroy(it);
			filter_delete(swout);
			break;
		}
		gpsm_swfile_set((gpsm_swfile_t *)it,
				filterpipe_sample_rate(pipe),
				filterpipe_sample_hangle(pipe));
		i++;
	} while (i < GTK_SWAPFILE_BUFFER_MAX_TRACKS);

	channels = i;
	net_prepare_bulk();
	filter_launch(net);
	filter_start(net);
	if (filter_wait(net) != 0)
		goto fail_cleanup;
	filter_delete(net);
	net_restore_default();

	/* Notify gpsm of the change. */
	gpsm_grp_foreach_item(group, it)
		gpsm_invalidate_swapfile(gpsm_swfile_filename(it));

	/* Insert the group into the gpsm tree. */
	gpsm_grp_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)group,
			-1, -1);

	/* Expand the parent widget. */
	gtk_tree_item_expand(GTK_TREE_ITEM(item));

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)group);
	if (grpw)
		edit_tree_label(grpw);

	gpsm_sync();

	return;

 fail_cleanup:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create importing network")));
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)group);
	net_restore_default();
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
					  "Drop into hbox");
		} else if (bevent->state & GDK_CONTROL_MASK) {
			DPRINTF("CTRL modifier\n");
			mode = 2;
			gnome_appbar_push(GNOME_APPBAR(glame_appbar),
					  "Drop into vbox");
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
			if (GPSM_ITEM_IS_GRP(dest))
				parent = (gpsm_grp_t *)dest;
			else
				parent = gpsm_item_parent(dest);
			if (!gpsm_grp_is_hbox(parent)) {
				DPRINTF("not a hbox\n");
				drag_widget = NULL;
				return;
			}
			gpsm_item_remove(source);
			if (GPSM_ITEM_IS_GRP(dest))
				gpsm_hbox_insert(parent, source,
						 gpsm_item_hsize(parent), 0);
			else
				gpsm_hbox_insert(parent, source,
						 gpsm_item_hposition(dest), 0);
		} else if (mode == 2) {
			/* Mode 2 - vbox insertion either before
			 * dropped item or at tail (if dropped
			 * on group). */
			if (GPSM_ITEM_IS_GRP(dest))
				parent = (gpsm_grp_t *)dest;
			else
				parent = gpsm_item_parent(dest);
			if (!gpsm_grp_is_vbox(parent)) {
				DPRINTF("not a vbox\n");
				drag_widget = NULL;
				return;
			}
			gpsm_item_remove(source);
			if (GPSM_ITEM_IS_GRP(dest))
				gpsm_vbox_insert(parent, source,
						 0, gpsm_item_vsize(parent));
			else
				gpsm_vbox_insert(parent, source,
						 0, gpsm_item_vposition(dest));
		}

		drag_widget = NULL;
		mode = -1;

	} else if (event->type == GDK_LEAVE_NOTIFY
		   && ((GdkEventCrossing *)event)->mode == GDK_CROSSING_NORMAL) {
		active_swapfilegui_item = NULL;

	} else if (event->type == GDK_ENTER_NOTIFY) {
		int ok = 1;
		active_swapfilegui_item = item;
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
				   drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "button_release_event",
				   drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "enter_notify_event",
				   drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "leave_notify_event",
				   drag_start_stop_cb, itemw);
	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		itemw->handler = glsig_add_handler(
			gpsm_item_emitter(item), GPSM_SIG_ITEM_CHANGED,
			handle_swfile, itemw);
		/* drag&drop handlers */
		gtk_signal_connect(GTK_OBJECT(itemw), "button_press_event",
				   drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "button_release_event",
				   drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "enter_notify_event",
				   drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "leave_notify_event",
				   drag_start_stop_cb, itemw);
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
	return gpsmitem2scm(active_swapfilegui->root);
}

static SCM gls_swapfilegui_active_item()
{
	if (!active_swapfilegui)
		return SCM_BOOL_F;
	if (!active_swapfilegui_item)
		return gpsmitem2scm(active_swapfilegui->root);
	return gpsmitem2scm(active_swapfilegui_item->item);
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

	/* Add the root group and cause "newitem" signals to be sent
	 * for each item. */
	swapfile->gpsm_handler = glsig_add_handler(gpsm_item_emitter(root),
			  GPSM_SIG_GRP_NEWITEM|GPSM_SIG_GRP_REMOVEITEM,
			  handle_grp, swapfile->tree);

	/* Track the active swapfilegui via enter/leave events. */
	gtk_signal_connect(GTK_OBJECT(swapfile), "enter_notify_event",
			   handle_enterleave, swapfile);

	/* Add all existing childs of the root group to the tree. */
	gpsm_grp_foreach_item(root, item)
		handle_grp_add_treeitem(GTK_OBJECT(swapfile->tree), item);

	gtk_widget_show(swapfile->tree);
	return swapfile;
}
