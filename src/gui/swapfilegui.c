/*
 * swapfilegui.c
 *
 * $Id: swapfilegui.c,v 1.42 2001/05/30 13:57:12 richi Exp $
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
#include <xmlmemory.h>
#include <parser.h>
#include "glame_types.h"
#include "swapfile.h"
#include "gltreeitem.h"
#include "waveeditgui.h"
#include "swapfilegui.h"
#include "filter.h"
#include "glame_gui_utils.h"
#include "clipboard.h"
#include "timeline.h"
#include <gnome.h>


/* Forward declarations. */
static int click_cb(GtkWidget *item, GdkEventButton *event,
		    gpointer data);
static void copyselected_cb(GtkWidget *menu, GlameTreeItem *item);
static void linkselected_cb(GtkWidget *menu, GlameTreeItem *item);
static void mergeparent_cb(GtkWidget *menu, GlameTreeItem *item);
static void flatten_cb(GtkWidget *menu, GlameTreeItem *item);
static void collect_cb(GtkWidget *menu, GlameTreeItem *item);
static void addgroup_cb(GtkWidget *menu, GlameTreeItem *item);
static void addclipboard_cb(GtkWidget *menu, GlameTreeItem *item);
static void addfile_cb(GtkWidget *menu, GlameTreeItem *item);
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
	GNOMEUIINFO_ITEM("Add empty wave", "addfile", addfile_cb, NULL),
        GNOMEUIINFO_ITEM("Link selected", "link", linkselected_cb, NULL),
        GNOMEUIINFO_ITEM("Copy selected", "copy", copyselected_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Merge with parent", "import", mergeparent_cb, NULL),
        GNOMEUIINFO_ITEM("Flatten", "flatten", flatten_cb, NULL),
        GNOMEUIINFO_ITEM("Collect", "collect", collect_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_SUBTREE("Apply operation", dummy1_menu),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Import...", "import", import_cb, NULL),
	GNOMEUIINFO_ITEM("Export...", "Export swapfile tracks", export_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_END
};
#define GROUP_MENU_APPLYOP_INDEX 16
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

/* Collect the group using gpsm_collect and replace it with the
 * collected group. */
static void collect_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_grp_t *group, *parent;
	gpsm_item_t *old;
	long hpos, vpos;

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;
	old = item->item;

	/* Collect the active group. */
	if (!(group = gpsm_collect_swfiles(old))) {
	        DPRINTF("gpsm_collect failed!?\n");
		return;
	}

	/* Destroy the active group and insert the collected one. */
	parent = gpsm_item_parent(old);
	hpos = gpsm_item_hposition(old);
	vpos = gpsm_item_vposition(old);
	gpsm_item_destroy(old);
	gpsm_grp_insert(parent, (gpsm_item_t *)group, hpos, vpos);
}

static void addfile_cb(GtkWidget *menu, GlameTreeItem *item)
{
	gpsm_swfile_t *swfile;
	GlameTreeItem *grpw;

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;

	/* Create new gpsm swfile and insert it. */
	swfile = gpsm_newswfile("Unnamed");
	if (gpsm_grp_is_hbox((gpsm_grp_t *)item->item))
		gpsm_hbox_insert((gpsm_grp_t *)item->item,
				 (gpsm_item_t *)swfile,
				 gpsm_item_hsize(item->item), 0);
	else if (gpsm_grp_is_vbox((gpsm_grp_t *)item->item))
		gpsm_vbox_insert((gpsm_grp_t *)item->item,
				 (gpsm_item_t *)swfile,
				 0, gpsm_item_vsize(item->item));
	else
		gpsm_grp_insert((gpsm_grp_t *)item->item,
				(gpsm_item_t *)swfile, 0, -1);

	/* Expand the parent widget. */
	gtk_tree_item_expand(GTK_TREE_ITEM(item));

	/* Find out which widget it got and open an edit field. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(item), (gpsm_item_t *)swfile);
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
	gpsm_grp_insert(grp, it, -1, -1);
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

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;

	/* Create new gpsm group. */
	grp = gpsm_newgrp("Unnamed");
	gpsm_grp_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)grp, -1, -1);

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

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;

	/* Create new gpsm group. */
	if (!(grp = clipboard_get()))
		return;
	gpsm_grp_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)grp, -1, -1);

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
	GtkWidget *we;
		
	we = glame_waveedit_gui_new(gpsm_item_label(item->item), item->item);
	if (!we) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Cannot open wave editor")));
		return;
	}
	gtk_widget_show_all(we);
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
	plugin_t *p_swapfile_in;
	filter_t *net, *swin, *writefile;
	filter_paramdb_t *db;
	filter_param_t *param;
	filter_port_t *source, *dest;
	gpsm_grp_t *grp;
	gpsm_item_t *it;

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
	writefile = filter_instantiate(plugin_get("write_file"));
	filter_add_node(net, writefile, "writefile");
	db = filter_paramdb(writefile);
	param = filterparamdb_get_param(db, "filename");
	if (filterparam_set(param, &filename) == -1)
		goto fail_cleanup;
	dest = filterportdb_get_port(filter_portdb(writefile), PORTNAME_IN); 

	p_swapfile_in = plugin_get("swapfile_in");
	gpsm_grp_foreach_item(grp, it) {
		if (!GPSM_ITEM_IS_SWFILE(it))
			goto fail_cleanup;
		swin = filter_instantiate(p_swapfile_in);
		filter_add_node(net, swin, "swapfile");
		db = filter_paramdb(swin);
		param = filterparamdb_get_param(db, "filename");
		filterparam_set(param, &gpsm_swfile_filename(it));
		param = filterparamdb_get_param(db, "rate");
		filterparam_set(param, &gpsm_swfile_samplerate(it));
		param = filterparamdb_get_param(db, "position");
		filterparam_set(param, &gpsm_swfile_position(item->item));
		source = filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT); 
		if (!filterport_connect(source, dest))
			goto fail_cleanup;
	}

	filter_launch(net);
	filter_start(net);
	filter_wait(net);
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)grp);
	return;

 fail_cleanup:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create exporting network")));
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)grp);
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
	filter_launch(net);
	filter_start(net);
	if (filter_wait(net) != 0)
		goto fail_cleanup;
	filter_delete(net);

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
}


/*
 * FIXME: We probably want to
 * - handle modifiers (shift, ctrl)
 * - show actual operation (based on modifier) in taskbar
 * - support horizontal "add before" (drop on swfile)
 * - support vertical "add before" (drop on swfile)
 * - support "add at tail" (drop on group)
 */
static void drag_start_stop_cb(GtkWidget *widget, GdkEventButton *event,
			       GlameTreeItem *item)
{
	static GlameTreeItem *drag_widget = NULL;
	static int mode = -1;
	gpsm_item_t *source, *dest, *parent;
	long hpos, vpos;

	if (event->button != 1)
		return;

	if (event->type == GDK_BUTTON_PRESS) {
		/* drag&drop start */
		drag_widget = NULL;
		mode = -1;
		if (event->state & GDK_SHIFT_MASK) {
			DPRINTF("SHIFT modifier\n");
			mode = 1;
		} else if (event->state & GDK_CONTROL_MASK) {
			DPRINTF("CTRL modifier\n");
			mode = 2;
		} else {
			DPRINTF("illegal modifier\n");
			return; /* modifier not valid */
		}
		drag_widget = item;

	} else if (event->type == GDK_BUTTON_RELEASE) {
		/* drag&drop end */
		if (!drag_widget)
			return; /* spurious event - ignore */
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
	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		itemw->handler = glsig_add_handler(
			gpsm_item_emitter(item), GPSM_SIG_ITEM_CHANGED,
			handle_swfile, itemw);
		/* drag&drop handlers */
		gtk_signal_connect(GTK_OBJECT(itemw), "button_press_event",
				   drag_start_stop_cb, itemw);
		gtk_signal_connect(GTK_OBJECT(itemw), "button_release_event",
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

static void handle_rootdestroy(GtkWidget *tree, glsig_handler_t *handler)
{
	glsig_delete_handler(handler);
}



/*
 * Externally visible API.
 */

GtkWidget *glame_swapfile_widget_new(gpsm_grp_t *root)
{
	GtkWidget *tree;
	gpsm_item_t *item;
	glsig_handler_t *handler;

	if (!root)
		return NULL;

	/* Create the toplevel tree. */
        tree = gtk_tree_new();
        gtk_tree_set_view_mode(GTK_TREE(tree), GTK_TREE_VIEW_LINE);
        gtk_tree_set_view_lines(GTK_TREE(tree), TRUE);
        gtk_tree_set_selection_mode(GTK_TREE(tree), GTK_SELECTION_MULTIPLE);

	/* Add the root group and cause "newitem" signals to be sent
	 * for each item. */
	handler = glsig_add_handler(gpsm_item_emitter(root),
			  GPSM_SIG_GRP_NEWITEM|GPSM_SIG_GRP_REMOVEITEM,
			  handle_grp, tree);
	gtk_signal_connect(GTK_OBJECT(tree), "destroy",
			   handle_rootdestroy, handler);

	/* Add all existing childs of the root group to the tree. */
	gpsm_grp_foreach_item(root, item)
		handle_grp_add_treeitem(GTK_OBJECT(tree), item);

	return tree;
}
