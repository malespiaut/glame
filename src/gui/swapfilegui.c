/*
 * swapfilegui.c
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
#include <gnome.h>


/* Forward declarations. */
static int rmb_menu_cb(GtkWidget *item, GdkEventButton *event,
		        gpointer data);
static int double_click_cb(GtkWidget *item, GdkEventButton *event,
		        gpointer data);
static void copyselected_cb(GtkWidget *menu, GlameTreeItem *item);
static void linkselected_cb(GtkWidget *menu, GlameTreeItem *item);
static void mergeparent_cb(GtkWidget *menu, GlameTreeItem *item);
static void addgroup_cb(GtkWidget *menu, GlameTreeItem *item);
static void edit_cb(GtkWidget *menu, GlameTreeItem *item);
static void import_cb(GtkWidget *menu, GlameTreeItem *item);
static void export_cb(GtkWidget *menu, GlameTreeItem *item);
static void delete_cb(GtkWidget *menu, GlameTreeItem *item);
static void handle_grp(glsig_handler_t *handler, long sig, va_list va);

static GnomeUIInfo group_menu_data[] = {
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Edit", "edit", edit_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Add group...", "addgroup", addgroup_cb, NULL),
        GNOMEUIINFO_ITEM("Merge with parent", "import", mergeparent_cb, NULL),
        GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Link selected", "link", linkselected_cb, NULL),
        GNOMEUIINFO_ITEM("Copy selected", "copy", copyselected_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Import...", "import", import_cb, NULL),
	GNOMEUIINFO_ITEM("Export...", "Export swapfile tracks", export_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_END
};
static GnomeUIInfo file_menu_data[] = {
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Edit", "edit", edit_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Export...", "Export swapfile tracks", export_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_END
};


static void entry_updated_cb(GtkEntry* entry, GlameTreeItem* item)
{
	char * text;
	
	text = gtk_editable_get_chars(GTK_EDITABLE(entry),0,-1);
	gtk_container_remove(GTK_CONTAINER(item),GTK_WIDGET(entry));
	gtk_widget_destroy(GTK_WIDGET(entry));
	if (text) {
		gpsm_item_set_label(item->item, text);
		g_free(text);
	}
	glame_tree_item_update(item);
}
		
void edit_tree_label(GlameTreeItem * item)
{
	GtkWidget * label;
	GtkWidget * entry;

	gtk_tree_item_deselect(GTK_TREE_ITEM(item));
	label = GTK_WIDGET((g_list_first(gtk_container_children(GTK_CONTAINER(item))))->data);
	gtk_container_remove(GTK_CONTAINER(item),label);
	entry = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(entry), gpsm_item_label(item->item));
	// FIXME This causes a assertion error ??
	//		gtk_widget_destroy(GTK_WIDGET(label));
	gtk_container_add(GTK_CONTAINER(item),entry);
	gtk_container_check_resize(GTK_CONTAINER(item));
	gtk_widget_show(entry);
	gtk_signal_connect(GTK_OBJECT(entry),"activate",entry_updated_cb,item);
	gtk_widget_grab_focus(GTK_WIDGET(entry));
}

static int double_click_cb(GtkWidget *item, GdkEventButton *event,
			   gpointer data)
{
	GlameTreeItem *i = GLAME_TREE_ITEM(item);
	
	if(event->button != 1)
		return FALSE;
	
	if(event->type == GDK_2BUTTON_PRESS){
		edit_tree_label(i);
		return TRUE;
	} else 
		return FALSE;
}

static int rmb_menu_cb(GtkWidget *item, GdkEventButton *event,
		        gpointer data)
{
	GlameTreeItem *i = GLAME_TREE_ITEM(item);
	GtkWidget *menu;

	if (event->button != 3){
		if(event->type == GDK_2BUTTON_PRESS){
			edit_tree_label(i);
			return TRUE;
		} else 
			return FALSE;
	}
	if (GPSM_ITEM_IS_SWFILE(i->item))
		menu = gnome_popup_menu_new(file_menu_data);
	else if (GPSM_ITEM_IS_GRP(i->item))
		menu = gnome_popup_menu_new(group_menu_data);
        else
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
		hpos = gpsm_item_hposition(i);
		vpos = gpsm_item_vposition(i);
		gpsm_item_remove(i);
		gpsm_grp_insert(gpsm_item_parent(group), i, hpos, vpos);
	}
	gpsm_item_destroy((gpsm_item_t *)group);
}

static void addgroup_string_cb(gchar *string, gpointer data)
{
	if (string) {
		strncpy(data, string, 255);
		free(string);
	}
}
static void addgroup_cb(GtkWidget *menu, GlameTreeItem *item)
{
	GtkWidget *dialog;
	char name[256];
	gpsm_grp_t *grp;

	if (!GPSM_ITEM_IS_GRP(item->item))
		return;

	dialog = gnome_request_dialog(FALSE, "Enter group name",
				      "Unnamed", 255,
				      addgroup_string_cb, name,
				      NULL);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;

	grp = gpsm_newgrp(name);
	gpsm_grp_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)grp, -1, -1);
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
		DPRINTF("Cannot create waveedit gui\n");
		return;
	}
	gtk_widget_show_all(we);
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
	filename = alloca(255);

	we = gnome_dialog_file_request("Export As...","Filename",&filename);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(we)))
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

	if (GPSM_ITEM_IS_SWFILE(item->item)) {
		p_swapfile_in = plugin_get("swapfile_in");
		swin = filter_instantiate(p_swapfile_in);
		filter_add_node(net, swin, "swapfile_in");
		db = filter_paramdb(swin);
		param = filterparamdb_get_param(db, "filename");
		filterparam_set(param, &gpsm_swfile_filename(item->item));
		param = filterparamdb_get_param(db, "rate");
		filterparam_set(param, &gpsm_swfile_samplerate(item->item));
		param = filterparamdb_get_param(db, "position");
		filterparam_set(param, &gpsm_swfile_position(item->item));
		source = filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT);
		if (!filterport_connect(source,dest))
			goto fail_cleanup;
	} else if (GPSM_ITEM_IS_GRP(item->item)) {
		gpsm_item_t *it;
		p_swapfile_in = plugin_get("swapfile_in");
		gpsm_grp_foreach_item(item->item, it) {
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
	}
	filter_launch(net);
	filter_start(net);
	filter_wait(net);
	filter_delete(net);
	return;

 fail_cleanup:
	filter_delete(net);
}

static void import_cb(GtkWidget *menu, GlameTreeItem *item)
{
	plugin_t *p_swapfile_out;
	filter_t *net = NULL, *readfile, *swout;
	filter_port_t *source;
	filter_pipe_t *pipe;
	gint i, channels;
	char *filenamebuffer, *groupnamebuffer;
	gpsm_grp_t *group = NULL;
	gpsm_item_t *it;

	filenamebuffer = alloca(128);
	groupnamebuffer = alloca(128);

	/* File request dialog with group name entry. */
	{
		GtkWidget *dialog, *dialogVbox;
		GtkWidget *filenameentry, *groupnameentry;

		dialog = gnome_dialog_new("Import Audio File",
					  GNOME_STOCK_BUTTON_CANCEL,
					  GNOME_STOCK_BUTTON_OK, NULL);
		dialogVbox = GTK_WIDGET(GNOME_DIALOG(dialog)->vbox);
		filenameentry = gnome_file_entry_new("import_cb", "Filename");
		gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(filenameentry))),
				   "changed", changeString_cb, filenamebuffer);
		create_label_widget_pair(dialogVbox, "Filename", filenameentry);
		groupnameentry = gtk_entry_new();
		gtk_signal_connect(GTK_OBJECT(groupnameentry), "changed",
				   changeString_cb, groupnamebuffer);
		create_label_widget_pair(dialogVbox, "Groupname", groupnameentry);

		if(!gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
			return;
	}

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
	if(!groupnamebuffer)
		group = gpsm_newgrp(g_basename(filenamebuffer));
	else
		group = gpsm_newgrp(groupnamebuffer);

	i = 0;
	do {
		char swfilename[256];
		snprintf(swfilename, 255, "%s-%i", groupnamebuffer, i);
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
	filter_wait(net); /* ok we could do that more nicely, but not now.. */
	filter_delete(net);

	/* Notify gpsm of the change. */
	gpsm_grp_foreach_item(group, it) {
		swfd_t fd;
		struct sw_stat st;
		fd = sw_open(gpsm_swfile_filename(it), O_RDONLY, TXN_NONE);
		sw_fstat(fd, &st);
		sw_close(fd);
		gpsm_swfile_notify_insert((gpsm_swfile_t *)it, 0, st.size/SAMPLE_SIZE);
	}

	/* Insert the group into the gpsm tree. */
	gpsm_grp_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)group,
			-1, -1);

	return;

 fail_cleanup:
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)group);
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
	GlameTreeItem *itemw;

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
	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		itemw->handler = glsig_add_handler(
			gpsm_item_emitter(item), GPSM_SIG_ITEM_CHANGED,
			handle_swfile, itemw);
	}

	/* Register gtk handlers and append the item widget. */
	//gtk_signal_connect_after(GTK_OBJECT(itemw), "button_press_event",
	//			 (GtkSignalFunc)double_click_cb,(gpointer)NULL);
	gtk_signal_connect_after(GTK_OBJECT(itemw), "button_press_event",
				 (GtkSignalFunc)rmb_menu_cb, (gpointer)NULL);
	glame_tree_append(tree, itemw);
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
		if (!(itemw = glame_tree_find_gpsm_item(tree, item)))
			DERROR("Cannot find item widget");

		/* Remove the item widget. */
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
		    && GLAME_TREE_ITEM(tree)->item != group)
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
        gtk_tree_set_selection_mode(GTK_TREE(tree), GTK_SELECTION_BROWSE);

	/* Add the root group and cause "newitem" signals to be sent
	 * for each item. -- FIXME we need to delete this handler on
	 * widget destruction... (gtk signal!?) */
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
