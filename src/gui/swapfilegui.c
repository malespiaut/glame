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
}
		
void edit_tree_label(GlameTreeItem * item)
{
	GtkWidget * label;
	GtkWidget * entry;

	label = GTK_LABEL((g_list_first(gtk_container_children(GTK_CONTAINER(item))))->data);
	gtk_container_remove(GTK_CONTAINER(item),label);
	entry = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(entry), gpsm_item_label(item->item));
	// FIXME This causes a assertion error ??
	//		gtk_widget_destroy(GTK_WIDGET(label));
	gtk_container_add(GTK_CONTAINER(item),entry);
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

	if (event->button != 3)
		return FALSE;

	if (GPSM_ITEM_IS_SWFILE(i->item))
		menu = gnome_popup_menu_new(file_menu_data);
	else if (GPSM_ITEM_IS_GRP(i->item))
		menu = gnome_popup_menu_new(group_menu_data);
        else
		return FALSE;

	gnome_popup_menu_do_popup(menu, NULL, NULL, event, i);
	return TRUE;
}

/* Copy (COW, new swapfiles) all selected items as childs of the
 * current item. */
static void copyselected_cb(GtkWidget *menu, GlameTreeItem *item)
{
#if 0 // FIXME
	GList *selected = GTK_TREE_SELECTION(swapfile_tree);

	while (selected) {
		GlameTreeItem *copy, *i = GLAME_TREE_ITEM(selected->data);
		long dest_name;
		swfd_t source, dest;
		struct sw_stat st;

		if ((source = sw_open(i->swapfile_name, O_RDONLY, TXN_NONE)) == -1)
			goto next;
		while ((dest = sw_open((dest_name = rand()), O_CREAT|O_RDWR|O_EXCL, TXN_NONE)) == -1)
			;
		sw_fstat(source, &st);
		sw_ftruncate(dest, st.size);
		sw_sendfile(dest, source, st.size, 0);
		sw_close(source);
		sw_close(dest);
		copy = glame_tree_copy(GTK_OBJECT(i));
		copy->swapfile_name = dest_name;
		sw_glame_tree_append(GTK_OBJECT(item), copy);
	next:
		selected = g_list_next(selected);
	}
#endif
}

/* Link (just new items, same swapfile name) all selected items as
 * childs of the current item. */
static void linkselected_cb(GtkWidget *menu, GlameTreeItem *item)
{
#if 0 // FIXME
	GList *selected = GTK_TREE_SELECTION(swapfile_tree);

	while (selected) {
		GlameTreeItem *i = GLAME_TREE_ITEM(selected->data);
		GlameTreeItem *copy = glame_tree_copy(GTK_OBJECT(i));
		sw_glame_tree_append(GTK_OBJECT(item), copy);
		selected = g_list_next(selected);
	}
#endif
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
	long names[GTK_SWAPFILE_BUFFER_MAX_TRACKS];
	int i;

	/* FIXME: have waveedit gui take an gpsm_item_t* */
	if (GPSM_ITEM_IS_SWFILE(item->item)) {
		we = glame_waveedit_gui_new(gpsm_item_label(item->item), 1,
					    gpsm_swfile_samplerate(item->item),
					    gpsm_swfile_filename(item->item));
	} else if (GPSM_ITEM_IS_GRP(item->item)) {
		gpsm_grp_t *group = (gpsm_grp_t *)item->item;
		gpsm_item_t *it;
		i=0;
		gpsm_grp_foreach_item(group, it) {
			if (!GPSM_ITEM_IS_SWFILE(it))
				return;
			names[i++] = gpsm_swfile_filename(it);
		}
		we = glame_waveedit_gui_new_a(gpsm_item_label(item->item), i,
					      gpsm_swfile_samplerate(it),
					      names);
	}
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
	long name;
	int i;
	char * foobar;
	plugin_t *p_writefile, *p_swapfile_in;
	filter_t *net, *swapfile_in[GTK_SWAPFILE_BUFFER_MAX_TRACKS], *writefile;
	filter_paramdb_t *db;
	filter_param_t *param;
	filter_port_t *source, *dest;
	foobar = calloc(sizeof(char),255);

	we = gnome_dialog_file_request("Export As...","Filename",&foobar);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(we))){
		
		net = filter_creat(NULL);

		p_writefile = plugin_get("write_file");
		writefile = filter_instantiate(p_writefile);
		filter_add_node(net, writefile, "writefile");
		db = filter_paramdb(writefile);
		param = filterparamdb_get_param(db, "filename");
		filterparam_set(param, &foobar);
		dest = filterportdb_get_port(filter_portdb(writefile), PORTNAME_IN); 

		if (GPSM_ITEM_IS_SWFILE(item->item)) {
			p_swapfile_in = plugin_get("swapfile_in");
			swapfile_in[0] = filter_instantiate(p_swapfile_in);
			filter_add_node(net, swapfile_in[0], "swapfile_in");
			db = filter_paramdb(swapfile_in[0]);
			param = filterparamdb_get_param(db, "filename");
			filterparam_set(param, &gpsm_swfile_filename(item->item));
			param = filterparamdb_get_param(db, "rate");
			filterparam_set(param, &gpsm_swfile_samplerate(item->item));
		
			source = filterportdb_get_port(filter_portdb(swapfile_in[0]), PORTNAME_OUT);
			filterport_connect(source,dest);
		} else if (GPSM_ITEM_IS_GRP(item->item)) {
			gpsm_item_t *it;
			p_swapfile_in = plugin_get("swapfile_in");
			gpsm_grp_foreach_item(item->item, it) {
				if (!GPSM_ITEM_IS_SWFILE(it))
					return;
				name = gpsm_swfile_filename(it);
				swapfile_in[i] = filter_instantiate(p_swapfile_in);
				filter_add_node(net, swapfile_in[i], "swapfile");
				db = filter_paramdb(swapfile_in[i]);
				param = filterparamdb_get_param(db, "filename");
				filterparam_set(param, &gpsm_swfile_filename(it));
				param = filterparamdb_get_param(db, "rate");
				filterparam_set(param, &gpsm_swfile_samplerate(it));
				source = filterportdb_get_port(filter_portdb(swapfile_in[i]), PORTNAME_OUT); 
				filterport_connect(source,dest);
			}
		}
		filter_launch(net);
		filter_start(net);
		filter_wait(net);
	}
	free(foobar);
}

static void import_cb(GtkWidget *menu, GlameTreeItem *item)
{

	/*   Blah!
	     FIXME!
	*/
	GtkWidget * dialog;
	GtkWidget * dialogVbox;
	plugin_t *p_readfile, *p_swapfile_out;
	filter_t *net, *readfile, *swapfile_out[2]; /* lame stereo hack */
	filter_param_t *param;
	filter_port_t *source, *dest;
	filter_pipe_t *pipe;
	gint i, channels;
	
	char filenamebuffer[128], groupnamebuffer[128];

	GtkWidget * filenameentry;
	GtkWidget * groupnameentry;
	gpsm_grp_t *group;
	gpsm_item_t *it;

	
	dialog = gnome_dialog_new("Import Audio File",GNOME_STOCK_BUTTON_CANCEL, GNOME_STOCK_BUTTON_OK,NULL);
	dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));
	filenameentry = gnome_file_entry_new("import_cb","Filename");
	gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(filenameentry))),
			   "changed",changeString_cb,filenamebuffer);
	create_label_widget_pair(dialogVbox,"Filename",filenameentry);
	groupnameentry = gtk_entry_new();
	gtk_signal_connect(GTK_OBJECT(groupnameentry),"changed",
			   changeString_cb,groupnamebuffer);
	create_label_widget_pair(dialogVbox,"Groupname",groupnameentry);
	
	if(!gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;

	if (!(p_readfile = plugin_get("read_file"))
	    || !(readfile = filter_instantiate(p_readfile)))
		return;
	net = filter_creat(NULL);
	filter_add_node(net, readfile, "readfile");

	if (!(p_swapfile_out = plugin_get("swapfile_out"))) {
		DPRINTF("swapfile_out not found\n");
		return;
	}
		
	g_assert((param = filterparamdb_get_param(filter_paramdb(readfile), "filename")));
	filterparam_set(param, &filenamebuffer); 
	g_assert((source = filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT)));
			
	if(!groupnamebuffer)
		group = gpsm_newgrp(g_basename(filenamebuffer));
	else
		group = gpsm_newgrp(groupnamebuffer);
	gpsm_grp_insert((gpsm_grp_t *)item->item, (gpsm_item_t *)group,
			-1, -1);

	i = 0;
	do {
		it = (gpsm_item_t *)gpsm_newswfile(groupnamebuffer);
		if (!(swapfile_out[i] = filter_instantiate(p_swapfile_out))
		    || !(param = filterparamdb_get_param(filter_paramdb(swapfile_out[i]), "filename")))
			goto fail_cleanup;
		filterparam_set(param, &gpsm_swfile_filename(it));
		filter_add_node(net, swapfile_out[i], "swapfile_out");
		if (!(dest = filterportdb_get_port(filter_portdb(swapfile_out[i]),
						   PORTNAME_IN))
		    || !(pipe = filterport_connect(source, dest))) {
			DPRINTF("Connection failed for channel %d\n",i+1);
			goto fail_cleanup;
		}
		gpsm_swfile_samplerate(it) = filterpipe_sample_rate(pipe);
		gpsm_swfile_position(it) = filterpipe_sample_hangle(pipe);
		gpsm_grp_insert(group, it, 0, i);
		i++;
	} while (i < GTK_SWAPFILE_BUFFER_MAX_TRACKS);

	channels = i;
	filter_launch(net);
	filter_start(net);
	filter_wait(net); /* ok we could do that more nicely, but not now.. */
	filter_delete(net);

	/* update items - FIXME! use GPSM_SIG_SWFILE_INSERT */
	gpsm_grp_foreach_item(group, it)
		glsig_emit(gpsm_item_emitter(it), GPSM_SIG_ITEM_CHANGED, it);

	return;

 fail_cleanup:
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)group);
}



static void handle_changeitem(glsig_handler_t *handler, long sigmask, va_list va)
{
	GtkObject *groupw = glsig_handler_private(handler);
	GlameTreeItem *itemw;
	gpsm_item_t *item;

	GLSIGH_GETARGS1(va, item);

	/* Find the item widget (child of group widget passed as
	 * private info) for the item and update it. */
	if (!(itemw = glame_tree_find_gpsm_item(groupw, item)))
		return;
	glame_tree_item_update(GLAME_TREE_ITEM(itemw));
}

static void handle_removeitem(glsig_handler_t *handler, long sigmask, va_list va)
{
	GtkObject *groupw = glsig_handler_private(handler);
	GlameTreeItem *itemw;
	gpsm_grp_t *group;
	gpsm_item_t *item;

	GLSIGH_GETARGS2(va, group, item);

	/* Find the item widget (child of group widget passed as
	 * private info) for the item and remove it. */
	if (!(itemw = glame_tree_find_gpsm_item(groupw, item)))
		return;
	glame_tree_remove(GLAME_TREE_ITEM(itemw));
}

static void handle_newitem(glsig_handler_t *handler, long sigmask, va_list va)
{
	GtkWidget *tree = glsig_handler_private(handler), *t;
	GtkWidget *itemw;
	gpsm_grp_t *group;
	gpsm_item_t *item;

	GLSIGH_GETARGS2(va, group, item);

	t = (GtkWidget *)glame_tree_find_gpsm_item((GtkObject *)tree,
						   (gpsm_item_t *)group);
	if (t)
		tree = t;

	/* Construct the item widget and register signal handlers
	 * to the new item. */
	itemw = glame_tree_item_new(item);
	if (GPSM_ITEM_IS_GRP(item)) {
		glsig_add_handler(gpsm_item_emitter(item),
				  GPSM_SIG_GRP_NEWITEM, handle_newitem, itemw);
		glsig_add_handler(gpsm_item_emitter(item),
				  GPSM_SIG_GRP_REMOVEITEM, handle_removeitem, itemw);
		glsig_add_handler(gpsm_item_emitter(item),
				  GPSM_SIG_ITEM_CHANGED, handle_changeitem, itemw);
	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		glsig_add_handler(gpsm_item_emitter(item),
				  GPSM_SIG_ITEM_CHANGED, handle_changeitem, itemw);
	} else
		return;

	/* Register gtk handlers and append the item widget. */
	gtk_signal_connect_after(GTK_OBJECT(itemw), "button_press_event",
			         (GtkSignalFunc)rmb_menu_cb, (gpointer)NULL);
	gtk_signal_connect_after(GTK_OBJECT(itemw), "button_press_event",
				 (GtkSignalFunc)double_click_cb,(gpointer)NULL);
	glame_tree_append(GTK_OBJECT(tree), GLAME_TREE_ITEM(itemw));
	glame_tree_item_update(GLAME_TREE_ITEM(itemw));
	gtk_widget_show(itemw);
}


static void gpsm_private_rebuild(gpsm_grp_t *group, glsig_handler_t *handler)
{
	gpsm_item_t *item;

	list_foreach(&group->items, gpsm_item_t, list, item) {
		glsig_handler_exec(handler, GPSM_SIG_GRP_NEWITEM,
				   group, item);
		if (GPSM_ITEM_IS_GRP(item))
			gpsm_private_rebuild((gpsm_grp_t *)item, handler);
	}
}


/*
 * Externally visible API.
 */

GtkWidget *glame_swapfile_widget_new(gpsm_grp_t *root)
{
	GtkWidget *tree;
	glsig_handler_t *handler;

	if (!root)
		return NULL;

	/* Create the toplevel tree. */
        tree = gtk_tree_new();
        gtk_tree_set_view_mode(GTK_TREE(tree), GTK_TREE_VIEW_LINE);
        gtk_tree_set_view_lines(GTK_TREE(tree), TRUE);
        gtk_tree_set_selection_mode(GTK_TREE(tree), GTK_SELECTION_BROWSE);

	/* Add the root group and cause "newitem" signals to be sent
	 * for each item. */
	handler = glsig_add_handler(gpsm_item_emitter(root),
			  GPSM_SIG_GRP_NEWITEM, handle_newitem, tree);
	gpsm_private_rebuild(root, handler);

	return tree;
}
