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
#include "edit_filter/gui.h"
#include <gnome.h>


/* The swapfile widget. */
static GtkTree *swapfile_tree = NULL;


/* Forward declarations. */
static void sw_glame_tree_append(GtkObject *tree, GlameTreeItem *item);
static int rmb_menu_cb(GtkWidget *item, GdkEventButton *event,
		        gpointer data);
static void addgroup_cb(GtkWidget *menu, GlameTreeItem *item);
static void edit_cb(GtkWidget *menu, GlameTreeItem *item);
static void import_cb(GtkWidget *menu, GlameTreeItem *item);
static void export_cb(GtkWidget *menu, GlameTreeItem *item);
static void delete_cb(GtkWidget *menu, GlameTreeItem *item);

static GnomeUIInfo group_menu_data[] = {
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Edit", "edit", edit_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Merge with parent", "import", NULL, NULL), /* FIXME */
        GNOMEUIINFO_ITEM("Flatten childs", "flatten", NULL, NULL), /* FIXME */
        GNOMEUIINFO_ITEM("Link selected", "link", NULL, NULL), /* FIXME */
        GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Add group...", "addgroup", addgroup_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Import...", "import", import_cb, NULL),
	GNOMEUIINFO_ITEM("Export...", "Export swapfile tracks", export_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
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





static int rmb_menu_cb(GtkWidget *item, GdkEventButton *event,
		        gpointer data)
{
	GlameTreeItem *i = GLAME_TREE_ITEM(item);
	GtkWidget *menu;

	if (event->button != 3)
		return FALSE;

	if (i->type == GLAME_TREE_ITEM_FILE)
		menu = gnome_popup_menu_new(file_menu_data);
	else if (i->type == GLAME_TREE_ITEM_GROUP)
		menu = gnome_popup_menu_new(group_menu_data);
        else
		return FALSE;

	gnome_popup_menu_do_popup(menu, NULL, NULL, event, i);
	return TRUE;
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
	GlameTreeItem *grp;

	dialog = gnome_request_dialog(FALSE, "Enter group name",
				      "Unnamed", 255,
				      addgroup_string_cb, name,
				      swapfile_tree);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;

	grp = GLAME_TREE_ITEM(glame_tree_item_new_group(name));
	sw_glame_tree_append(GTK_OBJECT(item), grp);
}

static void delete_cb(GtkWidget *menu, GlameTreeItem *item)
{
	if (item->type == GLAME_TREE_ITEM_FILE) {
		if (item->swapfile_name != -1)
			sw_unlink(item->swapfile_name);
		glame_tree_remove(item);
		gtk_object_destroy(GTK_OBJECT(item));
	} else if (item->type == GLAME_TREE_ITEM_GROUP) {
		GList *children = gtk_container_children(GTK_CONTAINER(GTK_TREE_ITEM_SUBTREE(item)));
		while (children) {
			GlameTreeItem *it = GLAME_TREE_ITEM(children->data);
			delete_cb(menu, it);
			children = g_list_next(children);
		}
		glame_tree_remove(item);
		gtk_object_destroy(GTK_OBJECT(item));
	}
}

static void edit_cb(GtkWidget *menu, GlameTreeItem *item)
{
	GtkWidget *we;
	long names[GTK_SWAPFILE_BUFFER_MAX_TRACKS];
	int i;
	GList *children;
	GlameTreeItem *it;

	if (item->type == GLAME_TREE_ITEM_FILE) {
		we = glame_waveedit_gui_new(item->label, 1, item->sample_rate,
					    item->swapfile_name);
	} else if (item->type == GLAME_TREE_ITEM_GROUP) {
		children = gtk_container_children(GTK_CONTAINER(GTK_TREE_ITEM_SUBTREE(item)));
		for (i=0; i<MIN(g_list_length(children), GTK_SWAPFILE_BUFFER_MAX_TRACKS); i++) {
			it = GLAME_TREE_ITEM(g_list_nth_data(children, i));
			if (it->type != GLAME_TREE_ITEM_FILE)
				return;
			names[i] = it->swapfile_name;
		}
		we = glame_waveedit_gui_new_a(item->label, g_list_length(children), it->sample_rate,
					      names);
	}
	if (!we) {
		DPRINTF("Cannot create waveedit gui\n");
		return;
	}
	gtk_widget_show_all(we);
}

void changeString_cb(GtkEditable *wid, char ** returnbuffer)
{
	strncpy(*returnbuffer,gtk_editable_get_chars(wid,0,-1),100);
}
static void export_cb(GtkWidget *menu, GlameTreeItem *item)
{
	GtkWidget * we;
	long name;
	int i;
	GList * children;
	GlameTreeItem *it;
	char * foobar;
	plugin_t *p_writefile, *p_swapfile_in;
	filter_t *net, *swapfile_in[GTK_SWAPFILE_BUFFER_MAX_TRACKS], *writefile;
	filter_paramdb_t *db;
	filter_param_t *param;
	filter_port_t *source, *dest;
	filter_pipe_t *pipe;
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

		if(item->type == GLAME_TREE_ITEM_FILE){
			p_swapfile_in = plugin_get("swapfile_in");
			swapfile_in[0] = filter_instantiate(p_swapfile_in);
			filter_add_node(net, swapfile_in[0], "swapfile_in");
			db = filter_paramdb(swapfile_in[0]);
			param = filterparamdb_get_param(db, "filename");
			filterparam_set(param, &(item->swapfile_name));
			param = filterparamdb_get_param(db, "rate");
			filterparam_set(param, &(item->sample_rate));
		
			source = filterportdb_get_port(filter_portdb(swapfile_in[0]), PORTNAME_OUT);
			filterport_connect(source,dest);
		} else if (item->type == GLAME_TREE_ITEM_GROUP) {
			p_swapfile_in = plugin_get("swapfile_in");
			children = gtk_container_children(GTK_CONTAINER(GTK_TREE_ITEM_SUBTREE(item)));
			for (i=0; i<MIN(g_list_length(children), GTK_SWAPFILE_BUFFER_MAX_TRACKS); i++) {
				it = GLAME_TREE_ITEM(g_list_nth_data(children, i));
				if (it->type != GLAME_TREE_ITEM_FILE)
					return;
				name = it->swapfile_name;
				swapfile_in[i] = filter_instantiate(p_swapfile_in);
				filter_add_node(net, swapfile_in[i], "swapfile");
				db = filter_paramdb(swapfile_in[i]);
				param = filterparamdb_get_param(db, "filename");
				filterparam_set(param, &(it->swapfile_name));
				param = filterparamdb_get_param(db, "rate");
				filterparam_set(param, &(it->sample_rate));
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
	GtkWidget * newItem;
	GtkWidget * newTrak;
	plugin_t *p_readfile, *p_swapfile_out;
	filter_t *net, *readfile, *swapfile_out[2]; /* lame stereo hack */
	filter_paramdb_t *db;
	filter_param_t *param;
	filter_port_t *source, *dest;
	filter_pipe_t *pipe;
	swfd_t fd;
	gint i, channels;
	glong name, names[GTK_SWAPFILE_BUFFER_MAX_TRACKS];
	
	char * filenamebuffer, *groupnamebuffer;

	GtkWidget * filenameentry;
	GtkWidget * groupnameentry;
	
	filenamebuffer = calloc(100,sizeof(char));
	groupnamebuffer = calloc(100,sizeof(char));
	
	dialog = gnome_dialog_new("Import Audio File",GNOME_STOCK_BUTTON_CANCEL, GNOME_STOCK_BUTTON_OK,NULL);
	dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));
	
	filenameentry = gnome_file_entry_new("import_cb","Filename");
	gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(filenameentry))),
			   "changed",changeString_cb,&filenamebuffer);
	

	create_label_widget_pair(dialogVbox,"Filename",filenameentry);

	groupnameentry = gtk_entry_new();
	gtk_signal_connect(GTK_OBJECT(groupnameentry),"changed",
			   changeString_cb,&groupnamebuffer);
	create_label_widget_pair(dialogVbox,"Groupname",groupnameentry);
	
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog))){

		
		
		//sprintf(cbuffer,"(file-to-swap \"%s\" %d %d)",filenamebuffer,name,name+1);
		//gh_eval_str(cbuffer);


		if (!(p_readfile = plugin_get("read_file"))) {
			DPRINTF("read_file not found\n");
			return;
		} else
			g_assert((readfile = filter_instantiate(p_readfile)));

		net = filter_creat(NULL);

		filter_add_node(net, readfile, "readfile");

		if (!(p_swapfile_out = plugin_get("swapfile_out"))) {
			DPRINTF("swapfile_out not found\n");
			return;
		} 
		
		g_assert((db = filter_paramdb(readfile)));
		g_assert((param = filterparamdb_get_param(db, "filename")));
		filterparam_set(param, &filenamebuffer); 
		g_assert((source = filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT)));
		
				
		if(!groupnamebuffer)
			groupnamebuffer = g_basename(filenamebuffer);
		newItem = glame_tree_item_new_group(groupnamebuffer);
		sw_glame_tree_append(GTK_OBJECT(item), GLAME_TREE_ITEM(newItem));
		
		i = 0;
		do {
			while((fd=sw_open((name=(rand()>>8)), O_CREAT | O_EXCL, TXN_NONE))==-1);
			names[i] = name;
			sw_close(fd);
			DPRINTF("Found free file at #%li\n", name);
			g_assert((swapfile_out[i] = filter_instantiate(p_swapfile_out)));
			g_assert((db = filter_paramdb(swapfile_out[i])));
			g_assert((param = filterparamdb_get_param(db, "filename")));
			filterparam_set(param, &name);
			filter_add_node(net, swapfile_out[i], "swapfile_out");
			g_assert((dest = filterportdb_get_port(filter_portdb(swapfile_out[i]), 
				 PORTNAME_IN)));
			if (!(filterport_connect(source, dest))) {
				DPRINTF("Connection failed for channel %d\n",i+1);
				sw_unlink(name);
				filter_delete(swapfile_out[i]);
				goto launch;
			} else {
				pipe = filterport_get_pipe(dest);
				newTrak = glame_tree_item_new_file((filterpipe_sample_hangle(pipe)<0)?"Left":"Right",name,
								   filterpipe_sample_rate(pipe));
				sw_glame_tree_append(GTK_OBJECT(newItem), GLAME_TREE_ITEM(newTrak));
				i++;
			}
		} while (i < GTK_SWAPFILE_BUFFER_MAX_TRACKS);
launch:
		channels = i;
		filter_launch(net);
		filter_start(net);
		filter_wait(net);	/* ok we could do that more nicely, but not now.. */
		
		filter_delete(net);
		

		/* update items */
		i = 0;
		do {
		  newTrak = glame_tree_find_filename(GTK_OBJECT(swapfile_tree),names[i]);
		  glame_tree_item_update(newTrak);
		  i++;
		} while (i < channels);
	}
}


/* Helper for item insertion (includes signal connect and show)
 */
static void sw_glame_tree_append(GtkObject *tree, GlameTreeItem *item)
{
	gtk_signal_connect_after(GTK_OBJECT(item), "button_press_event",
			         (GtkSignalFunc)rmb_menu_cb, (gpointer)NULL);
	glame_tree_append(tree, item);
	gtk_widget_show(GTK_WIDGET(item));
}


/*
 * Construct an GtkTree out of an xml document node.
 */
static void insert_node(GtkObject *tree, xmlNodePtr node);
static void insert_childs(GtkObject *tree, xmlNodePtr node)
{
        node = node->xmlChildrenNode;
        if (!node)
		return;

	while (node) {
		insert_node(tree, node);
		node = node->next;
	}
}
static void insert_node(GtkObject *tree, xmlNodePtr node)
{
        GtkWidget *item;

	if (strcmp(node->name, "file") == 0) {
		char *ilabel, *c;
		long ifd, irate;
		int fd;

		/* Extract file information. */
		if (!(c = xmlGetProp(node, "label")))
			c = "(unnamed)";
		ilabel = strdup(c);
		if (!(c = xmlGetProp(node, "fd")))
			c = "-1";
		ifd = atoi(c);
		if (!(c = xmlGetProp(node, "rate")))
			c = "44100";
		irate = atoi(c);

		/* Check, if the file is really there (and update info) */
		if ((fd = sw_open(ifd, O_RDONLY, TXN_NONE)) != -1) {
			sw_close(fd);
		} else {
			DPRINTF("%s does not exist\n", ilabel);
			return;
		}

		item = glame_tree_item_new_file(ilabel, ifd, irate);

	} else if (strcmp(node->name, "group") == 0) {
		char *ilabel, *c;

		if (!(c = xmlGetProp(node, "label")))
			c = "(unnamed)";
		ilabel = strdup(c);

		item = glame_tree_item_new_group(ilabel);

	} else if (strcmp(node->name, "swapfile") == 0) {
		DERROR("Illegal <swapfile> tag position");
	} else {
		return;
	}

	sw_glame_tree_append(GTK_OBJECT(tree), GLAME_TREE_ITEM(item));
	insert_childs(GTK_OBJECT(item), node);
}


/*
 * Scan the swapfile and add all non-xmled files to a seperate
 * group.
 */
void scan_swap(GtkTree *tree)
{
	GlameTreeItem *item;
	GlameTreeItem *grp;
	GtkTree *grptree;
	SWDIR *dir;
	long name;

	/* Add unknown group and iterate through all swapfiles adding those
	 * not already contained in the tree. */
	if (!(grp = glame_tree_find_group(GTK_OBJECT(tree), "unknown"))) {
		grp = GLAME_TREE_ITEM(glame_tree_item_new_group("unknown"));
		sw_glame_tree_append(GTK_OBJECT(tree), grp);
	}
	dir = sw_opendir();
	while ((name = sw_readdir(dir)) != -1) {
		if (name == 0)
			continue;
		if ((item = glame_tree_find_filename(GTK_OBJECT(tree), name)))
			continue;
		item = GLAME_TREE_ITEM(glame_tree_item_new_file("unnamed", name, -1));
		sw_glame_tree_append(GTK_OBJECT(grp), item);
	}
	sw_closedir(dir);

	/* Check if the unknown group is empty and if so, remove it.
	 * FIXME - segfaults */
#if 0
	grptree = GTK_TREE_ITEM_SUBTREE(grp);
	if (!grptree
	    || g_list_length(gtk_container_children(GTK_CONTAINER(grptree)))) {
		glame_tree_remove(grp);
		gtk_object_destroy(GTK_OBJECT(grp));
	}
#endif
}


/*
 * Dump the GtkTree into an xml document node.
 */
static void dump_tree(GtkTree *tree, xmlNodePtr node);
static void dump_item(GlameTreeItem *item, xmlNodePtr node)
{
	xmlNodePtr child;
	char s[256];

	if (item->type == GLAME_TREE_ITEM_GROUP) {
		child = xmlNewChild(node, NULL, "group", NULL);
		xmlSetProp(child, "label", item->label);
		dump_tree(GTK_TREE(GTK_TREE_ITEM_SUBTREE(item)), child);
	} else if (item->type == GLAME_TREE_ITEM_FILE) {
		child = xmlNewChild(node, NULL, "file", NULL);
		xmlSetProp(child, "label", item->label);
		snprintf(s, 255, "%li", item->swapfile_name);
		xmlSetProp(child, "fd", s);
		snprintf(s, 255, "%i", item->sample_rate);
		xmlSetProp(child, "rate", s);
	}
}
static void dump_tree(GtkTree *tree, xmlNodePtr node)
{
	if (!tree)
		return;
	gtk_container_foreach(GTK_CONTAINER(tree),
			      (GtkCallback)dump_item, node);
}


/*
 * Externally visible API.
 */

GtkWidget *glame_swapfile_gui_new(const char *swapfile)
{
	xmlDocPtr doc;
	GtkWidget *tree;
	char *xml;
	int fd;

	if (swapfile_tree)
		return NULL;
	if (swapfile_open(swapfile, 0) == -1) {
		if (errno != EBUSY) {
			perror("ERROR: Unable to open swap");
			return NULL;
		}
		fprintf(stderr, "WARNING: Unclean swap - running fsck\n");
		if (swapfile_fsck(swapfile) == -1) {
			perror("ERROR: Fsck failed");
			return NULL;
		}
		fprintf(stderr, "WARNING: Fsck successful\n");
		if (swapfile_open(swapfile, 0) == -1) {
			perror("ERROR: Still cannot open swap");
			return NULL;
		}
	}

	/* Read swapfile 0 into a character buffer for libxml. */
	if ((fd = sw_open(0, O_RDONLY, TXN_NONE)) != -1) {
		struct sw_stat st;
		if (sw_fstat(fd, &st) == -1
		    || !(xml = malloc(st.size+1))) {
			sw_close(fd);
			return NULL;
		}
		if (sw_read(fd, xml, st.size) != st.size) {
			sw_close(fd);
			free(xml);
			return NULL;
		}
		xml[st.size] = '\0';
		sw_close(fd);
	} else {
		/* Seems to be empty swapfile - use "default" xml. */
		xml = strdup("\
<?xml version=\"1.0\"?>
<swapfile>
</swapfile>");
	}

	/* Try to parse the xml string. */
	if (!(doc = xmlParseMemory(xml, strlen(xml)))) {
		free(xml);
		return NULL;
	}

	/* Create the toplevel tree. */
        tree = gtk_tree_new();
        gtk_tree_set_view_mode(GTK_TREE(tree), GTK_TREE_VIEW_LINE);
        gtk_tree_set_view_lines(GTK_TREE(tree), TRUE);
        gtk_tree_set_selection_mode(GTK_TREE(tree), GTK_SELECTION_BROWSE);

	/* Recurse down the xml tree. */
        insert_childs(GTK_OBJECT(tree), xmlDocGetRootElement(doc));

	/* Search for not xml-ed swapfile. */
	scan_swap(GTK_TREE(tree));

	swapfile_tree = GTK_TREE(tree);
	gtk_widget_show(tree);

	free(xml);
	xmlFreeDoc(doc);

	return GTK_WIDGET(swapfile_tree);
}


void glame_swapfile_gui_destroy()
{
	xmlDocPtr doc;
	xmlNodePtr root;
	xmlChar *xml;
	int size, fd;

	if (!swapfile_tree)
		return;

	/* Save swapfile_tree as xml into swapfile 0 */
	doc = xmlNewDoc("1.0");
	root = xmlNewNode(NULL, "swapfile");
	dump_tree(swapfile_tree, root);
	xmlDocSetRootElement(doc, root);
	xmlDocDumpMemory(doc, &xml, &size);
	DPRINTF("%i bytes xml %s\n", size, xml);
	fd = sw_open(0, O_RDWR|O_CREAT|O_TRUNC, TXN_NONE);
	sw_write(fd, xml, size);
	sw_close(fd);
	free(xml);
	xmlFreeDoc(doc);

	/* FIXME: kill swapfile_tree */
	gtk_widget_destroy(swapfile_tree);
	swapfile_tree = NULL;

	swapfile_close();
}

void glame_swapfile_gui_add_toplevel_group(const char *name)
{
	GlameTreeItem *grp = GLAME_TREE_ITEM(glame_tree_item_new_group(name));
	sw_glame_tree_append(GTK_OBJECT(swapfile_tree), grp);
}
