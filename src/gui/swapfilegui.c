/*
 * swapfilegui.c
 *
 * Copyright (C) 2001 Richard Guenther
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
static GtkWidget *swapfile_gui = NULL;
static GtkTree *swapfile_tree = NULL;


/* Forward declarations. */
static int rmb_menu_cb(GtkWidget *item, GdkEventButton *event,
		        gpointer data);

static void edit_cb(GtkWidget *menu, GlameTreeItem *item);
static void import_cb(GtkWidget *menu, GlameTreeItem *item);

static GnomeUIInfo group_menu_data[] = {
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Edit", "edit", edit_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Merge with parent", "import", NULL, NULL), /* FIXME */
        GNOMEUIINFO_ITEM("Flatten childs", "flatten", NULL, NULL), /* FIXME */
        GNOMEUIINFO_ITEM("Link selected", "link", NULL, NULL), /* FIXME */
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Import...", "import", import_cb, NULL),
        GNOMEUIINFO_ITEM("Delete", "delete", NULL, NULL), /* FIXME */
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_END
};
static GnomeUIInfo file_menu_data[] = {
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Edit", "edit", edit_cb, NULL),
        GNOMEUIINFO_SEPARATOR,
        GNOMEUIINFO_ITEM("Delete", "delete", NULL, NULL), /* FIXME */
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

void
create_label_widget_pair(GtkWidget *vbox,const char *clabel, GtkWidget *w)
{
	GtkWidget*hbox,*label;
	hbox = gtk_hbox_new(TRUE,5);
	gtk_container_add(GTK_CONTAINER(vbox),hbox);
	label = gtk_label_new(clabel);
	gtk_container_add(GTK_CONTAINER(hbox),label);
	gtk_container_add(GTK_CONTAINER(hbox),w);
	gtk_widget_show(hbox);
	gtk_widget_show(label);
	gtk_widget_show(w);

}

static void import_cb(GtkWidget *menu, GlameTreeItem *item)
{

	/*   Blah!
	     FIXME!
	*/
	gchar *buffer;
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
	ssize_t len;
	struct sw_stat st;
	char cbuffer[50];
	
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
			g_print("read_file not found");
			return;
		} else
			g_assert((readfile = filter_instantiate(p_readfile)));

		net = filter_creat(NULL);

		filter_add_node(net, readfile, "readfile");

		if (!(p_swapfile_out = plugin_get("swapfile_out"))) {
			g_print("swapfile_out not found");
			return;
		} 
		
		g_assert((db = filter_paramdb(readfile)));
		g_assert((param = filterparamdb_get_param(db, "filename")));
		filterparam_set(param, &filenamebuffer); 
		g_assert((source = filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT)));
		
				
		if(!groupnamebuffer)
			groupnamebuffer = g_basename(filenamebuffer);
		newItem = glame_tree_item_new_group(groupnamebuffer);
		gtk_signal_connect_after(GTK_OBJECT(newItem), "button_press_event",
					 (GtkSignalFunc)rmb_menu_cb, (gpointer)NULL);
		glame_tree_append(item,newItem);
		
		i = 0;
		do {
			while((fd=sw_open((name=rand()), O_CREAT | O_EXCL, TXN_NONE))==-1);
			names[i] = name;
			sw_close(fd);
			g_print("Found free file at #%d\n", name);
			g_assert((swapfile_out[i] = filter_instantiate(p_swapfile_out)));
			g_assert((db = filter_paramdb(swapfile_out[i])));
			g_assert((param = filterparamdb_get_param(db, "filename")));
			filterparam_set(param, &name);
			filter_add_node(net, swapfile_out[i], "swapfile_out");
			g_assert((dest = filterportdb_get_port(filter_portdb(swapfile_out[i]), 
				 PORTNAME_IN)));
			if (!(filterport_connect(source, dest))) {
				g_print("Connection failed for channel %d\n",i+1);
				sw_unlink(name);
				filter_delete(swapfile_out[i]);
				goto launch;
			} else {
				pipe = filterport_get_pipe(dest);
				newTrak = glame_tree_item_new_file((filterpipe_sample_hangle(pipe)<0)?"Left":"Right",name,
								   filterpipe_sample_rate(pipe), 
								   0);
				gtk_signal_connect_after(GTK_OBJECT(newTrak), "button_press_event",
							 (GtkSignalFunc)rmb_menu_cb, (gpointer)NULL);
				glame_tree_append(newItem,newTrak);
				i++;
			}
		} while (i < GTK_SWAPFILE_BUFFER_MAX_TRACKS);
launch:
		channels = i;
		filter_launch(net);
		filter_start(net);
		filter_wait(net);	/* ok we could do that more nicely, but not now.. */
		
		filter_delete(net);
	}

	
	// find out size
	
	i = 0;
	do {
		fd = sw_open(names[i],O_RDONLY,TXN_NONE);
		sw_fstat(fd,&st);
		sw_close(fd);
		len = st.size/SAMPLE_SIZE;
	
		newTrak = glame_tree_find_filename(swapfile_tree,names[i]);
		GLAME_TREE_ITEM(newTrak)->size = len;
		glame_tree_item_update(newTrak);
		i++;
	} while (i < channels);
/*
	gtk_signal_connect_after(GTK_OBJECT(newTrak), "button_press_event",
				 (GtkSignalFunc)rmb_menu_cb, (gpointer)NULL);
	glame_tree_append(newItem,newTrak);
	newTrak = glame_tree_item_new_file("right",name+1,44100,-1);
	gtk_signal_connect_after(GTK_OBJECT(newTrak), "button_press_event",
				 (GtkSignalFunc)rmb_menu_cb, (gpointer)NULL);
	glame_tree_append(newItem,newTrak);
*/
		
};

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
		long ifd, irate, isize;
		struct sw_stat st;
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
		if (!(c = xmlGetProp(node, "size")))
			c = "-1";
		isize = atoi(c);

		/* Check, if the file is really there (and update info) */
		if ((fd = sw_open(ifd, O_RDONLY, TXN_NONE)) != -1) {
			sw_fstat(fd, &st);
			if (isize != -1 && st.size != isize*SAMPLE_SIZE) {
				DPRINTF("size %li does not match %i\n",
					isize, st.size);
			}
			isize = st.size/SAMPLE_SIZE;
			sw_close(fd);
		} else {
			DPRINTF("%s does not exist\n", ilabel);
			return;
		}

		item = glame_tree_item_new_file(ilabel, ifd, irate, isize);

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

	gtk_signal_connect_after(GTK_OBJECT(item), "button_press_event",
			   (GtkSignalFunc)rmb_menu_cb, (gpointer)NULL);
	glame_tree_append(GTK_OBJECT(tree), GLAME_TREE_ITEM(item));
	gtk_widget_show(item);

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
	SWDIR *dir;
	long name;

	if (!(grp = glame_tree_find_group(GTK_OBJECT(tree), "unnamed"))) {
		grp = GLAME_TREE_ITEM(glame_tree_item_new_group("unnamed"));
		glame_tree_append(GTK_OBJECT(tree), grp);
	}
	dir = sw_opendir();
	while ((name = sw_readdir(dir)) != -1) {
		if (name == 0)
			continue;
		if ((item = glame_tree_find_filename(GTK_OBJECT(tree), name)))
			continue;
		item = GLAME_TREE_ITEM(glame_tree_item_new_file("unnamed", name, -1, -1));
		glame_tree_append(GTK_OBJECT(grp), item);
	}
	sw_closedir(dir);
}


/*
 * Dump the GtkTree into an xml document node.
 */
static void dump_tree(GtkTree *tree, xmlNodePtr node);
static void dump_item(GlameTreeItem *item, xmlNodePtr node)
{
	xmlNodePtr child;
	char s[256];

	if (GTK_TREE_ITEM_SUBTREE(item)) {
		child = xmlNewChild(node, NULL, "group", NULL);
		xmlSetProp(child, "label", item->label);
		dump_tree(GTK_TREE(GTK_TREE_ITEM_SUBTREE(item)), child);
	} else {
		child = xmlNewChild(node, NULL, "file", NULL);
		xmlSetProp(child, "label", item->label);
		snprintf(s, 255, "%li", item->swapfile_name);
		xmlSetProp(child, "fd", s);
		snprintf(s, 255, "%i", item->sample_rate);
		xmlSetProp(child, "rate", s);
		snprintf(s, 255, "%li", item->size);
		xmlSetProp(child, "size", s);
	}
}
static void dump_tree(GtkTree *tree, xmlNodePtr node)
{
	gtk_container_foreach(GTK_CONTAINER(tree),
			      (GtkCallback)dump_item, node);
}


/*
 * Externally visible API.
 */

GtkWidget *glame_swapfile_gui_new(const char *swapfile)
{
	xmlDocPtr doc;
	GtkWidget *tree, *window;
	char *xml;
	int fd;

	if (swapfile_gui)
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
<group label=\"project1\"/>
</swapfile>");
	}

	/* Try to parse the xml string. */
	if (!(doc = xmlParseMemory(xml, strlen(xml)))) {
		free(xml);
		return NULL;
	}

	/* Create the swapfile window. */
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "Swapfile browser");

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
	gtk_container_add(GTK_CONTAINER(window), tree);
	gtk_widget_show(tree);

	swapfile_gui = window;
	free(xml);
	xmlFreeDoc(doc);

	return swapfile_gui;
}


void glame_swapfile_gui_destroy()
{
	xmlDocPtr doc;
	xmlNodePtr root;
	xmlChar *xml;
	int size, fd;

	if (!swapfile_gui)
		return;

	/* Save swapfile_gui as xml into swapfile 0 */
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

	/* FIXME: kill swapfile_gui */
	gtk_widget_destroy(swapfile_gui);
	swapfile_gui = NULL;
	swapfile_tree = NULL;

	swapfile_close();
}
