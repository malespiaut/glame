#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <gnome-xml/xmlmemory.h>
#include <gnome-xml/parser.h>

#include <gnome.h>
#include <gtk/gtk.h>
#include <gtk/gtktree.h>
#include <gtk/gtktreeitem.h>

#include "util.h"
#include "tree.h"


static void node_select(GtkWidget *item, xmlNodePtr node)
{
	printf("Selected %s\n", node->name);
}

static void node_menu(GtkWidget *item, xmlNodePtr node)
{
	/* FIXME */
}

void insert_node(GtkTree *tree, xmlNodePtr node)
{
	GtkWidget *item;

	item = gtk_tree_item_new_with_label(node->name);
	gtk_signal_connect(GTK_OBJECT(item), "select",
			   (GtkSignalFunc)node_select, node);
	gtk_signal_connect(GTK_OBJECT(item), "button_press_event",
			   (GtkSignalFunc)node_menu, node);
	gtk_tree_append(tree, item);
	gtk_widget_show(item);

	node = node->xmlChildrenNode;
	if (node) {
		tree = GTK_TREE(gtk_tree_new());
		if (!tree)
			PANIC("tree NULL");
		while (node) {
			insert_node(tree, node);
			node = node->next;
		}
		gtk_tree_item_set_subtree(GTK_TREE_ITEM(item),
					  GTK_WIDGET(tree));
		gtk_widget_show(GTK_WIDGET(tree));
		gtk_tree_item_expand(GTK_TREE_ITEM(item));
	}
}



static xmlDocPtr tree_doc = NULL;
static GtkWidget *tree_widget = NULL;

void tree_delete()
{
  if (tree_widget)
    gtk_widget_unref(tree_widget);
  if (tree_doc)
    xmlFreeDoc(tree_doc);
  tree_widget = NULL;
  tree_doc = NULL;
}

GtkWidget *tree_deserialize(const char *xml)
{
  xmlDocPtr doc;
  GtkWidget *tree;

  if (!xml)
    xml = "<?xml version=\"1.0\"?><glame></glame>";
  doc = xmlParseMemory((char *)xml, strlen(xml));
  if (!doc)
    return NULL;

	tree = gtk_tree_new();
	gtk_tree_set_view_mode(GTK_TREE(tree), GTK_TREE_VIEW_LINE);
	gtk_tree_set_view_lines(GTK_TREE(tree), TRUE);
	gtk_tree_set_selection_mode(GTK_TREE(tree), GTK_SELECTION_BROWSE);
	insert_node(GTK_TREE(tree), xmlDocGetRootElement(doc));
	tree_delete();
	tree_doc = doc;
	tree_widget = tree;

	return tree;
}

char *tree_serialize()
{
  xmlChar *xml;
  int size;

  xmlDocDumpMemory(tree_doc, &xml, &size);

  /* FIXME: xmlChar is not char* */
  return (char *)xml;
}


/* Adds the xml node node as child to the parent xml node and
 * as gtk tree item at the appropriate place. */
static void tree_add_xml_gui(xmlNodePtr parent, xmlNodePtr node)
{
  xmlAddChild(parent, node);
  /* FIXME */
  insert_node(GTK_TREE(tree_widget), node);
}

int tree_add_file(long swname, const char *url, const char *name)
{
  xmlNodePtr node;
  char s[32];

  if (!tree_doc || !name)
    return -1;

  /* Create node
   *   <file swfile="17" url="/home/joe/test.wav" name="test wave"/>
   * with url being optional.
   */
  node = xmlNewNode(NULL, "file");
  snprintf(s, 32, "%li", swname);
  xmlSetProp(node, "swfile", s);
  if (url)
    xmlSetProp(node, "url", url);
  xmlSetProp(node, "name", name);

  /* Add as "global" node for now. */
  tree_add_xml_gui(xmlDocGetRootElement(tree_doc), node);

  return 0;
}
