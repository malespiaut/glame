#include "gltreeitem.h"
#include "swapfilegui.h"


/* The swapfile widget. */
static GtkWidget *swapfile_gui = NULL;


static void insert_node(GtkTree *tree, xmlNodePtr node)
{
        GtkWidget *item;

        item = glame_tree_item_new();
	/* FIXME - fill info, connect signals */
	glame_tree_item_update_label(GLAME_TREE_ITEM(item));
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


GtkWidget *glame_swapfile_gui_new(const char *swapfile)
{
	xmlDocPtr doc;
	GtkWidget *tree;
	char *xml;

	if (swapfile_gui)
		return NULL;
	if (swapfile_open(swapfile, 0) == -1)
		return NULL;

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
<glame>
</glame>");
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
        insert_node(GTK_TREE(tree), xmlDocGetRootElement(doc));

	swapfile_gui = tree;
	free(xml);
	/* FIXME: free doc */

	return swapfile_gui;
}

void glame_swapfile_gui_destroy()
{
	if (!swapfile_gui)
		return;

	/* FIXME: save swapfile_gui as xml into swapfile 0 */

	/* FIXME: kill swapfile_gui */
	swapfile_gui = NULL;

	swapfile_close();
}
