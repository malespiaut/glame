#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <xmlmemory.h>
#include <parser.h>

#include <gnome.h>
#include <gtk/gtk.h>
#include <gtk/gtktree.h>
#include <gtk/gtktreeitem.h>

#include "swapfile.h"
#include "util.h"
#include "tree.h"


static void quit(GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
}

static void load(GtkMenuItem *menuItem, gpointer data)
{
  /* FIXME */
}

static GnomeUIInfo file1_menu_uiinfo[] =
{
  {
    GNOME_APP_UI_ITEM, "Load",
    NULL,
    load, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
    0, 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  {
    GNOME_APP_UI_ITEM, "Quit",
    NULL,
    quit, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_EXIT,
    0, 0, NULL
  },
  GNOMEUIINFO_END
};

static GnomeUIInfo menubar1_uiinfo[] =
{
  {
    GNOME_APP_UI_SUBTREE, "File",
    NULL,
    file1_menu_uiinfo, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_BLANK,
    0, 0, NULL
  },
  GNOMEUIINFO_END
};

static GtkWidget*
create_window1 (GtkWidget *tree)
{
  GtkWidget *window1;
  GtkWidget *vbox1;
  GtkWidget *menubar1;

  window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_object_set_data (GTK_OBJECT (window1), "window1", window1);
  gtk_window_set_title (GTK_WINDOW (window1), "window1");

  vbox1 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "vbox1", vbox1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox1);
  gtk_container_add (GTK_CONTAINER (window1), vbox1);

  menubar1 = gtk_menu_bar_new ();
  gtk_widget_ref (menubar1);
  gtk_object_set_data_full (GTK_OBJECT (window1), "menubar1", menubar1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (menubar1);
  gtk_box_pack_start (GTK_BOX (vbox1), menubar1, FALSE, FALSE, 0);
  gnome_app_fill_menu (GTK_MENU_SHELL (menubar1), menubar1_uiinfo,
                       NULL, FALSE, 0);

  gtk_widget_ref (menubar1_uiinfo[0].widget);
  gtk_object_set_data_full (GTK_OBJECT (window1), "file1",
                            menubar1_uiinfo[0].widget,
                            (GtkDestroyNotify) gtk_widget_unref);

  gtk_widget_ref (file1_menu_uiinfo[0].widget);
  gtk_object_set_data_full (GTK_OBJECT (window1), "load1",
                            file1_menu_uiinfo[0].widget,
                            (GtkDestroyNotify) gtk_widget_unref);

  gtk_widget_ref (file1_menu_uiinfo[1].widget);
  gtk_object_set_data_full (GTK_OBJECT (window1), "separator1",
                            file1_menu_uiinfo[1].widget,
                            (GtkDestroyNotify) gtk_widget_unref);

  gtk_widget_ref (file1_menu_uiinfo[2].widget);
  gtk_object_set_data_full (GTK_OBJECT (window1), "quit1",
                            file1_menu_uiinfo[2].widget,
                            (GtkDestroyNotify) gtk_widget_unref);

  gtk_widget_ref (tree);
  gtk_object_set_data_full (GTK_OBJECT (window1), "tree", tree,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (tree);
  gtk_box_pack_start (GTK_BOX (vbox1), tree, TRUE, TRUE, 0);

  return window1;
}


int main(int argc, char **argv)
{
	GtkWidget *window, *tree;
	swfd_t fd;
	char *xml;

	if (argc < 2) {
	  fprintf(stderr, "Usage: %s swapfile\n", argv[0]);
	  exit(1);
	}

	if (swapfile_open(argv[1], 0) == -1) {
	  fprintf(stderr, "Unable to open swapfile\n");
	  exit(1);
	}


	gnome_init("treetest", "no version", argc, argv);

	xml = NULL;
	if ((fd = sw_open(0, O_RDONLY, TXN_NONE)) != -1) {
	  struct sw_stat st;
	  sw_fstat(fd, &st);
	  xml = malloc(st.size+1);
	  sw_read(fd, xml, st.size);
	  xml[st.size] = '\0';
	  sw_close(fd);
	}
	tree = tree_deserialize(xml);
	if (!tree) {
	  fprintf(stderr, "Cannot parse xml\n");
	  goto cleanup;
	}

	window = create_window1(tree);
	gtk_signal_connect(GTK_OBJECT(window), "delete_event",
			   (GtkSignalFunc)quit, NULL);
	gtk_widget_show(window);
	gtk_main();

 cleanup:
	swapfile_close();

	return 0;
}
