/*
 * gui_main.c
 *
 * Copyright (C) 2001 Johannes Hirche
 * $Id: gui_main.c,v 1.4 2001/03/15 14:41:02 xwolf Exp $
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
#  include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include <gnome.h>
#include "swapfile.h"
#include "edit_filter/gui.h"
#include "swapfilegui.h"

/* The application */
GtkWidget *app;

/* Forward declarations. */
static void create_new_project_cb(GtkWidget *menu, void * blah);
extern void gui_quit(GtkWidget *widget, gpointer data);
GtkWidget * gnome_dialog_file_request(const char *windowtitle,
				      const char *label,
				      char ** returnbuffer);

static GnomeUIInfo swapfile_menu_uiinfo[] = {
	GNOMEUIINFO_MENU_NEW_ITEM (N_("_New Project"), "Creates a new Project group", create_new_project_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_MENU_EXIT_ITEM (gui_quit, NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo filter_menu_uiinfo[] = {
	GNOMEUIINFO_ITEM(N_("_Open Filternetwork editor"),"Opens a new filternetwork window",gui_network_new,NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo help_menu_uiinfo[] =
{
  GNOMEUIINFO_MENU_ABOUT_ITEM (handle_about, NULL),
  GNOMEUIINFO_END
};


static GnomeUIInfo menubar_uiinfo[] =
{
  {
    GNOME_APP_UI_SUBTREE, N_("Project"),
    NULL,
    swapfile_menu_uiinfo, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, 0, NULL
  },
  {
    GNOME_APP_UI_SUBTREE, N_("Filternetwork"),
    NULL,
    filter_menu_uiinfo, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, 0, NULL
  },
  
  GNOMEUIINFO_MENU_HELP_TREE (help_menu_uiinfo),
  GNOMEUIINFO_END
};


GtkWidget * gnome_dialog_file_request(const char *windowtitle,
				      const char *label,
				      char ** returnbuffer)
{
	GtkWidget * dialog;
	GtkWidget * fileEntry;
	GtkWidget * dialogVbox;
	
	
	dialog = gnome_dialog_new(windowtitle,GNOME_STOCK_BUTTON_CANCEL,GNOME_STOCK_BUTTON_OK,NULL);
        dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));

        fileEntry = gnome_file_entry_new(NULL,label);
        gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fileEntry))),
                           "changed",changeString,returnbuffer);
        create_label_widget_pair(dialogVbox,"Filename",fileEntry);
	return dialog;
}





void 
create_new_project_cb(GtkWidget *menu, void * blah)
{
	GtkWidget * dialog, *error;
	char *bla;
	fprintf(stderr,"FIXME\n");
	return;
	bla=calloc(sizeof(char),255);
	dialog = gnome_request_dialog(FALSE,"Enter Project Name",
				      "Unnamed",
				      255,
				      NULL,
				      &bla,
				      app);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog))){
		fprintf(stderr,"%s\n",bla);		
	}
	free(bla);
}

GtkWidget *
gui_main_new(void)
{
	GtkWidget *app1;
	GtkWidget *dock1;
	GtkWidget *appbar1;
	
	app1 = gnome_app_new ("Glame", NULL);
	gtk_object_set_data (GTK_OBJECT (app1), "app1", app1);
	
	dock1 = GNOME_APP (app1)->dock;
	gtk_widget_ref (dock1);
	gtk_object_set_data_full (GTK_OBJECT (app1), "dock1", dock1,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (dock1);
	
	gnome_app_create_menus (GNOME_APP (app1), menubar_uiinfo);
	
	appbar1 = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);

	gtk_widget_show (appbar1);
	gnome_app_set_statusbar (GNOME_APP (app1), appbar1);
	
	gnome_app_install_menu_hints (GNOME_APP (app1), menubar_uiinfo);
	app = app1;
	return app1;
}
