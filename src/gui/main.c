/*
 * main.c
 *
 * $Id: main.c,v 1.14 2001/03/16 09:58:09 richi Exp $
 *
 * Copyright (C) 2001 Johannes Hirche, Richard Guenther
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

#include <stdlib.h>
#include "swapfile.h"
#include "glmid.h"
#include "swapfilegui.h"
#include "waveeditgui.h"
#include "edit_filter/gui.h"


/* Globals. */
static char *swname;
static GtkWidget *swapfile;
static GtkWidget *app;



/* Forward declarations. */
static void create_new_project_cb(GtkWidget *menu, void * blah);
static void gui_quit(GtkWidget *widget, gpointer data);
GtkWidget * gnome_dialog_file_request(const char *windowtitle,
				      const char *label,
				      char ** returnbuffer);
void glame_about(void);


/* Menus. */
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
  GNOMEUIINFO_MENU_ABOUT_ITEM (glame_about, NULL),
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



static void create_new_project_string_cb(gchar *string, gpointer data)
{
	if (string) {
		strncpy(data, string, 255);
		free(string);
	}
}
static void create_new_project_cb(GtkWidget *menu, void * blah)
{
	GtkWidget *dialog;
	char name[256];

	dialog = gnome_request_dialog(FALSE, "Enter project name",
				      "Unnamed", 255,
				      create_new_project_string_cb, name,
				      app);
	if(gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;

	glame_swapfile_gui_add_toplevel_group(name);
}


/*
 * The GLAME About dialog.
 */

static void glame_about(void)
{
	const gchar *authors[]={
		"Richard Guenther [Richi]",
		"Alexander Ehlert [OzMag]",
		"Daniel Kobras [*nold]",
		"Johannes Hirche [XWolf]",
		"and others",
		NULL
	};
	GtkWidget *about;

	about = gnome_about_new ("GLAME", VERSION, 
				 _("Copyright (C) 1999,2000 Alexander Ehlert, Richard Guenther."),
				 authors,
				 _("GLAME comes with ABSOLUTELY NO WARRANTY. \nThis is free software."),
				 GLAME_LOGO);
	gtk_object_set_data (GTK_OBJECT (about), "about", about);
	gtk_window_set_modal (GTK_WINDOW (about), TRUE);
	gtk_window_set_wmclass (GTK_WINDOW (about), "Glameabout", "Glame");
	gtk_widget_show(about);
}


/*
 * Real main and cleanup stuff.
 */

static void gui_quit(GtkWidget *widget, gpointer data)
{
	glame_waveedit_cleanup();
	glame_swapfile_gui_destroy();
	gtk_main_quit();
}

static void gui_main()
{
	GtkWidget *dock;
	GtkWidget *appbar;
	gboolean defaultval;
	char configpath[255];
	char homedir[255];
	char * path;

	/* check for prefs */
	sprintf(configpath,"/%s/",g_get_prgname());
	gnome_config_push_prefix(configpath);
	sprintf(configpath,"swapfile/defaultpath=%s/.glameswap",g_get_home_dir());
	path = gnome_config_get_string_with_default(configpath,&defaultval);
	if(defaultval){
		sprintf(configpath,"swapfile/defaultpath");;
		sprintf(homedir,"%s/.glameswap",g_get_home_dir());
		gnome_config_set_string(configpath,homedir);
		gnome_config_sync();
	}
	fprintf(stderr,"path: %s\n",path);
	if(!g_file_test(path,G_FILE_TEST_ISDIR)){
		if(swapfile_creat(path,-1)){
			DERROR("error creating swapfile\n");
		}
	}

	/* create swapfile gui */
	swapfile = glame_swapfile_gui_new(path);
	g_free(path);
	if (!swapfile)
		return;

	/* Create main window. */
	app = gnome_app_new ("Glame", NULL);
	gtk_object_set_data (GTK_OBJECT (app), "app", app);
	dock = GNOME_APP (app)->dock;
	gtk_widget_ref (dock);
	gtk_object_set_data_full (GTK_OBJECT (app), "dock", dock,
                                 (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (dock);
	gnome_app_create_menus (GNOME_APP (app), menubar_uiinfo);
	appbar = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);
	gtk_widget_show (appbar);
	gnome_app_set_statusbar (GNOME_APP (app), appbar);
	gnome_app_install_menu_hints (GNOME_APP (app), menubar_uiinfo);

	/* Connect signals and insert swapfile gui into the main window. */
	gtk_signal_connect(GTK_OBJECT(app), "delete-event",
			   GTK_SIGNAL_FUNC(gui_quit), NULL);
	gtk_widget_show(swapfile);
	gnome_app_set_contents(GNOME_APP(app),swapfile);
	gtk_widget_show(app);

	/* Cleanup handler. */
	atexit((void (*)(void))gui_quit);

	/* main loop */
       	gtk_main();
}


int main(int argc, char **argv)
{
	/* setup gnome/gtk  */
	gnome_init("glame", VERSION, argc, argv);

	/* init glame */
	glame_init_with_guile(gui_main);

	return 1;
}
