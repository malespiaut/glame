/*
 * main.c
 *
 * $Id: main.c,v 1.20 2001/03/30 08:53:35 richi Exp $
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
#include "filtergui.h"
#include "glame_gui_utils.h"
#include "gltreeitem.h"
#include "gpsm.h"

/* Globals. */
static char *swname = NULL;
static GtkWidget *swapfile;
static GtkWidget *app;



/* Forward declarations. */
static void create_new_project_cb(GtkWidget *menu, void * blah);
static void gui_quit(GtkWidget *widget, gpointer data);
static void preferences_cb(GtkWidget *menu,void *blah);
GtkWidget * gnome_dialog_file_request(const char *windowtitle,
				      const char *label,
				      char ** returnbuffer);
void glame_about(void);
//extern GtkTree *swapfile_tree;

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

static GnomeUIInfo glame_setting_uiinfo[] =
{
	        GNOMEUIINFO_MENU_PREFERENCES_ITEM (preferences_cb, NULL),
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
  
  GNOMEUIINFO_MENU_SETTINGS_TREE (glame_setting_uiinfo),
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



static void create_new_project_cb(GtkWidget *menu, void * blah)
{
	char* name;
	GlameTreeItem *grp;
	name = "Unnamed\0";
//	grp = GLAME_TREE_ITEM(glame_tree_item_new_group(name));
//	sw_glame_tree_append(GTK_OBJECT(swapfile_tree),grp);
//	edit_tree_label(grp);
	
}

static void
setBoolean(GtkWidget * foo, gboolean * bar)
{
	*bar = TRUE;
}

void
preferences_cb(GtkWidget * wid, void * bla)
{
	GtkWidget * prop_box;
	GtkWidget * tablabel;
	GtkWidget * vbox;
	GtkWidget * entry;
	GtkWidget * error;
	GtkWidget * notelabel;
	
	char *path,*defaultpath;
	int val = 0;
	gboolean ok=FALSE;

        path = calloc(sizeof(char),255);
        prop_box = gnome_property_box_new();
        tablabel = gtk_label_new(_("Swapfile"));

        vbox = gtk_vbox_new(FALSE,1);
        gtk_widget_show(vbox);

        entry = gnome_file_entry_new(NULL,"Swapfilepath");
        create_label_widget_pair(vbox,"Swapfile Path",entry);
        defaultpath = gnome_config_get_string("swapfile/defaultpath");
        gtk_entry_set_text(GTK_ENTRY(gnome_file_entry_gtk_entry(entry)),defaultpath);
        g_free(defaultpath);

        notelabel = gtk_label_new("NOTE: Swapfile settings take effect after restart only");
        gtk_widget_show(notelabel);
        gtk_container_add(GTK_CONTAINER(vbox),notelabel);

        gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(entry))),"changed",changeString,&path);
        gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->ok_button),"clicked",setBoolean,&ok);
        gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->apply_button));
        gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->help_button));
        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box),vbox,tablabel);

        gtk_widget_show(prop_box);

        gnome_dialog_run_and_close(GNOME_DIALOG(prop_box));
        if(ok){
                gnome_config_set_string("swapfile/defaultpath",path);
                gnome_config_sync();
        }
        fprintf(stderr,"dialog: %d\n",val);
}



/*
 * The GLAME About dialog.
 */

static void glame_about(void)
{
	const gchar *authors[]={
		"Richard Guenther [Richi]",
		"Alexander Ehlert [Mag]",
		"Daniel Kobras [*nold]",
		"Johannes Hirche [XWolf]",
		"and others",
		NULL
	};
	GtkWidget *about;

	about = gnome_about_new ("GLAME", VERSION, 
				 _("Copyright (C) 1999-2001 Alexander Ehlert, Richard Guenther."),
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
	gpsm_close();
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
	if(!g_file_test(path,G_FILE_TEST_ISDIR)
	   && !swname /* for debugging */){
		if(swapfile_creat(path,-1)){
			DERROR("error creating swapfile\n");
		}
	}

	/* create swapfile gui */
	if (swname)
		gpsm_init(swname);
	else
		gpsm_init(path);
	g_free(path);
	swapfile = glame_swapfile_widget_new(gpsm_root());
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

	/* remember argv[1], if necessary */
	if (argc >= 2)
		swname = argv[1];

	/* init glame */
	glame_init_with_guile(gui_main);

	return 1;
}
