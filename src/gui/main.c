/*
 * main.c
 *
 * $Id: main.c,v 1.42 2001/04/24 15:53:40 xwolf Exp $
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
#include <unistd.h>
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

extern guint nPopupTimeout;
extern gboolean bMac;

/* Forward declarations. */
static void create_new_project_cb(GtkWidget *menu, void * blah);
static void gui_quit(GtkWidget *widget, gpointer data);
static void preferences_cb(GtkWidget *menu,void *blah);
static GtkWidget* glame_about(void);
extern void canvas_load_network(GtkWidget *bla, void *blu);
static void load_plugin_cb(GtkWidget* foo, void *bar);

/* Menus. */
static GnomeUIInfo swapfile_menu_uiinfo[] = {
	GNOMEUIINFO_MENU_NEW_ITEM (N_("_New Project"), "Creates a new Project group", create_new_project_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM(N_("_Load Plugin"),"Loads and registers a plugin", load_plugin_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_MENU_EXIT_ITEM (gui_quit, NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo filter_menu_uiinfo[] = {
	GNOMEUIINFO_MENU_NEW_ITEM(N_("_New Filternetwork"),"Creates a new filternetwork",gui_network_new,NULL),
	GNOMEUIINFO_MENU_OPEN_ITEM(canvas_load_network,NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo help_menu_uiinfo[] =
{
	GNOMEUIINFO_ITEM_DATA("Help","Opens a gnome help browser",gnome_help_goto,"info:glame",NULL),
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



extern void edit_tree_label(GlameTreeItem * item);
static void create_new_project_cb(GtkWidget *menu, void * blah)
{
	gpsm_grp_t *grp;
	GlameTreeItem *grpw;

	/* Create new gpsm group. */
	grp = gpsm_newgrp("Unnamed");
	gpsm_grp_insert(gpsm_root(), (gpsm_item_t *)grp, 0, -1);

	/* Find out which widget it got. */
	grpw = glame_tree_find_gpsm_item(GTK_OBJECT(swapfile), (gpsm_item_t *)grp);
	if (!grpw) {
		DPRINTF("Umm, cant find widget for new project.\n");
		return;
	}
	edit_tree_label(grpw);
}


static void load_plugin_cb(GtkWidget*bla,void*blu)
{
	GtkWidget *dialog;
	char filenamebuffer[256];

	dialog = glame_dialog_file_request("Load Plugin",
					   "main:load_plugin", "Filename",
					   NULL, filenamebuffer);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
		return;

	if (glame_load_plugin(filenamebuffer) == -1)
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Error loading plugin")));
}


/* Update globals derived from preferences and provide defaults to
 * all configurables. */
static void update_preferences()
{
	char *cfg1, *cfg2, s[256];
	filter_t *filter;
	gboolean def;

	/* Update globals. */
	nPopupTimeout = gnome_config_get_int_with_default(
		"edit_filter/popupTimeout=200", &def);
	bMac = gnome_config_get_bool_with_default(
		"edit_filter/macMode=false", &def);

	/* Set default swapfile path. */
	sprintf(s, "swapfile/defaultpath=%s/.glameswap", g_get_home_dir());
	cfg1 = gnome_config_get_string_with_default(s, &def);
	g_free(cfg1);

	/* Update IO plugin setup - audio_out */
	cfg1 = gnome_config_get_string_with_default(
		"audio_io/output_plugin=audio_out", &def);
	if (!plugin_get(cfg1)) {
		g_free(cfg1);
		cfg1 = strdup("audio_out");
		if (!plugin_get(cfg1))
			goto ain;
	}
	cfg2 = filterparam_val_string(filterparamdb_get_param(filter_paramdb((filter_t *)plugin_query(plugin_get(cfg1), PLUGIN_FILTER)), "device"));
	snprintf(s, 255, "audio_io/output_dev=%s", cfg2 ? cfg2 : "");
	cfg2 = gnome_config_get_string_with_default(s, &def);
	filter = filter_instantiate(plugin_get(cfg1));
	if (filter) {
		filterparam_set(filterparamdb_get_param(filter_paramdb(filter),
							"device"), &cfg2);
		filter_register(filter, plugin_get("audio_out"));
	}
	g_free(cfg1);
	g_free(cfg2);

 ain:
	/* Update IO plugin setup - audio_in */
	cfg1 = gnome_config_get_string_with_default(
		"audio_io/input_plugin=audio_in", &def);
	if (!plugin_get(cfg1)) {
		g_free(cfg1);
		cfg1 = strdup("audio_in");
		if (!plugin_get(cfg1))
			goto sync;
	}
	cfg2 = filterparam_val_string(filterparamdb_get_param(filter_paramdb((filter_t *)plugin_query(plugin_get(cfg1), PLUGIN_FILTER)), "device"));
	snprintf(s, 255, "audio_io/input_dev=%s", cfg2 ? cfg2 : "");
	cfg2 = gnome_config_get_string_with_default(s, &def);
	filter = filter_instantiate(plugin_get(cfg1));
	if (filter) {
		filterparam_set(filterparamdb_get_param(filter_paramdb(filter),
							"device"), &cfg2);
		filter_register(filter, plugin_get("audio_in"));
	}
	g_free(cfg1);
	g_free(cfg2);

 sync:
	/* Sync changes. */
	gnome_config_sync();
}


static void
setBoolean(GtkWidget * foo, gboolean * bar)
{
	*bar = TRUE;
}
static void
toggle_cb(GtkWidget*foo, gboolean *bar)
{
	*bar = (*bar)?FALSE:TRUE;
}

static void
preferences_cb(GtkWidget * wid, void * bla)
{
	GtkWidget * prop_box;
	GtkWidget * tablabel;
	GtkWidget * vbox;
	GtkWidget * entry;
	GtkWidget * notelabel;
	GtkWidget * macMode;
	GtkWidget *combo;
	GList *combo_items;
	char *cfg, *path, *numberbuffer, *aindev, *aoutdev;
	char *ainplugin, *aoutplugin;
	gboolean ok=FALSE;
	gboolean mac, foo;

	/* New box. */
        prop_box = gnome_property_box_new();

	/* Swapfile with
	 * - swapfile location */
        tablabel = gtk_label_new(_("Swapfile"));
        vbox = gtk_vbox_new(FALSE,1);
        gtk_widget_show(vbox);

        entry = gnome_file_entry_new("swapfilepath", "Swapfilepath");
        create_label_widget_pair(vbox, "Swapfile Path", entry);
	cfg = gnome_config_get_string_with_default("swapfile/defaultpath=~/.glameswap", &foo);
        path = alloca(256);
	strncpy(path, cfg, 255);
	g_free(cfg);
        gtk_entry_set_text(GTK_ENTRY(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(entry))), path);
        gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(entry))), "changed", changeString, &path);
        notelabel = gtk_label_new("NOTE: Swapfile settings take effect after restart only");
        gtk_widget_show(notelabel);
        gtk_container_add(GTK_CONTAINER(vbox), notelabel);
        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box),vbox,tablabel);

	/* Edit Filter with
	 * - popup timeout
	 * - mac mode */
	tablabel = gtk_label_new(_("Edit Filter"));
	vbox = gtk_vbox_new(FALSE,1);
	gtk_widget_show(vbox);

	nPopupTimeout = gnome_config_get_int("edit_filter/popupTimeout");
	numberbuffer = alloca(256);
	snprintf(numberbuffer, 255, "%d", nPopupTimeout);
	create_label_edit_pair(vbox, "Popup timeout [ms]", "popupTimeout",
			       numberbuffer);

	mac = gnome_config_get_bool("edit_filter/macMode");
	bMac = mac;
	macMode = gtk_check_button_new();
	gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(macMode),mac);
	gtk_signal_connect(GTK_OBJECT(macMode),"toggled",toggle_cb,&mac);
	gtk_widget_show(macMode);
	create_label_widget_pair(vbox,"Mac Mode",macMode);

        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box),vbox,tablabel);


	/* Audio I/O with
	 * - input device
	 * - output device */
	tablabel = gtk_label_new(_("Audio I/O"));
	vbox = gtk_vbox_new(FALSE, 1);
	gtk_widget_show(vbox);

	/* input filter */
	combo = gtk_combo_new();
	combo_items = NULL;
	if (plugin_get("audio_in"))
		combo_items = g_list_append(combo_items, _("audio_in"));
	if (plugin_get("oss_audio_in"))
		combo_items = g_list_append(combo_items, _("oss_audio_in"));
	if (plugin_get("esd_audio_in"))
		combo_items = g_list_append(combo_items, _("esd_audio_in"));
	if (plugin_get("alsa_audio_in"))
		combo_items = g_list_append(combo_items, _("alsa_audio_in"));
	if (plugin_get("sgi_audio_in"))
		combo_items = g_list_append(combo_items, _("sgi_audio_in"));
	gtk_combo_set_popdown_strings(GTK_COMBO(combo), combo_items);
	g_list_free(combo_items);
	gtk_widget_show(combo);
	cfg = gnome_config_get_string("audio_io/input_plugin");
	ainplugin = alloca(256);
	strncpy(ainplugin, cfg, 255);
	g_free(cfg);
	gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(combo)->entry), ainplugin);
	gtk_signal_connect(GTK_OBJECT(GTK_COMBO(combo)->entry), "changed", changeString, &ainplugin);
	gtk_widget_show(GTK_COMBO(combo)->entry);
	create_label_widget_pair(vbox, "Default input plugin", combo);

	/* input device */
	aindev = alloca(256);
	cfg = gnome_config_get_string("audio_io/input_dev");
	strncpy(aindev, cfg, 255);
	g_free(cfg);
	create_label_edit_pair(vbox, "Default input device", "aindev", aindev);

	/* output filter */
	combo = gtk_combo_new();
	combo_items = NULL;
	if (plugin_get("audio_out"))
		combo_items = g_list_append(combo_items, _("audio_out"));
	if (plugin_get("oss_audio_out"))
		combo_items = g_list_append(combo_items, _("oss_audio_out"));
	if (plugin_get("esd_audio_out"))
		combo_items = g_list_append(combo_items, _("esd_audio_out"));
	if (plugin_get("alsa_audio_out"))
		combo_items = g_list_append(combo_items, _("alsa_audio_out"));
	if (plugin_get("sgi_audio_out"))
		combo_items = g_list_append(combo_items, _("sgi_audio_out"));
	gtk_combo_set_popdown_strings(GTK_COMBO(combo), combo_items);
	g_list_free(combo_items);
	gtk_widget_show(combo);
	cfg = gnome_config_get_string("audio_io/output_plugin");
	aoutplugin = alloca(256);
	strncpy(aoutplugin, cfg, 255);
	g_free(cfg);
	gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(combo)->entry), aoutplugin);
	gtk_signal_connect(GTK_OBJECT(GTK_COMBO(combo)->entry), "changed", changeString, &aoutplugin);
	gtk_widget_show(GTK_COMBO(combo)->entry);
	create_label_widget_pair(vbox, "Default output plugin", combo);

	/* output device */
	aoutdev = alloca(256);
	cfg = gnome_config_get_string("audio_io/output_dev");
	strncpy(aoutdev, cfg, 255);
	g_free(cfg);
	create_label_edit_pair(vbox, "Default output device",
			       "aoutdev", aoutdev);

        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box), vbox,
				       tablabel);


	/* Finish. */
        gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->ok_button),"clicked",setBoolean,&ok);
        gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->apply_button));
        gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->help_button));
        gtk_widget_show(prop_box);


	/* Run the dialog. */
        gnome_dialog_run_and_close(GNOME_DIALOG(prop_box));
        if(!ok)
		return;

	/* Update gnome config. */
	gnome_config_set_string("swapfile/defaultpath", path);
	nPopupTimeout = atoi(numberbuffer);
	gnome_config_set_int("edit_filter/popupTimeout", nPopupTimeout);
	bMac = mac;
	gnome_config_set_bool("edit_filter/macMode",bMac);
	gnome_config_set_string("audio_io/input_dev", aindev);
	gnome_config_set_string("audio_io/input_plugin", ainplugin);
	gnome_config_set_string("audio_io/output_dev", aoutdev);
	gnome_config_set_string("audio_io/output_plugin", aoutplugin);

	/* Update config derived stuff. */
	update_preferences();
}



/*
 * The GLAME About dialog.
 */

static GtkWidget* glame_about(void)
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
	char *logo;

	logo = GLAME_LOGO;
	if (access(logo, R_OK) == -1)
		logo = "../data/pixmaps/glame-logo.jpg";
	about = gnome_about_new ("GLAME", VERSION, 
				 _("Copyright (C) 1999-2001 R. Guenther, A. Ehlert, J. Hirche, D. Kobras"),
				 authors,
				 _("GLAME comes with ABSOLUTELY NO WARRANTY. \nThis is free software."),
				 logo);
	gtk_object_set_data (GTK_OBJECT (about), "about", about);
	gtk_window_set_modal (GTK_WINDOW (about), TRUE);
	gtk_window_set_wmclass (GTK_WINDOW (about), "Glameabout", "Glame");
	gtk_window_set_position (GTK_WINDOW (about), GTK_WIN_POS_CENTER_ALWAYS);
	gtk_widget_show(about);
	return about;
}

static gint remove_splash(GtkWidget* foo)
{
	gtk_object_destroy(GTK_OBJECT(foo));
	return FALSE;
}
static void glame_splash(void)
{
	GtkWidget* foo;

	foo = glame_about();
	gtk_object_destroy(GTK_OBJECT((GNOME_DIALOG(foo))->action_area));

#ifdef DEBUG
	gtk_timeout_add(1000, (GtkFunction)remove_splash, foo);
#else
	gtk_timeout_add(5000, (GtkFunction)remove_splash, foo);
#endif
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
	char configpath[255];
	char *path;

	/* Update preferences. */
	sprintf(configpath,"/%s/",g_get_prgname());
	gnome_config_push_prefix(configpath);
	update_preferences();

	path = gnome_config_get_string("swapfile/defaultpath");
	DPRINTF("path: %s\n",path);
	if (!g_file_test(path,G_FILE_TEST_ISDIR)) {
		if (swapfile_creat(path, -1)) {
			DERROR("error creating swapfile\n");
		}
	}
	gpsm_init(path);
	g_free(path);

	/* create swapfile gui */
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

	/* Pop up splash screen. */
	glame_splash();

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
