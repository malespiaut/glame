/*
 * main.c
 *
 * $Id: main.c,v 1.134 2005/03/11 17:00:16 richi Exp $
 *
 * Copyright (C) 2000, 2001, 2002, 2003, 2004 Johannes Hirche,
 *	Richard Guenther
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

#include <gnome.h>
#include <stdlib.h>
#include <unistd.h>
#ifdef HAVE_LIBGLADE
#include <glade/glade.h>
#endif
#include <libintl.h>
#include "glmid.h"
#include "tree/gltree.h"
#include "waveeditgui.h"
#include "edit_filter/filtereditgui.h"
#include "util/glame_gui_utils.h"
#include "util/gtknob.h"
#include "gpsm.h"
#include "clipboard.h"
#include "glame_accelerator.h"
#include "glame_console.h"
#include "network_utils.h"
#include "glconfig.h"
#include "importexport.h"


/* Stuff we reference/export from/to our C modules.  */
/* HACK */
void swcluster_set_cache(int, int, int, size_t);

GtkWidget *glame_appbar;

extern long nPopupTimeout;
extern long bMac;

/* Globals. */
static GtkWidget *swapfile;
static GtkWidget *app;

/* Forward declarations. */ 
 static void create_new_project_cb(GtkWidget *menu, void * blah); 
 static void edit_file_cb(GtkWidget *menu, void *data); 
 static void record_wave_cb(GtkWidget *menu, void *data);
 static void import_cb(GtkWidget *menu, void *data); 
 static void show_console_cb(GtkWidget *menu, void *blah); 
 static void emptytrash_cb(GtkWidget *menu, void *blah); 
 static void sync_cb(GtkWidget *menu, void *blah); 
 static void gui_quit(GtkWidget *widget, gpointer data); 
 static int preferences_cb(GtkWidget *menu,void *blah); 
 static GtkWidget* glame_about(void); 
/* Menus. */
 static GnomeUIInfo swapfile_menu_uiinfo[] = { 
 	GNOMEUIINFO_MENU_NEW_ITEM (N_("_New Project"), N_("Creates a new Project group"), create_new_project_cb, NULL), 
 	GNOMEUIINFO_ITEM (N_("Edit File..."), N_("Imports a file and opens the waveedit window"), edit_file_cb, NULL), 
	GNOMEUIINFO_ITEM (N_("Record Wave..."), N_("Opens a new waveedit window for recording"), record_wave_cb, NULL),
 	GNOMEUIINFO_ITEM (N_("Import..."), N_("Imports a file"), import_cb, NULL), 
 	GNOMEUIINFO_SEPARATOR, 
 	GNOMEUIINFO_ITEM (N_("Empty [deleted]"), N_("Kills [deleted] folder"), emptytrash_cb, NULL), 
 	GNOMEUIINFO_SEPARATOR, 
 	GNOMEUIINFO_ITEM (N_("Show _console"), N_("Shows the GLAME console"), show_console_cb, NULL), 
 	GNOMEUIINFO_ITEM (N_("Sync"), N_("Syncs meta to disk"), sync_cb, NULL), 
 	GNOMEUIINFO_SEPARATOR, 
 	GNOMEUIINFO_MENU_EXIT_ITEM (gui_quit, NULL), 
 	GNOMEUIINFO_END 
 }; 

static void new_network_cb(GtkWidget *menu, void *blah); 
void glame_load_network(GtkWidget *bla, void *blu);
static void load_plugin_cb(GtkWidget* foo, void *bar);
static GnomeUIInfo filter_menu_uiinfo[] = { 
 	GNOMEUIINFO_MENU_NEW_ITEM(N_("New Filternetwork"), N_("Creates a new filternetwork"), new_network_cb, NULL), 
	GNOMEUIINFO_MENU_OPEN_ITEM(glame_load_network, NULL), 
 	GNOMEUIINFO_SEPARATOR, 
 	GNOMEUIINFO_ITEM(N_("Load Plugin"),N_("Loads and registers a plugin"), load_plugin_cb,NULL),
 	GNOMEUIINFO_END 
 };

static GnomeUIInfo help_menu_uiinfo[] =
{ 
	GNOMEUIINFO_ITEM_DATA(N_("_Help"),N_("Opens a gnome help browser"),glame_help_goto,(gpointer)"info:glame",NULL), 
 	GNOMEUIINFO_ITEM_DATA(N_("Quick Start Guide"), N_("Opens a gnome help browser with the quick start guide"), glame_help_goto, (gpointer)"info:glame#Quick_Start_Guide", NULL), 
 	GNOMEUIINFO_ITEM_DATA(N_("List key-bindings"), N_("Lists the current key-bindings"), glame_accel_widget_data_cb, (gpointer)"list_keybindings", NULL), 
 	GNOMEUIINFO_SEPARATOR, 
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
     GNOME_APP_UI_SUBTREE, N_("_Project"), 
     NULL, 
     swapfile_menu_uiinfo, NULL, NULL, 
     GNOME_APP_PIXMAP_NONE, NULL, 
     0, (GdkModifierType)0, NULL 
   }, 
   { 
     GNOME_APP_UI_SUBTREE, N_("_Filternetwork"), 
     NULL, 
     filter_menu_uiinfo, NULL, NULL, 
     GNOME_APP_PIXMAP_NONE, NULL, 
     0, (GdkModifierType)0, NULL 
   }, 
   GNOMEUIINFO_MENU_SETTINGS_TREE (glame_setting_uiinfo), 
   GNOMEUIINFO_MENU_HELP_TREE (help_menu_uiinfo), 
   GNOMEUIINFO_END 
 }; 


static void show_console_cb(GtkWidget *menu, void * blah) 
{ 
	glame_console_show(); 
} 

static void sync_cb(GtkWidget *menu, void * blah) 
{ 
 	gpsm_sync(); 
} 

static void emptytrash_cb(GtkWidget *menu, void * blah) 
{ 
        gpsm_grp_t *deleted; 

 	if (!(deleted = gpsm_find_grp_label(gpsm_root(), NULL, GPSM_GRP_DELETED_LABEL))) 
 		return; 
	gpsm_item_destroy((gpsm_item_t *)deleted); 
	gpsm_sync(); 
} 

static void new_network_cb(GtkWidget *menu, void * blah) 
{ 
 	GtkWidget *feg; 
 	feg = glame_filtereditgui_new(NULL, FALSE); 
 	gtk_quit_add_destroy(1, GTK_OBJECT(feg)); 
 	gtk_widget_show(feg); 
} 

static void create_new_project_cb(GtkWidget *menu, void * blah) 
{ 
	gpsm_grp_t *grp; 

	/* Create new gpsm group. */
 	grp = gpsm_newgrp(_("New project")); 
 	if (gpsm_item_place(gpsm_root(), (gpsm_item_t *)grp, 
 			    0, gpsm_item_vsize(gpsm_root())) == -1 
 	    && gpsm_item_place(gpsm_root(), (gpsm_item_t *)grp, 
 			       0, gpsm_item_vsize(gpsm_root())+1) == -1) 
		DPRINTF("Cannot insert new group!?\n");
} 

static void edit_file_cleanup_cb(GtkObject *widget, gpsm_item_t *file) 
{ 
 	gpsm_item_destroy(file); 
} 
static void edit_file_mark_modified_cb(glsig_handler_t *h, long sig, va_list va) 
{ 
 	WaveeditGui *we = WAVEEDIT_GUI(glsig_handler_private(h)); 
 	we->modified = 1; 
} 
static void edit_file_cb(GtkWidget *menu, void *data) 
{ 
 	gpsm_item_t *file, *item; 
 	WaveeditGui *we; 

/* Run the import dialog. */
 	file = glame_import_dialog(GTK_WINDOW(app)); 
	if (!file) 
 		return; 

 	/* Open the waveedit window and register a handler for gpsm 
 	 * deletion after widget destroy. Also register handlers for 
 	 * track modification to update bool. */
 	we = glame_waveedit_gui_new(gpsm_item_label(file), file); 
 	if (!we) { 
		glame_error_dialog(_("Cannot open wave editor"), NULL);
 		gpsm_item_destroy(file); 
 		return; 
 	} 
 	gtk_signal_connect(GTK_OBJECT(we), "destroy", 
 			   (GtkSignalFunc)edit_file_cleanup_cb, file); 
 	gpsm_grp_foreach_item(file, item) 
 		glsig_add_handler(gpsm_item_emitter(item), GPSM_SIG_SWFILE_CHANGED|GPSM_SIG_SWFILE_CUT|GPSM_SIG_SWFILE_INSERT, 
 				  edit_file_mark_modified_cb, we); 
 	gtk_quit_add_destroy(1, GTK_OBJECT(we)); 
 	gtk_widget_show_all(GTK_WIDGET(we)); 
} 

static void record_wave_cb(GtkWidget *menu, void *data) 
{ 
	gpsm_grp_t *grp;
	gpsm_swfile_t *left, *right;
 	gpsm_item_t *item; 
 	WaveeditGui *we; 

	/* Create a new stereo track.  */
	grp = gpsm_newgrp(_("New wave"));
	left = gpsm_newswfile(_("left"));
	gpsm_swfile_set_position(left, FILTER_PIPEPOS_LEFT);
	right = gpsm_newswfile(_("right"));
	gpsm_swfile_set_position(right, FILTER_PIPEPOS_RIGHT);
	gpsm_vbox_insert(grp, (gpsm_item_t *)left, 0, 0);
	gpsm_vbox_insert(grp, (gpsm_item_t *)right, 0, 1);

 	/* Open the waveedit window and register a handler for gpsm 
 	 * deletion after widget destroy. Also register handlers for 
 	 * track modification to update bool. */
 	we = glame_waveedit_gui_new("New wave", grp); 
 	if (!we) { 
		glame_error_dialog(_("Cannot open wave editor"), NULL);
 		gpsm_item_destroy(grp); 
 		return; 
 	} 
 	gtk_signal_connect(GTK_OBJECT(we), "destroy", 
 			   (GtkSignalFunc)edit_file_cleanup_cb, grp); 
 	gpsm_grp_foreach_item(grp, item) 
 		glsig_add_handler(gpsm_item_emitter(item), GPSM_SIG_SWFILE_CHANGED|GPSM_SIG_SWFILE_CUT|GPSM_SIG_SWFILE_INSERT, 
 				  edit_file_mark_modified_cb, we); 
 	gtk_quit_add_destroy(1, GTK_OBJECT(we)); 
 	gtk_widget_show_all(GTK_WIDGET(we)); 
} 

static void import_cb(GtkWidget *menu, void *data) 
{ 
 	gpsm_item_t *file; 
 	gpsm_grp_t *deleted; 
 	int res; 

 	/* Run the import dialog. */ 
 	file = glame_import_dialog(GTK_WINDOW(app)); 
 	if (!file) 
 		return; 

 	if ((deleted = gpsm_find_grp_label(gpsm_root(), NULL, GPSM_GRP_DELETED_LABEL))) 
 		gpsm_item_remove((gpsm_item_t *)deleted); 
 	res = gpsm_vbox_insert(gpsm_root(), file, 
 			       0, gpsm_item_vsize(gpsm_root())); 
 	if (deleted) 
 		gpsm_item_place(gpsm_root(), (gpsm_item_t *)deleted, 
 				0, GPSM_GRP_DELETED_VPOS); 
 	if (res == -1) { 
 		glame_error_dialog(_("Cannot place imported wave"), NULL); 
 		gpsm_item_destroy(file); 
 	} 
 } 

static void load_plugin_cb(GtkWidget*bla,void*blu) 
{ 
 	GtkWidget *dialog; 
 	char *filename;

	dialog = gtk_file_chooser_dialog_new(
		_("Load Plugin"), NULL,
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, NULL);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		if (glame_load_plugin(filename) == -1) 
			glame_error_dialog(_("Error loading plugin"), NULL);
		g_free(filename);
	}
	gtk_widget_destroy(dialog);
} 



/* Update globals derived from preferences and provide defaults to
 * all configurables.
 * Returns -1 if it seems we didnt have a valid config for now. */
static int update_preferences()
{
	filter_t *filter;
	char *swappath = NULL;
	char *ainplugin = NULL, *aindev = NULL;
	char *aoutplugin = NULL, *aoutdev = NULL;
	char s[256];
	long maxundo, res = 0;
	long wbufsize, maxlru, maxfds, maxmaps, maxvm;
	long rate;

	/* Check, if we have anything configured already. */
	if (glame_config_get_string("swapfile/defaultpath", &swappath) == -1)
		res = -1;
	else
		free(swappath);

	/* Set default swapfile path and max. undo depth */
	snprintf(s, 255, "%s/.glameswap", g_get_home_dir());
	swappath = glame_config_get_string_with_default("swapfile/defaultpath", s);

	glame_config_get_long("swapfile/maxundo", &maxundo);
	gpsm_set_max_saved_ops(maxundo);

	maxlru = glame_config_get_long_with_default("swapfile/maxlru", 2048);
	maxfds = glame_config_get_long_with_default("swapfile/maxfds", 128);
	maxmaps = glame_config_get_long_with_default("swapfile/maxmaps", 256);
	maxvm = glame_config_get_long_with_default("swapfile/maxvm", 256*1024*1024);
	swcluster_set_cache(maxlru, maxfds, maxmaps, maxvm);

	/* GLAME_WBUFSIZE */
	glame_config_get_long("filter/wbufsize", &wbufsize);
	_GLAME_WBUFSIZE = wbufsize;

	/* Update IO plugin setup - audio_out */
	aoutplugin = glame_config_get_string_with_default(
		"audio_io/output_plugin", "audio_out");
	if (!plugin_get(aoutplugin)) {
		DPRINTF("No plugin %s - using audio_out\n", aoutplugin);
		free(aoutplugin);
		aoutplugin = strdup("audio_out");
		if (!plugin_get(aoutplugin))
			goto ain;
	}
	aoutdev = filterparam_val_string(filterparamdb_get_param(filter_paramdb((filter_t *)plugin_query(plugin_get(aoutplugin), PLUGIN_FILTER)), "device"));
	snprintf(s, 255, "%s", aoutdev ? aoutdev : "");
	aoutdev = glame_config_get_string_with_default("audio_io/output_dev", s);
	filter = filter_instantiate(plugin_get(aoutplugin));
	if (filter) {
		filterparam_set(filterparamdb_get_param(filter_paramdb(filter),
							"device"), &aoutdev);
		filter_register(filter, plugin_get("audio_out"));
	}

 ain:
	/* Update IO plugin setup - audio_in */
	rate = glame_config_get_long_with_default(
		"audio_io/input_rate", GLAME_DEFAULT_SAMPLERATE);
	ainplugin = glame_config_get_string_with_default(
		"audio_io/input_plugin", "audio_in");
	if (!plugin_get(ainplugin)) {
		DPRINTF("No plugin %s - using audio_in\n", ainplugin);
		free(ainplugin);
		ainplugin = strdup("audio_in");
		if (!plugin_get(ainplugin))
			goto sync;
	}
	aindev = filterparam_val_string(filterparamdb_get_param(filter_paramdb((filter_t *)plugin_query(plugin_get(ainplugin), PLUGIN_FILTER)), "device"));
	snprintf(s, 255, "%s", aindev ? aindev : "");
	aindev = glame_config_get_string_with_default("audio_io/input_dev", s);
	filter = filter_instantiate(plugin_get(ainplugin));
	if (filter) {
		filterparam_set(filterparamdb_get_param(filter_paramdb(filter),
							"device"), &aindev);
		filterparam_set(filterparamdb_get_param(filter_paramdb(filter),
							"rate"), &rate);
		filter_register(filter, plugin_get("audio_in"));
	}

 sync:
	/* Sync changes. */
	glame_config_sync();

	/* Display summary. */
	DPRINTF(
"Preferences:\n"
"\tSwapfile directory %s\n"
"\tUndo stack depth is %li\n"
"\tAudio input plugin %s, device \"%s\", rate %li\n"
"\tAudio output plugin %s, device \"%s\"\n"
"\tGLAME_WBUFSIZE %i\n"
"\tPopup timeout is %lims\n"
"\tMac mode is %s\n",
                swappath, maxundo, ainplugin, aindev, rate, aoutplugin, aoutdev, _GLAME_WBUFSIZE,
                nPopupTimeout, bMac ? "on" : "off");

	/* Free temp. storage. */
	g_free(swappath);
	g_free(aoutplugin);
	if (aoutdev)
		g_free(aoutdev);
	g_free(ainplugin);
	if (aindev)
		g_free(aindev);

	return res;
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

static GtkWidget *preferences_tab_filteredit(long *fe_popup_timeout, long *fe_mac)
{
	GtkWidget *vbox, *macMode;

	vbox = gtk_vbox_new(FALSE, 1);

	glame_config_get_long("edit_filter/popupTimeout", fe_popup_timeout);
	create_label_long_pair(vbox, _("Property popup timeout [ms]"),
			       fe_popup_timeout, 50, 1000);

	glame_config_get_long("edit_filter/macMode", fe_mac);
	macMode = gtk_check_button_new();
	gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(macMode), *fe_mac);
	gtk_signal_connect(GTK_OBJECT(macMode), "toggled", (GtkSignalFunc)toggle_cb, fe_mac);
	create_label_widget_pair(vbox, _("Mac mode (one mouse button mode)"), macMode);
	gtk_widget_show_all(vbox);
	return vbox;
}

static GtkWidget *preferences_tab_waveedit(long *we_scroll)
{
	GtkWidget *vbox, *macMode;

	vbox = gtk_vbox_new(FALSE, 1);

	glame_config_get_long("waveedit/scroll", we_scroll);
	macMode = gtk_check_button_new();
	gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(macMode), *we_scroll);
	gtk_signal_connect(GTK_OBJECT(macMode), "toggled", (GtkSignalFunc)toggle_cb, we_scroll);
	create_label_widget_pair(vbox, _("Scroll wave while playing"), macMode);
	gtk_widget_show_all(vbox);
	return vbox;
}

static GtkWidget *preferences_tab_swapfile(char *sw_path,
					   long *sw_maxvm,
					   long *sw_maxundo)
{
	GtkWidget *vbox, *entry;
	char *cfg;

        vbox = gtk_vbox_new(FALSE, 1);

        gtk_container_add(GTK_CONTAINER(vbox), gtk_label_new(
            _("You need lots of diskspace available at the swapfile location.")));
        gtk_container_add(GTK_CONTAINER(vbox), gtk_label_new(
            _("GLAME doesnt handle running out of disk space very well.")));

        entry = gnome_file_entry_new("swapfilepath", _("Swapfile Path"));
	gnome_file_entry_set_modal(GNOME_FILE_ENTRY(entry), TRUE);
        create_label_widget_pair(vbox, _("Swapfile Path"), entry);
	glame_config_get_string("swapfile/defaultpath", &cfg);
	strncpy(sw_path, cfg, 255);
	g_free(cfg);
        gtk_entry_set_text(GTK_ENTRY(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(entry))), sw_path);
        gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(entry))), "changed", (GtkSignalFunc)changeString, sw_path);

	glame_config_get_long("swapfile/maxvm", sw_maxvm);
	*sw_maxvm /= 1024*1024;
	create_label_long_pair(vbox, _("Maximum virtual memory used for caching [MB]"), sw_maxvm, 32, 512);

        gtk_container_add(GTK_CONTAINER(vbox), gtk_label_new(
            _("NOTE: Swapfile settings take effect after restart only")));

	gtk_container_add(GTK_CONTAINER(vbox), gtk_hseparator_new());

	glame_config_get_long("swapfile/maxundo", sw_maxundo);
	create_label_long_pair(vbox, _("Depth of undo stack"), sw_maxundo, 1, 32);

	gtk_widget_show_all(vbox);
	return vbox;
}

static gint changeCombo(GtkComboBox *combo, char *string)
{
	GtkTreeModel *model;
	GtkTreeIter i;
	GValue val = { 0, };

	if (!gtk_combo_box_get_active_iter(combo, &i))
		return FALSE;
	model = gtk_combo_box_get_model(combo);
	gtk_tree_model_get_value(model, &i, 0, &val);
	strcpy(string, g_value_get_string(&val));

	return TRUE;
}

static GtkWidget *preferences_tab_audioio(char *ainplugin, char *aindev,
					  char *aoutplugin, char *aoutdev,
					  long *rate, long *wbufsize)
{
	GtkWidget *vbox, *combo;
	char *cfg;

	vbox = gtk_vbox_new(FALSE, 1);

	/* input filter */
	combo = gtk_combo_box_new_text();
	gtk_widget_show(combo);
	glame_config_get_string("audio_io/input_plugin", &cfg);
	strncpy(ainplugin, cfg, 255);
	g_free(cfg);
	gtk_combo_box_append_text(GTK_COMBO_BOX(combo), ainplugin);
	if (plugin_get("oss_audio_in"))
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo), "oss_audio_in");
	if (plugin_get("esd_audio_in"))
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo), "esd_audio_in");
	if (plugin_get("alsa_audio_in"))
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo), "alsa_audio_in");
	if (plugin_get("sgi_audio_in"))
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo), "sgi_audio_in");
	gtk_combo_box_set_active(GTK_COMBO_BOX(combo), 0);
	gtk_signal_connect(GTK_OBJECT(combo), "changed", (GtkSignalFunc)changeCombo, ainplugin);
	create_label_widget_pair(vbox, _("Default input plugin (audio_in)"), combo);

	/* input device */
	glame_config_get_string("audio_io/input_dev", &cfg);
	strncpy(aindev, cfg, 255);
	g_free(cfg);
	create_label_edit_pair(vbox, _("Default input device"), "aindev", aindev);

	/* sample rate */
	glame_config_get_long("audio_io/input_rate", rate);
	create_label_long_pair(vbox, _("Default input sample rate"), rate, 11025, 48000);

	gtk_container_add(GTK_CONTAINER(vbox), gtk_hseparator_new());

	/* output filter */
	combo = gtk_combo_box_new_text();
	glame_config_get_string("audio_io/output_plugin", &cfg);
	strncpy(aoutplugin, cfg, 255);
	g_free(cfg);
	gtk_combo_box_append_text(GTK_COMBO_BOX(combo), aoutplugin);
	if (plugin_get("oss_audio_out"))
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo), "oss_audio_out");
	if (plugin_get("esd_audio_out"))
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo), "esd_audio_out");
	if (plugin_get("alsa_audio_out"))
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo), "alsa_audio_out");
	if (plugin_get("sgi_audio_out"))
		gtk_combo_box_append_text(GTK_COMBO_BOX(combo), "sgi_audio_out");
	gtk_combo_box_set_active(GTK_COMBO_BOX(combo), 0);
	gtk_signal_connect(GTK_OBJECT(combo), "changed", (GtkSignalFunc)changeCombo, aoutplugin);
	create_label_widget_pair(vbox, _("Default output plugin (audio_out)"), combo);

	/* output device */
	glame_config_get_string("audio_io/output_dev", &cfg);
	strncpy(aoutdev, cfg, 255);
	g_free(cfg);
	create_label_edit_pair(vbox, _("Default output device"),
			       "aoutdev", aoutdev);

	gtk_container_add(GTK_CONTAINER(vbox), gtk_hseparator_new());

	/* GLAME_WBUFSIZE */
	glame_config_get_long("filter/wbufsize", wbufsize);
	create_label_long_pair(vbox, _("Size hint for audio buffers [samples]"),
			       wbufsize, 64, 4096);

	gtk_widget_show_all(vbox);
	return vbox;
}

static void pref_help_cb(GnomePropertyBox *box, gint page)
{
	glame_help_goto(NULL, "info:glame#The_Preferences_Dialog");
}

static int
preferences_cb(GtkWidget * wid, void * bla)
{
	GtkWidget *prop_box, *tablabel, *vbox;
	char aio_inplugin[256], aio_outplugin[256];
	char aio_indevice[256], aio_outdevice[256];
	long aio_wbufsize, aio_rate; 
	char sw_path[256];
	long sw_maxundo, sw_maxlru, sw_maxfds, sw_maxmaps, sw_maxvm;
	long fe_popup_timeout;
	long fe_mac, we_scroll;
	gboolean ok = FALSE;

	/* New box. */
        prop_box = gnome_property_box_new();

	/* Swapfile with
	 * - swapfile location */
        tablabel = gtk_label_new(_("Swapfile"));
	vbox = preferences_tab_swapfile(sw_path, &sw_maxvm, &sw_maxundo);
        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box),
				       vbox, tablabel);

	/* Edit Filter with
	 * - popup timeout
	 * - mac mode */
	tablabel = gtk_label_new(_("Filternetwork"));
	vbox = preferences_tab_filteredit(&fe_popup_timeout, &fe_mac);
        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box),
				       vbox, tablabel);

	/* Waveedit with
	 * - scrolling */
	tablabel = gtk_label_new(_("Waveedit"));
	vbox = preferences_tab_waveedit(&we_scroll);
        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box),
				       vbox, tablabel);

	/* Audio I/O with
	 * - input device
	 * - output device */
	tablabel = gtk_label_new(_("Audio I/O"));
	vbox = preferences_tab_audioio(aio_inplugin, aio_indevice,
				       aio_outplugin, aio_outdevice,
				       &aio_rate, &aio_wbufsize);
        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box), vbox,
				       tablabel);

	/* Finish. */
        gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->ok_button),"clicked",(GtkSignalFunc)setBoolean,&ok);
        gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->apply_button));
	gtk_signal_connect(GTK_OBJECT(prop_box), "help", GTK_SIGNAL_FUNC(pref_help_cb), NULL);
        gtk_widget_show(prop_box);


	/* Run the dialog. */
        gnome_dialog_run(GNOME_DIALOG(prop_box));
        if(!ok)
		return 0;

	/* Update glame config.
	 */
	glame_config_set_string("swapfile/defaultpath", sw_path);
	glame_config_set_long("swapfile/maxundo", sw_maxundo);
	/* Do some magic with sw_maxvm number to derive the rest. */
	sw_maxlru = 2048 * sw_maxvm / 256;
	sw_maxfds = 128 * sw_maxvm / 256;
	sw_maxmaps = 256 * sw_maxvm / 256;
	sw_maxvm *= 1024*1024;
	glame_config_set_long("swapfile/maxlru", sw_maxlru);
	glame_config_set_long("swapfile/maxfds", sw_maxfds);
	glame_config_set_long("swapfile/maxmaps", sw_maxmaps);
	glame_config_set_long("swapfile/maxvm", sw_maxvm);
	/* Filteredit */
	nPopupTimeout = fe_popup_timeout;
	glame_config_set_long("edit_filter/popupTimeout", nPopupTimeout);
	bMac = fe_mac;
	glame_config_set_long("edit_filter/macMode", bMac);
	/* Waveedit */
	glame_config_set_long("waveedit/scroll", we_scroll);
	/* Audio I/O */
	glame_config_set_string("audio_io/input_dev", aio_indevice);
	glame_config_set_string("audio_io/input_plugin", aio_inplugin);
	glame_config_set_long("audio_io/input_rate", aio_rate);
	glame_config_set_string("audio_io/output_dev", aio_outdevice);
	glame_config_set_string("audio_io/output_plugin", aio_outplugin);
	glame_config_set_long("filter/wbufsize", aio_wbufsize);
	glame_config_sync();

	/* Update config derived stuff. */
	update_preferences();

	return 1;
}



/*
 * The GLAME About dialog.
 */

static GtkWidget* glame_about(void)
{
	/* FIXME: gnome_about_new() apparently does not gettext() the
	 * authors argument, and gettext() cannot handle argvz-style
	 * string arrays. Looks like we might have to work around the
	 * issue by allocating dynamically. But it sucks. Leave it
	 * untranslated for now.
	 */
	GdkPixbuf* log;
	const gchar *authors[]={
		N_("Richard Guenther"),
		N_("Alexander Ehlert"),
		N_("Daniel Kobras"),
		N_("Johannes Hirche"),
		N_("and others"),
		NULL
	};
	const gchar *documenters[]={
		N_("Richard Guenther"),
		N_("Daniel Kobras"),
		N_("Alexander Ehlert"),
		NULL
	};
	GtkWidget *about;
	char *logo;
	
	logo = GLAME_LOGO;

	if (access(logo, R_OK) == -1)
		logo = "../data/pixmaps/glame-logo.jpg";
	log = gdk_pixbuf_new_from_file(logo,NULL);
	about = gnome_about_new ("GLAME", 
				 VERSION, 
				 _("Copyright (C) 1999-2004 R. Guenther, A. Ehlert, J. Hirche, D. Kobras and others"),
				 _(
				   "This program is free software; you can redistribute it and/or modify "
				   "it under the terms of the GNU General Public License as published by "
				   "the Free Software Foundation; either version 2 of the License, or "
				   "(at your option) any later version.\n"
				   "This program is distributed in the hope that it will be useful, "
				   "but WITHOUT ANY WARRANTY; without even the implied warranty of "
				   "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the "
				   "GNU General Public License for more details.\n"
				   "You should have received a copy of the GNU General Public License "
				   "along with this program; if not, write to the Free Software "
				   "Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA"),
				 authors,
				 documenters,
				 _("Daniel Kobras\nLaurent Georget\nBenno Senoner\nAllan Lyckegaard"),
				 log);
	gtk_object_set_data (GTK_OBJECT (about), "about", about);
	gtk_window_set_modal (GTK_WINDOW (about), FALSE);
	gtk_window_set_wmclass (GTK_WINDOW (about), "Glameabout", "Glame");
	gtk_window_set_position (GTK_WINDOW (about), GTK_WIN_POS_CENTER_ALWAYS);
	gtk_widget_show(about);
	return about;
}

static GtkWidget *glame_splash(void)
{
	GtkWidget *a;

	/* We may "specialize" the splash somewhat in the future.
	 * For now just use the about dialog with disabled close. */
	a = glame_about();
	gtk_dialog_set_default_response(GTK_DIALOG(a), GTK_RESPONSE_NONE);
	gtk_dialog_set_response_sensitive(GTK_DIALOG(a), GTK_RESPONSE_CLOSE, FALSE);

	return a;
}


static long maxundo;
static pthread_mutex_t giw_mx = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t giw_condmx = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t giw_cond = PTHREAD_COND_INITIALIZER;
static void *gpsm_init_wrapper(void *path_)
{
	int res;
	pthread_mutex_lock(&giw_mx);
	pthread_mutex_lock(&giw_condmx);
	pthread_cond_broadcast(&giw_cond);
	pthread_mutex_unlock(&giw_condmx);
	res = gpsm_init(path_, maxundo);
	pthread_mutex_unlock(&giw_mx);
	if (res == -1)
		return NULL;
	else
		return (void *)-1;
}


/*
 * Real main and cleanup stuff.
 */

static void on_swapfile_panic(const char *msg)
{
	char message[1024];

	/* Tell the user what happened. */
	snprintf(message, 1023,
		 _("The GLAME swapfile subsystem is about to commit suicide.\n"
		 "The reason for this is:\n"
		 "    %s\n"
		 "The current pending libc error is:\n"
		 "    %s\n"
		 "Just restart GLAME after fixing the above (which may be\n"
		 "an internal GLAME error, too).\n\n"
		 "-- BYE BYE.\n"),
		 msg, strerror(errno));
	glame_error_dialog(message, NULL);

	/* Try to save the current gpsm tree. May even lock up. Ugh. */
	gpsm_sync();
}

static void resize_horiz_cb(GtkWidget *widget, GtkRequisition *req,
			    GtkWidget *window)
{
	static int last_width = 0, last_height = 0;
	if (last_width == req->width && last_height == req->height)
		return;
	last_width = req->width;
	last_height = req->height;
	gtk_window_set_default_size(GTK_WINDOW(window),
				    req->width + 10,
				    window->allocation.height);
}

static void gui_quit(GtkWidget *widget, gpointer data)
{
	/* This doesnt seem to destroy windows "correctly"
	 * - cleanup is still missing for Edit File... */
	gtk_main_quit();
}

static GtkWidget *splash;
static void gui_main()
{
	GtkWidget *dock, *scrollview;
	char *path;
	void *gpsm_init_result;
	pthread_t gpsm_init_thread;

	/* Init GUI dependend subsystems. */
	glame_accel_init();
	glame_waveeditgui_init();
	glame_filtereditgui_init();

	/* Update preferences. */
	if (update_preferences() == -1) {
		glame_info_dialog(
_("Welcome first-time user of GLAME.\n"
"We need to do some basic setup stuff. Please run through\n"
"the preferences dialog and check the \"Swapfile Path\" and\n"
"\"Audio IO\" settings.\n"), NULL);
	run_prefs:
		if (!preferences_cb(NULL, NULL)) {
			glame_error_dialog(_("You didnt change/check the configuration.\nGLAME is exiting now.\n"), NULL);
			exit(1);
		}
	}

	glame_config_get_string("swapfile/defaultpath", &path);
	DPRINTF("path: %s\n",path);
	if (!g_file_test(path,G_FILE_TEST_IS_DIR)) {
		if (swapfile_creat(path, 0)) {
			char msg[256];
			char *errmsg = strerror(errno);
			snprintf(msg, 255, _("GLAME was unable to create its swapfile\nbecause of \"%s\".\nPlease check the configuration.\n"), errmsg);
			glame_error_dialog(msg, NULL);
			goto run_prefs;
		}
	}

	/* Asynchronly try to initialize the gpsm. */
	glame_config_get_long("swapfile/maxundo", &maxundo);
	pthread_mutex_lock(&giw_condmx);
	pthread_create(&gpsm_init_thread, NULL, gpsm_init_wrapper, path);
	pthread_cond_wait(&giw_cond, &giw_condmx);

	/* Poll for thread completion. */
	while (pthread_mutex_trylock(&giw_mx)) {
		gtk_main_iteration_do(FALSE);
		sched_yield();
	}
	pthread_join(gpsm_init_thread, &gpsm_init_result);
	pthread_mutex_unlock(&giw_mx);

	if (gpsm_init_result == NULL) {
		char msg[256];
		char *errmsg = strerror(errno);
		snprintf(msg, 255, "GLAME was unable to open/init its swapfile\nbecause of \"%s\".\nPlease check the configuration and/or check for\nGLAME messages on the console.\n", errmsg);
		glame_error_dialog(msg, NULL);
		goto run_prefs;
	}
	g_free(path);

	/* Register a swapfile panic handler. */
	swapfile_register_panic_handler(on_swapfile_panic);

	/* create swapfile gui - in a scrolled window */
	swapfile = glame_gltree_init(gpsm_root());
	if (!swapfile)
		return;
	scrollview = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollview),
				       GTK_POLICY_NEVER,
				       GTK_POLICY_ALWAYS);
	gtk_scrolled_window_set_placement(GTK_SCROLLED_WINDOW(scrollview),
					  GTK_CORNER_TOP_RIGHT);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrollview),
					      swapfile);
	gtk_widget_show(swapfile);
	gtk_widget_show(scrollview);

	/* Create main window. */
	app = gnome_app_new ("glame0.7", NULL);
	gtk_object_set_data (GTK_OBJECT (app), "app", app);
	dock = GNOME_APP (app)->dock;
	gtk_widget_ref (dock);
	gtk_object_set_data_full (GTK_OBJECT (app), "dock", dock,
                                 (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (dock);
	gnome_app_create_menus (GNOME_APP (app), menubar_uiinfo);
	glame_appbar = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);
	gtk_widget_show (glame_appbar);
	gnome_app_set_statusbar (GNOME_APP (app), glame_appbar);
	gnome_app_install_menu_hints (GNOME_APP (app), menubar_uiinfo);

	/* Connect signals and insert swapfile gui into the main window. */
	gtk_signal_connect(GTK_OBJECT(app), "delete-event",
			   GTK_SIGNAL_FUNC(gui_quit), NULL);
	gnome_app_set_contents(GNOME_APP(app), scrollview);
	gtk_window_set_default_size(GTK_WINDOW(app), 300, 200);
	gtk_widget_show(app);

	/* Connect auto-horizontal-resize callback. */
	gtk_signal_connect(GTK_OBJECT(swapfile), "size_request",
			   (GtkSignalFunc)resize_horiz_cb, app);

	/* Pop up console. */
	glame_console_init();
	glame_console_printf("%s%s%s",
_("    GLAME version "), VERSION, _(", Copyright (C) 1999-2004 by\n"
"    Alexander Ehlert, Richard Guenther, Johannes Hirche,\n"
"    Daniel Kobras and others.\n"
"    GLAME comes with ABSOLUTELY NO WARRANTY. This is free software,\n"
"    and you are welcome to redistribute it under certain conditions.\n\n"));
#ifndef HAVE_LADSPA
	glame_console_printf("WARNING! This copy of GLAME was compiled without LADSPA plugin support\n");
#endif
#ifndef HAVE_LIBGLADE
	glame_console_printf("WARNING! This copy of GLAME was compiled without libglade support\n");
#endif

	/* Finally, destroy splash. */
	gtk_object_destroy(GTK_OBJECT(splash));

	/* Main event loop */
       	gtk_main();

	/* Cleanup after gtk - tear down GLAME midlayer/backends. */
	clipboard_empty();
	gpsm_close();
	/* glame_accel_sync(); -- disabled, until we get GUI support */

	/* Exit to guile here - no way back. */
}


int main(int argc, char **argv)
{
        bind_textdomain_codeset("glame", "UTF-8");
	textdomain("glame");

	/* setup gnome/gtk  */
	gnome_program_init("glame2", VERSION, LIBGNOMEUI_MODULE,
			   argc, argv, GNOME_PARAM_NONE);

	/* Pop up splash screen. */
	splash = glame_splash();
	while (gtk_events_pending())
		gtk_main_iteration_do(FALSE) ;

#ifdef HAVE_LIBGLADE
	glade_init();
	gtk_knob_glade_register();
#endif

	/* init glame */
	glame_init(gui_main, argc, argv);

	DPRINTF("Returned to main\n");

	return 1;
}
