/*
 * main.c
 *
 * $Id: main.c,v 1.92 2001/11/29 10:04:44 richi Exp $
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
#ifdef HAVE_LIBGLADE
#include <glade/glade.h>
#endif
#include <libintl.h>
#include "swapfile.h"
#include "glmid.h"
#include "swapfilegui.h"
#include "waveeditgui.h"
#include "timeline/timeline.h"
#include "edit_filter/filtereditgui.h"
#include "util/glame_gui_utils.h"
#include "gltreeitem.h"
#include "gpsm.h"
#include "clipboard.h"
#include "glame_accelerator.h"
#include "glame_console.h"
#include "network_utils.h"
#include "glconfig.h"

/* HACK */
extern void blafoobar(int);
extern void swcluster_set_cache(int, int, int, size_t);


/* Globals. */
static GtkWidget *swapfile;
static GtkWidget *app;
GtkWidget *glame_appbar;

extern long nPopupTimeout;
extern long bMac;

/* Forward declarations. */
static void create_new_project_cb(GtkWidget *menu, void * blah);
static void edit_file_cb(GtkWidget *menu, void *data);
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
extern void glame_load_network(GtkWidget *bla, void *blu);
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
	GNOMEUIINFO_ITEM_DATA(N_("_Help"),N_("Opens a gnome help browser"),gnome_help_goto,"info:glame",NULL),
	GNOMEUIINFO_ITEM_DATA(N_("Quick Start Guide"), N_("Opens a gnome help browser with the quick start guide"), gnome_help_goto, "info:glame#Quick_Start_Guide", NULL),
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
    0, 0, NULL
  },
  {
    GNOME_APP_UI_SUBTREE, N_("_Filternetwork"),
    NULL,
    filter_menu_uiinfo, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, 0, NULL
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
	gtk_widget_show(glame_filtereditgui_new(NULL, FALSE));
}

extern void edit_tree_label(GlameTreeItem * item);
static void create_new_project_cb(GtkWidget *menu, void * blah)
{
	gpsm_grp_t *grp;
	GlameTreeItem *grpw;

	/* Create new gpsm group. */
	grp = gpsm_newgrp(_("Unnamed"));
	if (gpsm_item_place(gpsm_root(), (gpsm_item_t *)grp,
			    0, gpsm_item_vsize(gpsm_root())) == -1)
		DPRINTF("Cannot insert new group!?\n");

	/* Find out which widget it got. */
	grpw = glame_tree_find_gpsm_item(
		GTK_OBJECT(SWAPFILE_GUI(swapfile)->tree), (gpsm_item_t *)grp);
	if (!grpw) {
		DPRINTF("Umm, cant find widget for new project.\n");
		return;
	}
	edit_tree_label(grpw);
}

static void edit_file_cleanup_cb(GtkObject *widget, gpsm_item_t *file)
{
	DPRINTF("Destroying subtree %s\n", gpsm_item_label(file));
	gpsm_item_destroy(file);
}

static void edit_file_cb(GtkWidget *menu, void *data)
{
	plugin_t *import;
	int (*operation)(gpsm_item_t *, long, long);
	long vpos;
	gpsm_item_t *file;
	WaveeditGui *we;

	/* HACK(?) */
	vpos = gpsm_item_vsize(gpsm_root());

	/* Use the import plugin for importing. */
	import = plugin_get("import");
	if (!import) {
		gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(_("You dont have the import plugin - impossible."))));
		return;
	}
	operation = plugin_query(import, PLUGIN_GPSMOP);
	if (!operation
	    || operation((gpsm_item_t *)gpsm_root(), 0, 0) == -1) {
		gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(_("Error importing"))));
		return;
	}

	/* Again HACK - find the file in the gpsm tree.
	 * -- FIXME: with direct import dont put this into the global
	 *           gpsm tree anyway.
	 */
	if (!(file = gpsm_find_swfile_vposition(gpsm_root(), NULL, vpos))) {
		DPRINTF("No file at %li\n", vpos);
		return;
	} else if (!(file = gpsm_item_parent(file))) {
		DPRINTF("Cannot find imported file at %li\n", vpos);
		return;
	}

	/* Open the waveedit window and register a handler for gpsm
	 * deletion after widget destroy. */
	we = glame_waveedit_gui_new(gpsm_item_label(file), file);
	if (!we) {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog(_("Cannot open wave editor"))));
		return;
	}
	gtk_signal_connect(GTK_OBJECT(we), "destroy",
			   (GtkSignalFunc)edit_file_cleanup_cb, file);
	gtk_widget_show_all(GTK_WIDGET(we));
}


static void load_plugin_cb(GtkWidget*bla,void*blu)
{
	GtkWidget *dialog;
	char filenamebuffer[256];

	filenamebuffer[0] = '\0';
	dialog = glame_dialog_file_request(_("Load Plugin"),
					   "main:load_plugin", _("Filename"),
					   NULL, filenamebuffer);
	if (!gnome_dialog_run_and_close(GNOME_DIALOG(dialog))
	    || !*filenamebuffer)
		return;

	if (glame_load_plugin(filenamebuffer) == -1)
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog(_("Error loading plugin"))));
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
	aoutplugin = glame_config_get_string_with_default("audio_io/output_plugin",
					     "audio_out");
	if (!plugin_get(aoutplugin)) {
		DPRINTF("No plugin %s - using audio_out\n", aoutplugin);
		g_free(aoutplugin);
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
	ainplugin = glame_config_get_string_with_default("audio_io/input_plugin",
					    "audio_in");
	if (!plugin_get(ainplugin)) {
		DPRINTF("No plugin %s - using audio_in\n", ainplugin);
		g_free(ainplugin);
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
"\tAudio input plugin %s, device \"%s\"\n"
"\tAudio output plugin %s, device \"%s\"\n"
"\tGLAME_WBUFSIZE %i\n"
"\tPopup timeout is %lims\n"
"\tMac mode is %s\n",
                swappath, maxundo, ainplugin, aindev, aoutplugin, aoutdev, _GLAME_WBUFSIZE,
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

static GtkWidget *audio_io_preferences_widget_new()
{
	return NULL;
}

static int
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
	char *cfg, *path, *numberbuffer, *aindev = NULL, *aoutdev = NULL;
	char *ainplugin = NULL, *aoutplugin = NULL, *maxundobuf, *wbufsizebuf;
	char *maxlrubuf, *maxfdsbuf, *maxmapsbuf, *maxvmbuf;
	gboolean ok=FALSE;
	long mac;
	long maxundo, wbufsize, maxlru, maxfds, maxmaps, maxvm;

	/* New box. */
        prop_box = gnome_property_box_new();

	/* Swapfile with
	 * - swapfile location */
        tablabel = gtk_label_new(_("Swapfile"));
        vbox = gtk_vbox_new(FALSE,1);
        gtk_widget_show(vbox);

        notelabel = gtk_label_new(_("You need lots of diskspace available at the swapfile location."));
        gtk_widget_show(notelabel);
        gtk_container_add(GTK_CONTAINER(vbox), notelabel);
        notelabel = gtk_label_new(_("GLAME doesnt handle running out of disk space very well."));
        gtk_widget_show(notelabel);
        gtk_container_add(GTK_CONTAINER(vbox), notelabel);
        entry = gnome_file_entry_new("swapfilepath", _("Swapfile Path"));
        create_label_widget_pair(vbox, _("Swapfile Path"), entry);
	glame_config_get_string("swapfile/defaultpath", &cfg);
        path = alloca(256);
	strncpy(path, cfg, 255);
	g_free(cfg);
        gtk_entry_set_text(GTK_ENTRY(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(entry))), path);
        gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(entry))), "changed", (GtkSignalFunc)changeString, &path);
        notelabel = gtk_label_new(_("Dont change values below, if you dont know what you are doing."));
        gtk_widget_show(notelabel);
        gtk_container_add(GTK_CONTAINER(vbox), notelabel);
	glame_config_get_long("swapfile/maxlru", &maxlru);
	maxlrubuf = alloca(256);
	snprintf(maxlrubuf, 255, "%li", maxlru);
	create_label_edit_pair(vbox, _("Maximum number of cached fragment headers"), "prefs::maxlru",
			       maxlrubuf);
	glame_config_get_long("swapfile/maxfds", &maxfds);
	maxfdsbuf = alloca(256);
	snprintf(maxfdsbuf, 255, "%li", maxfds);
	create_label_edit_pair(vbox, _("Maximum number of cached file descriptors"), "prefs::maxfds",
			       maxfdsbuf);
	glame_config_get_long("swapfile/maxmaps", &maxmaps);
	maxmapsbuf = alloca(256);
	snprintf(maxmapsbuf, 255, "%li", maxmaps);
	create_label_edit_pair(vbox, _("Maximum number of cached memory mappings"), "prefs::maxmaps",
			       maxmapsbuf);
	glame_config_get_long("swapfile/maxvm", &maxvm);
	maxvmbuf = alloca(256);
	snprintf(maxvmbuf, 255, "%li", maxvm);
	create_label_edit_pair(vbox, _("Maximum virtual memory used for caching"), "prefs::maxvm",
			       maxvmbuf);
        notelabel = gtk_label_new(_("NOTE: Swapfile settings take effect after restart only"));
        gtk_widget_show(notelabel);
        gtk_container_add(GTK_CONTAINER(vbox), notelabel);
	glame_config_get_long("swapfile/maxundo", &maxundo);
	maxundobuf = alloca(256);
	snprintf(maxundobuf, 255, "%li", maxundo);
	create_label_edit_pair(vbox, _("Depth of undo stack"), "prefs::maxundo",
			       maxundobuf);
        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box),vbox,tablabel);

	/* Edit Filter with
	 * - popup timeout
	 * - mac mode */
	tablabel = gtk_label_new(_("Filternetwork"));
	vbox = gtk_vbox_new(FALSE,1);
	gtk_widget_show(vbox);

	glame_config_get_long("edit_filter/popupTimeout", &nPopupTimeout);
	numberbuffer = alloca(256);
	snprintf(numberbuffer, 255, "%li", nPopupTimeout);
	create_label_edit_pair(vbox, _("Property popup timeout [ms]"), "popupTimeout",
			       numberbuffer);

	glame_config_get_long("edit_filter/macMode", &mac);
	bMac = mac;
	macMode = gtk_check_button_new();
	gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(macMode),mac);
	gtk_signal_connect(GTK_OBJECT(macMode),"toggled",(GtkSignalFunc)toggle_cb,&mac);
	gtk_widget_show(macMode);
	create_label_widget_pair(vbox, _("Mac mode (one mouse button mode)"),macMode);

        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box),vbox,tablabel);


	/* Audio I/O with
	 * - input device
	 * - output device */
	tablabel = gtk_label_new(_("Audio I/O"));
	vbox = gtk_vbox_new(FALSE, 1);
	gtk_widget_show(vbox);

	/* input filter */
	combo_items = NULL;
	if (plugin_get("oss_audio_in"))
		combo_items = g_list_append(combo_items, "oss_audio_in");
	if (plugin_get("esd_audio_in"))
		combo_items = g_list_append(combo_items, "esd_audio_in");
	if (plugin_get("alsa_audio_in"))
		combo_items = g_list_append(combo_items, "alsa_audio_in");
	if (plugin_get("sgi_audio_in"))
		combo_items = g_list_append(combo_items, "sgi_audio_in");
	if (combo_items) {
		combo = gtk_combo_new();
		gtk_combo_set_popdown_strings(GTK_COMBO(combo), combo_items);
		g_list_free(combo_items);
		gtk_widget_show(combo);
		glame_config_get_string("audio_io/input_plugin", &cfg);
		ainplugin = alloca(256);
		strncpy(ainplugin, cfg, 255);
		g_free(cfg);
		gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(combo)->entry), ainplugin);
		gtk_signal_connect(GTK_OBJECT(GTK_COMBO(combo)->entry), "changed", (GtkSignalFunc)changeString, &ainplugin);
		gtk_widget_show(GTK_COMBO(combo)->entry);
		create_label_widget_pair(vbox, _("Default input plugin (audio_in)"), combo);

		/* input device */
		aindev = alloca(256);
		glame_config_get_string("audio_io/input_dev", &cfg);
		strncpy(aindev, cfg, 255);
		g_free(cfg);
		create_label_edit_pair(vbox, _("Default input device"), "aindev", aindev);
	} else {
		combo = gtk_label_new(_("No audio input plugin"));
		gtk_container_add(GTK_CONTAINER(vbox), combo);
		gtk_widget_show(combo);
	}

	/* output filter */
	combo_items = NULL;
	if (plugin_get("oss_audio_out"))
		combo_items = g_list_append(combo_items, "oss_audio_out");
	if (plugin_get("esd_audio_out"))
		combo_items = g_list_append(combo_items, "esd_audio_out");
	if (plugin_get("alsa_audio_out"))
		combo_items = g_list_append(combo_items, "alsa_audio_out");
	if (plugin_get("sgi_audio_out"))
		combo_items = g_list_append(combo_items, "sgi_audio_out");
	if (combo_items) {
		combo = gtk_combo_new();
		gtk_combo_set_popdown_strings(GTK_COMBO(combo), combo_items);
		g_list_free(combo_items);
		gtk_widget_show(combo);
		glame_config_get_string("audio_io/output_plugin", &cfg);
		aoutplugin = alloca(256);
		strncpy(aoutplugin, cfg, 255);
		g_free(cfg);
		gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(combo)->entry), aoutplugin);
		gtk_signal_connect(GTK_OBJECT(GTK_COMBO(combo)->entry), "changed", (GtkSignalFunc)changeString, &aoutplugin);
		gtk_widget_show(GTK_COMBO(combo)->entry);
		create_label_widget_pair(vbox, _("Default output plugin (audio_out)"), combo);

		/* output device */
		aoutdev = alloca(256);
		glame_config_get_string("audio_io/output_dev", &cfg);
		strncpy(aoutdev, cfg, 255);
		g_free(cfg);
		create_label_edit_pair(vbox, _("Default output device"),
				       "aoutdev", aoutdev);
	} else {
		combo = gtk_label_new(_("No audio output plugin"));
		gtk_container_add(GTK_CONTAINER(vbox), combo);
		gtk_widget_show(combo);
	}

	/* GLAME_WBUFSIZE */
	glame_config_get_long("filter/wbufsize", &wbufsize);
	wbufsizebuf = alloca(256);
	snprintf(wbufsizebuf, 255, "%li", wbufsize);
	create_label_edit_pair(vbox, _("Size hint for audio buffers [samples]"), "prefs::wbufsize",
			       wbufsizebuf);

        gnome_property_box_append_page(GNOME_PROPERTY_BOX(prop_box), vbox,
				       tablabel);


	/* Finish. */
        gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->ok_button),"clicked",(GtkSignalFunc)setBoolean,&ok);
        gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->apply_button));
        gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(prop_box)->help_button));
        gtk_widget_show(prop_box);


	/* Run the dialog. */
        gnome_dialog_run_and_close(GNOME_DIALOG(prop_box));
        if(!ok)
		return 0;

	/* Update gnome config. */
	glame_config_set_string("swapfile/defaultpath", path);
	if (sscanf(maxundobuf, "%li", &maxundo) == 1)
		glame_config_set_long("swapfile/maxundo", maxundo);
	if (sscanf(maxlrubuf, "%li", &maxlru) == 1)
		glame_config_set_long("swapfile/maxlru", maxlru);
	if (sscanf(maxfdsbuf, "%li", &maxfds) == 1)
		glame_config_set_long("swapfile/maxfds", maxfds);
	if (sscanf(maxmapsbuf, "%li", &maxmaps) == 1)
		glame_config_set_long("swapfile/maxmaps", maxmaps);
	if (sscanf(maxvmbuf, "%li", &maxvm) == 1)
		glame_config_set_long("swapfile/maxvm", maxvm);
	if (sscanf(numberbuffer, "%li", &nPopupTimeout) == 1)
		glame_config_set_long("edit_filter/popupTimeout", nPopupTimeout);
	bMac = mac;
	glame_config_set_long("edit_filter/macMode",bMac);
	if (aindev)
		glame_config_set_string("audio_io/input_dev", aindev);
	if (ainplugin)
		glame_config_set_string("audio_io/input_plugin", ainplugin);
	if (aoutdev)
		glame_config_set_string("audio_io/output_dev", aoutdev);
	if (aoutplugin)
		glame_config_set_string("audio_io/output_plugin", aoutplugin);
	if (sscanf(wbufsizebuf, "%li", &wbufsize) == 1)
		glame_config_set_long("filter/wbufsize", wbufsize);
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
	const gchar *authors[]={
		N_("Richard Guenther [Richi]"),
		N_("Alexander Ehlert [Mag]"),
		N_("Daniel Kobras [*nold]"),
		N_("Johannes Hirche [XWolf]"),
		N_("and others"),
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

				 _(
"This program is free software; you can redistribute it and/or modify"
"it under the terms of the GNU General Public License as published by"
"the Free Software Foundation; either version 2 of the License, or"
"(at your option) any later version.\n"
"This program is distributed in the hope that it will be useful,"
"but WITHOUT ANY WARRANTY; without even the implied warranty of"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
"GNU General Public License for more details.\n"
"You should have received a copy of the GNU General Public License"
"along with this program; if not, write to the Free Software"
"Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA"),
				 logo);
	gtk_object_set_data (GTK_OBJECT (about), "about", about);
	gtk_window_set_modal (GTK_WINDOW (about), TRUE);
	gtk_window_set_wmclass (GTK_WINDOW (about), "Glameabout", "Glame");
	gtk_window_set_position (GTK_WINDOW (about), GTK_WIN_POS_CENTER_ALWAYS);
	gtk_widget_show(about);
	return about;
}

static gint glame_splash_timeout(GtkWidget *about)
{
	gtk_object_destroy(GTK_OBJECT(about));
	return FALSE;
}
static void glame_splash_destroy_cb(GtkWidget *about, gpointer tid)
{
	gtk_timeout_remove((guint)tid);
}
static void glame_splash(void)
{
	GtkWidget *about;
	guint tid;

	about = glame_about();
	gtk_object_destroy(GTK_OBJECT((GNOME_DIALOG(about))->action_area));

#ifdef DEBUG
	tid = gtk_timeout_add(1000, (GtkFunction)glame_splash_timeout, about);
#else
	tid = gtk_timeout_add(5000, (GtkFunction)glame_splash_timeout, about);
#endif
	gtk_signal_connect(GTK_OBJECT(about), "destroy",
			   (GtkSignalFunc)glame_splash_destroy_cb, (gpointer)tid);
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
	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(message)));

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
	/* FIXME: we want these to occour _after_ gtk shutdown.
	 *        This seems not to be possible.
	 */
	clipboard_empty();
	gpsm_close();
	/* glame_accel_sync(); -- disabled, until we get GUI support */

	gtk_main_quit();
}

static void gui_main()
{
	GtkWidget *dock, *scrollview;
	char *path;

	/* Init GUI dependend subsystems. */
	glame_accel_init();
	glame_swapfilegui_init();
	glame_waveeditgui_init();
	glame_filtereditgui_init();
	glame_timeline_init();

	/* Update preferences. */
	if (update_preferences() == -1) {
		gnome_dialog_run_and_close(GNOME_DIALOG(gnome_ok_dialog(
_("Welcome first-time user of GLAME.\n"
"We need to do some basic setup stuff. Please run through\n"
"the preferences dialog and check the \"Swapfile Path\" and\n"
"\"Audio IO\" settings.\n"))));
	run_prefs:
		if (!preferences_cb(NULL, NULL)) {
			gnome_dialog_run_and_close(
				GNOME_DIALOG(gnome_error_dialog("You didnt change/check the configuration.\nGLAME is exiting now.\n")));
			exit(1);
		}
	}

	glame_config_get_string("swapfile/defaultpath", &path);
	DPRINTF("path: %s\n",path);
	if (!g_file_test(path,G_FILE_TEST_ISDIR)) {
		if (swapfile_creat(path, -1)) {
			char msg[256];
			char *errmsg = strerror(errno);
			snprintf(msg, 255, "GLAME was unable to create its swapfile\nbecause of \"%s\".\nPlease check the configuration.\n", errmsg);
			gnome_dialog_run_and_close(
				GNOME_DIALOG(gnome_error_dialog(msg)));
			goto run_prefs;
		}
	}
	if (gpsm_init(path) == -1) {
		char msg[256];
		char *errmsg = strerror(errno);
		snprintf(msg, 255, "GLAME was unable to open/init its swapfile\nbecause of \"%s\".\nPlease check the configuration and/or check for\nGLAME messages on the console.\n", errmsg);
		gnome_dialog_run_and_close(
			GNOME_DIALOG(gnome_error_dialog(msg)));
		goto run_prefs;
	}
	g_free(path);

	/* Register a swapfile panic handler. */
	swapfile_register_panic_handler(on_swapfile_panic);

	/* create swapfile gui - in a scrolled window */
	swapfile = GTK_WIDGET(glame_swapfile_widget_new(gpsm_root()));
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
	app = gnome_app_new ("glame0.5", NULL);
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

	/* Register accelerators. */
	SWAPFILE_GUI(swapfile)->accel_handler =
		glame_accel_install(app, "swapfile", NULL);
	SWAPFILE_GUI(swapfile)->app = app;

	/* Pop up splash screen. */
	glame_splash();

	/* Pop up console. */
	glame_console_init();
	glame_console_printf("%s%s%s",
_("    GLAME version "), VERSION, _(", Copyright (C) 1999-2001 by\n"
"    Alexander Ehlert, Richard Guenther, Johannes Hirche,\n"
"    Daniel Kobras. GLAME comes with ABSOLUTELY NO\n"
"    WARRANTY. This is free software, and you are welcome to\n"
"    redistribute it under certain conditions.\n\n"));

	/* main loop */
       	gtk_main();
}


int main(int argc, char **argv)
{
	/* HACK */
	blafoobar(0);

	textdomain("glame");

	/* setup gnome/gtk  */
	gnome_init("glame", VERSION, argc, argv);

#ifdef HAVE_LIBGLADE
	glade_init();
#endif

	/* init glame */
	glame_init(gui_main, argc, argv);

	return 1;
}
