/*
 * importexport.c
 * $Id: importexport.c,v 1.20 2001/11/14 09:10:14 mag Exp $
 *
 * Copyright (C) 2001 Alexander Ehlert
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <gtk/gtk.h>
#include <gnome.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "gpsm.h"
#include "swapfile.h"
#include "util/glame_gui_utils.h"
#include "network_utils.h"
#include "waveeditgui.h"
#include "glame_audiofile.h"

PLUGIN_SET(importexport, "import export")

static char ftlabel[8][10] = { "raw", "aiffc", "aiff", "nextsnd", "wav", "sf", "ogg", "mp3" };
static char sflabel[6][32] = { "16 bit signed", "24 bit signed", "32 bit signed", "8 bit unsigned", "32 bit float", "64 bit float" };
#define MAX_SFLABEL 6
#define MAX_PROPS 7

static char fproplabel[7][20] = { "Format", "Samplerate", "Quality", "Channels", 
			   "Duration", "Max RMS", "DC Offset" };

#define IMPORT  0
/*#define PREVIEW 1*/
#define CANCEL  1

struct impexp_s {
	filter_t *net;
	int cancelled;
	int importing;
	int destroyed;
	gpsm_grp_t *grp;
	GtkWidget *fi_plabel[MAX_PROPS];
	GtkWidget *dialog;
	GtkWidget *edit, *appbar, *checkresample, *rateentry;
	filter_t *readfile;
	gchar *filename;
	unsigned int frames;
	int rate, chancnt, gotfile, gotstats;
	float maxrms, dcoffset;
	plugin_t *resample;
};

struct exp_s {
	GtkWidget *dialog, *otypemenu, *compmenu, *ocompmenu;
	int typecnt;
	int *indices;
	GtkWidget *rbutton[4];
};

void ie_import_cleanup(struct impexp_s *ie) 
{
	if (ie->dialog)
		gnome_dialog_close(GNOME_DIALOG(ie->dialog));

	if (ie->readfile!=NULL)
		filter_delete(ie->readfile);
	free(ie);
	
}

static void ie_cancel_cb(GtkWidget *bla, struct impexp_s *ie) {
	DPRINTF("cancel pressed\n");
	if (ie->importing==1) {
		filter_terminate(ie->net);
		ie->cancelled = 1;
		DPRINTF("Network was running. Importing terminated.\n");
	}
	else {
		ie_import_cleanup(ie);
		DPRINTF("Nothing running. Just destroyed import dialog.\n");
	}
}

static gint ie_windowkilled(GtkWidget *bla,  GdkEventAny *event, gpointer data) {
	struct impexp_s *ie = (struct impexp_s*)data;
	if(ie->importing==1) {
		filter_terminate(ie->net);
	}
	ie->destroyed=1;
	ie->cancelled=1;
	return TRUE;
}

static void ie_import_cb(GtkWidget *bla, struct impexp_s *ie) {
	filter_t *readfile, *swout, *resample;
	filter_port_t *source, *nsource;
	filter_pipe_t *pipe;
	filter_param_t *pos_param;
	float percentage;
	gpsm_grp_t *group = NULL;
	int i, newrate;
	gboolean dorsmpl;
	gpsm_item_t *it;
	GtkWidget *ed;

	/* nullify net and set cancel status to zero */
	ie->net = NULL;
	ie->importing = 0;

	if(ie->gotfile==0) {
		ed=gnome_error_dialog("Select a file first!");
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(ie->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
		return;
	}

	ie->net = filter_creat(NULL);
	ie->importing = 1;
	ie->cancelled = 0;
	if (!(readfile = filter_instantiate(plugin_get("read_file"))))
		return;

	gtk_widget_set_sensitive(bla, FALSE);

	if (filterparam_set(filterparamdb_get_param(filter_paramdb(readfile),
						    "filename"), &(ie->filename))==-1)
		goto ie_fail_cleanup;

	source = filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT);
	filter_add_node(ie->net, readfile, "readfile");

	dorsmpl = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ie->checkresample));

	if ((ie->resample) && (dorsmpl)) {
		newrate = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(ie->rateentry));
		DPRINTF("Resample to %d Hz\n", newrate);
		if(ie->rate==newrate) {
			DPRINTF("new samplerate equals old samplerate\n");
			dorsmpl=FALSE;
		}
	} else
		dorsmpl=FALSE;
		
	/* Setup gpsm group. */
	group = gpsm_newgrp(g_basename(ie->filename));
	
	i = 0;
	do {
		char swfilename[256];
		snprintf(swfilename, 255, "track-%i", i);
		if (!(it = (gpsm_item_t *)gpsm_newswfile(swfilename)))
			goto ie_fail_cleanup;
		gpsm_item_place(group, it, 0, i);
		swout = net_add_gpsm_output(ie->net, (gpsm_swfile_t *)it,
					    0, -1, 3);

		/* Insert resampler here */
		if(dorsmpl) {
			resample = filter_instantiate(ie->resample);
			filter_add_node(ie->net, resample, "resample");
			filterparam_set(filterparamdb_get_param(filter_paramdb(resample),
								"frequency"), &newrate);

			nsource = filterportdb_get_port(filter_portdb(resample),
							PORTNAME_IN);
			if (nsource == NULL)
				DPRINTF("(1)nsource = NULL\n");

			if (!(pipe = filterport_connect(source, nsource))) {
				DPRINTF("Connection to resample failed");
				gpsm_item_destroy(it);
				filter_delete(resample);
				break;
			}
			nsource = filterportdb_get_port(filter_portdb(resample),
							PORTNAME_OUT);
			if(nsource==NULL)
				DPRINTF("(2) nsource = NULL\n");
		} else 
			nsource = source;

			
		if (!(pipe = filterport_connect(
			nsource, filterportdb_get_port(
				filter_portdb(swout), PORTNAME_IN)))) {
			DPRINTF("Connection failed for channel %d\n",i+1);
			gpsm_item_destroy(it);
			filter_delete(swout);
			break;
		}
		gpsm_swfile_set((gpsm_swfile_t *)it,
				filterpipe_sample_rate(pipe),
				filterpipe_sample_hangle(pipe));
		i++;
	} while (i < ie->chancnt);
	
	pos_param = filterparamdb_get_param(filter_paramdb(readfile),
					FILTERPARAM_LABEL_POS);
	
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), "Importing");
	

	if ((filter_launch(ie->net, GLAME_BULK_BUFSIZE) == -1) ||
	    (filter_start(ie->net) == -1))
		goto ie_fail_cleanup;

	while(!filter_is_ready(ie->net)) {
		while (gtk_events_pending())
			gtk_main_iteration();
		
		usleep(40000);
		percentage = (float)filterparam_val_pos(pos_param)/(float)ie->frames;
		if(percentage>1.0)
			percentage = 1.0;
		gnome_appbar_set_progress(GNOME_APPBAR(ie->appbar),
					  percentage);
	}
	
	filter_delete(ie->net);
	
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), "Done.");
	gnome_appbar_set_progress(GNOME_APPBAR(ie->appbar),
				  0.0);

	if (ie->cancelled==0) {
		/* Notify gpsm of the change. */
		gpsm_grp_foreach_item(group, it)
			gpsm_invalidate_swapfile(gpsm_swfile_filename(it));
		
		/* Insert the group into the gpsm tree. */
		if (gpsm_vbox_insert(ie->grp, (gpsm_item_t *)group,
				     0, gpsm_item_vsize(ie->grp)) == -1)
			goto ie_fail_cleanup;
	} else
		gpsm_item_destroy((gpsm_item_t *)group);

	ie_import_cleanup(ie);
	return;

ie_fail_cleanup:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create importing network")));
	filter_delete(ie->net);
	gpsm_item_destroy((gpsm_item_t *)group);
	gtk_widget_set_sensitive(bla, TRUE);	
}

static void ie_preview_cb(GtkWidget *bla, struct impexp_s *ie) {
	DPRINTF("Not implemented\n");
}
static void ie_update_plabels(struct impexp_s *ie) {
	filter_param_t *fparam;
	gchar *property;
	unsigned int frames, minutes, seconds;
	int rate, ftype, version;
	gchar buffer[255];

	fparam = filterparamdb_get_param(filter_paramdb(ie->readfile), "filename");
	
	property = filterparam_get_property(fparam, "#format");
	if(property) {
		sscanf(property, "%d %d", &ftype, &version);
		DPRINTF("format property: %s, ftype=%d, version=%d\n", property,ftype,version);
		if ((ftype>=0) && (ftype<8))
			sprintf(buffer, "%s format", ftlabel[ftype]);
		else
			sprintf(buffer, "You shouldn't see this :)");
	} else	
		sprintf(buffer, "nan");

	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[0]), buffer);
	
	property = filterparam_get_property(fparam, "#samplerate");
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[1]), property);
	if (property) {
		sscanf(property, "%d", &rate);
		ie->rate = rate;
	}
	else
		rate = 0;

	property = filterparam_get_property(fparam, "#quality");
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[2]), property);

	property = filterparam_get_property(fparam, "#channels");
	if(property)
		sscanf(property, "%d", &(ie->chancnt));
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[3]), property);

	property = filterparam_get_property(fparam, "#framecount");
	if(property) {
		sscanf(property, "%d", &frames);
		ie->frames = frames;
	}
	else
		frames = 0;
	
	if (rate) {
		seconds = frames/rate;
		minutes = seconds / 60;
		seconds -= minutes * 60;
		sprintf(buffer, "%d:%02d min", minutes, seconds);
	} else
		sprintf(buffer, "nan");

	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[4]), buffer);
	sprintf(buffer, "%f", ie->maxrms);
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[5]), buffer);
}

static void ie_stats_cb(GtkWidget *bla, struct impexp_s *ie) {
	filter_t **ssp, **maxrms, *readfile;
	filter_param_t *param;
	GtkWidget *ed;
	float rms, mrms, percentage;
	int i, bsize=1;
	char buffer[128];

	if(ie->gotfile==0) {
		ed=gnome_error_dialog("Select a file first!");
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(ie->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
		return;
	}

	gtk_widget_set_sensitive(bla, FALSE);

	ssp = ALLOCN(ie->chancnt, filter_t*);
	maxrms = ALLOCN(ie->chancnt, filter_t*);

	ie->net = filter_creat(NULL);
	ie->importing = 1;

	readfile = net_add_plugin_by_name(ie->net, "read_file");
	filterparam_set(filterparamdb_get_param(filter_paramdb(readfile), "filename"), 
			&(ie->filename));

	DPRINTF("setup %d channels\n", ie->chancnt);

	for(i=0; i<ie->chancnt; i++) {
		ssp[i] = net_add_plugin_by_name(ie->net, "ssp_streamer");
		if(ssp[i]==NULL) {
			gnome_error_dialog("Couldn't create ssp plugin");
			goto _ie_stats_cleanup;
		}
			
		filterparam_set(filterparamdb_get_param(filter_paramdb(ssp[i]), "bsize"), &bsize);
		maxrms[i] = net_add_plugin_by_name(ie->net, "maxrms");
		if(maxrms[i]==NULL) {
			gnome_error_dialog("Couldn't create maxrms plugin");
			goto _ie_stats_cleanup;
		}

		if (!filterport_connect(filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT),
                                        filterportdb_get_port(filter_portdb(ssp[i]), PORTNAME_IN)))
			goto _ie_stats_cleanup;

		if (!filterport_connect(filterportdb_get_port(filter_portdb(ssp[i]), PORTNAME_OUT),
                                        filterportdb_get_port(filter_portdb(maxrms[i]), PORTNAME_IN)))
			goto _ie_stats_cleanup;
		
	}
	param = filterparamdb_get_param(filter_paramdb(readfile),
					FILTERPARAM_LABEL_POS);
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), "Analyzing");
	
	ie->cancelled=0;

	if ((filter_launch(ie->net, GLAME_BULK_BUFSIZE) == -1) ||
	    (filter_start(ie->net) == -1))
		goto _ie_stats_cleanup;
	
	DPRINTF("Start network for %d samples\n",ie->frames);

	while(!filter_is_ready(ie->net)) {
		while (gtk_events_pending())
			gtk_main_iteration();
		
		usleep(40000);
		percentage = (float)filterparam_val_pos(param)/(float)ie->frames;
		if(percentage>1.0)
			percentage = 1.0;
		gnome_appbar_set_progress(GNOME_APPBAR(ie->appbar),
					  percentage);
	}

	if (ie->cancelled==0) {
		ie->frames = filterparam_val_pos(param);
		sprintf(buffer, "%d", ie->frames);
		filterparam_set_property(filterparamdb_get_param(filter_paramdb(ie->readfile), "filename"),
					 "#framecount", 
					 buffer);
		ie->gotstats = 1;


		gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), "Done.");
		gnome_appbar_set_progress(GNOME_APPBAR(ie->appbar), 0.0);
		
		mrms = 0.0;
		
		for(i=0; i<ie->chancnt; i++) {
			param = filterparamdb_get_param(filter_paramdb(maxrms[i]),
						"maxrms");
			rms = filterparam_val_float(param);
			if(rms>mrms)
				mrms = rms;
		}

		ie->maxrms = mrms;

		sprintf(buffer, "%f", mrms);
		gtk_label_set_text(GTK_LABEL(ie->fi_plabel[5]), buffer);

		ie_update_plabels(ie);
	} else if (ie->destroyed!=1) {
		gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), "Cancelled.");
		gnome_appbar_set_progress(GNOME_APPBAR(ie->appbar), 0.0);
	}
	
 _ie_stats_cleanup:
	DPRINTF("Cleanup");
	free(ssp);
	free(maxrms);
	ie->importing=0;
	ie->cancelled=0;
	filter_delete(ie->net);
	gtk_widget_set_sensitive(bla, TRUE);
	if (ie->destroyed==1)
		ie_import_cleanup(ie);
}


static gint ie_filename_cb(GtkEditable *edit, struct impexp_s *ie) {
	gchar *filename;
	filter_param_t *fparam;
	int i;

	filename = gtk_editable_get_chars(edit, 0, -1);
	if(ie->filename != NULL)
		g_free(ie->filename);
	ie->filename = g_strdup(filename);
	DPRINTF("Got filename %s\n", ie->filename);
	g_free(filename);
	fparam = filterparamdb_get_param(filter_paramdb(ie->readfile), "filename");
	if (filterparam_set(fparam, &(ie->filename))==-1) {
		for(i=0; i<MAX_PROPS; i++)
			gtk_label_set_text(GTK_LABEL(ie->fi_plabel[i]), "nan");
		return FALSE;
	}
	
	ie_update_plabels(ie);
	ie->gotfile = 1;
	ie->gotstats = 0;
	return TRUE;
}

void glame_import_dialog(struct impexp_s *ie) 
{
	GtkWidget *dialog_vbox2;
	GtkWidget *vbox2;
	GtkWidget *frame3;
	GtkWidget *fileentry2;
	GtkWidget *combo_entry2;
	GtkWidget *vbox4;
	GtkWidget *frame6;
	GtkWidget *statbutton;
	GtkWidget *vbox3;
	GtkWidget *frame4;
	GtkWidget *hbox4;
	GtkWidget *checkresample;
	GtkWidget *vseparator1;
	GtkWidget *ratelabel;
	GtkObject *rateentry_adj;
	GtkWidget *rateentry;
	GtkWidget *frame5;
	GtkWidget *normalizebutton;
	GtkWidget *appbar2;
	GtkWidget *dialog_action_area2;
	GtkWidget *importbutton;
	/*	GtkWidget *previewbutton; */
	GtkWidget *cancelbutton;
	GtkWidget *table, *tsep, *tlabel;
	int i;

	ie->dialog = gnome_dialog_new (NULL, NULL);
	ie->importing = 0;
	ie->destroyed = 0;
	gtk_container_set_border_width (GTK_CONTAINER (ie->dialog), 1);
	gtk_window_set_policy (GTK_WINDOW (ie->dialog), FALSE, FALSE, FALSE);
	gnome_dialog_close_hides(GNOME_DIALOG(ie->dialog), TRUE);
	gnome_dialog_set_close(GNOME_DIALOG(ie->dialog), FALSE);

	dialog_vbox2 = GNOME_DIALOG (ie->dialog)->vbox;
	gtk_widget_show (dialog_vbox2);

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox2, TRUE, TRUE, 0);
	
	frame3 = gtk_frame_new (NULL);
	
	gtk_widget_show (frame3);
	gtk_box_pack_start (GTK_BOX (vbox2), frame3, TRUE, TRUE, 0);
	
	fileentry2 = gnome_file_entry_new ("gpsmop::import::filename", "Import File");
	gtk_widget_show (fileentry2);
	gtk_container_add (GTK_CONTAINER (frame3), fileentry2);
	
	combo_entry2 = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (fileentry2));
	gtk_widget_show (combo_entry2);
	
	ie->edit = GTK_WIDGET(GTK_EDITABLE(gnome_entry_gtk_entry(
						   GNOME_ENTRY(GNOME_FILE_ENTRY(fileentry2)->gentry))));

	gtk_signal_connect(GTK_OBJECT(ie->edit), "changed",
				   (GtkSignalFunc)ie_filename_cb, ie);

	vbox4 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox4);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox4, FALSE, FALSE, 0);
	
	frame6 = gtk_frame_new (_("File Information"));
	gtk_widget_show (frame6);
	gtk_box_pack_start (GTK_BOX (vbox4), frame6, TRUE, TRUE, 0);
	
	table = gtk_table_new (8, 3, FALSE);
	gtk_widget_show (table);
	gtk_container_add (GTK_CONTAINER (frame6), table);
	
	for(i=0; i<MAX_PROPS; i++) {
		tsep = gtk_vseparator_new();
		gtk_widget_show(tsep);
		gtk_table_attach (GTK_TABLE (table), tsep, 1, 2, i, i+1,
				  (GtkAttachOptions) (GTK_FILL),
				  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
		tlabel = gtk_label_new(fproplabel[i]);
		gtk_widget_show(tlabel);
		gtk_table_attach (GTK_TABLE (table), tlabel, 0, 1, i, i+1,
				  (GtkAttachOptions) (GTK_FILL),
				  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
		gtk_misc_set_padding (GTK_MISC (tlabel), 20, 0);
		gtk_label_set_justify (GTK_LABEL (tlabel), GTK_JUSTIFY_LEFT);
		gtk_misc_set_alignment (GTK_MISC (tlabel), 0, 0.5);

		ie->fi_plabel[i] = gtk_label_new("-");
		gtk_widget_show(ie->fi_plabel[i]);
		gtk_table_attach (GTK_TABLE (table), ie->fi_plabel[i], 2, 3, i, i+1,
				  (GtkAttachOptions) (GTK_FILL),
				  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
		gtk_label_set_justify (GTK_LABEL (ie->fi_plabel[i]), GTK_JUSTIFY_LEFT);
		gtk_misc_set_padding (GTK_MISC (ie->fi_plabel[i]), 20, 0);
		gtk_misc_set_alignment (GTK_MISC (ie->fi_plabel[i]), 0, 0.5);
	}
		
	statbutton = gtk_button_new_with_label (_("Get RMS & DC-Offset"));
	gtk_signal_connect(GTK_OBJECT(statbutton), "clicked",
				   (GtkSignalFunc)ie_stats_cb, ie);

	gtk_widget_show (statbutton);
	gtk_box_pack_start (GTK_BOX (vbox4), statbutton, FALSE, FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (statbutton), 3);

	vbox3 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox3);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox3, TRUE, TRUE, 0);

	frame4 = gtk_frame_new (NULL);
	gtk_widget_show (frame4);
	gtk_box_pack_start (GTK_BOX (vbox3), frame4, TRUE, TRUE, 0);

	hbox4 = gtk_hbox_new (FALSE, 5);
	gtk_widget_show (hbox4);
	gtk_container_add (GTK_CONTAINER (frame4), hbox4);

	/* Disable resample if there's no plugin */
	if(ie->resample==NULL)
		gtk_widget_set_sensitive(hbox4, FALSE);

	ie->checkresample = checkresample = gtk_check_button_new_with_label (_("Resample"));
	gtk_widget_show (checkresample);
	gtk_box_pack_start (GTK_BOX (hbox4), checkresample, FALSE, FALSE, 0);

	vseparator1 = gtk_vseparator_new ();
	gtk_widget_show (vseparator1);
	gtk_box_pack_start (GTK_BOX (hbox4), vseparator1, FALSE, FALSE, 0);

	ratelabel = gtk_label_new (_("Rate"));
	gtk_widget_show (ratelabel);
	gtk_box_pack_start (GTK_BOX (hbox4), ratelabel, FALSE, FALSE, 0);

	rateentry_adj = gtk_adjustment_new (44100, 0, 96000, 10, 100, 1000);
	ie->rateentry = rateentry = gtk_spin_button_new (GTK_ADJUSTMENT (rateentry_adj), 1, 0);
	gtk_widget_show (rateentry);
	gtk_box_pack_start (GTK_BOX (hbox4), rateentry, TRUE, TRUE, 0);
	
	frame5 = gtk_frame_new (NULL);
	gtk_widget_show (frame5);
	gtk_box_pack_start (GTK_BOX (vbox3), frame5, TRUE, TRUE, 0);

	normalizebutton = gtk_check_button_new_with_label (_("Normalize"));
	gtk_widget_show (normalizebutton);
	gtk_container_add (GTK_CONTAINER (frame5), normalizebutton);

	ie->appbar = appbar2 = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);
	gtk_widget_show (appbar2);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), appbar2, TRUE, TRUE, 0);

	dialog_action_area2 = GNOME_DIALOG (ie->dialog)->action_area;
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area2), GTK_BUTTONBOX_END);
	gtk_button_box_set_spacing (GTK_BUTTON_BOX (dialog_action_area2), 8);
	
	gnome_dialog_append_button_with_pixmap (GNOME_DIALOG (ie->dialog),
						_("Import"), GNOME_STOCK_PIXMAP_OPEN);
	importbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);
	gtk_widget_show (importbutton);
	GTK_WIDGET_SET_FLAGS (importbutton, GTK_CAN_DEFAULT);

/*
	gnome_dialog_append_button_with_pixmap (GNOME_DIALOG (ie->dialog),
						_("Preview"), GNOME_STOCK_PIXMAP_VOLUME);
	previewbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);
	gtk_widget_show (previewbutton);
	GTK_WIDGET_SET_FLAGS (previewbutton, GTK_CAN_DEFAULT);
*/

	gnome_dialog_append_button (GNOME_DIALOG (ie->dialog), GNOME_STOCK_BUTTON_CANCEL);
	cancelbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);

	gtk_widget_show (cancelbutton);
	GTK_WIDGET_SET_FLAGS (cancelbutton, GTK_CAN_DEFAULT);

	gnome_dialog_set_default(GNOME_DIALOG(ie->dialog), IMPORT);
	gnome_dialog_set_sensitive(GNOME_DIALOG(ie->dialog), IMPORT, TRUE);
/*	gnome_dialog_set_sensitive(GNOME_DIALOG(ie->dialog), PREVIEW, TRUE);*/
	gnome_dialog_set_sensitive(GNOME_DIALOG(ie->dialog), CANCEL, TRUE);
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), IMPORT,
				    ie_import_cb, ie);
/*	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), PREVIEW, ie_preview_cb, ie); */
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), CANCEL, 
				    ie_cancel_cb, ie);

	/* in case we have a non gnome compliant wm */
	gtk_signal_connect(GTK_OBJECT(ie->dialog),
			   "delete-event",
			   GTK_SIGNAL_FUNC(ie_windowkilled), ie);

	gtk_widget_show(ie->dialog);
};

static gint ie_comp_menu_cb(GtkMenu *menu, struct exp_s *ie) {
	DPRINTF("Compression Type chosen\n");
	return TRUE;
}

void make_comp_menu(struct exp_s *ie, int ftype) {
	int comptypes, i, sformtypes;
	int *comparray;
	int *sformarray;

	gchar *complabel;
	GtkWidget *menuitem;

	gtk_widget_destroy(ie->compmenu);

	ie->compmenu = gtk_menu_new();
	gtk_widget_show(ie->compmenu);
	
	menuitem=gtk_menu_item_new_with_label("none");
	gtk_widget_show(menuitem);
	gtk_menu_append(GTK_MENU(ie->compmenu), menuitem);

	sformtypes = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_SAMPLE_FORMATS, AF_QUERY_VALUE_COUNT, ftype, 0);
	DPRINTF("%d sample formats\n", sformtypes);
	sformarray = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_SAMPLE_FORMATS, AF_QUERY_VALUES, ftype, 0);
	for(i=0; i<sformtypes;i ++) 
		DPRINTF("%d\n", sformarray[i]);
	
	comptypes = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_COMPRESSION_TYPES, AF_QUERY_VALUE_COUNT, ftype, 0);
	if (comptypes>0) {
		comparray = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_COMPRESSION_TYPES, AF_QUERY_VALUES, ftype, 0);
		for(i=0; i<comptypes;i++) {
			complabel = (char*)afQueryPointer(AF_QUERYTYPE_COMPRESSION, AF_QUERY_LABEL, comparray[i], 0 ,0);
			menuitem = gtk_menu_item_new_with_label(complabel);
			gtk_widget_show(menuitem);
			gtk_menu_append(GTK_MENU(ie->compmenu), menuitem);
		}
		gtk_widget_set_sensitive(ie->ocompmenu, TRUE);
		gtk_signal_connect(GTK_OBJECT(ie->compmenu),
				   "selection_done",
				   (GtkSignalFunc)ie_comp_menu_cb, ie);
	} else
		gtk_widget_set_sensitive(ie->ocompmenu, FALSE);
	
	gtk_option_menu_set_menu(GTK_OPTION_MENU (ie->ocompmenu), ie->compmenu);
	gtk_option_menu_set_history (GTK_OPTION_MENU (ie->ocompmenu), 0);
			   
}


static gint ie_type_menu_cb(GtkMenu *menu, struct exp_s *ie) {
	GtkWidget *act;
	GList *list;
	int val;

	act = gtk_menu_get_active(menu);
	DPRINTF("Menu %p - Active %p\n", menu, act);
	list = gtk_container_children(GTK_CONTAINER(menu));
	val = 0;
	while (list) {
		DPRINTF("%i - %p\n", val, list->data);
		if ((GtkWidget *)(list->data) == act)
			break;
		list = g_list_next(list);
		val++;
	}
	if(list) {
		gchar *suffix = (char*)afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL, ie->indices[val] ,0 ,0);	
		DPRINTF("val=%d choose type %s\n", val, suffix);
		make_comp_menu(ie, ie->indices[val]);
	}
	return TRUE;
}

GtkWidget *glame_export_dialog(struct exp_s *ie)  {
	GtkWidget *dialog, *optionmenu, *menu, *mitem, *bigbox, *typecompbox, *valbox;
	GtkWidget *dialog_vbox2, *vbox, *frame, *frame2, *frame3, *fentry;
	GSList *rbuttons;
	int i;
	gchar *suffix;

	/* open new dialog window */
	dialog = ie->dialog = gnome_dialog_new(NULL, NULL);
	gtk_window_set_policy(GTK_WINDOW(dialog), FALSE, FALSE, FALSE);
	gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);
	gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);

	dialog_vbox2 = GNOME_DIALOG (ie->dialog)->vbox;
	gtk_widget_show (dialog_vbox2);

	vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox, TRUE, TRUE, 0);

	bigbox = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (bigbox);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), bigbox, TRUE, TRUE, 0);

	typecompbox = gtk_vbox_new (TRUE, 5);
	gtk_widget_show (typecompbox);
	gtk_box_pack_start (GTK_BOX (bigbox), typecompbox, TRUE, TRUE, 0);

	fentry = gnome_file_entry_new ("gpsmop::export::filename", "Export File");
	gtk_widget_show(fentry);
	gtk_box_pack_start (GTK_BOX (vbox), fentry, TRUE, TRUE, 0);

	frame = gtk_frame_new("File Format");
	gtk_widget_show(frame);
	gtk_box_pack_start (GTK_BOX (typecompbox), frame, TRUE, TRUE, 0);

	frame2 = gtk_frame_new("Compression Type");
	gtk_widget_show(frame2);
	gtk_box_pack_start (GTK_BOX (typecompbox), frame2, TRUE, TRUE, 0);

	frame3 = gtk_frame_new("Sample Format");
	gtk_widget_show(frame3);
	gtk_box_pack_start (GTK_BOX (bigbox), frame3, TRUE, TRUE, 0);
	valbox = gtk_vbox_new (TRUE, 5);
	gtk_widget_show (valbox);
	gtk_container_add(GTK_CONTAINER(frame3), valbox);
	
	rbuttons = NULL;
	for(i=0; i<MAX_SFLABEL; i++) {
		ie->rbutton[i] = gtk_radio_button_new_with_label(rbuttons, sflabel[i]);
		rbuttons = gtk_radio_button_group(GTK_RADIO_BUTTON(ie->rbutton[i]));
	}

	for(i=0; i<MAX_SFLABEL; i++) {
		gtk_widget_show(ie->rbutton[i]);
		gtk_box_pack_start (GTK_BOX (valbox), ie->rbutton[i], TRUE, TRUE, 0);
	}

	/* now construct option menu with available filetypes */
	ie->typecnt = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_ID_COUNT,0 ,0 ,0);

	ie->indices = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_IDS, 0 ,0, 0);

	ie->otypemenu = optionmenu = gtk_option_menu_new ();
	gtk_widget_show(optionmenu);

	menu = gtk_menu_new();
	gtk_widget_show(menu);
	
	DPRINTF("typecnt=%d\n", ie->typecnt);

	for(i=0; i<ie->typecnt; i++)  {
		suffix = (char*)afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL, ie->indices[i] ,0 ,0);
		mitem = gtk_menu_item_new_with_label(suffix);
		gtk_widget_show(mitem);
		gtk_menu_append (GTK_MENU (menu), mitem);
	}
	gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu), menu);
	gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), 0);
	gtk_container_add(GTK_CONTAINER(frame), optionmenu);
	gtk_container_set_border_width(GTK_CONTAINER(optionmenu), 5);



	ie->ocompmenu = gtk_option_menu_new ();
	gtk_widget_show(ie->ocompmenu);
	gtk_container_add(GTK_CONTAINER(frame2), ie->ocompmenu);
	gtk_container_set_border_width(GTK_CONTAINER(ie->ocompmenu), 5);



	ie->compmenu = gtk_menu_new();
	gtk_widget_show(ie->compmenu);

	if (ie->typecnt>0) {
		make_comp_menu(ie, ie->indices[0]);
	}

	gnome_dialog_append_button(GNOME_DIALOG (ie->dialog), GNOME_STOCK_BUTTON_OK);
	gnome_dialog_append_button(GNOME_DIALOG (ie->dialog), GNOME_STOCK_BUTTON_CANCEL);
	gtk_widget_show(dialog);
	
	gtk_signal_connect(GTK_OBJECT(menu),
			   "selection_done",
			   (GtkSignalFunc)ie_type_menu_cb, ie);
	return dialog;
}

static int export_gpsm(gpsm_item_t *item, long start, long length) 
{	
	struct exp_s *ie;

	ie = (struct exp_s*)calloc(1,sizeof(struct exp_s));
	glame_export_dialog(ie);
	return 0;
}

static int import_gpsm(gpsm_item_t *item, long start, long length) 
{
	struct impexp_s *ie;
	plugin_t        *rf;

	if (!(GPSM_ITEM_IS_GRP(item)))
		return -1;

	ie = (struct impexp_s*)calloc(1,sizeof(struct impexp_s));
	ie->grp = (gpsm_grp_t*)item;
	rf = plugin_get("read-file");
	if (rf==NULL)
		return -1;
	
	ie->resample = plugin_get("Resample"); /* scheme plugin */

	ie->readfile = filter_instantiate(rf);
	ie->gotfile = 0;
	glame_import_dialog(ie);
	return 0;

/* richi doesn't like my fast but ungeneric code
 * so let's integrate this code here to file_io.c at some stage
	afSetVirtualSampleFormat(afile, AF_DEFAULT_TRACK, AF_SAMPFMT_FLOAT, 32);
	afSetVirtualPCMMapping(afile, AF_DEFAULT_TRACK, 1.0, 0.0, -1.0, 1.0);

	pframes = aframes = afGetFrameCount(afile, AF_DEFAULT_TRACK);
	channels = afGetChannels(afile, AF_DEFAULT_TRACK);
	rate = (int)afGetRate(afile, AF_DEFAULT_TRACK);
	framesize = afGetVirtualFrameSize(afile, AF_DEFAULT_TRACK, 0);
*/

}

int export_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_CATEGORY, "ImEx");
  	plugin_set(p, PLUGIN_GPSMOP, export_gpsm);
	plugin_set(p, PLUGIN_LABEL, "Export");
	return 0;
}

int import_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_CATEGORY, "ImEx");
  	plugin_set(p, PLUGIN_GPSMOP, import_gpsm);
	plugin_set(p, PLUGIN_LABEL, "Import");
	return 0;
}
