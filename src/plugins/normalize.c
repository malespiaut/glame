/*
 * normalize.c
 * $Id: normalize.c,v 1.11 2002/01/01 18:29:40 mag Exp $
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
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gnome.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "math.h"
#include "gpsm.h"
#include "network_utils.h"

PLUGIN(normalize)

typedef struct task_entry task_entry_t;

struct task_entry {
	struct glame_list_head list;
	float rms;
	gpsm_item_t* item;
};

struct normalize_s {
	GtkWidget *dialog, *appbar, *text, *spin_db, *spin_abs, *spin_freq;
	GtkWidget *analyze_button, *simulate_button, *ok_button;
	task_entry_t *head;
	gpsm_item_t *grp;
	long total_size, start, length;
	int mode;
	float ampl_abs, ampl_db, maxrms;
	int freq;	
	int running;
	int changed;
};



static void cleanup_task_list(struct normalize_s *ns) {
	task_entry_t *task, *last, *next;

	if (ns->head==NULL)
		return;

	last = task = ns->head;
	do {
		next = glame_list_getnext(&task->list, task, task_entry_t, list);
		free(task);
		task=next;
	} while ( (task!=last) && (task!=NULL));
	ns->head = NULL;
}

static gint normmode_cb(GtkMenu *menu, struct normalize_s* ns)
{
	int oldmode = ns->mode;
	ns->mode =  glame_menu_get_active_index(menu);
	DPRINTF("Selected normalize mode %d\n", ns->mode);
	switch (ns->mode) {
	case 0 : 
		gtk_widget_set_sensitive(ns->spin_abs, FALSE);
		gtk_widget_set_sensitive(ns->spin_db, FALSE);
		gtk_widget_set_sensitive(ns->spin_freq, FALSE);
		ns->ampl_abs = 1.0;
		ns->ampl_db = 0.0;
		if (oldmode==2)
			ns->changed=1;
		break;
	case 1:
		gtk_widget_set_sensitive(ns->spin_abs, TRUE);
		gtk_widget_set_sensitive(ns->spin_db, TRUE);
		gtk_widget_set_sensitive(ns->spin_freq, FALSE);
		break;
	case 2:
		gtk_widget_set_sensitive(ns->spin_abs, TRUE);
		gtk_widget_set_sensitive(ns->spin_db, TRUE);
		gtk_widget_set_sensitive(ns->spin_freq, TRUE);
		if (oldmode!=2)
			ns->changed=1;
	}

	return TRUE;
}


static gint ampl_abs_cb(GtkSpinButton *button, struct normalize_s* ns) {
	ns->ampl_abs = gtk_spin_button_get_value_as_float(button);
	ns->ampl_db = GAIN2DB(ns->ampl_abs);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(ns->spin_db), ns->ampl_db);
	return TRUE;
}

static gint ampl_db_cb(GtkSpinButton *button, struct normalize_s* ns) {
	ns->ampl_db = gtk_spin_button_get_value_as_float(button);
	ns->ampl_abs = DB2GAIN(ns->ampl_db);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(ns->spin_abs), ns->ampl_abs);
	return TRUE;
}

static gint norm_freq_cb(GtkSpinButton *button, struct normalize_s* ns) {
	ns->freq = gtk_spin_button_get_value_as_int(button); 
	ns->changed = 1;
	return TRUE;
}

static gint cancel_cb(GtkWidget *button, struct normalize_s* ns) {
	if (ns->running==1)
		ns->running = 0;
	gtk_widget_destroy(ns->dialog);
	return TRUE;
}

static gint paranoia_cb(GtkWidget *bla, struct normalize_s* ns) {
	if (ns->running==1)
		ns->running = 0;
	return TRUE;
}

static void simulate_cb(GtkWidget *, struct normalize_s*);
static void analyze_cb(GtkWidget *, struct normalize_s*);
static void normalize_cb(GtkWidget *, struct normalize_s*);

void normalize_dialog(struct normalize_s* norms)
{
  GtkWidget *dialog1;
  GtkWidget *dialog_vbox1;
  GtkWidget *frame1;
  GtkWidget *vbox1, *hbox1;
  GtkWidget *table1;
  GtkWidget *label3;
  GtkWidget *label2;
  GtkWidget *table2;
  GtkWidget *label5;
  GtkWidget *label6;
  GtkObject *spinbutton1_adj;
  GtkWidget *spinbutton1;
  GtkObject *spinbutton2_adj;
  GtkWidget *spinbutton2;
  GtkWidget *optionmenu1;
  GtkWidget *optionmenu1_menu;
  GtkWidget *glade_menuitem;
  GtkWidget *label7;
  GtkObject *spinbutton3_adj;
  GtkWidget *spinbutton3;
  GtkWidget *frame2;
  GtkWidget *scrolledwindow1;
  GtkWidget *text1;
  GtkWidget *button4, *button5;
  GtkWidget *appbar1;
  GtkWidget *dialog_action_area1;
  GtkWidget *button1;
  GtkWidget *button3;

  norms->dialog = dialog1 = gnome_dialog_new (NULL, NULL);
  gtk_object_set_data (GTK_OBJECT (dialog1), "dialog1", dialog1);
  gtk_window_set_policy (GTK_WINDOW (dialog1), FALSE, FALSE, FALSE);

  dialog_vbox1 = GNOME_DIALOG (dialog1)->vbox;
  gtk_object_set_data (GTK_OBJECT (dialog1), "dialog_vbox1", dialog_vbox1);
  gtk_widget_show (dialog_vbox1);

  frame1 = gtk_frame_new (_("Normalize"));
  gtk_widget_ref (frame1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "frame1", frame1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (frame1);
  gtk_box_pack_start (GTK_BOX (dialog_vbox1), frame1, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (frame1), 3);

  vbox1 = gtk_vbox_new (FALSE, 6);
  gtk_widget_ref (vbox1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "vbox1", vbox1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox1);
  gtk_container_add (GTK_CONTAINER (frame1), vbox1);
  gtk_container_set_border_width (GTK_CONTAINER (vbox1), 3);

  table1 = gtk_table_new (3, 2, FALSE);
  gtk_widget_ref (table1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "table1", table1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table1);
  gtk_box_pack_start (GTK_BOX (vbox1), table1, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (table1), 4);
  gtk_table_set_row_spacings (GTK_TABLE (table1), 8);

  label3 = gtk_label_new (_("Volume:"));
  gtk_widget_ref (label3);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "label3", label3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label3);
  gtk_table_attach (GTK_TABLE (table1), label3, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label3), 0, 0.5);

  label2 = gtk_label_new (_("Mode:"));
  gtk_widget_ref (label2);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "label2", label2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label2);
  gtk_table_attach (GTK_TABLE (table1), label2, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label2), 0, 0.5);

  table2 = gtk_table_new (2, 2, FALSE);
  gtk_widget_ref (table2);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "table2", table2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (table2);
  gtk_table_attach (GTK_TABLE (table1), table2, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  label5 = gtk_label_new (_("Abs"));
  gtk_widget_ref (label5);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "label5", label5,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label5);
  gtk_table_attach (GTK_TABLE (table2), label5, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label5), 0, 0.5);

  label6 = gtk_label_new (_("dB"));
  gtk_widget_ref (label6);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "label6", label6,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label6);
  gtk_table_attach (GTK_TABLE (table2), label6, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label6), 0, 0.5);

  spinbutton1_adj = gtk_adjustment_new (1.0, 0.0, 1.0, 0.01, 0.1, 0.0);
  norms->spin_abs = spinbutton1 = gtk_spin_button_new (GTK_ADJUSTMENT (spinbutton1_adj), 1, 2);
  gtk_widget_ref (spinbutton1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "spinbutton1", spinbutton1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (spinbutton1);
  gtk_table_attach (GTK_TABLE (table2), spinbutton1, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  
  gtk_widget_set_sensitive(spinbutton1, FALSE);

  gtk_signal_connect(GTK_OBJECT(spinbutton1),
		     "changed",
		     (GtkSignalFunc)ampl_abs_cb, norms);

  spinbutton2_adj = gtk_adjustment_new (0.0, -100.0, 0.0, 0.1, 0.5, 0.0);
  norms->spin_db = spinbutton2 = gtk_spin_button_new (GTK_ADJUSTMENT (spinbutton2_adj), 1, 2);
  gtk_widget_ref (spinbutton2);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "spinbutton2", spinbutton2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (spinbutton2);
  gtk_table_attach (GTK_TABLE (table2), spinbutton2, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbutton2), TRUE);

  gtk_widget_set_sensitive(spinbutton2, FALSE);
  gtk_signal_connect(GTK_OBJECT(spinbutton2),
		     "changed",
		     (GtkSignalFunc)ampl_db_cb, norms);

  optionmenu1 = gtk_option_menu_new ();
  gtk_widget_ref (optionmenu1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "optionmenu1", optionmenu1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (optionmenu1);
  gtk_table_attach (GTK_TABLE (table1), optionmenu1, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  optionmenu1_menu = gtk_menu_new ();
  glade_menuitem = gtk_menu_item_new_with_label (_("Peak"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Volume"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  glade_menuitem = gtk_menu_item_new_with_label (_("Volume/Frequency"));
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu1), optionmenu1_menu);

  gtk_signal_connect(GTK_OBJECT(optionmenu1_menu),
		     "selection_done",
		     (GtkSignalFunc)normmode_cb, norms);

  norms->mode = 0;

  label7 = gtk_label_new (_("Frequency:"));
  gtk_widget_ref (label7);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "label7", label7,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (label7);
  gtk_table_attach (GTK_TABLE (table1), label7, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_label_set_justify (GTK_LABEL (label7), GTK_JUSTIFY_LEFT);
  gtk_misc_set_alignment (GTK_MISC (label7), 0, 0.5);

  spinbutton3_adj = gtk_adjustment_new (1000, 1, 96000, 1, 100, 10);
  norms->spin_freq = spinbutton3 = gtk_spin_button_new (GTK_ADJUSTMENT (spinbutton3_adj), 1, 2);
  gtk_widget_ref (spinbutton3);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "spinbutton3", spinbutton3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (spinbutton3);
  gtk_table_attach (GTK_TABLE (table1), spinbutton3, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spinbutton3), TRUE);
  gtk_widget_set_sensitive(norms->spin_freq, FALSE);

  gtk_signal_connect(GTK_OBJECT(spinbutton3),
		     "changed",
		     (GtkSignalFunc)norm_freq_cb, norms);

  norms->freq = 1000;

  frame2 = gtk_frame_new (_("Operation Stats"));
  gtk_widget_ref (frame2);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "frame2", frame2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (frame2);
  gtk_box_pack_start (GTK_BOX (vbox1), frame2, TRUE, TRUE, 0);

  scrolledwindow1 = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_ref (scrolledwindow1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "scrolledwindow1", scrolledwindow1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (scrolledwindow1);
  gtk_container_add (GTK_CONTAINER (frame2), scrolledwindow1);
  gtk_widget_set_usize (scrolledwindow1, 400, 150);
  gtk_container_set_border_width (GTK_CONTAINER (scrolledwindow1), 3);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow1), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  norms->text = text1 = gtk_text_new (NULL, NULL);
  gtk_widget_ref (text1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "text1", text1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (text1);
  gtk_container_add (GTK_CONTAINER (scrolledwindow1), text1);

  hbox1 = gtk_hbox_new(TRUE, 2);
  gtk_widget_show(hbox1);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox1, FALSE, FALSE, 0);

  norms->analyze_button = button5 = gtk_button_new_with_label (_("Analyze"));
  gtk_widget_ref (button5);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "button5", button5,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button5);
  gtk_box_pack_start (GTK_BOX (hbox1), button5, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(button5), "clicked",
		     (GtkSignalFunc)analyze_cb, norms);

  norms->simulate_button = button4 = gtk_button_new_with_label (_("Simulate"));
  gtk_widget_ref (button4);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "button4", button4,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button4);
  gtk_box_pack_start (GTK_BOX (hbox1), button4, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(button4), "clicked",
		     (GtkSignalFunc)simulate_cb, norms);

  norms->appbar = appbar1 = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);
  gtk_widget_ref (appbar1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "appbar1", appbar1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (appbar1);
  gtk_box_pack_start (GTK_BOX (vbox1), appbar1, TRUE, TRUE, 0);

  dialog_action_area1 = GNOME_DIALOG (dialog1)->action_area;
  gtk_object_set_data (GTK_OBJECT (dialog1), "dialog_action_area1", dialog_action_area1);
  gtk_widget_show (dialog_action_area1);
  gtk_button_box_set_spacing (GTK_BUTTON_BOX (dialog_action_area1), 8);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area1), GTK_BUTTONBOX_EDGE);

  gnome_dialog_append_button (GNOME_DIALOG (dialog1), GNOME_STOCK_BUTTON_OK);
  norms->ok_button = button1 = GTK_WIDGET (g_list_last (GNOME_DIALOG (dialog1)->buttons)->data);
  gtk_widget_ref (button1);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "button1", button1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button1);
  GTK_WIDGET_SET_FLAGS (button1, GTK_CAN_DEFAULT);

  gtk_signal_connect(GTK_OBJECT(button1), "clicked",
		     (GtkSignalFunc)normalize_cb, norms);
  
  gnome_dialog_append_button (GNOME_DIALOG (dialog1), GNOME_STOCK_BUTTON_CANCEL);
  button3 = GTK_WIDGET (g_list_last (GNOME_DIALOG (dialog1)->buttons)->data);
  gtk_widget_ref (button3);
  gtk_object_set_data_full (GTK_OBJECT (dialog1), "button3", button3,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button3);
  GTK_WIDGET_SET_FLAGS (button3, GTK_CAN_DEFAULT);
  gtk_signal_connect(GTK_OBJECT(button3), "clicked",
		     (GtkSignalFunc)cancel_cb, norms);

  /* paranoia */
  gtk_signal_connect(GTK_OBJECT(norms->dialog),
		     "delete-event",
		     GTK_SIGNAL_FUNC(paranoia_cb), norms);

  norms->running = 0;
  norms->ampl_abs = 1.0;
  norms->ampl_db = 0.0;
  norms->changed = 1;
}

static float get_max_rms(task_entry_t* head, gpsm_item_t** item) {
	task_entry_t* task, *last;
	float maxrms=0.0;

	task = last = head;

	do {
		if (task->rms > maxrms) {
			maxrms = task->rms;
			*item = task->item;
		}
	       task = glame_list_getnext(&task->list, task, task_entry_t, list);
	} while ( (task!=last) && (task!=NULL));

	return maxrms;
}

static void analyze_rms(struct normalize_s *ns) {
	task_entry_t *te, *ote, *task, *last;
	filter_t *net, *ssp, *maxrms, *swap;
	filter_param_t	*param;
	float percentage, mrms;
	int num = 0;
	gpsm_item_t* item;
	char *label;
	long done = 0;
	int bsize;
	char *string;

	label = alloca(128);
	string = ALLOCN(2048, char);
	
	ote = NULL;


	gpsm_grp_foreach_item(ns->grp, item) {
		num++;
		snprintf(label, 128, "Analyzing Track %s", gpsm_item_label(item));
		gnome_appbar_set_status(GNOME_APPBAR(ns->appbar), label);

		te =  ALLOC(task_entry_t);
		GLAME_INIT_LIST_HEAD(&te->list);
		te->rms = 0.0;
		te->item = item;

		if (ote!=NULL)
			glame_list_add(&te->list, &ote->list);
		ote = te;
		ns->head = te;

		if (ns->mode < 2)
			bsize = 1;
		else {
			bsize = MAX(gpsm_swfile_samplerate(item)/ns->freq, 1);
		}

		net = filter_creat(NULL);
		ssp = net_add_plugin_by_name(net, "ssp_streamer");
		maxrms = net_add_plugin_by_name(net, "maxrms");
		swap = net_add_gpsm_input(net, (gpsm_swfile_t *)item,
 					  ns->start, MIN(ns->length, gpsm_item_hsize(item)), 0);
		DPRINTF("Processing %ld samples\n", MIN(ns->length, (gpsm_item_hsize(item))));
		net_apply_node(net, ssp);
		net_apply_node(net, maxrms);

		param = filterparamdb_get_param(filter_paramdb(ssp), "bsize");
		filterparam_set(param, &bsize);

		if (filter_launch(net, GLAME_BULK_BUFSIZE) == -1
		    || filter_start(net) == -1)
			goto fail_cleanup;

		param = filterparamdb_get_param(filter_paramdb(swap), 
						FILTERPARAM_LABEL_POS);

		ns->running=1;
		while(!filter_is_ready(net)) {
			while (gtk_events_pending())
				gtk_main_iteration();
			usleep(40000);
			
			percentage = (float)(done+filterparam_val_pos(param))/
				     (float)ns->total_size;

			if (ns->running==0)
				goto cancel_cleanup;

			gnome_appbar_set_progress(GNOME_APPBAR(ns->appbar),
						  percentage);
		}
		ns->running = 0;

		done+=filterparam_val_pos(param);
		DPRINTF("posparam=%ld\n", filterparam_val_pos(param));

		param = filterparamdb_get_param(filter_paramdb(maxrms), "maxrms");
		te->rms = filterparam_val_float(param);
		DPRINTF("found rms = %f\n", te->rms);
		
		filter_delete(net);
	}
	ns->running = 0;
	
	task = last = te;

	strcat(string, "Results:\n");
	do {
		snprintf(label, 128, "%s (max rms = %.3f = %.3f dB)\n", gpsm_item_label(task->item), task->rms, GAIN2DB(task->rms));
		strcat(string, label);
		task = glame_list_getnext(&task->list, task, task_entry_t, list);
	} while ( (task!=last) && (task!=NULL));
	
	ns->maxrms = mrms = get_max_rms(te, &item);
	snprintf(label, 128, "Found maximum rms = %.3f(%.3f dB) in track %s.\n\n", mrms, GAIN2DB(mrms), gpsm_item_label(item));
	strcat(string, label);
	gtk_text_insert(GTK_TEXT(ns->text), NULL, NULL, NULL, string, strlen(string));

	free(string);
	ns->changed = 0;
	return;

 cancel_cleanup:
	free(string);
	filter_terminate(net);
	filter_delete(net);
	gpsm_item_destroy(ns->grp);
	cleanup_task_list(ns);
	free(ns);
	return;

 fail_cleanup:
	filter_delete(net);
	free(te);
	free(string);
	return;
}

static void normalize_do_task(struct normalize_s *ns) {
	int num;
	filter_t *net, *vadjust, *swapi, *swapo;
	filter_param_t	*param;
	char *label;
	float gain, percentage;
	long done = 0;
	gpsm_item_t * item;

	label=alloca(128);

	/* FIXME 
	   This is probably ok for peak normalizing, but otherwise ?
	*/

	if (ns->changed==1)
		analyze_rms(ns);

	gain = ns->ampl_abs/ns->maxrms;

	gpsm_op_prepare(ns->grp);

	gpsm_grp_foreach_item(ns->grp, item) {
		num++;
		snprintf(label, 128, "Normalizing Track %s", gpsm_item_label(item));
		gnome_appbar_set_status(GNOME_APPBAR(ns->appbar), label);
		net = filter_creat(NULL);
		vadjust = net_add_plugin_by_name(net, "volume_adjust");
		swapi = net_add_gpsm_input(net, (gpsm_swfile_t *)item,
					   ns->start, MIN(ns->length, gpsm_item_hsize(item)) , 0);

		net_apply_node(net, vadjust);
		swapo = net_add_gpsm_output(net, (gpsm_swfile_t *)item,
					    ns->start, MIN(ns->length, gpsm_item_hsize(item)) , 0);
		net_apply_node(net, swapo);

		param = filterparamdb_get_param(filter_paramdb(vadjust), "factor");
		filterparam_set(param, &gain);


		if (filter_launch(net, GLAME_BULK_BUFSIZE) == -1
		    || filter_start(net) == -1)
			goto fail_cleanup;

		param = filterparamdb_get_param(filter_paramdb(swapi), 
						FILTERPARAM_LABEL_POS);

		ns->running=1;
		while(!filter_is_ready(net)) {
			while (gtk_events_pending())
				gtk_main_iteration();
			usleep(40000);
			
			percentage = (float)(done+filterparam_val_pos(param))/
				     (float)ns->total_size;

			if (ns->running==0)
				goto cancel_cleanup;

			gnome_appbar_set_progress(GNOME_APPBAR(ns->appbar),
						  percentage);
		}
		ns->running = 0;
		gpsm_notify_swapfile_change(gpsm_swfile_filename(item), 
					    ns->start, MIN(ns->length, (gpsm_item_hsize(item)-ns->start+1)));

		done+=filterparam_val_pos(param);
		DPRINTF("posparam=%ld\n", filterparam_val_pos(param));

		filter_delete(net);
	}

	gtk_widget_destroy(ns->dialog);
	cleanup_task_list(ns);
	free(ns);
	return;

 cancel_cleanup:
	filter_terminate(net);
	filter_delete(net);
	cleanup_task_list(ns);
	gpsm_op_undo_and_forget(ns->grp);
	free(ns);
	return;
 fail_cleanup:
	filter_delete(net);
	DPRINTF("Error starting normalizing network!\n");
	return;
}

static long get_total_size(struct normalize_s *ns) {
	long size = 0;
	int num = 0;
	gpsm_item_t* item;

	gpsm_grp_foreach_item(ns->grp, item) {
		num++;
		size+=MIN(gpsm_item_hsize(item), ns->length);
		DPRINTF("hsize=%ld\n vsize = %ld processed length=%ld\n", 
			gpsm_item_hsize(item), 
			gpsm_item_vsize(item),
			MIN(gpsm_item_hsize(item), ns->length));
		
	}

	DPRINTF("%d tracks with total size %ld\n", num, size);
	return size;
}

static void simulate_cb(GtkWidget *button, struct normalize_s* ns) {
	DPRINTF("working hard ;)\n");
}

static void normalize_cb(GtkWidget *button, struct normalize_s* ns) {
	DPRINTF("working like a dog every night and day ;)\n");
	gtk_widget_set_sensitive(ns->ok_button, FALSE);
	gtk_widget_set_sensitive(ns->analyze_button, FALSE);
	gtk_widget_set_sensitive(ns->simulate_button, FALSE);
	normalize_do_task(ns);
}

static void analyze_cb(GtkWidget *button, struct normalize_s* ns) {
	gtk_widget_set_sensitive(ns->analyze_button, FALSE);
	gtk_widget_set_sensitive(ns->ok_button, FALSE);
	gtk_widget_set_sensitive(ns->simulate_button, FALSE);
	analyze_rms(ns);
	gtk_widget_set_sensitive(ns->analyze_button, TRUE);
	gtk_widget_set_sensitive(ns->ok_button, TRUE);
	gtk_widget_set_sensitive(ns->simulate_button, TRUE);
}


static int normalize_gpsm(gpsm_item_t *grp, long start, long length)
{
	struct normalize_s *ns;

	ns = ALLOCN(1, struct normalize_s);

	normalize_dialog(ns);
	gtk_widget_show(ns->dialog);

	ns->grp = (gpsm_item_t*)gpsm_collect_swfiles(grp);
	if (ns->grp==NULL)
		return -1;

	ns->start = start;
	ns->length = length;
	ns->total_size = get_total_size(ns);


	DPRINTF("Size of all selected tracks: %ld\n", ns->total_size);

	return 0;
}

int normalize_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_GPSMOP, normalize_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION, "normalizes a gpsm subtree");
	plugin_set(p, PLUGIN_CATEGORY, "Volume");
	
	return 0;
}
/* Generalization strikes 240 lines for a simple function :) */
