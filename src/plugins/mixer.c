/*
 * mixer.c
 *
 * $Id: mixer.c,v 1.21 2004/10/23 13:09:29 richi Exp $
 *
 * Copyright (C) 2002, 2003 Laurent Georget
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
#include "util/glame_param.h"
#include "edit_filter/filtereditgui.h"
#include "util/glame_gui_utils.h"
#include "util/gtknob.h"

PLUGIN(mixer)

struct apply_data_s {
	filter_t *net;
	filter_t *pos;
	filter_t *pos2;
	filter_launchcontext_t *context;
	gpsm_item_t *item_a;
	gpsm_item_t *grp;
	gpsm_swfile_t *result_left;
	gpsm_swfile_t *result_right;
	filter_param_t *posparam;
	GtkWidget *dialog;
	GtkWidget *mixer_hbox;
	GtkWidget *counter;
	GtkWidget *progress;
	GtkObject *adjust;
	GtkWidget *hscroll;
	int stereo;
	int previewing;
	int applying;
	guint timeout_id;
	long start, length;
	int sr;
	div_t tot_time;
};

/* datas for special buttons (reset,solo,mute...):one for each button */
struct button_s {
	filter_param_t *param;
	double value;
	double before_mute;
	GtkWidget *mute_button;
	GtkWidget *solo_button;
};

/* globals */
static struct button_s *r[264];
static double before_solo[32];
/* double valsolo; */
static filter_param_t **param_solo;
static GtkWidget **solo_button;
static GtkWidget **mute_button;
static int buttons_count;
static int solos_count;
static int chanels_count;

/* Button numbers. */
#define PREVIEW 0
#define APPLY 1
#define CANCEL 2
#define HELP 3
#define ADV10 4

static void preview_cb(GtkWidget * bla, struct apply_data_s *a);
static void preview_start(struct apply_data_s *a);
static void preview_stop(struct apply_data_s *a);
static void apply_cb(GtkWidget * bla, struct apply_data_s *a);
static void close_cb(GtkWidget * bla, struct apply_data_s *a);
char *mixer_knob_formatter(gfloat lower, gfloat val, gpointer data);
GtkWidget *glame_param_slider_new(filter_param_t * param,
				  char *label_short,
				  gfloat value,
				  gfloat lower,
				  gfloat upper,
				  gfloat step_increment,
				  gfloat page_increment, gfloat page_size);

/* static void reset_cb(GtkWidget * widget, NULL); */





static void cleanup(struct apply_data_s *a)
{
	int index;

	DPRINTF("cleanup\n");

	if (a->timeout_id != -1)
		gtk_timeout_remove(a->timeout_id);
	gtk_widget_hide(a->dialog);
	
/* 	for (index = 0; index < buttons_count; index++) { */
/* 	  printf("index=%i butcount=%i \n",index,buttons_count); */
/* 	  gtk_widget_destroy(r[index]->mute_button); */
/* 	  gtk_widget_destroy(r[index]->solo_button); */
/* 	  free (r[index]); */
/* 	} */

	
	gtk_widget_destroy(a->dialog);
	if (a->net) {
		filter_delete(a->net);
	}
	gpsm_item_destroy((gpsm_item_t *) a->grp);

	free(a);

	/* FIX ME This part  segfaults sometimes : the buttons are already destroyed */
	/* for (index = 0; index < buttons_count; index++) { */
		/* printf("index=%i butcount=%i \n",index,buttons_count); */
		 /* free(r[index]); */
/* 	} */
}

static gint poll_net_cb(struct apply_data_s *a)
{
	/* filter_param_t *posparam; */
	char labelcount[48];
	int pos;
	div_t time;
	/* int sr; */

	if (filter_is_ready(a->context)) {
		gtk_timeout_remove(a->timeout_id);
		a->timeout_id = -1;

		if (a->previewing) {
			preview_stop(a);
			return FALSE;
		}
		if (a->applying) {
			gpsm_grp_t *grp;
			char *label;
			/* int is_not_centered=1; */
			filter_wait(a->context);

			/* insert  track(s) in a new group */
			label = alloca(128);
			snprintf(label, 128, "Mixed: %s",
				 gpsm_item_label(a->item_a));
			grp = gpsm_newgrp("mix");
			gpsm_item_set_label((gpsm_item_t *) grp, label);
			/*now, grp is inserted at root level */
			if (gpsm_item_place
			    (gpsm_root(), (gpsm_item_t *) grp, 0,
			     gpsm_item_vsize(gpsm_root())) == -1)
				DPRINTF("Cannot insert new group!?\n");
			gpsm_vbox_insert(grp,
					 (gpsm_item_t *) a->result_left, 0,
					 0);
			if (a->stereo == 1)
				gpsm_vbox_insert(grp,
						 (gpsm_item_t *) a->
						 result_right, 0, 1);

			/* previously: new group was inserted from a->item position, not working if mixer called from waveedit gui */
			/*  gpsm_vbox_insert(gpsm_item_parent(a->item_a), (gpsm_item_t *) grp, */
			/*  gpsm_item_hposition(a->item_a), */
/* 		gpsm_item_vposition(a->item_a)); */


			gpsm_invalidate_swapfile(gpsm_swfile_filename
						 (a->result_left));
			if (a->stereo == 1)
				gpsm_invalidate_swapfile
				    (gpsm_swfile_filename
				     (a->result_right));
			a->applying = 0;

		}
		/*  close_cb(NULL,a); */
		cleanup(a);
		return FALSE;
	}

	/* update progressbar */
	if (a->previewing) {

		/* a->posparam = */
/* 	  filterparamdb_get_param(filter_paramdb(a->pos2), */
/* 				  FILTERPARAM_LABEL_POS); */
		gtk_progress_bar_update(GTK_PROGRESS_BAR(a->progress),
					MIN(1.0,
					    (float)
					    filterparam_val_long(a->
								 posparam)
					    / (float) (a->length)));
		pos = filterparam_val_long(a->posparam);
		time = div((pos / a->sr), 60);
		snprintf(labelcount, 24, "%i mn %i s / %i mn %i s",
			 time.quot, time.rem, (a->tot_time).quot,
			 (a->tot_time).rem);
		gtk_label_set_text(GTK_LABEL(a->counter), labelcount);
	}
	if (a->applying) {
		a->posparam =
		    filterparamdb_get_param(filter_paramdb(a->pos2),
					    FILTERPARAM_LABEL_POS);
		gtk_progress_bar_update(GTK_PROGRESS_BAR(a->progress),
					MIN(1.0,
					    (float)
					    filterparam_val_long(a->
								 posparam)
					    / (float) (a->length)));
		snprintf(labelcount, 48, "Generating , please wait...");
		gtk_label_set_text(GTK_LABEL(a->counter), labelcount);
	}
	return TRUE;


}




static void preview_start(struct apply_data_s *a)
{
	/* filter_t *f; */


	if (filter_is_ready(a->context) == 0) {
		DPRINTF("filter has not finished\n\n");
		return;
	}

	if (!(a->context = filter_launch(a->net, _GLAME_WBUFSIZE)) ||
	    (filter_start(a->context) == -1))
		goto cleanup;




	a->timeout_id = gtk_timeout_add(300, (GtkFunction) poll_net_cb, a);
	a->previewing = 1;
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, FALSE);
	return;

      cleanup:
	cleanup(a);
}

static void preview_stop(struct apply_data_s *a)
{
	filter_terminate(a->context);
	gtk_timeout_remove(a->timeout_id);
	a->previewing = 0;
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, TRUE);
	return;
}

static void preview_cb(GtkWidget * widget, struct apply_data_s *a)
{
	/* Two functions
	 * 1. start previewing
	 * 2. stop previewing
	 */
	if (a->previewing)
		preview_stop(a);
	else
		preview_start(a);
}


/* make stereo mix if a->stereo=1 , else make mono mix */
static void apply_cb(GtkWidget * bla, struct apply_data_s *a)
{

	filter_t *swap_out_left, *swap_out_right, *render, *net_out;
	filter_port_t *out, *in_l, *in_r;
	filter_pipe_t *pipe, *pipe1;
	double pos;
	long swname;
	printf("a->stereo=%i\n\n", a->stereo);
	if (filter_is_ready(a->context) == 0) {
		DPRINTF("filter has not finished\n\n");
		return;
	}
	net_out = filter_get_node(a->net, "audio-out");
	if (!net_out) {
		DPRINTF("no net_out");
		goto cleanup;
	}
	filter_delete(net_out);


	if (a->stereo == 0) {

		DPRINTF("\n\nmono mix\n\n");
		a->result_left = gpsm_newswfile("mixed ");
		render = filter_get_node(a->net, "render");
		if (!render) {
			DPRINTF("no net_out");
			goto cleanup;
		}

		if (!(swap_out_left =
		      filter_instantiate(plugin_get("swapfile-out")))) {
			DPRINTF("error swapout left create\n\n");
			goto cleanup;
		}
		swname = gpsm_swfile_filename(a->result_left);
		filterparam_set(filterparamdb_get_param
				(filter_paramdb(swap_out_left),
				 "filename"), &swname);
		if ((filter_add_node
		     (a->net, swap_out_left, "swap_out_left")) == -1) {
			DPRINTF("error adding node swap_out_left\n\n");
			goto cleanup;
		}
		in_l =
		    filterportdb_get_port(filter_portdb(swap_out_left),
					  PORTNAME_IN);
		if (!(out =
		      filterportdb_get_port(filter_portdb(render),
					    PORTNAME_OUT))) {
			DPRINTF("error getting port l\n\n");
			goto cleanup;
		}

		if (!(pipe = filterport_connect(out, in_l))) {
			DPRINTF("error getting pipe\n\n");
			goto cleanup;
		}
		gpsm_swfile_set_samplerate(a->result_left,
					   filterpipe_sample_rate(pipe));
		if (!(a->context = filter_launch(a->net, GLAME_BULK_BUFSIZE))
		    || (filter_start(a->context) == -1))
			goto cleanup;
		/*      if (filter_wait(a->context) == -1) */
/* 			goto cleanup; */
	}

	/*stereo */
	else {

		DPRINTF("stereo mix");
		a->result_left = gpsm_newswfile("mixed left");
		gpsm_swfile_set_position(a->result_left,
					 FILTER_PIPEPOS_LEFT);
		a->result_right = gpsm_newswfile("mixed right");
		gpsm_swfile_set_position(a->result_right,
					 FILTER_PIPEPOS_RIGHT);

		render = filter_get_node(a->net, "render");
		if (!render) {
			DPRINTF("no net_out");
			goto cleanup;
		}

		if (!
		    (swap_out_left =
		     filter_instantiate(plugin_get("swapfile-out")))) {
			DPRINTF("error swapout left create\n\n");
			goto cleanup;
		}
		if (!
		    (swap_out_right =
		     filter_instantiate(plugin_get("swapfile-out")))) {
			DPRINTF("error swapout right create\n\n");
			goto cleanup;
		}

		swname = gpsm_swfile_filename(a->result_left);
		filterparam_set(filterparamdb_get_param
				(filter_paramdb(swap_out_left),
				 "filename"), &swname);
		if ((filter_add_node
		     (a->net, swap_out_left, "swap_out_left")) == -1) {
			DPRINTF("error adding node swap_out_left\n\n");
			goto cleanup;
		}
		swname = gpsm_swfile_filename(a->result_right);
		filterparam_set(filterparamdb_get_param
				(filter_paramdb(swap_out_right),
				 "filename"), &swname);
		if ((filter_add_node
		     (a->net, swap_out_right, "swap_out_right")) == -1) {
			DPRINTF("error adding node swap_out_right\n\n");
			goto cleanup;
		}



		in_l =
		    filterportdb_get_port(filter_portdb(swap_out_left),
					  PORTNAME_IN);
		if (!
		    (out =
		     filterportdb_get_port(filter_portdb(render),
					   PORTNAME_OUT))) {
			DPRINTF("error getting port l\n\n");
			goto cleanup;
		}

		if (!(pipe = filterport_connect(out, in_l))) {
			DPRINTF("error getting pipe\n\n");
			goto cleanup;
		}
		gpsm_swfile_set_samplerate(a->result_left,
					   filterpipe_sample_rate(pipe));
		pos = FILTER_PIPEPOS_LEFT;
		filterparam_set(filterparamdb_get_param
				(filterpipe_sourceparamdb(pipe),
				 "position"), &pos);

		in_r =
		    filterportdb_get_port(filter_portdb(swap_out_right),
					  PORTNAME_IN);
		if (!(pipe1 = filterport_connect(out, in_r))) {
			DPRINTF("error connecting port \n\n");
			goto cleanup;
		}
		gpsm_swfile_set_samplerate(a->result_right,
					   filterpipe_sample_rate(pipe1));
		pos = FILTER_PIPEPOS_RIGHT;
		filterparam_set(filterparamdb_get_param
				(filterpipe_sourceparamdb(pipe1),
				 "position"), &pos);
		if (!(a->context = filter_launch(a->net, GLAME_BULK_BUFSIZE))
		    || (filter_start(a->context) == -1))
			goto cleanup;

	}
	a->pos2 = swap_out_left;
	a->applying = 1;
	a->timeout_id = gtk_timeout_add(200, (GtkFunction) poll_net_cb, a);
	/* Disable buttons. */
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), PREVIEW,
				   FALSE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, FALSE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), CANCEL, FALSE);



	return;

      cleanup:
	cleanup(a);
}


static void close_cb(GtkWidget * bla, struct apply_data_s *a)
{
	gtk_timeout_remove(a->timeout_id);
	if (a->net) {
		filter_terminate(a->context);
		filter_delete(a->net);
	}
	if (a->grp)
		gpsm_item_destroy(a->grp);
	gtk_widget_destroy(a->dialog);
	free(a);
	return;
}

static gint delete_cb(GtkWidget * w, GdkEventAny * event,
		      struct apply_data_s *a)
{
	close_cb(w, a);
	return TRUE;
}


void mono_sel_cb(GtkWidget * widget, struct apply_data_s *a)
{
	a->stereo = 0;
}



void stereo_sel_cb(GtkWidget * widget, struct apply_data_s *a)
{
	a->stereo = 1;
	printf("a->stereo=%i\n\n", a->stereo);
}



static int mixer_gpsm(gpsm_item_t * obj, long start, long length)
{

	GtkWidget *label, *scrolledwindow, *viewport;
	GtkWidget *button, *hbox1;
	GSList *group;
	struct apply_data_s *a;
	gpsm_item_t *item;
	plugin_t *test;
	char *text_obj, text[48], labelcount[24];
	int ladspa_present;
	/* filter_param_t *posparam; */

	a = (struct apply_data_s *) malloc(sizeof(struct apply_data_s));
	a->net = NULL;
	a->context = NULL;
	a->stereo = 1;		/*default to stereo mix */
	a->previewing = 0;
	a->applying = 0;
	a->timeout_id = -1;
	a->length = length;

	buttons_count = 0;
	solos_count = 0;
	chanels_count = 0;
	param_solo = ALLOCN(32, filter_param_t *);
	solo_button = ALLOCN(32, GtkWidget *);
	mute_button = ALLOCN(32, GtkWidget *);

	/* Check if triplePara LADSPA plugin is available */
	test = plugin_get("triplePara");
	if (!test) {
		GtkWidget *dialog;
		int ret;
		dialog = gnome_message_box_new("TriplePara LADSPA plugin not found, you can get\n" 
                "it at http://plugin.org.uk .\n"
                "Equalization will be disabled.",
					       GNOME_MESSAGE_BOX_INFO, " OK ", NULL);
		gtk_window_set_position(GTK_WINDOW(dialog),
					GTK_WIN_POS_CENTER);
		ret = gnome_dialog_run(GNOME_DIALOG(dialog));
	}

	a->dialog = gtk_type_new(gnome_dialog_get_type());
	gnome_dialog_close_hides(GNOME_DIALOG(a->dialog), FALSE);
	gnome_dialog_set_close(GNOME_DIALOG(a->dialog), FALSE);
	gtk_window_set_position(GTK_WINDOW(a->dialog), GTK_WIN_POS_CENTER);
	gtk_window_set_policy(GTK_WINDOW(a->dialog), FALSE, TRUE, TRUE);
	if (test) {
		gtk_window_set_default_size(GTK_WINDOW(a->dialog), 850,
					    550);
	} else {
		gtk_window_set_default_size(GTK_WINDOW(a->dialog), 750,
					    450);
	}
	gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(a->dialog),
					       _("Preview"),
					       GNOME_STOCK_PIXMAP_VOLUME);
	gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(a->dialog),
					       _("Apply"),
					       GNOME_STOCK_PIXMAP_FORWARD);
	gnome_dialog_append_button(GNOME_DIALOG(a->dialog),
				   GNOME_STOCK_BUTTON_CANCEL);
	/* if (help) */
	/*            gnome_dialog_append_button( */
	/*                    GNOME_DIALOG(a->dialog), GNOME_STOCK_BUTTON_HELP); */
	gnome_dialog_set_default(GNOME_DIALOG(a->dialog), APPLY);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), PREVIEW, TRUE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, TRUE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), CANCEL, TRUE);
	/* gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), HELP, TRUE); */
	gtk_signal_connect(GTK_OBJECT(a->dialog),
			   "delete_event", (GtkSignalFunc) delete_cb, a);
	/*    gtk_signal_connect(GTK_OBJECT(a->dialog), "destroy", */
	/*                       (GtkSignalFunc)close_cb, a); */
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog),
				    PREVIEW, preview_cb, a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog),
				    APPLY, apply_cb, a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog),
				    CANCEL, close_cb, a);

	/* Mixer title */
	text_obj = gpsm_item_label(obj);
	snprintf(text, 127, "Mixer: %s", text_obj);
	label = gtk_label_new(text);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), label,
			   FALSE, FALSE, 3);
	gtk_widget_show(label);

	scrolledwindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(scrolledwindow);
	gtk_window_set_policy(GTK_WINDOW(a->dialog), FALSE, TRUE, TRUE);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox),
			   scrolledwindow, TRUE, TRUE, 0);

	viewport = gtk_viewport_new(NULL, NULL);
	gtk_widget_show(viewport);
	gtk_container_add(GTK_CONTAINER(scrolledwindow), viewport);
	/* 1 hbox for all chanels  */
	a->mixer_hbox = gtk_hbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(viewport), a->mixer_hbox);
	gtk_widget_show(a->mixer_hbox);
	a->grp = (gpsm_item_t *) gpsm_collect_swfiles(obj);
	if (!a->grp)
		goto cleanup;
	a->item_a = obj;
	if (!(a->net = filter_creat(NULL))) {
		DPRINTF("error creating network");
		goto cleanup;
	}

	/* the big  loop */
	gpsm_grp_foreach_item(a->grp, item) {
		filter_param_t *param;
		filter_t *swap_in, *volmix, *ladspa1, *pan2;
		GtkWidget *glame_param;
		gchar *text;
		GtkWidget *chanel_vbox, *chanel_frame, *eq_vbox,
		    *eq_low_frame, *eq_low_hbox, *eq_mid_frame,
		    *eq_mid_hbox, *eq_hi_frame, *eq_hi_hbox, *eq_lvl_frame,
		    *eq_lvl_hbox;
		double gain;

		if (gpsm_item_hposition(item) != 0)
			goto cleanup;	/* we dont want to handle non-aligned
					   tracks */
		text = gpsm_item_label(item);
		DPRINTF("Adding :%s \n", text);

		/* 1 frame for each  chanel ,a vbox in it */
		chanel_frame = gtk_frame_new(text);
		gtk_widget_show(chanel_frame);
		gtk_box_pack_start(GTK_BOX(a->mixer_hbox),
				   GTK_WIDGET(chanel_frame), FALSE, FALSE,
				   0);
		/* gtk_container_add(GTK_CONTAINER(a->mixer_hbox), a->mixer_hbox); */
		chanel_vbox = gtk_vbox_new(FALSE, 0);
		gtk_container_add(GTK_CONTAINER(chanel_frame),
				  chanel_vbox);
		gtk_container_set_border_width(GTK_CONTAINER(chanel_vbox),
					       3);
		gtk_widget_show(chanel_vbox);
		/* vbox for eq controls */
		eq_vbox = gtk_vbox_new(TRUE, 0);
		gtk_container_add(GTK_CONTAINER(chanel_vbox), eq_vbox);
		gtk_container_set_border_width(GTK_CONTAINER(eq_vbox), 3);
		gtk_widget_show(eq_vbox);

		/* net begin */
		swap_in =
		    net_add_gpsm_input(a->net, (gpsm_swfile_t *) item, 0,
				       -1, 0);
		if (!swap_in) {
			DPRINTF("error getting swap_in");
			goto cleanup;
		}
		/* a->pos2=swap_in; */

		/* triplePara parameters */
		/* Ports:  "Low-shelving gain (dB)" input, control, -70 to 30 */
		/*         "Low-shelving frequency (Hz)" input, control, 0 to 0.5*srate */
		/*         "Low-shelving slope" input, control, 0 to 1 */
		/*         "Band 1 gain (dB)" input, control, -70 to 30 */
		/*         "Band 1 frequency (Hz)" input, control, 0 to 0.5*srate */
		/*         "Band 1 bandwidth (octaves)" input, control, 0 to 4 */
		/*         "Band 2 gain (dB)" input, control, -70 to 30 */
		/*         "Band 2 frequency (Hz)" input, control, 0 to 0.5*srate */
		/*         "Band 2 bandwidth (octaves)" input, control, 0 to 4 */
		/*         "Band 3 gain (dB)" input, control, -70 to 30 */
		/*         "Band 3 frequency (Hz)" input, control, 0 to 0.5*srate */
		/*         "Band 3 bandwidth (octaves)" input, control, 0 to 4 */
		/*         "High-shelving gain (dB)" input, control, -70 to 30 */
		/*         "High-shelving frequency (Hz)" input, control, 0 to 0.5*srate */
		/*         "High-shelving slope" input, control, 0 to 1 */
		/*         "Input" input, audio, -1 to 1 */
		/*         "Output" output, audio, -1 to 1 */

		if (test) {	/* ladspa triple para is there */
			DPRINTF("ladspa triple para is there\n");
			ladspa1 =
			    net_add_plugin_by_name(a->net, "triplePara");
			if (!ladspa1) {
				DPRINTF(" error adding ladspa1\n");
				ladspa_present = 0;
			}
			ladspa_present = 1;
			if (!filterport_connect
			    (filterportdb_get_port
			     (filter_portdb(swap_in), PORTNAME_OUT),
			     filterportdb_get_port(filter_portdb(ladspa1),
						   "Input"))) {
				DPRINTF(" error connecting ladspa1\n");
				goto cleanup;
			}
			/*set the triplePra gains to 0 db and the 3 frequences as desired */
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Low-shelving gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			gain = 0;
			filterparam_set(param, &gain);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "High-shelving gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			gain = 0;
			filterparam_set(param, &gain);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 1 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			gain = 0;
			filterparam_set(param, &gain);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 1 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			gain = 100;
			filterparam_set(param, &gain);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 2 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			gain = 0;
			filterparam_set(param, &gain);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 2 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			gain = 1000;
			filterparam_set(param, &gain);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 3 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			gain = 0;
			filterparam_set(param, &gain);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 3 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			gain = 8000;
			filterparam_set(param, &gain);
			/* 1 frame for eq_low , eq_low_hbox in it */
			eq_low_frame = gtk_frame_new("Eq low");
			gtk_widget_show(eq_low_frame);
			gtk_box_pack_start(GTK_BOX(eq_vbox),
					   GTK_WIDGET(eq_low_frame), FALSE,
					   FALSE, 0);
			eq_low_hbox = gtk_hbox_new(FALSE, 0);
			gtk_container_add(GTK_CONTAINER(eq_low_frame),
					  eq_low_hbox);
			gtk_container_set_border_width(GTK_CONTAINER
						       (chanel_vbox), 3);
			gtk_widget_show(eq_low_hbox);
			/* 1 frame for eq_mid , eq_mid_hbox in it */
			eq_mid_frame = gtk_frame_new("Eq mid");
			gtk_widget_show(eq_mid_frame);
			gtk_box_pack_start(GTK_BOX(eq_vbox),
					   GTK_WIDGET(eq_mid_frame), FALSE,
					   FALSE, 0);
			eq_mid_hbox = gtk_hbox_new(FALSE, 0);
			gtk_container_add(GTK_CONTAINER(eq_mid_frame),
					  eq_mid_hbox);
			gtk_container_set_border_width(GTK_CONTAINER
						       (chanel_vbox), 3);
			gtk_widget_show(eq_mid_hbox);
			/* 1 frame for eq_hi , eq_hi_hbox in it */
			eq_hi_frame = gtk_frame_new("Eq hi");
			gtk_widget_show(eq_hi_frame);
			gtk_box_pack_start(GTK_BOX(eq_vbox),
					   GTK_WIDGET(eq_hi_frame), FALSE,
					   FALSE, 0);
			eq_hi_hbox = gtk_hbox_new(FALSE, 0);
			gtk_container_add(GTK_CONTAINER(eq_hi_frame),
					  eq_hi_hbox);
			gtk_container_set_border_width(GTK_CONTAINER
						       (chanel_vbox), 3);
			gtk_widget_show(eq_hi_hbox);


			/* eq1 low controls */
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 1 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =
			    glame_param_slider_new(param, "Hz", 100, 50,
						   500, 1, 100, 100);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_low_hbox),
					  glame_param);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 1 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =
			    glame_param_slider_new(param, "dB", 0, -20, 20,
						   1, 1, 1);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_low_hbox),
					  glame_param);
			/* eq2 mid controls */
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 2 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =
			    glame_param_slider_new(param, "Hz", 1000, 400,
						   4000, 1, 100, 100);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_mid_hbox),
					  glame_param);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 2 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =
			    glame_param_slider_new(param, "dB", 0, -20, 20,
						   1, 1, 1);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_mid_hbox),
					  glame_param);
			/* eq3 hi controls */
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 3 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =
			    glame_param_slider_new(param, "Hz", 8000, 4000,
						   15000, 1, 100, 100);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_hi_hbox),
					  glame_param);
			param =
			    filterparamdb_get_param(filter_paramdb
						    (ladspa1),
						    "Band 3 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =
			    glame_param_slider_new(param, "dB", 0, -20, 20,
						   1, 1, 1);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_hi_hbox),
					  glame_param);
		}

		else {
			DPRINTF("LADSPA TRPLE EQ NOT THERE, skipping\n");
			label = gtk_label_new("Eq. disabled");
			gtk_box_pack_start(GTK_BOX(eq_vbox), label, FALSE,
					   FALSE, 3);
			gtk_widget_show(label);
		}
		/* 1 frame for level , eq_lvl_hbox in it */
		eq_lvl_frame = gtk_frame_new("Level");
		gtk_widget_show(eq_lvl_frame);
		gtk_box_pack_start(GTK_BOX(chanel_vbox),
				   GTK_WIDGET(eq_lvl_frame), FALSE, FALSE,
				   0);
		eq_lvl_hbox = gtk_hbox_new(FALSE, 0);
		gtk_container_add(GTK_CONTAINER(eq_lvl_frame),
				  eq_lvl_hbox);
		gtk_container_set_border_width(GTK_CONTAINER(chanel_vbox),
					       3);
		gtk_widget_show(eq_lvl_hbox);
		/* volume */
		if (!
		    (volmix =
		     filter_instantiate((plugin_get("volume_adjust"))))) {
			DPRINTF("error getting volume_adjust");
			goto cleanup;
		}
		filter_add_node(a->net, volmix, "mix_volume");
		if (test) {
			DPRINTF
			    ("LADSPA TRPLE EQ is THERE,connecting  it to volmix  \n");
			if (!filterport_connect
			    (filterportdb_get_port
			     (filter_portdb(ladspa1), "Output"),
			     filterportdb_get_port(filter_portdb(volmix),
						   PORTNAME_IN))) {
				DPRINTF
				    (" error connecting ladspa1 to volmix\n");
				goto cleanup;
			}
		} else {
			DPRINTF
			    ("LADSPA TRPLE EQ NOT THERE,connecting swapin to volmix directly \n");
			if (!filterport_connect
			    (filterportdb_get_port
			     (filter_portdb(swap_in), PORTNAME_OUT),
			     filterportdb_get_port(filter_portdb(volmix),
						   PORTNAME_IN))) {
				DPRINTF
				    (" error connecting swap_in to volmix\n");
				goto cleanup;
			}
		}
		param =
		    filterparamdb_get_param(filter_paramdb(volmix),
					    "dbgain");
		if (!param) {
			DPRINTF("Unable to get volmix param");
			goto cleanup;
		}
		glame_param =
		    glame_param_slider_new(param, "dB", 0, -80.000, 20.000,
					   1.000, 4.000, 1.000);
		if (!glame_param) {
			DPRINTF("Unable to get volmix param widget");
			goto cleanup;
		}
		gtk_widget_show_all(glame_param);
		gtk_container_add(GTK_CONTAINER(eq_lvl_hbox), glame_param);

		/* pan2 */
		if (!(pan2 = filter_instantiate((plugin_get("pan2"))))) {
			DPRINTF("error getting pan2");
			goto cleanup;
		}
		filter_add_node(a->net, pan2, "pan2");
		if (!filterport_connect
		    (filterportdb_get_port
		     (filter_portdb(volmix), PORTNAME_OUT),
		     filterportdb_get_port(filter_portdb(pan2),
					   PORTNAME_IN))) {
			DPRINTF(" error connecting pan2 to volmix\n");
			goto cleanup;
		}
		param =
		    filterparamdb_get_param(filter_paramdb(pan2),
					    "position");
		if (!param) {
			DPRINTF("Unable to get pan2 param");
			goto cleanup;
		}
		filterparam_set_double(param, gpsm_swfile_position(item));

		glame_param =
		    glame_param_slider_new(param, "pan",
					   gpsm_swfile_position(item),
					   -M_PI, M_PI, M_PI / 8.0,
					   M_PI / 2.0, 0.0);
		if (!glame_param) {
			DPRINTF("Unable to get pan2 param widget");
			goto cleanup;
		}
		/*      printf("pos=%f\n",FILTER_PIPEPOS_CENTRE); */
		gtk_widget_show_all(glame_param);
		gtk_container_add(GTK_CONTAINER(eq_lvl_hbox), glame_param);

	}
	/*end of loop */

/* glame_filtereditgui_new(a->net, TRUE); *//*uncomment to see the network ,(also remove modal) */
	a->progress = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(a->progress),
					 GTK_PROGRESS_LEFT_TO_RIGHT);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox),
			   a->progress, FALSE, FALSE, 3);
	gtk_widget_show(a->progress);

	if (!(a->pos2 = net_apply_audio_out(a->net))) {
		DPRINTF("error applying audio_out");
		goto cleanup;
	}
	a->posparam = filterparamdb_get_param(filter_paramdb(a->pos2),
					      FILTERPARAM_LABEL_POS);



	/*scrollbar (for future use ) */
/* 	a->adjust = gtk_adjustment_new (0.0, 0.0, 100.0, 1.0, 1.0, 1.0); */
/* 	a->hscroll = gtk_hscrollbar_new (GTK_ADJUSTMENT (a->adjust)); */
	/* gtk_signal_connect (GTK_OBJECT (a->adjust), "value_changed", */
/* 			    GTK_SIGNAL_FUNC (scroll_move_cb), a); */
/* 	gtk_box_pack_start (GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), a->hscroll, FALSE, FALSE, 0); */
/* 	gtk_widget_show (a->hscroll); */

/* 	hbox for monostereo sel and time counter */
	hbox1 = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), hbox1,
			   FALSE, FALSE, 0);
	gtk_widget_show(hbox1);


	/* mono stereo selection */

	label = gtk_label_new("Mix to");
	gtk_box_pack_start(GTK_BOX(hbox1), label, FALSE, FALSE, 3);
	gtk_widget_show(label);
	button = gtk_radio_button_new_with_label(NULL, "mono");
	gtk_signal_connect(GTK_OBJECT(button), "clicked",
			   GTK_SIGNAL_FUNC(mono_sel_cb), a);
	gtk_box_pack_start(GTK_BOX(hbox1), button, FALSE, FALSE, 0);
	gtk_widget_show(button);

	group = gtk_radio_button_group(GTK_RADIO_BUTTON(button));
	button = gtk_radio_button_new_with_label(group, "stereo");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), TRUE);
	gtk_signal_connect(GTK_OBJECT(button), "clicked",
			   GTK_SIGNAL_FUNC(stereo_sel_cb), a);
	gtk_box_pack_start(GTK_BOX(hbox1), button, FALSE, FALSE, 0);
	gtk_widget_show(button);
	a->stereo = 1;





	/* time counter */
	a->length = length;
	a->sr = filterpipe_sample_rate(filterport_get_pipe
				       (filterportdb_get_port
					(filter_portdb(a->pos2),
					 PORTNAME_IN)));
	a->tot_time = div((a->length / a->sr), 60);
	snprintf(labelcount, 24, "%i mn %i s/ %i mn %i s", 0,
		 0, (a->tot_time).quot, (a->tot_time).rem);
	a->counter = gtk_label_new(labelcount);
	gtk_box_pack_end(GTK_BOX(hbox1), a->counter, FALSE, FALSE, 3);
	gtk_widget_show(a->counter);
	/* gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(a->dialog), */
	/*                                           _("Preview"), */
	/*                                           GNOME_STOCK_PIXMAP_VOLUME); */
	/*            gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), ADV10, TRUE); */
	/*  gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), ADV10, */
	/*                                advance10s_cb, a); */

	gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);
	gtk_widget_show(a->dialog);
	return 0;
      cleanup:
	cleanup(a);
	return -1;
}


static void reset_cb(GtkWidget * widget, struct button_s *r)
{
	filterparam_set_double(r->param, r->value);
}

static void solo_cb(GtkWidget * widget, struct button_s *r)
{
	int i;
	double valsolo;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(r->solo_button))
	    == TRUE) {
		valsolo = filterparam_val_double(r->param);
		for (i = 0; i < solos_count; i++) {
			before_solo[i] = filterparam_val_double(param_solo[i]);	/* keep current value */
			filterparam_set_double(param_solo[i], -100);
			gtk_widget_set_sensitive(GTK_WIDGET
						 (solo_button[i]), FALSE);
			gtk_widget_set_sensitive(GTK_WIDGET
						 (mute_button[i]), FALSE);
		}
		filterparam_set_double(r->param, valsolo);
		gtk_widget_set_sensitive(GTK_WIDGET(r->solo_button), TRUE);
	}

	else {
		for (i = 0; i < solos_count; i++) {
			filterparam_set_double(param_solo[i],
					       before_solo[i]);
			gtk_widget_set_sensitive(GTK_WIDGET
						 (solo_button[i]), TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET
						 (mute_button[i]), TRUE);
		}
	}
}

static void mute_cb(GtkWidget * widget, struct button_s *r)
{
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(r->mute_button))
	    == TRUE) {
		r->before_mute = filterparam_val_double(r->param);	/* keep current value */
		filterparam_set_double(r->param, -100);
	} else {
		filterparam_set_double(r->param, r->before_mute);
	}
}

char *mixer_knob_formatter(gfloat lower, gfloat val, gpointer data)
{
	char buf[1024];

	if (lower > 300) {	/*freq mid/high: display as x/1000K */
		snprintf(buf, 1023,
			 "(lambda (x) (string-append (number-&gt;string (/ (round (* x 1)) 1000)) \"K\"))");
	} else if ((int) (lower * 10) == (int) ((-M_PI) * 10)) {
		snprintf(buf, 1023, " (lambda (x)\n" 
                          "(if (= (round (* 10 x)) \n"
                           "( round (* 10 1.570))) \"RIGHT\" \n"
                           "(if (= (round (* 10 x)) \n"
                           "( round (* 10 -1.570))) \"LEFT\" \n"
                           "(if (= (round(* 10 x)) \n"
                           " 0 )\"CENTER\" \n"
                           "(if (= (round(* 10 x)) \n"
                           " 1 )\"CENTER\"       \n"
                           "(number-&gt;string (/(round (* x 10))10)) )))))");
	}
	/* it seems we need to extend the centered area between 0 and 0.1 */
	else
		snprintf(buf, 1023,
			 " (lambda (x) (number-&gt;string (/(round (* x 10))10)))");
	return g_strdup(buf);
}

GtkWidget *glame_param_slider_new(filter_param_t * param,
				  char *label_short,
				  gfloat value,
				  gfloat lower,
				  gfloat upper,
				  gfloat step_increment,
				  gfloat page_increment, gfloat page_size)
{
	char xml[1512], formatted[1024];
	GtkWidget *hbox, *vbox, *label, *widget, *button, *toggle_button;

	r[buttons_count] =
	    (struct button_s *) malloc(sizeof(struct button_s));
	if (r[buttons_count] == NULL) {
		DPRINTF("alloc error");
		return NULL;
	}

	snprintf(formatted, 1023,
		 mixer_knob_formatter(lower, value, NULL));
	snprintf(xml, 1511,
"<?xml version=\"1.0\" standalone=\"no\"?>"
"<!DOCTYPE glade-interface SYSTEM \"http://glade.gnome.org/glade-2.0.dtd\">"
"<glade-interface>" 
		 "<widget class=\"GtkKnob\" id=\"widget\">" 
		 "<property name=\"can_focus\">True</property>"
		 "<property name=\"draw_value\">True</property>"
		 "<property name=\"value_pos\">GTK_POS_TOP</property>"
		 "<property name=\"digits\">1</property>"
		 "<property name=\"policy\">GTK_UPDATE_CONTINUOUS</property>"
		 "<property name=\"value\">%.3f</property>" 
		 "<property name=\"lower\">%.3f</property>"
		 "<property name=\"upper\">%.3f</property>" 
		 "<property name=\"step\">%.3f</property>"
		 "<property name=\"page\">%.3f</property>" 
		 "<property name=\"page_size\">%.3f</property>"
		 "<property name=\"formatter\">%s</property>" 
		 "<property name=\"tick\">%.3f</property>"
		 "</widget>" 
"</glade-interface>", 
		 value, lower, upper,
		 step_increment, page_increment, page_size, formatted,
		 value);
	filterparam_set_property(param, FILTERPARAM_GLADEXML, strdup(xml));
	hbox = gtk_hbox_new(FALSE, 0);
	vbox = gtk_vbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), GTK_WIDGET(vbox), FALSE, FALSE,
			   0);
	label = gtk_label_new(label_short);
	gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
	widget = glame_param_new_without_label(param);
	gtk_box_pack_start(GTK_BOX(hbox), widget, FALSE, FALSE, 0);
	/*      reset button */
	button = gtk_button_new_with_label("R");
	r[buttons_count]->param = param;
	r[buttons_count]->value = value;
	gtk_signal_connect(GTK_OBJECT(button), "clicked",
			   GTK_SIGNAL_FUNC(reset_cb), r[buttons_count]);
	gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);

	if (lower == -80) {	/* a volume_adjust param so display solo and mute buttons */
		/* solo button */
		toggle_button = gtk_toggle_button_new_with_label("S");
		r[buttons_count]->solo_button = toggle_button;
		param_solo[solos_count] = param;
		solo_button[solos_count] = toggle_button;
		gtk_signal_connect(GTK_OBJECT(toggle_button), "clicked",
				   GTK_SIGNAL_FUNC(solo_cb),
				   r[buttons_count]);
		gtk_box_pack_start(GTK_BOX(vbox), toggle_button, FALSE,
				   FALSE, 0);


		/* mute button */
		toggle_button = gtk_toggle_button_new_with_label("M");
		gtk_signal_connect(GTK_OBJECT(toggle_button), "clicked",
				   GTK_SIGNAL_FUNC(mute_cb),
				   r[buttons_count]);
		gtk_box_pack_start(GTK_BOX(vbox), toggle_button, FALSE,
				   FALSE, 0);
		r[buttons_count]->mute_button = toggle_button;
		mute_button[solos_count] = toggle_button;
		solos_count++;
	}
	buttons_count++;
	return hbox;
}


int mixer_register(plugin_t * p)
{
	plugin_set(p, PLUGIN_GPSMOP, mixer_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION,
		   "mix selected items with volume adjusts");
	plugin_set(p, PLUGIN_CATEGORY, "Volume");
	return 0;
}
