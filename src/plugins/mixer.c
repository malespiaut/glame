/*
 * mixer.c
 * $Id: mixer.c,v 1.3 2002/02/11 12:44:46 richi Exp $
 *
 * Copyright (C) 2002 Laurent Georget
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

PLUGIN(mixer)

struct apply_data_s {
	filter_t *net;
	filter_t *pos;
	gpsm_item_t *item_a;
	GtkWidget *dialog;
	GtkWidget *mixer_hbox;
	GtkWidget *counter;
	GtkWidget *progress;
	gpsm_item_t *grp;
	int stereo;
	int previewing;
	guint timeout_id;
	long start, length;
	int sr;
	div_t tot_time;
};


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
static void advance10s_cb(GtkWidget * bla, struct apply_data_s *a);
GtkWidget *glame_param_slider_new(filter_param_t * param,char *label_short, gfloat value,
				  gfloat lower, gfloat upper, gfloat step_increment,
				  gfloat page_increment, gfloat page_size);

static void cleanup(struct apply_data_s *a)
{
	DPRINTF("cleanup\n");
	if (a->timeout_id != -1)
		gtk_timeout_remove(a->timeout_id);
	gtk_widget_hide(a->dialog);
	gtk_widget_destroy(a->dialog);
	if (a->net)
		filter_delete(a->net);
	gpsm_item_destroy((gpsm_item_t *) a->grp);
	free(a);
}

static gint poll_net_cb(struct apply_data_s *a)
{
	filter_param_t *posparam;
	char labelcount[24];
	int pos;
	div_t time;
	/* int sr; */

	if (filter_is_ready(a->net)) {
		gtk_timeout_remove(a->timeout_id);
		a->timeout_id = -1;
		if (a->previewing)
			preview_stop(a);
		/* else if (a->applying) { */
		/*                  gpsm_item_t *swfile; */
		/*                  filter_wait(a->net); */
		/*                  gpsm_grp_foreach_item(a->item, swfile) */
		/*                          gpsm_notify_swapfile_change(gpsm_swfile_filename(swfile), a->start, a->length); */
		/*                  cleanup(a); */
		/*          } */
		return FALSE;
	}
	/* update progressbar */
	posparam =
	    filterparamdb_get_param(filter_paramdb(a->pos),
				    FILTERPARAM_LABEL_POS);
	gtk_progress_bar_update(GTK_PROGRESS_BAR(a->progress),
				MIN(1.0,
				    (float) filterparam_val_pos(posparam) /
				    (float) (a->length)));


	pos = filterparam_val_pos(posparam);
	/* sr=filterpipe_sample_rate(filterport_get_pipe(filterportdb_get_port(filter_portdb(a->pos),PORTNAME_IN))); */
	time = div((pos / a->sr), 60);
	snprintf(labelcount, 24, "%i mn %i sec / %i mn %i s", time.quot,
		 time.rem, (a->tot_time).quot, (a->tot_time).rem);
	gtk_label_set_text((a->counter), labelcount);
	return TRUE;

}

static void preview_start(struct apply_data_s *a)
{
	filter_t *f;

	if (filter_is_ready(a->net) == 0) {
		DPRINTF("filter has not finished\n\n");
		return;
	}

	/*check current volumes values : */
	filter_foreach_node(a->net, f) {
		const char *test;
		filter_param_t *param;



		test = filter_name(f);
		DPRINTF("filter name=%s\n", test);
		param =
		    filterparamdb_get_param(filter_paramdb(f), "dbgain");
		if (!param) {
			DPRINTF("not a volume_adjust filter: skipping.\n");
		} else {
			float fl;
			const char *test2;
			test2 = filterparam_to_string(param);
			DPRINTF("property value=%s\n", test);
			fl = filterparam_val_float(param);
			DPRINTF("param val=%f\n", fl);
		}
	}


	if ((filter_launch(a->net, _GLAME_WBUFSIZE) == -1) ||
	    (filter_start(a->net) == -1))
		goto cleanup;
	a->timeout_id = gtk_timeout_add(100, (GtkFunction) poll_net_cb, a);
	a->previewing = 1;

	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, FALSE);
	return;

      cleanup:
	cleanup(a);
}

static void preview_stop(struct apply_data_s *a)
{
	filter_terminate(a->net);
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
	gpsm_swfile_t *result_left, *result_right;
	gpsm_grp_t *grp;
	filter_t *swap_out_left, *swap_out_right, *render, *net_out;
	filter_port_t *out, *in_l, *in_r;
	filter_pipe_t *pipe;
	float pos;
	long swname;
	char *label;

	if (filter_is_ready(a->net) == 0) {
		DPRINTF("filter has not finished\n\n");
		return;
	}

	net_out = filter_get_node(a->net, "audio-out");
	if (!net_out) {
		DPRINTF("no net_out");
		goto cleanup;
	}
	filter_delete(net_out);

	/* mono */
	if (a->stereo == 0) {
		result_left = gpsm_newswfile("mixed ");
		render = filter_get_node(a->net, "render");
		if (!render) {
			DPRINTF("no net_out");
			goto cleanup;
		}

		if (!(swap_out_left =filter_instantiate(plugin_get("swapfile-out")))) {
			DPRINTF("error swapout left create\n\n");
			goto cleanup;
		}
		swname = gpsm_swfile_filename(result_left);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap_out_left),"filename"), &swname);
		if ((filter_add_node(a->net, swap_out_left, "swap_out_left")) == -1) {
			DPRINTF("error adding node swap_out_left\n\n");
			goto cleanup;
		}
		in_l =
filterportdb_get_port(filter_portdb(swap_out_left),PORTNAME_IN);
		if (!
		    (out =
		     filterportdb_get_port(filter_portdb(render),PORTNAME_OUT))) {
			DPRINTF("error getting port l\n\n");
			goto cleanup;
		}

		if (!(pipe = filterport_connect(out, in_l))) {
			DPRINTF("error getting pipe\n\n");
			goto cleanup;
		}

		if ((filter_launch(a->net, _GLAME_WBUFSIZE) == -1) ||
		    (filter_start(a->net) == -1))
			goto cleanup;
		if (filter_wait(a->net) == -1)
			goto cleanup;
	}

	/*stereo */
	/*FIXME: see if one pass for left and right is possible */
	else {
		result_left = gpsm_newswfile("mixed left");
		gpsm_swfile_set_position(result_left, FILTER_PIPEPOS_LEFT);
		result_right = gpsm_newswfile("mixed right");
		gpsm_swfile_set_position(result_right, FILTER_PIPEPOS_RIGHT);

		render = filter_get_node(a->net, "render");
		if (!render) {
			DPRINTF("no net_out");
			goto cleanup;
		}

		if (!(swap_out_left =filter_instantiate(plugin_get("swapfile-out")))) {
			DPRINTF("error swapout left create\n\n");
			goto cleanup;
		}
		if (!(swap_out_right = filter_instantiate(plugin_get("swapfile-out")))) {
			DPRINTF("error swapout right create\n\n");
			goto cleanup;
		}

		swname = gpsm_swfile_filename(result_left);
filterparam_set(filterparamdb_get_param(filter_paramdb(swap_out_left),"filename"), &swname);
		if ((filter_add_node(a->net, swap_out_left, "swap_out_left")) == -1) {
			DPRINTF("error adding node swap_out_left\n\n");
			goto cleanup;
		}
		swname = gpsm_swfile_filename(result_right);
		filterparam_set(filterparamdb_get_param(filter_paramdb(swap_out_right),"filename"), &swname);

		/*for left */
		in_l =filterportdb_get_port(filter_portdb(swap_out_left),PORTNAME_IN);
		if (!(out =   filterportdb_get_port(filter_portdb(render),PORTNAME_OUT))) {
			DPRINTF("error getting port l\n\n");
			goto cleanup;
		}

		if (!(pipe = filterport_connect(out, in_l))) {
			DPRINTF("error getting pipe\n\n");
			goto cleanup;
		}
		pos = FILTER_PIPEPOS_LEFT;
		filterparam_set(filterparamdb_get_param(filterpipe_sourceparamdb(pipe),"position"), &pos);

		if ((filter_launch(a->net, GLAME_BULK_BUFSIZE) == -1) ||(filter_start(a->net) == -1))
			goto cleanup;
		if (filter_wait(a->net) == -1)
			goto cleanup;

		/* one more time for right */
		filter_delete(swap_out_left);
		if ((filter_add_node
		     (a->net, swap_out_right, "swap_out_right")) == -1) {
			DPRINTF("error adding node swap_out_right\n\n");
			goto cleanup;
		}
		in_r =filterportdb_get_port(filter_portdb(swap_out_right),PORTNAME_IN);
		if (!(pipe = filterport_connect(out, in_r))) {
			DPRINTF("error connecting port \n\n");
			goto cleanup;
		}
		pos = FILTER_PIPEPOS_RIGHT;
		filterparam_set(filterparamdb_get_param	(filterpipe_sourceparamdb(pipe), "position"), &pos);
		if ((filter_launch(a->net, GLAME_BULK_BUFSIZE) == -1)|| (filter_start(a->net) == -1))
			goto cleanup;
		if (filter_wait(a->net) == -1)
			goto cleanup;
	}

	/* insert  track(s) in a new group */
	label = alloca(128);
	snprintf(label, 128, "MIXED: %s", gpsm_item_label(a->item_a));
	grp = gpsm_newgrp("mix");
	gpsm_item_set_label((gpsm_item_t *) grp, label);
	gpsm_vbox_insert(grp, (gpsm_item_t *) result_left, 0, 0);
	if (a->stereo == 1)
		gpsm_vbox_insert(grp, (gpsm_item_t *) result_right, 0, 1);
	gpsm_vbox_insert(gpsm_item_parent(a->item_a), (gpsm_item_t *) grp,
			 gpsm_item_hposition(a->item_a),
			 gpsm_item_vposition(a->item_a));
	gpsm_invalidate_swapfile(gpsm_swfile_filename(result_left));
	if (a->stereo == 1)
		gpsm_invalidate_swapfile(gpsm_swfile_filename
					 (result_right));

      cleanup:
	cleanup(a);
}

static void close_cb(GtkWidget * bla, struct apply_data_s *a)
{
	gtk_timeout_remove(a->timeout_id);
	if (a->net) {
		filter_terminate(a->net);
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


static int mixer_gpsm(gpsm_item_t * obj, long start, long length)
{

	GtkWidget *label;
	struct apply_data_s *a;
	gpsm_item_t *item;
	plugin_t *test;
	char *text_obj, text[48], labelcount[24];
	int ladspa_present;

	a = (struct apply_data_s *) malloc(sizeof(struct apply_data_s));
	a->net = NULL;
	a->stereo = 0;		/*default to mono mix */
	a->previewing = 0;
	a->timeout_id = -1;
	a->length = length;


	/* Check if triplePara LADSPA plugin is available */
	test = plugin_get("triplePara");
	if (!test) {
		GtkWidget *dialog;
		int ret;

		dialog =
		    gnome_message_box_new
		    ("TriplePara LADSPA plugin not found, you can get it at http://plugin.org.uk Equalization will be disabled.",
		     GNOME_MESSAGE_BOX_INFO, " OK ", NULL);
		gtk_window_set_position(GTK_WINDOW(dialog),
					GTK_WIN_POS_CENTER);
		ret = gnome_dialog_run(GNOME_DIALOG(dialog));
	}

	a->dialog = gtk_type_new(gnome_dialog_get_type());
	gnome_dialog_close_hides(GNOME_DIALOG(a->dialog), FALSE);
	gnome_dialog_set_close(GNOME_DIALOG(a->dialog), FALSE);
	gtk_window_set_position(GTK_WINDOW(a->dialog), GTK_WIN_POS_CENTER);
	gtk_window_set_policy(GTK_WINDOW(a->dialog), TRUE, TRUE, FALSE);
	gnome_dialog_append_button_with_pixmap(GNOME_DIALOG (a->dialog),_("Preview"),GNOME_STOCK_PIXMAP_VOLUME);
	gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(a->dialog),_("Apply"),GNOME_STOCK_PIXMAP_FORWARD);
	gnome_dialog_append_button(GNOME_DIALOG(a->dialog),GNOME_STOCK_BUTTON_CANCEL);
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
        /*gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);*/	
	gtk_widget_show(a->dialog);
	/* Mixer title */
        text_obj = gpsm_item_label(obj);
	snprintf(text, 127, "Mixer: %s", text_obj);
	label = gtk_label_new(text);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox),label, FALSE, FALSE, 3);
	gtk_widget_show(label);
	/* 1 hbox for all chanels  */
	a->mixer_hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX
			   (GNOME_DIALOG(a->dialog)->vbox),GTK_WIDGET(a->mixer_hbox), FALSE, FALSE, 0);
	gtk_widget_show(a->mixer_hbox);
	a->grp = (gpsm_item_t *) gpsm_collect_swfiles(obj);
	if (!a->grp)
		goto cleanup;
	a->item_a = obj;
	if (!(a->net = filter_creat(NULL))) {
		DPRINTF("error creating network");
		goto cleanup;
	}


	gpsm_grp_foreach_item(a->grp, item) {
		filter_param_t *param;
		filter_t *swap_in, *volmix, *ladspa1;
		GtkWidget *glame_param, *glame_param_vol;
		gchar *text;
		GtkWidget *chanel_vbox, *chanel_frame, *eq_low_frame,
		    *eq_low_hbox, *eq_mid_frame, *eq_mid_hbox,
		    *eq_hi_frame, *eq_hi_hbox;
		float phi,gain
;
		if (gpsm_item_hposition(item) != 0)
			goto cleanup;	/* we dont want to handle non-aligned
					   tracks */
		text = gpsm_item_label(item);
		DPRINTF("Adding :%s \n", text);
		/* 1 frame for each  chanel ,a vbox in it */
		chanel_frame = gtk_frame_new(text);
		gtk_widget_ref(chanel_frame);
		gtk_object_set_data_full(GTK_OBJECT(a->mixer_hbox),"chanel_frame", chanel_frame,
					 (GtkDestroyNotify)gtk_widget_unref);
		gtk_widget_show(chanel_frame);
		gtk_box_pack_start(GTK_BOX(a->mixer_hbox),
				   GTK_WIDGET(chanel_frame), FALSE,
				   FALSE, 0);
		chanel_vbox = gtk_vbox_new(FALSE, 0);
		gtk_container_add(GTK_CONTAINER(chanel_frame),chanel_vbox);
		gtk_container_set_border_width(GTK_CONTAINER(chanel_vbox), 3);
		gtk_widget_show(chanel_vbox);
		/* net begin */
		swap_in = net_add_gpsm_input(a->net,(gpsm_swfile_t *) item,0, -1, 0);
		if (!swap_in) {
			DPRINTF("error getting swap_in");
			goto cleanup;
		}
		phi = filterparam_val_float(filterparamdb_get_param(filter_paramdb(swap_in), "position"));
		DPRINTF("phi=%f\n", phi);
		if (phi != 0)
			a->stereo = 1;	/*non centered track : stereo mix */

		/* triplePara */
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

		ladspa1 = net_add_plugin_by_name(a->net, "triplePara");
		if (!ladspa1) {
			DPRINTF(" error adding ladspa1\n");
			ladspa_present = 0;
		}

		if (ladspa1) {
			ladspa_present = 1;
			if (!filterport_connect(filterportdb_get_port(filter_portdb(swap_in), PORTNAME_OUT),
			     filterportdb_get_port(filter_portdb(ladspa1),"Input"))) {
				DPRINTF(" error connecting ladspa1\n");
				goto cleanup;
			}
                        /*set the triplePra gains to 0 db and the 3 frequences as desired */                        
                        param =filterparamdb_get_param(filter_paramdb(ladspa1),"Low-shelving gain (dB)");
			if (!param) {DPRINTF("Unable to get param");
                        goto cleanup;}
                        gain=-20;
                        filterparam_set(param, &gain);
                        param =filterparamdb_get_param(filter_paramdb(ladspa1),"High-shelving gain (dB)");
			if (!param) {DPRINTF("Unable to get param");
                        goto cleanup;}
                        gain=0;
                        filterparam_set(param, &gain);
                        param =filterparamdb_get_param(filter_paramdb(ladspa1),"Band 1 gain (dB)");
			if (!param) {DPRINTF("Unable to get param");
                        goto cleanup;}
                        gain=0;
                        filterparam_set(param, &gain);
                        param =filterparamdb_get_param(filter_paramdb(ladspa1),"Band 1 frequency (Hz)");
			if (!param) {DPRINTF("Unable to get param");
                        goto cleanup;}
                        gain=100;
                        filterparam_set(param, &gain);
                        param =filterparamdb_get_param(filter_paramdb(ladspa1),"Band 2 gain (dB)");
			if (!param) {DPRINTF("Unable to get param");
                        goto cleanup;}
                        gain=0;
                        filterparam_set(param, &gain);
                        param =filterparamdb_get_param(filter_paramdb(ladspa1),"Band 2 frequency (Hz)");
			if (!param) {DPRINTF("Unable to get param");
                        goto cleanup;}
                        gain=1000;
                        filterparam_set(param, &gain);
                        param =filterparamdb_get_param(filter_paramdb(ladspa1),"Band 3 gain (dB)");
			if (!param) {DPRINTF("Unable to get param");
                        goto cleanup;}
                        gain=0;
                        filterparam_set(param, &gain);
                        param =filterparamdb_get_param(filter_paramdb(ladspa1),"Band 3 frequency (Hz)");
			if (!param) {DPRINTF("Unable to get param");
                        goto cleanup;}
                        gain=8000;
                        filterparam_set(param, &gain);
			/* 1 frame for eq_low , eq_low_hbox in it */
			eq_low_frame = gtk_frame_new("Eq low");
			gtk_widget_ref(chanel_frame);
			gtk_object_set_data_full(GTK_OBJECT(chanel_vbox),"eq_low_frame",eq_low_frame,
						 (GtkDestroyNotify)gtk_widget_unref);
			gtk_widget_show(eq_low_frame);
			gtk_box_pack_start(GTK_BOX(chanel_vbox), GTK_WIDGET(eq_low_frame), FALSE,
					   FALSE, 0);
			eq_low_hbox = gtk_hbox_new(FALSE, 0);
			gtk_container_add(GTK_CONTAINER(eq_low_frame),eq_low_hbox);
			/* gtk_container_set_border_width (GTK_CONTAINER (chanel_vbox), 3); */
			gtk_widget_show(eq_low_hbox);
			/* 1 frame for eq_mid , eq_mid_hbox in it */
			eq_mid_frame = gtk_frame_new("Eq mid");
			gtk_widget_ref(chanel_frame);
			gtk_object_set_data_full(GTK_OBJECT(chanel_vbox),"eq_mid_frame",eq_mid_frame,
						 (GtkDestroyNotify)gtk_widget_unref);
			gtk_widget_show(eq_mid_frame);
			gtk_box_pack_start(GTK_BOX(chanel_vbox),GTK_WIDGET(eq_mid_frame), FALSE,
					   FALSE, 0);
			eq_mid_hbox = gtk_hbox_new(FALSE, 0);
			gtk_container_add(GTK_CONTAINER(eq_mid_frame), eq_mid_hbox);
			/*                gtk_container_set_border_width (GTK_CONTAINER (chanel_vbox), 3); */
			gtk_widget_show(eq_mid_hbox);
			/* 1 frame for eq_hi , eq_hi_hbox in it */
			eq_hi_frame = gtk_frame_new("Eq hi");
			gtk_widget_ref(chanel_frame);
                        gtk_object_set_data_full(GTK_OBJECT(chanel_vbox), "eq_hi_frame",eq_hi_frame,
						 (GtkDestroyNotify) gtk_widget_unref);
			gtk_widget_show(eq_hi_frame);
			gtk_box_pack_start(GTK_BOX(chanel_vbox),GTK_WIDGET(eq_hi_frame), FALSE,
					   FALSE, 0);
			eq_hi_hbox = gtk_hbox_new(FALSE, 0);
			gtk_container_add(GTK_CONTAINER(eq_hi_frame), eq_hi_hbox);
			/*                gtk_container_set_border_width (GTK_CONTAINER (chanel_vbox), 3); */
			gtk_widget_show(eq_hi_hbox);
			/* eq1 low controls */
			param =
			    filterparamdb_get_param(filter_paramdb (ladspa1),"Band 1 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =
			    glame_param_slider_new(param, "Hz", 100, 50,500, 1, 100, 100);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_low_hbox),
					  glame_param);
			param =
			    filterparamdb_get_param(filter_paramdb(ladspa1),"Band 1 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			glame_param =glame_param_slider_new(param, "dB", 0, -20, 20,1, 1, 1);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_low_hbox),glame_param);
			/* eq2 mid controls */
			param =
			    filterparamdb_get_param(filter_paramdb(ladspa1),"Band 2 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =glame_param_slider_new(param, "Hz", 1000, 400,4000, 1, 100, 100);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_mid_hbox),glame_param);
			param =
			    filterparamdb_get_param(filter_paramdb(ladspa1),"Band 2 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			glame_param = glame_param_slider_new(param, "dB", 0, -20, 20,1, 1, 1);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
                        gtk_container_add(GTK_CONTAINER(eq_mid_hbox), glame_param);
			/* eq3 hi controls */
			param =filterparamdb_get_param(filter_paramdb(ladspa1),"Band 3 frequency (Hz)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}

			glame_param =glame_param_slider_new(param, "Hz", 8000,3900,15000, 1, 100, 100);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_hi_hbox),
					  glame_param);
			param =filterparamdb_get_param(filter_paramdb(ladspa1),"Band 3 gain (dB)");
			if (!param) {
				DPRINTF("Unable to get param");
				goto cleanup;
			}
			glame_param =glame_param_slider_new(param, "dB", 0, -20,20, 1, 1, 1);
			if (!glame_param) {
				DPRINTF("Unable to get param widget");
				goto cleanup;
			}
			gtk_widget_show_all(glame_param);
			gtk_container_add(GTK_CONTAINER(eq_hi_hbox),
					  glame_param);
		}

		/* volume */
		if (!
		    (volmix =
		     filter_instantiate((plugin_get("volume_adjust"))))) {
			DPRINTF("error getting volume_adjust");
			goto cleanup;
		}
		filter_add_node(a->net, volmix, "mix_volume");
		if (ladspa1) {
			if (!filterport_connect(filterportdb_get_port(filter_portdb(ladspa1), "Output"),
			     filterportdb_get_port(filter_portdb(volmix),
						   PORTNAME_IN))) {
				DPRINTF
				    (" error connecting ladspa1 to volmix\n");
				goto cleanup;
			}
		} else {
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
		    filterparamdb_get_param(filter_paramdb(volmix),"dbgain");
		if (!param) {
			DPRINTF("Unable to get volmix param");
			goto cleanup;
		}
		glame_param_vol = glame_param_slider_new(param, "Lvl dB", 0, -100, 20, 1, 4,1);
		if (!glame_param_vol) {
			DPRINTF("Unable to get volmix param widget");
			goto cleanup;
		}
		gtk_widget_show_all(glame_param_vol);
		gtk_container_add(GTK_CONTAINER(chanel_vbox),glame_param_vol);
		/* gtk_box_pack_start(GTK_BOX(chanel_frame), */
		/*                             GTK_WIDGET(glame_param_vol), FALSE, */
		/*                             FALSE, 0); */
	}			/*end of loop */

        /* glame_filtereditgui_new(a->net, TRUE); */
	a->progress = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(a->progress),GTK_PROGRESS_LEFT_TO_RIGHT);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), a->progress, FALSE, FALSE, 3);
	gtk_widget_show(a->progress);
	if (!(a->pos = net_apply_audio_out(a->net))) {
		DPRINTF("error applying audio_out");
		goto cleanup;
	}

	/* time counter */
	a->length = length;
	a->sr =
	    filterpipe_sample_rate(filterport_get_pipe(filterportdb_get_port(filter_portdb(a->pos), PORTNAME_IN)));
	a->tot_time = div((a->length / a->sr), 60);
	snprintf(labelcount, 24, "%i mn %i s/ %i mn %i s", 0,
		 0, (a->tot_time).quot, (a->tot_time).rem);
	a->counter = gtk_label_new(labelcount);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox),a->counter, FALSE, FALSE, 3);
	gtk_widget_show(a->counter);
	/* gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(a->dialog), */
	/*                                           _("Preview"), */
	/*                                           GNOME_STOCK_PIXMAP_VOLUME); */
	/*            gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), ADV10, TRUE); */
	/*  gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), ADV10, */
	/*                                advance10s_cb, a); */
	return 0;
      cleanup:
	cleanup(a);
	return -1;
}

static void advance10s_cb(GtkWidget * bla, struct apply_data_s *a)
{

}



GtkWidget *glame_param_slider_new(filter_param_t *param,
				  char *label_short,
				  gfloat value,
				  gfloat lower,
				  gfloat upper,
				  gfloat step_increment,
				  gfloat page_increment, gfloat page_size)
{
	char xml[1024];

	snprintf(xml, 1023,
"<?xml version=\"1.0\"?><GTK-Interface>"
"  <widget>"
"    <class>GtkHScale</class>"
"    <name>widget</name>"
"    <can_focus>True</can_focus>"
"    <draw_value>True</draw_value>"
"    <value_pos>GTK_POS_LEFT</value_pos>"
"    <digits>3</digits>"
"    <policy>GTK_UPDATE_CONTINUOUS</policy>"
"    <value>%.3f</value>"
"    <lower>%.3f</lower>"
"    <upper>%.3f</upper>"
"    <step>%.3f</step>"
"    <page>%.3f</page>"
"    <page_size>%.3f</page_size>"
"  </widget>"
"</GTK-Interface>",
		 value, lower, upper, step_increment,
		 page_increment, page_size);
	filterparam_set_property(param, FILTERPARAM_GLADEXML, strdup(xml));

	return glame_param_new(param);
}

int mixer_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_GPSMOP, mixer_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION,
		   "mix selected items with volume adjusts");
	plugin_set(p, PLUGIN_CATEGORY, "Volume");
	return 0;
}
