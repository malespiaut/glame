/*
 * mixer.c
 * $Id: mixer.c,v 1.2 2002/01/26 13:44:40 richi Exp $
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
	GtkLabel *counter;
	GtkWidget *progress;
	gpsm_item_t *grp;
	int stereo;
	int previewing;
	guint timeout_id;
	long start, length;
};


/* Button numbers. */
#define PREVIEW 0
#define APPLY 1
#define CANCEL 2
#define HELP 3

static void preview_cb(GtkWidget * bla, struct apply_data_s *a);
static void preview_start(struct apply_data_s *a);
static void preview_stop(struct apply_data_s *a);
static void apply_cb(GtkWidget * bla, struct apply_data_s *a);
static void close_cb(GtkWidget * bla, struct apply_data_s *a);

static void cleanup(struct apply_data_s *a)
{
  	DPRINTF("cleanup\n");
	if (a->timeout_id != -1)
		gtk_timeout_remove(a->timeout_id);
	gtk_widget_hide(a->dialog);
	gtk_widget_destroy(a->dialog);
	if (a->net)
		filter_delete(a->net);
	gpsm_item_destroy((gpsm_item_t *)a->grp);
	free(a);
}

static gint poll_net_cb(struct apply_data_s *a)
{
	filter_param_t *posparam;
	char labelcount[24];

	if (filter_is_ready(a->net)) {
		gtk_timeout_remove(a->timeout_id);
		a->timeout_id = -1;
		if (a->previewing)
			preview_stop(a);
		/* else if (a->applying) { */
/* 			gpsm_item_t *swfile; */
/* 			filter_wait(a->net); */
/* 			gpsm_grp_foreach_item(a->item, swfile) */
/* 				gpsm_notify_swapfile_change(gpsm_swfile_filename(swfile), a->start, a->length); */
/* 			cleanup(a); */
/* 		} */
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

		if (!
		    (swap_out_left =
		     filter_instantiate(plugin_get("swapfile-out")))) {
			DPRINTF("error swapout left create\n\n");
			goto cleanup;
		}
		swname = gpsm_swfile_filename(result_left);
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

		if ((filter_launch(a->net, _GLAME_WBUFSIZE) == -1) ||
		    (filter_start(a->net) == -1))
			goto cleanup;
		if (filter_wait(a->net) == -1)
			goto cleanup;
	}

	/*stereo */
	else {
		result_left = gpsm_newswfile("mixed left");
		gpsm_swfile_set_position(result_left, FILTER_PIPEPOS_LEFT);
		result_right = gpsm_newswfile("mixed right");
		gpsm_swfile_set_position(result_right,
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

		swname = gpsm_swfile_filename(result_left);
		filterparam_set(filterparamdb_get_param
				(filter_paramdb(swap_out_left),
				 "filename"), &swname);
		if ((filter_add_node
		     (a->net, swap_out_left, "swap_out_left")) == -1) {
			DPRINTF("error adding node swap_out_left\n\n");
			goto cleanup;
		}
		swname = gpsm_swfile_filename(result_right);
		filterparam_set(filterparamdb_get_param
				(filter_paramdb(swap_out_right),
				 "filename"), &swname);

		/*for left */
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
		pos = FILTER_PIPEPOS_LEFT;
		filterparam_set(filterparamdb_get_param
				(filterpipe_sourceparamdb(pipe),
				 "position"), &pos);

		if ((filter_launch(a->net, _GLAME_WBUFSIZE) == -1) ||
		    (filter_start(a->net) == -1))
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
		in_r =
		    filterportdb_get_port(filter_portdb(swap_out_right),
					  PORTNAME_IN);
		if (!(pipe = filterport_connect(out, in_r))) {
			DPRINTF("error connecting port \n\n");
			goto cleanup;
		}
		pos = FILTER_PIPEPOS_RIGHT;
		filterparam_set(filterparamdb_get_param
				(filterpipe_sourceparamdb(pipe),
				 "position"), &pos);
		if ((filter_launch(a->net, _GLAME_WBUFSIZE) == -1)
		    || (filter_start(a->net) == -1))
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
	char *text_obj,text[48];

	a = (struct apply_data_s *) malloc(sizeof(struct apply_data_s));
	a->net = NULL;
	a->stereo = 0;		/*default to mono mix */
	a->previewing = 0;
	a->timeout_id = -1;
	a->length = length;
/* Gtk Stuff */


	a->dialog = gtk_type_new(gnome_dialog_get_type());
	gnome_dialog_close_hides(GNOME_DIALOG(a->dialog), FALSE);
	gnome_dialog_set_close(GNOME_DIALOG(a->dialog), FALSE);

	gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(a->dialog),
					       _("Preview"),
					       GNOME_STOCK_PIXMAP_VOLUME);
	gnome_dialog_append_button_with_pixmap(GNOME_DIALOG(a->dialog),
					       _("Apply"),
					       GNOME_STOCK_PIXMAP_FORWARD);
	gnome_dialog_append_button(GNOME_DIALOG(a->dialog),
				   GNOME_STOCK_BUTTON_CANCEL);
	/* if (help) */
/* 		gnome_dialog_append_button( */
/* 			GNOME_DIALOG(a->dialog), GNOME_STOCK_BUTTON_HELP); */

	gnome_dialog_set_default(GNOME_DIALOG(a->dialog), APPLY);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), PREVIEW, TRUE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, TRUE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), CANCEL, TRUE);
	/* gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), HELP, TRUE); */

	gtk_signal_connect(GTK_OBJECT(a->dialog), "delete_event",
			   (GtkSignalFunc) delete_cb, a);
/* 	gtk_signal_connect(GTK_OBJECT(a->dialog), "destroy", */
/* 			   (GtkSignalFunc)close_cb, a); */

	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), PREVIEW,
				    preview_cb, a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), APPLY,
				    apply_cb, a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), CANCEL,
				    close_cb, a);
	gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);
	gtk_widget_show(a->dialog);
	
       
	text_obj=gpsm_item_label(obj);
	snprintf(text, 127, "Mixer: %s",text_obj); 
	label = gtk_label_new(text);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), label,
			   TRUE, TRUE, 3);
	gtk_widget_show(label);
	
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
		filter_t *swap_in, *volmix;
		GtkWidget *glame_param_vol;
		gchar *text;
		GtkWidget *label, *separator;
		float phi;

		if (gpsm_item_hposition(item) != 0)
			goto cleanup;	/* we dont want to handle non-aligned
					   tracks */

		text = gpsm_item_label(item);
		DPRINTF("Adding :%s \n", text);
		separator = gtk_hseparator_new();
		gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox),
				   separator, FALSE, TRUE, 0);
		gtk_widget_show(separator);

		label = gtk_label_new(text);
		gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox),
				   label, FALSE, FALSE, 0);
		gtk_widget_show(label);


		swap_in = net_add_gpsm_input(a->net,
					     (gpsm_swfile_t *) item, 0, -1,
					     0);
		if (!swap_in) {
			DPRINTF("error getting swap_in");
			goto cleanup;
		}
		phi =
		    filterparam_val_float(filterparamdb_get_param
					  (filter_paramdb(swap_in),
					   "position"));
		DPRINTF("phi=%f\n", phi);
		if (phi != 0)
			a->stereo = 1;	/*non centered track : stereo mix */
		if (!
		    (volmix =
		     filter_instantiate((plugin_get("volume_adjust"))))) {
			DPRINTF("error getting volume_adjust");
			goto cleanup;
		}
		filter_add_node(a->net, volmix, "mix_volume");

		if (!filterport_connect
		    (filterportdb_get_port
		     (filter_portdb(swap_in), PORTNAME_OUT),
		     filterportdb_get_port(filter_portdb(volmix),
					   PORTNAME_IN))) {
			DPRINTF(" error connecting swap_in to volmix\n");
			goto cleanup;
		}
		param = filterparamdb_get_param(filter_paramdb(volmix),
						"dbgain");
		if (!param) {
			DPRINTF("Unable to get volmix param");
			goto cleanup;
		}
		glame_param_vol = glame_param_new(param);
		if (!glame_param_vol) {
			DPRINTF("Unable to get volmix param widget");
			goto cleanup;
		}
		gtk_widget_show_all(glame_param_vol);
		gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox),
				   GTK_WIDGET(glame_param_vol), FALSE,
				   TRUE, 0);

	}/*end of loop*/

	a->progress = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(a->progress),
					 GTK_PROGRESS_LEFT_TO_RIGHT);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox),
			   a->progress, TRUE, TRUE, 3);
	gtk_widget_show(a->progress);

	if (!(a->pos = net_apply_audio_out(a->net))) {
		DPRINTF("error applying audio_out");
		goto cleanup;
	}
	return 0;

      cleanup:
	cleanup(a);
	return -1;
}


int mixer_register(plugin_t * p)
{
	plugin_set(p, PLUGIN_GPSMOP, mixer_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION,
		   "mix selected items with volume adjusts");
	plugin_set(p, PLUGIN_CATEGORY, "Volume");

	return 0;
}
