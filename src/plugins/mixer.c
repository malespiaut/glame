/*
 * mixer.c
 * $Id: mixer.c,v 1.1 2002/01/07 23:09:17 richi Exp $
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
	gpsm_item_t *item_a;
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *vbox2;
	gpsm_item_t *grp;
};



static void preview_cb(GtkWidget * bla, struct apply_data_s *a);
static void preview_stop(GtkWidget * bla, struct apply_data_s *a);
static void apply_cb(GtkWidget * bla, struct apply_data_s *a);
static void close_cb(GtkWidget * bla, struct apply_data_s *a);

static int mixer_gpsm(gpsm_item_t * obj, long start, long length)
{
	GtkWidget *hbox2;
	GtkWidget *button;
	int err;
	struct apply_data_s *a;
	filter_t *audio_out;
	gpsm_item_t *item;


	a = (struct apply_data_s *) malloc(sizeof(struct apply_data_s));
	a->net = NULL;

/* Gtk Stuff */


	a->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_modal(GTK_WINDOW(a->window), FALSE);

	a->vbox = gtk_vbox_new(FALSE, 1);
	hbox2 = gtk_hbox_new(FALSE, 1);

	gtk_container_add(GTK_CONTAINER(a->window), a->vbox);
	gtk_container_add(GTK_CONTAINER(a->vbox), hbox2);

	gtk_widget_show(a->vbox);
	gtk_widget_show(hbox2);
	gtk_widget_show(a->window);




	button = gtk_button_new_with_label("preview start");
	gtk_signal_connect(GTK_OBJECT(button), "clicked",
			   GTK_SIGNAL_FUNC(preview_cb), a);
	gtk_box_pack_start(GTK_BOX(hbox2), button, TRUE, TRUE, 0);
	gtk_widget_show(button);

	button = gtk_button_new_with_label("preview stop");
	gtk_signal_connect(GTK_OBJECT(button), "clicked",
			   GTK_SIGNAL_FUNC(preview_stop), a);
	gtk_box_pack_start(GTK_BOX(hbox2), button, TRUE, TRUE, 0);
	gtk_widget_show(button);

	button = gtk_button_new_with_label("apply");
	gtk_signal_connect(GTK_OBJECT(button), "clicked",
			   GTK_SIGNAL_FUNC(apply_cb), a);
	gtk_box_pack_start(GTK_BOX(hbox2), button, TRUE, TRUE, 0);
	gtk_widget_show(button);

	button = gtk_button_new_with_label("cancel");
	gtk_signal_connect(GTK_OBJECT(button), "clicked",
			   GTK_SIGNAL_FUNC(close_cb), a);
	gtk_box_pack_start(GTK_BOX(hbox2), button, TRUE, TRUE, 0);


	gtk_widget_show(button);


	a->grp = (gpsm_item_t *) gpsm_collect_swfiles(obj);
	if (!a->grp)
		goto cleanup;

	a->item_a = obj;


	a->vbox2 = gtk_vbox_new(FALSE, 1);
	gtk_container_add(GTK_CONTAINER(a->vbox), a->vbox2);
	gtk_widget_show(a->vbox2);

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

		if (gpsm_item_hposition(item) != 0)
			goto cleanup;	/* we dont want to handle non-aligned
					   tracks */

		text = gpsm_item_label(item);
		DPRINTF("Adding :%s \n", text);
		separator = gtk_hseparator_new();
		gtk_box_pack_start(GTK_BOX(a->vbox2), separator, FALSE,
				   TRUE, 0);
		gtk_widget_show(separator);

		label = gtk_label_new(text);
		gtk_box_pack_start(GTK_BOX(a->vbox2), label, FALSE, FALSE,
				   0);
		gtk_widget_show(label);


		swap_in =
		    net_add_gpsm_input(a->net,
				       (gpsm_swfile_t *) item, 0, -1, 0);
		if (!swap_in) {
			DPRINTF("error getting swap_in");
			goto cleanup;
		}

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
		gtk_box_pack_start(GTK_BOX(a->vbox2),
				   GTK_WIDGET(glame_param_vol), FALSE,
				   TRUE, 0);

	}


	if (!(audio_out = net_apply_audio_out(a->net))) {
		DPRINTF("error applying audio_out");
		goto cleanup;
	}

/* glame_filtereditgui_new(a->net, FALSE);  */

	return 0;

      cleanup:
	puts("in cleanup \n");
	DPRINTF("err = %d\n", err);
	if (a->net)
		filter_delete(a->net);
	if (a->grp)
		gpsm_item_destroy(a->grp);
	gtk_widget_destroy(a->window);
	free(a);
	return err;
}


int mixer_register(plugin_t * p)
{
	plugin_set(p, PLUGIN_GPSMOP, mixer_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION,
		   "mix selected items with volume adjusts");
	plugin_set(p, PLUGIN_CATEGORY, "Volume");

	return 0;
}




static void preview_cb(GtkWidget * bla, struct apply_data_s *a)
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

	return;

      cleanup:
	DPRINTF("cleanup\n");
	if (a->net)
		filter_delete(a->net);
	if (a->grp)
		gpsm_item_destroy(a->grp);
	gtk_widget_destroy(a->window);
	free(a);
	return;
}



/* only make stereo mix (1 swfile for each track)*/
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

	result_left = gpsm_newswfile("mixed left");
	gpsm_swfile_set_position(result_left, FILTER_PIPEPOS_LEFT);
	result_right = gpsm_newswfile("mixed right");
	gpsm_swfile_set_position(result_right, FILTER_PIPEPOS_RIGHT);

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
			(filter_paramdb(swap_out_left), "filename"),
			&swname);
	if ((filter_add_node(a->net, swap_out_left, "swap_out_left")) ==
	    -1) {
		DPRINTF("error adding node swap_out_left\n\n");
		goto cleanup;
	}
	swname = gpsm_swfile_filename(result_right);
	filterparam_set(filterparamdb_get_param
			(filter_paramdb(swap_out_right), "filename"),
			&swname);

	/*for left */
	in_l =
	    filterportdb_get_port(filter_portdb(swap_out_left),
				  PORTNAME_IN);
	if (!
	    (out =
	     filterportdb_get_port(filter_portdb(render), PORTNAME_OUT))) {
		DPRINTF("error getting port l\n\n");
		goto cleanup;
	}

	if (!(pipe = filterport_connect(out, in_l))) {
		DPRINTF("error getting pipe\n\n");
		goto cleanup;
	}
	pos = FILTER_PIPEPOS_LEFT;
	filterparam_set(filterparamdb_get_param
			(filterpipe_sourceparamdb(pipe), "position"),
			&pos);

	if ((filter_launch(a->net, _GLAME_WBUFSIZE) == -1) ||
	    (filter_start(a->net) == -1))
		goto cleanup;
	if (filter_wait(a->net) == -1)
		goto cleanup;

	/* one more time for right */
	filter_delete(swap_out_left);
	if ((filter_add_node(a->net, swap_out_right, "swap_out_right")) ==
	    -1) {
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
			(filterpipe_sourceparamdb(pipe), "position"),
			&pos);
	if ((filter_launch(a->net, _GLAME_WBUFSIZE) == -1)
	    || (filter_start(a->net) == -1))
		goto cleanup;
	if (filter_wait(a->net) == -1)
		goto cleanup;

	/* insert both tracks in a new group */
	label = alloca(128);
	snprintf(label, 128, "MIXED: %s", gpsm_item_label(a->item_a));
	grp = gpsm_newgrp("mix");
	gpsm_item_set_label((gpsm_item_t*)grp, label);
	gpsm_vbox_insert(grp, (gpsm_item_t*)result_left, 0, 0);
	gpsm_vbox_insert(grp, (gpsm_item_t*)result_right, 0, 1);
	gpsm_vbox_insert(gpsm_item_parent(a->item_a),(gpsm_item_t*)grp,
			 gpsm_item_hposition(a->item_a),
			 gpsm_item_vposition(a->item_a));
	gpsm_invalidate_swapfile(gpsm_swfile_filename(result_left));
	gpsm_invalidate_swapfile(gpsm_swfile_filename(result_right));

      cleanup:
	puts("in cleanup \n");
	if (a->net)
		filter_delete(a->net);
	gtk_widget_destroy(a->window);
	free(a);
	return;
}

static void close_cb(GtkWidget * bla, struct apply_data_s *a)
{
	if (a->net) {
		filter_terminate(a->net);
		filter_delete(a->net);
	}
	if (a->grp)
		gpsm_item_destroy(a->grp);
	gtk_widget_destroy(a->window);
	free(a);
	return;
}


static void preview_stop(GtkWidget * bla, struct apply_data_s *a)
{
	filter_terminate(a->net);
	return;
}
