/*
 * resample.c
 * $Id: resample.c,v 1.1 2003/05/20 19:45:36 richi Exp $
 *
 * Copyright (C) 2003 Richard Guenther
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
#include "util/glame_gui_utils.h"
#include "network_utils.h"

PLUGIN(resample)

struct resample_s {
	GtkWidget *dialog, *adjustment;
	gpsm_item_t *grp;
	long samplerate;
};


#if 0
static void normalize_do_task(struct normalize_s *ns) {
	int num;
	filter_t *net, *vadjust, *swapi, *swapo;
	filter_param_t	*param;
	filter_launchcontext_t *context;
	char label[128];
	double gain, percentage;
	long done = 0;
	gpsm_item_t * item;

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


		if (!(context = filter_launch(net, GLAME_BULK_BUFSIZE))
		    || filter_start(context) == -1)
			goto fail_cleanup;

		param = filterparamdb_get_param(filter_paramdb(swapi), 
						FILTERPARAM_LABEL_POS);

		ns->running=1;
		while(!filter_is_ready(context)) {
			while (gtk_events_pending())
				gtk_main_iteration();
			usleep(40000);
			
			percentage = (float)(done+filterparam_val_long(param))/
				     (float)ns->total_size;

			if (ns->running==0)
				goto cancel_cleanup;

			gnome_appbar_set_progress_percentage(GNOME_APPBAR(ns->appbar),
						  	     percentage);
		}
		ns->running = 0;
		filter_launchcontext_unref(&context);
	
		done+=filterparam_val_long(param);
		DPRINTF("posparam=%ld\n", filterparam_val_long(param));

		filter_delete(net);
	}

	gpsm_grp_foreach_item(ns->grp, item) {
		gpsm_notify_swapfile_change(gpsm_swfile_filename(item), 
					    ns->start, MIN(ns->length, (gpsm_item_hsize(item)-ns->start+1)));
	}

	gtk_widget_destroy(ns->dialog);
	gpsm_item_destroy(ns->grp);
	cleanup_task_list(ns);
	free(ns);
	return;

 cancel_cleanup:
	filter_terminate(context);
	filter_delete(net);
	cleanup_task_list(ns);
	gpsm_op_undo_and_forget(ns->grp);
	gpsm_item_destroy(ns->grp);
	free(ns);
	return;
 fail_cleanup:
	filter_delete(net);
	DPRINTF("Error starting normalizing network!\n");
	return;
}
#endif


static void dialog_cb(GnomeDialog *dialog, gint button, struct resample_s *rs)
{
	if (button == 2) {
		glame_help_goto(NULL, "Resample");
		return;
	} else if (button == 0) {
		DPRINTF("FIXME\n");
	}
	gpsm_item_destroy(rs->grp);
	free(rs);
	gnome_dialog_close(dialog);
}

static void resample_dialog(struct resample_s *rs)
{
  GtkWidget *frame1;
  GtkWidget *hbox1;
  GtkWidget *label3;
  GtkWidget *spinbutton1;

  rs->dialog = gnome_dialog_new (NULL, NULL);

  frame1 = gtk_frame_new (_("Resample"));
  gtk_box_pack_start (GTK_BOX (GNOME_DIALOG(rs->dialog)->vbox),
		      frame1, TRUE, TRUE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (frame1), 3);

  hbox1 = gtk_hbox_new (FALSE, 6);
  gtk_container_add (GTK_CONTAINER (frame1), hbox1);
  gtk_container_set_border_width (GTK_CONTAINER (hbox1), 3);

  label3 = gtk_label_new (_("Samplerate:"));
  gtk_misc_set_alignment (GTK_MISC (label3), 0, 0.5);
  gtk_container_add (GTK_CONTAINER (hbox1), label3);

  rs->adjustment = gtk_adjustment_new (rs->samplerate,
				       2756, 192000, 100, 1000, 0.0);
  spinbutton1 = gtk_spin_button_new (GTK_ADJUSTMENT (rs->adjustment), 100000, 0);
  gtk_container_add (GTK_CONTAINER (hbox1), spinbutton1);

  gnome_dialog_append_button(GNOME_DIALOG(rs->dialog), GNOME_STOCK_BUTTON_OK);
  gnome_dialog_append_button(GNOME_DIALOG(rs->dialog), GNOME_STOCK_BUTTON_CANCEL);
  gnome_dialog_append_button(GNOME_DIALOG(rs->dialog), GNOME_STOCK_BUTTON_HELP);
  gtk_signal_connect(GTK_OBJECT(rs->dialog), "clicked",
		     dialog_cb, rs);
}


static int resample_gpsm(gpsm_item_t *grp, long start, long length)
{
	struct resample_s *rs;
	gpsm_item_t *item;

	rs = ALLOCN(1, struct resample_s);

	rs->grp = (gpsm_item_t*)gpsm_collect_swfiles(grp);
	if (rs->grp==NULL)
		return -1;

	rs->samplerate = gpsm_swfile_samplerate(gpsm_grp_first(rs->grp));
	gpsm_grp_foreach_item(rs->grp, item)
		if (gpsm_swfile_samplerate(item) != rs->samplerate) {
			gnome_dialog_run_and_close(gnome_error_dialog("Not all tracks have the same samplerate"));
			return -1;
		}

	resample_dialog(rs);
	gtk_widget_show_all(rs->dialog);

	return 0;
}

int resample_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_GPSMOP, resample_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION, "resample a gpsm subtree");
	plugin_set(p, PLUGIN_CATEGORY, "Frequency");
	
	return 0;
}
