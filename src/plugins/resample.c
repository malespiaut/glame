/*
 * resample.c
 * $Id: resample.c,v 1.6 2004/12/26 20:58:12 richi Exp $
 *
 * Copyright (C) 2003, 2004 Richard Guenther
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
	GtkWidget *dialog;
	GtkAdjustment *adjustment;
	gpsm_item_t *grp;
	long samplerate;
};

static void do_resample(gpsm_grp_t *swfiles, long samplerate)
{
	gpsm_item_t *item;
	filter_param_t *input_filename, *input_samplerate, *output_filename;
	filter_t *net;

	/* Build a basic network for resampling of one track. */
	net = filter_creat(NULL);
	{
		filter_t *swin, *resamp, *swout;

		swin = net_add_plugin_by_name(net, "swapfile_in");
		input_filename = filterparamdb_get_param(filter_paramdb(swin), "filename");
		input_samplerate = filterparamdb_get_param(filter_paramdb(swin), "rate");
		resamp = net_add_plugin_by_name(net, "Resample");
		filterparam_set_long(filterparamdb_get_param(
			filter_paramdb(resamp), "frequency"), samplerate);
		swout = net_add_plugin_by_name(net, "swapfile_out");
		filterparam_set_long(filterparamdb_get_param(
			filter_paramdb(swout), "flags"), 2 /* truncate */);
		output_filename = filterparamdb_get_param(
			filter_paramdb(swout), "filename");
		filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT),
				   filterportdb_get_port(filter_portdb(resamp), PORTNAME_IN));
		filterport_connect(filterportdb_get_port(filter_portdb(resamp), PORTNAME_OUT),
				   filterportdb_get_port(filter_portdb(swout), PORTNAME_IN));
	}

	gpsm_grp_foreach_item(swfiles, item) {
		filter_launchcontext_t *context;
		gpsm_swfile_t *file;

		/* Skip, if already ok. */
		if (gpsm_swfile_samplerate(item) == samplerate)
			continue;

		/* Prepare the file, set the filename and run the
		 * resampling network. */
		gpsm_op_prepare(item);
		file = gpsm_swfile_cow((gpsm_swfile_t *)item);
		filterparam_set_long(input_filename, gpsm_swfile_filename(file));
		filterparam_set_long(input_samplerate, gpsm_swfile_samplerate(file));
		filterparam_set_long(output_filename, gpsm_swfile_filename(item));
		context = filter_launch(net, GLAME_BULK_BUFSIZE);
		filter_start(context);
		filter_wait(context);
		filter_launchcontext_unref(&context);
		gpsm_item_destroy((gpsm_item_t *)file);
		gpsm_invalidate_swapfile(gpsm_swfile_filename(item));

		/* Adapt all gpsm references to the swapfile. */
		file = NULL;
		while ((file = gpsm_find_swfile_filename(gpsm_root(), (gpsm_item_t *)file, gpsm_swfile_filename(item)))) {
			if (gpsm_swfile_samplerate(file) == samplerate)
				continue;
			gpsm_swfile_set_samplerate(file, samplerate);
		}
	}

	filter_delete(net);
}

static void dialog_cb(GnomeDialog *dialog, gint button, struct resample_s *rs)
{
	if (button == 2) {
		glame_help_goto(NULL, "Resample");
		return;
	} else if (button == 0)
		do_resample((gpsm_grp_t *)rs->grp, gtk_adjustment_get_value(rs->adjustment));
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

	rs->adjustment = GTK_ADJUSTMENT(gtk_adjustment_new(
		rs->samplerate, 2756, 192000, 100, 1000, 0.0));
	spinbutton1 = gtk_spin_button_new (rs->adjustment, 100000, 0);
	gtk_container_add (GTK_CONTAINER (hbox1), spinbutton1);

	gnome_dialog_append_button(GNOME_DIALOG(rs->dialog), GNOME_STOCK_BUTTON_OK);
	gnome_dialog_append_button(GNOME_DIALOG(rs->dialog), GNOME_STOCK_BUTTON_CANCEL);
	gnome_dialog_append_button(GNOME_DIALOG(rs->dialog), GNOME_STOCK_BUTTON_HELP);
	gtk_signal_connect(GTK_OBJECT(rs->dialog), "clicked",
			   GTK_SIGNAL_FUNC(dialog_cb), rs);
}


static int resample_gpsm(gpsm_item_t *grp, long start, long length)
{
	struct resample_s *rs;

	rs = ALLOCN(1, struct resample_s);

	rs->grp = (gpsm_item_t*)gpsm_collect_swfiles(grp);
	if (rs->grp == NULL)
		return -1;
	rs->samplerate = gpsm_swfile_samplerate(gpsm_grp_first(rs->grp));

	resample_dialog(rs);
	gtk_widget_show_all(rs->dialog);

	return 0;
}

int resample_register(plugin_t *p)
{
	/* We need the Resample scm plugin. */
	if (!plugin_get("Resample"))
		return -1;
	plugin_set(p, PLUGIN_GPSMOP, resample_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION, "resample a gpsm subtree");
	plugin_set(p, PLUGIN_CATEGORY, "Frequency");
	plugin_set(p, PLUGIN_LABEL, "Resample");
	
	return 0;
}
