/*
 * normalize.c
 * $Id: normalize.c,v 1.6 2001/07/13 08:58:50 richi Exp $
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

static int normalize_gpsm(gpsm_item_t *grp, long start, long length)
{
	filter_t *net, *swap, *ssp, *maxrms, *adjust;
	gpsm_item_t	*item;
	filter_param_t	*param;
	int 	err = -1, bsize = 1, chanum = 1;
	float	rms, mrms, gain;
	char *aname;
	
	/* Gtk Stuff */
	GtkWidget	*procbar, *vbox;
	GtkWidget	*label;
	GtkWidget	*window;

	aname = alloca(256);

	/* We dont want to handle single swfile special. */
	if (!(grp = (gpsm_item_t *)gpsm_collect_swfiles(grp)))
		return -1;

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_modal(GTK_WINDOW (window), TRUE);
	
	vbox = gtk_vbox_new(FALSE, 0);
	gtk_widget_show(vbox);
	gtk_container_add(GTK_CONTAINER(window),vbox);
	
	label = gtk_label_new("Analyzing...");
	gtk_widget_show(label);
	gtk_box_pack_start(GTK_BOX (vbox), label, FALSE, FALSE, 0);
	
	procbar = gtk_progress_bar_new();
	gtk_box_pack_start(GTK_BOX (vbox), procbar, FALSE, FALSE, 0);
	gtk_widget_show(procbar);
	gtk_progress_configure(GTK_PROGRESS (procbar), 0, 0, 100);
	gtk_progress_set_show_text(GTK_PROGRESS (procbar), TRUE);

	gtk_widget_show(window);
	
	/* Gtk Crap first round finished */

	mrms = 0.0;

	gpsm_grp_foreach_item(grp, item) {
		sprintf(aname, "Analyzing Track %d", chanum++);
		gtk_label_set_text(GTK_LABEL(label), aname);
		net = filter_creat(NULL);
		ssp = net_add_plugin_by_name(net, "ssp_streamer");
		maxrms = net_add_plugin_by_name(net, "maxrms");
		swap = net_add_gpsm_input(net, (gpsm_swfile_t *)item,
					  start, length);

		if (!filterport_connect(filterportdb_get_port(filter_portdb(swap), PORTNAME_OUT), 
					filterportdb_get_port(filter_portdb(ssp), PORTNAME_IN)))
			goto cleanup;

		if (!filterport_connect(filterportdb_get_port(filter_portdb(ssp), PORTNAME_OUT), 
				filterportdb_get_port(filter_portdb(maxrms), PORTNAME_IN)))
			goto cleanup;

		filterparam_set(filterparamdb_get_param(filter_paramdb(ssp), "bsize"), &bsize);
	
		if ((filter_launch(net) == -1) ||
	    		(filter_start(net) == -1))
			goto cleanup;

		param = filterparamdb_get_param(filter_paramdb(swap), 
						FILTERPARAM_LABEL_POS);
	
		/* just put all this crap in some help function next time */
		while(!filter_is_ready(net)) {
			/* we need this otherwise the window doesn't popup 
	 		 * gtk sucks... 
	 		*/

			while (gtk_events_pending())
				gtk_main_iteration();

			/* I not it's ugly but what can you do? */
			usleep(40000);
			gtk_progress_bar_update(GTK_PROGRESS_BAR(procbar), 
				(float)filterparam_val_pos(param)/(float)length);
		}

		param = filterparamdb_get_param(filter_paramdb(maxrms), "maxrms");
		rms = filterparam_val_float(param);
		if (rms > mrms)
			mrms = rms;

		filter_delete(net);
	}
	
	gain = 1.0 / mrms;
	
	DPRINTF("Found RMS = %f setting gain = %f\n", mrms, gain);
	
	net = filter_creat(NULL);
	
	if (gpsm_op_prepare((gpsm_item_t*)grp) == -1)
		DPRINTF("Error preparing for undo\n");
	
	gpsm_grp_foreach_item(grp, item) {
		adjust = net_add_plugin_by_name(net, "volume_adjust");
		swap = net_add_gpsm_input(net, (gpsm_swfile_t *)item,
					  start, length);
		if (!filterport_connect(filterportdb_get_port(filter_portdb(swap), PORTNAME_OUT), 
					filterportdb_get_port(filter_portdb(adjust), PORTNAME_IN)))
			goto cleanup;

		swap = net_add_gpsm_output(net, (gpsm_swfile_t *)item,
					   start, length, 0);
		if (!filterport_connect(filterportdb_get_port(filter_portdb(adjust), PORTNAME_OUT), 
					filterportdb_get_port(filter_portdb(swap), PORTNAME_IN)))
			goto cleanup;
	
		filterparam_set(filterparamdb_get_param(filter_paramdb(adjust), "factor"), 
				&gain);
	}
	
	if ((filter_launch(net) == -1) ||
	    (filter_start(net) == -1))
		goto cleanup;

	param = filterparamdb_get_param(filter_paramdb(swap), 
					FILTERPARAM_LABEL_POS);
	
	gtk_label_set_text(GTK_LABEL(label), "Normalizing...");
	/* just put all this crap in some help function next time */
	while(!filter_is_ready(net)) {
		/* we need this otherwise the window doesn't popup 
	 	 * gtk sucks... 
	 	*/

		while (gtk_events_pending())
			gtk_main_iteration();

		/* I not it's ugly but what can you do? */
		usleep(40000);
		gtk_progress_bar_update(GTK_PROGRESS_BAR(procbar), 
			(float)filterparam_val_pos(param)/(float)length);
	}


	gpsm_grp_foreach_item(grp, item) {
		if (start >= 0) 
			gpsm_notify_swapfile_change(gpsm_swfile_filename(item),
						    start, length); 
		else 
			gpsm_invalidate_swapfile(gpsm_swfile_filename(item));
	}
	
	err = 0;

cleanup:
	DPRINTF("err = %d\n", err);
	filter_delete(net);
	gpsm_item_destroy(grp);
	
	gtk_widget_destroy(window);
	return err;
}

int normalize_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_GPSMOP, normalize_gpsm);
	plugin_set(p, PLUGIN_DESCRIPTION, "normalizes a gpsm subtree");
	plugin_set(p, PLUGIN_CATEGORY, "Volume");
	
	return 0;
}
/* Generalization strikes 240 lines for a simple function :) */
