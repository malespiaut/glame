/*
 * apply.c
 *
 * $Id: apply.c,v 1.15 2002/02/17 13:53:31 richi Exp $
 *
 * Copyright (C) 2001 Richard Guenther
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

#include <gnome.h>
#include "gpsm.h"
#include "filter.h"
#include "glplugin.h"
#include "util/glame_gui_utils.h"
#include "network_utils.h"



/* Apply filter. Generic with lots of features.
 * A "builtin" gpsm op.
 */

/* Struct holding the whole state so we can be driven
 * by gtk callbacks. */
struct apply_plugin_s {
	gpsm_grp_t *item;
	long start, length;
	filter_t *effect;
	GtkWidget *properties;
	GtkWidget *progress;
	GtkWidget *dialog;
	filter_t *net;
	filter_t *pos;
	int have_undo;
	int previewing;
	int applying;
	guint timeout_id;
};

/* Button numbers. */
#define PREVIEW 0
#define APPLY 1
#define CANCEL 2
#define HELP 3

/* Struct cleanup. */
static void cleanup(struct apply_plugin_s *a)
{
	if (a->timeout_id != -1)
		gtk_timeout_remove(a->timeout_id);
	gtk_widget_hide(a->dialog);
	gtk_widget_destroy(a->dialog);
	if (a->net)
		filter_delete(a->net);
	filter_delete(a->effect);
	gpsm_item_destroy((gpsm_item_t *)a->item);
	free(a);
}


/* The callbacks.
 */

static void preview_stop(struct apply_plugin_s *a);

static gint poll_net_cb(struct apply_plugin_s *a)
{
	filter_param_t *posparam;

	if (filter_is_ready(a->net)) {
		gtk_timeout_remove(a->timeout_id);
		a->timeout_id = -1;
		if (a->previewing)
			preview_stop(a);
		else if (a->applying) {
			gpsm_item_t *swfile;
			filter_wait(a->net);
			gpsm_grp_foreach_item(a->item, swfile)
				gpsm_notify_swapfile_change(gpsm_swfile_filename(swfile), a->start, a->length);
			cleanup(a);
		}
		return FALSE;
	}
	/* update progressbar */
	posparam = filterparamdb_get_param(filter_paramdb(a->pos), FILTERPARAM_LABEL_POS);
	gtk_progress_bar_update(GTK_PROGRESS_BAR(a->progress), MIN(1.0, (float)filterparam_val_long(posparam)/(float)(a->length)));
	return TRUE;
}

static void preview_start(struct apply_plugin_s *a)
{
	gpsm_item_t *swfile;
	const char *errmsg;
	filter_t *n;

	/* Create the preview network. */
	a->net = filter_creat(NULL);
	gpsm_grp_foreach_item(a->item, swfile)
		if (!net_add_gpsm_input(a->net, (gpsm_swfile_t *)swfile, a->start, a->length, 0)) {
			errmsg = _("Unable to create input node");
			goto err;
		}
	if (net_apply_effect(a->net, a->effect) == -1) {
		errmsg = _("Unable to apply effect node(s)");
		goto err;
	}
	a->pos = net_apply_audio_out(a->net);

	/* Allow updating params of preview network. */
	filter_foreach_node(a->net, n)
		if (n->plugin == a->effect->plugin)
			net_link_params(n, a->effect);

	if (filter_launch(a->net, _GLAME_WBUFSIZE) == -1) {
		errmsg = _("Unable to launch network");
		goto err;
	}
	filter_start(a->net);
	a->timeout_id = gtk_timeout_add(100, (GtkFunction)poll_net_cb, a);

	/* Disable buttons / change function. */
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, FALSE);
	/* FIXME - change "Preview" to "Stop" */
	a->previewing = 1;

	return;

 err:
	gtk_window_set_modal(GTK_WINDOW(a->dialog), FALSE);
	gtk_main_iteration_do(FALSE);
	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(errmsg)));
	gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);
	if (a->net) {
		filter_delete(a->net);
		a->net = NULL;
	}
}

static void preview_stop(struct apply_plugin_s *a)
{
	/* Cleanup after the network. */
	filter_terminate(a->net);
	filter_delete(a->net);
	a->net = NULL;

	/* Enable buttons / change function. */
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, TRUE);
	/* FIXME - change "Stop" to "Preview" */
	a->previewing = 0;
}

static void preview_cb(GtkWidget *widget, struct apply_plugin_s *a)
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

static void apply_cb(GtkWidget *widget, struct apply_plugin_s *a)
{
	gpsm_item_t *swfile;
	filter_t *swin, *swout, *e;
	const char *errmsg;

	/* Create the apply network. */
	a->net = filter_creat(NULL);
	gpsm_grp_foreach_item(a->item, swfile) {
		filter_port_t *ein, *eout;
		swin = net_add_gpsm_input(a->net, (gpsm_swfile_t *)swfile, a->start, a->length, 0);
		swout = net_add_gpsm_output(a->net, (gpsm_swfile_t *)swfile, a->start, a->length, 0);
		e = filter_creat(a->effect);
		filter_add_node(a->net, e, "effect");
		if (!swin || !swout || !e) {
			errmsg = _("Cannot construct network");
			goto err;
		}
		filterportdb_foreach_port(filter_portdb(e), ein)
			if (filterport_is_input(ein))
				break;
		filterportdb_foreach_port(filter_portdb(e), eout)
			if (filterport_is_output(eout))
				break;
		filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT),
				   ein);
		filterport_connect(eout,
				   filterportdb_get_port(filter_portdb(swout), PORTNAME_IN));
	}
	a->pos = swout;

	gpsm_op_prepare((gpsm_item_t *)a->item);
	a->have_undo = 1;

	if (filter_launch(a->net, GLAME_BULK_BUFSIZE) == -1) {
		errmsg = _("Unable to launch network");
		goto err;
	}
	filter_start(a->net);
	a->timeout_id = gtk_timeout_add(100, (GtkFunction)poll_net_cb, a);

	/* Disable buttons. */
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), PREVIEW, FALSE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, FALSE);
	a->applying = 1;

	return;

 err:
	gtk_window_set_modal(GTK_WINDOW(a->dialog), FALSE);
	gtk_main_iteration_do(FALSE);
	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(errmsg)));
	gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);
	if (a->net) {
		filter_delete(a->net);
		a->net = NULL;
	}
}

static void cancel_cb(GtkWidget *widget, struct apply_plugin_s *a)
{
	if (a->net)
		filter_terminate(a->net);
	if (a->have_undo)
		gpsm_op_undo_and_forget((gpsm_item_t *)a->item);
	cleanup(a);
}

static gint delete_cb(GtkWidget *w, GdkEventAny *event,
		      struct apply_plugin_s *a)
{
	cancel_cb(w, a);
	return TRUE;
}

static gint key_cb(GtkWidget *w, GdkEventKey *event, struct apply_plugin_s *a)
{
	/* Catch ESCAPE - prevent default handler from running. */
	if (event->keyval == GDK_Escape) {
		gtk_signal_emit_stop_by_name(GTK_OBJECT(a->dialog),
					     "key_press_event");
		return TRUE;
	}
	return FALSE;
}

int gpsmop_apply_plugin(gpsm_item_t *item, plugin_t *plugin,
			long start, long length)
{
	struct apply_plugin_s *a;
	GtkWidget *label;
	char s[256], *help;

	if (!item || !plugin)
		return -1;

	/* Basic init. */
	a = (struct apply_plugin_s *)malloc(sizeof(struct apply_plugin_s));
	a->item = gpsm_collect_swfiles(item);
	a->start = start;
	a->length = length;
	a->effect = filter_instantiate(plugin);
	if (!a->effect)
		goto err;
	a->net = NULL;
	a->have_undo = 0;
	a->previewing = 0;
	a->applying = 0;
	a->timeout_id = -1;
	help = plugin_query(plugin, PLUGIN_GUI_HELP_PATH);

	/* Build the dialog. */
	a->dialog = gtk_type_new(gnome_dialog_get_type());
	gnome_dialog_close_hides(GNOME_DIALOG(a->dialog), FALSE);
	gnome_dialog_set_close(GNOME_DIALOG(a->dialog), FALSE);

	gnome_dialog_append_button_with_pixmap(
		GNOME_DIALOG(a->dialog), _("Preview"), GNOME_STOCK_PIXMAP_VOLUME);
	gnome_dialog_append_button_with_pixmap(
		GNOME_DIALOG(a->dialog), _("Apply"), GNOME_STOCK_PIXMAP_FORWARD);
	gnome_dialog_append_button(
		GNOME_DIALOG(a->dialog), GNOME_STOCK_BUTTON_CANCEL);
	if (help)
		gnome_dialog_append_button(
			GNOME_DIALOG(a->dialog), GNOME_STOCK_BUTTON_HELP);

	gnome_dialog_set_default(GNOME_DIALOG(a->dialog), APPLY);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), PREVIEW, TRUE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, TRUE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), CANCEL, TRUE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), HELP, TRUE);

	snprintf(s, 255, _("Parameters of %s"), plugin_name(plugin));
	label = gtk_label_new(s);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), label,
			   TRUE, TRUE, 3);
	gtk_widget_show(label);

	a->properties = glame_gui_from_paramdb(filter_paramdb(a->effect));
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), a->properties,
			   TRUE, TRUE, 3);
	gtk_widget_show(a->properties);

	a->progress = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(a->progress), GTK_PROGRESS_LEFT_TO_RIGHT);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), a->progress,
			   TRUE, TRUE, 3);
	gtk_widget_show(a->progress);

	/* The signals. */
	gtk_signal_connect(GTK_OBJECT(a->dialog), "delete_event",
			   (GtkSignalFunc)delete_cb, a);
	gtk_signal_connect(GTK_OBJECT(a->dialog), "key_press_event",
			   (GtkSignalFunc)key_cb, a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), PREVIEW,
				    preview_cb, a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), APPLY,
				    apply_cb, a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), CANCEL,
				    cancel_cb, a);
	if (help)
		gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), HELP,
				    	    glame_help_cb, help);

	/* Just show the dialog. Modal for now. */
	gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);
	gtk_widget_show(a->dialog);

	return 0;

 err:
	free(a);
	return -1;
}
