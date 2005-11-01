/*
 * apply.c
 *
 * $Id: apply.c,v 1.28 2005/11/01 13:27:05 richi Exp $
 *
 * Copyright (C) 2001, 2002, 2003 Richard Guenther
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
#include "swapfile.h"
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
	gpsm_grp_t *dest;
	long start, length;
	filter_t *effect;
	GtkWidget *properties;
	GtkWidget *progress;
	GtkWidget *dialog;
	GtkWidget *checkbox, *loop_checkbox;
	filter_t *net;
	filter_t *pos;
	int previewing;
	int applying;
	guint timeout_id;
	filter_launchcontext_t *context;
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
	if (a->dest)
		gpsm_item_destroy((gpsm_item_t *)a->dest);
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

	if (filter_is_ready(a->context)) {
		gtk_timeout_remove(a->timeout_id);
		a->timeout_id = -1;
		if (a->previewing)
			preview_stop(a);
		else if (a->applying) {
			gpsm_item_t *swfile, *dst;
			gboolean lock_size;
			filter_wait(a->context);
			lock_size = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(a->checkbox));
			gpsm_op_prepare((gpsm_item_t *)a->item);
			dst = gpsm_grp_first(a->dest);
			gpsm_grp_foreach_item(a->item, swfile) {
				swfd_t sfd, dfd;
				struct sw_stat st;
				sfd = sw_open(gpsm_swfile_filename(dst), O_RDONLY);
				sw_fstat(sfd, &st);
				dfd = sw_open(gpsm_swfile_filename(swfile), O_RDWR);
				sw_lseek(dfd, a->start*SAMPLE_SIZE, SEEK_SET);
				if (lock_size) {
					/* old behavior - keep size */
					sw_sendfile(SW_NOFILE, dfd, MIN(st.size, a->length*SAMPLE_SIZE), SWSENDFILE_CUT);
					sw_lseek(dfd, a->start*SAMPLE_SIZE, SEEK_SET);
					sw_sendfile(dfd, sfd, MIN(st.size, a->length*SAMPLE_SIZE), SWSENDFILE_INSERT);
					gpsm_notify_swapfile_change(gpsm_swfile_filename(swfile), a->start, MIN(a->length, st.size/SAMPLE_SIZE));
				} else {
					/* new behavior - allow extending/shrinking */
					sw_sendfile(SW_NOFILE, dfd, a->length*SAMPLE_SIZE, SWSENDFILE_CUT);
					sw_lseek(dfd, a->start*SAMPLE_SIZE, SEEK_SET);
					sw_sendfile(dfd, sfd, st.size, SWSENDFILE_INSERT);
					if (st.size/SAMPLE_SIZE > a->length)
						gpsm_notify_swapfile_insert(gpsm_swfile_filename(swfile), a->start + a->length, st.size/SAMPLE_SIZE - a->length);
					else if (st.size/SAMPLE_SIZE < a->length)
						gpsm_notify_swapfile_cut(gpsm_swfile_filename(swfile), a->start, a->length - st.size/SAMPLE_SIZE);
					gpsm_notify_swapfile_change(gpsm_swfile_filename(swfile), a->start, MIN(a->length, st.size/SAMPLE_SIZE));
				}
				dst = gpsm_grp_next(a->dest, dst);
			}
			cleanup(a);
		}
		filter_launchcontext_unref(&a->context);
		return FALSE;
	}

	/* update progressbar */
	posparam = filterparamdb_get_param(filter_paramdb(a->pos),
					   FILTERPARAM_LABEL_POS);
	gtk_progress_bar_update(GTK_PROGRESS_BAR(a->progress),
				MIN(1.0, (float)(filterparam_val_long(posparam) % a->length)/(float)(a->length)));
	return TRUE;
}

static void preview_start(struct apply_plugin_s *a)
{
	gpsm_item_t *swfile;
	const char *errmsg;
	filter_t *swin, *e, *ec = NULL;
	filter_port_t *port;
	int nrin, currin, i;
	gboolean loop;

	/* Find out plugin input count. */
	nrin = 0;
	filterportdb_foreach_port(filter_portdb(a->effect), port)
		if (filterport_is_input(port)
		    && !filterport_get_property(port, "!CONTROL"))
			nrin++;

	loop = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(a->loop_checkbox));

	/* Create the preview network.
	 * DONT clone from a->effect! This would copy update handlers
	 * for > 2 nodes! Instead clone from dummy clone... */
	a->net = filter_creat(NULL);
	ec = filter_creat(a->effect);
	currin = nrin;
	gpsm_grp_foreach_item(a->item, swfile) {
		swin = net_add_gpsm_input(a->net, (gpsm_swfile_t *)swfile, a->start, a->length, loop ? 1 : 0);
		if (!swin) {
			errmsg = _("Unable to create input node");
			goto err;
		}
		if (currin >= nrin) {
			e = filter_creat(ec);
			filter_add_node(a->net, e, "effect");
			net_link_params(e, a->effect);
			currin = 0;
		}
		i = 0;
		filterportdb_foreach_port(filter_portdb(e), port) {
			if (!filterport_is_input(port))
				continue;
			if (filterport_get_property(port, "!CONTROL")) /* ignore LADSPA control ports */
				continue;
			if (i == currin)
				break;
			i++;
		}
		filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT), port);
		currin++;
	}
	net_apply_audio_out(a->net);
	a->pos = swin;
	filter_delete(ec);

	if (!(a->context = filter_launch(a->net, _GLAME_WBUFSIZE))) {
		errmsg = _("Unable to launch network");
		goto err;
	}
	filter_start(a->context);
	a->timeout_id = gtk_timeout_add(100, (GtkFunction)poll_net_cb, a);

	/* Disable buttons / change function. */
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, FALSE);
	/* FIXME - change "Preview" to "Stop" */
	a->previewing = 1;

	return;

 err:
	gtk_window_set_modal(GTK_WINDOW(a->dialog), FALSE);
	gtk_main_iteration_do(FALSE);
	glame_error_dialog(errmsg, NULL);
	gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);
	if (a->net) {
		filter_delete(a->net);
		a->net = NULL;
	}
	if (ec)
		filter_delete(ec);
}

static void preview_stop(struct apply_plugin_s *a)
{
	/* Cleanup after the network. */
	filter_terminate(a->context);
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
	filter_port_t *port;
	const char *errmsg;
	int i = 0, nrin, currin, j;

	/* Find out plugin input count. */
	nrin = 0;
	filterportdb_foreach_port(filter_portdb(a->effect), port)
		if (filterport_is_input(port)
		    && !filterport_get_property(port, "!CONTROL"))
			nrin++;

	/* Create the apply network and the destination swfile. */
	a->net = filter_creat(NULL);
	a->dest = gpsm_newgrp("Apply");
	currin = nrin;
	gpsm_grp_foreach_item(a->item, swfile) {
		filter_port_t *ein, *eout;
		gpsm_swfile_t *dst;
		swin = net_add_gpsm_input(a->net, (gpsm_swfile_t *)swfile, a->start, a->length, 0);
		dst = gpsm_newswfile("Apply");
		gpsm_vbox_insert(a->dest, (gpsm_item_t *)dst, 0, i++);
		swout = net_add_gpsm_output(a->net, dst, 0, -1, 0);
		if (currin >= nrin) {
			e = filter_creat(a->effect);
			filter_add_node(a->net, e, "effect");
			currin = 0;
		}
		if (!swin || !swout || !e) {
			errmsg = _("Cannot construct network");
			goto err;
		}
		j = 0;
		filterportdb_foreach_port(filter_portdb(e), ein) {
			if (!filterport_is_input(ein))
				continue;
			if (filterport_get_property(ein, "!CONTROL")) /* ignore LADSPA control ports */
				continue;
			if (j++ == currin)
				break;
		}
		j = 0;
		filterportdb_foreach_port(filter_portdb(e), eout) {
			if (!filterport_is_output(eout))
				continue;
			if (filterport_get_property(eout, "!CONTROL")) /* ignore LADSPA control ports */
				continue;
			if (j++ == currin)
				break;
		}
		filterport_connect(filterportdb_get_port(filter_portdb(swin), PORTNAME_OUT),
				   ein);
		filterport_connect(eout,
				   filterportdb_get_port(filter_portdb(swout), PORTNAME_IN));
		currin++;
	}
	a->pos = swin;

	if (!(a->context = filter_launch(a->net, _GLAME_WBUFSIZE))) {
		errmsg = _("Unable to launch network");
		goto err;
	}
	filter_start(a->context);
	a->timeout_id = gtk_timeout_add(100, (GtkFunction)poll_net_cb, a);

	/* Disable buttons. */
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), PREVIEW, FALSE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(a->dialog), APPLY, FALSE);
	a->applying = 1;

	return;

 err:
	gtk_window_set_modal(GTK_WINDOW(a->dialog), FALSE);
	gtk_main_iteration_do(FALSE);
	glame_error_dialog(errmsg, NULL);
	gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);
	if (a->net) {
		filter_delete(a->net);
		a->net = NULL;
	}
	if (a->dest) {
		gpsm_item_destroy((gpsm_item_t *)a->dest);
		a->dest = NULL;
	}
}

static void cancel_cb(GtkWidget *widget, struct apply_plugin_s *a)
{
	if (a->net)
		filter_terminate(a->context);
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
	GtkWidget *label, *hbox;
	char s[256], *help;
	const char *name;

	if (!item || !plugin)
		return -1;

	/* Basic init. */
	a = (struct apply_plugin_s *)malloc(sizeof(struct apply_plugin_s));
	a->item = gpsm_collect_swfiles(item);
	a->dest = NULL;
	a->start = start;
	a->length = length;
	a->effect = filter_instantiate(plugin);
	if (!a->effect)
		goto err;
	a->net = NULL;
	a->previewing = 0;
	a->applying = 0;
	a->timeout_id = -1;
	help = (char *)plugin_query(plugin, PLUGIN_GUI_HELP_PATH);
	name = (char *)plugin_query(plugin, PLUGIN_LABEL);
	if (!name)
		name = plugin_name(plugin);

	/* Build the dialog. */
	a->dialog = GTK_WIDGET(gtk_type_new(gnome_dialog_get_type()));
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

	snprintf(s, 255, _("Parameters of %s"), name);
	label = gtk_label_new(s);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), label,
			   TRUE, TRUE, 3);
	gtk_widget_show(label);

	a->properties = glame_gui_from_paramdb(filter_paramdb(a->effect));
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), a->properties,
			   TRUE, TRUE, 3);
	gtk_widget_show(a->properties);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(a->dialog)->vbox), hbox,
			   TRUE, TRUE, 3);
	a->checkbox = gtk_check_button_new_with_label(_("Lock size"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(a->checkbox), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), a->checkbox, TRUE, TRUE, 3);
	gtk_widget_show(a->checkbox);
	a->loop_checkbox = gtk_check_button_new_with_label(_("Loop previewing"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(a->loop_checkbox), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), a->loop_checkbox, TRUE, TRUE, 3);
	gtk_widget_show(a->loop_checkbox);
	gtk_widget_show(hbox);

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
				    GTK_SIGNAL_FUNC(preview_cb), a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), APPLY,
				    GTK_SIGNAL_FUNC(apply_cb), a);
	gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), CANCEL,
				    GTK_SIGNAL_FUNC(cancel_cb), a);
	if (help)
		gnome_dialog_button_connect(GNOME_DIALOG(a->dialog), HELP,
				    	    GTK_SIGNAL_FUNC(glame_help_cb), help);

	/* Just show the dialog. Modal for now. */
	gtk_window_set_modal(GTK_WINDOW(a->dialog), TRUE);
	gtk_widget_show(a->dialog);

	return 0;

 err:
	free(a);
	return -1;
}
