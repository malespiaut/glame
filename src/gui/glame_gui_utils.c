
/*
 * glame_gui_utils.c
 *
 * $Id: glame_gui_utils.c,v 1.11 2001/04/29 11:48:56 richi Exp $
 *
 * Copyright (C) 2001 Johannes Hirche
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

#include <sys/param.h>
#include "glmid.h"
#include "glame_gui_utils.h"
#include "canvas.h"



/*
 * The generic play bar.
 */

typedef struct {
	GnomeDialog * dia;
	filter_t *net;
	gui_network *gui;
	gboolean playing;
	guint handler_id;
	gint play_index, pause_index, stop_index, cancel_index;
	int close_on_stop;
	GtkFunction atExitFunc;
	gpointer data;
} play_struct_t;

static gint network_finish_check_cb(play_struct_t *play)
{
	if (filter_is_ready(play->net) && play->playing) {
		gtk_timeout_remove(play->handler_id);
		filter_terminate(play->net);
		gnome_dialog_close(play->dia);
		return FALSE;
	}
	return TRUE;
}

static void play_cb(GnomeDialog * dia, play_struct_t* play)
{
	if (play->gui)
		network_error_reset(play->gui);
	if (filter_launch(play->net) == -1
	    || filter_start(play->net) == -1) {
		if (play->gui)
			network_draw_error(play->gui);
		else
			gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("Error processing network")));
		gnome_dialog_close(play->dia);
		return;
	}
	play->playing = TRUE;
	gnome_dialog_set_sensitive(play->dia, play->play_index, FALSE);
	if (play->pause_index != -1)
		gnome_dialog_set_sensitive(play->dia, play->pause_index, TRUE);
	if (play->stop_index != -1)
		gnome_dialog_set_sensitive(play->dia, play->stop_index, TRUE);
	play->handler_id = gtk_timeout_add(100, (GtkFunction)network_finish_check_cb, play);
}

static void pause_cb(GnomeDialog * dia, play_struct_t * play)
{
	if(FILTER_IS_LAUNCHED(play->net)){
		if(play->playing){
			play->playing = FALSE;
			filter_pause(play->net);
		} else {
			filter_start(play->net);
			play->playing = TRUE;
		}
	}
}

static void stop_cb(GnomeDialog *dia, play_struct_t* play)
{
	if(play->handler_id)
		gtk_timeout_remove(play->handler_id);
	if(FILTER_IS_LAUNCHED(play->net)){
		if (play->pause_index != -1)
			gnome_dialog_set_sensitive(play->dia,
						   play->pause_index, FALSE);
		gnome_dialog_set_sensitive(play->dia, play->stop_index, FALSE);
		filter_terminate(play->net);
		play->playing = FALSE;
		gnome_dialog_set_sensitive(play->dia, play->play_index, TRUE);
		if (play->close_on_stop)
			gnome_dialog_close(play->dia);
	}
}

static void cancel_cb(GnomeDialog *dia, play_struct_t *play)
{
	if(play->handler_id)
		gtk_timeout_remove(play->handler_id);
	if(FILTER_IS_LAUNCHED(play->net))
		filter_terminate(play->net);
	gnome_dialog_close(play->dia);
}


static void play_destroy_cb(GtkWidget *widget, play_struct_t *play)
{
	if (FILTER_IS_LAUNCHED(play->net))
		filter_terminate(play->net);
	if (play->atExitFunc)
		play->atExitFunc(play->data);
	free(play);
}

int glame_gui_play_network(filter_t *network, gui_network *gui, int modal,
			   GtkFunction atExit, gpointer data,
			   const char *start_label,
			   const char *pause_label,
			   const char *stop_label, int close_on_stop)
{
	play_struct_t *play;
	int i;

	if (!network || !FILTER_IS_NETWORK(network))
		return -1;

	if (!(play = malloc(sizeof(play_struct_t))))
		return -1;
	play->net = network;
	play->gui = gui;
	play->atExitFunc = atExit;
	play->data = data;
	play->dia = GNOME_DIALOG(gnome_dialog_new(filter_name(network),NULL));
	play->playing = FALSE;
	play->handler_id = 0;
	play->play_index = -1;
	play->pause_index = -1;
	play->stop_index = -1;
	play->cancel_index = -1;
	play->close_on_stop = close_on_stop;

	i = 0;
	if (!start_label)
		start_label = "Play";
	play->play_index = i++;
	gnome_dialog_append_button_with_pixmap(play->dia, start_label,
					       GNOME_STOCK_PIXMAP_FORWARD);
	gnome_dialog_set_default(play->dia, play->play_index);
	gnome_dialog_button_connect(play->dia, play->play_index,
				    play_cb, play);

	if (pause_label) {
		play->pause_index = i++;
		gnome_dialog_append_button_with_pixmap(play->dia, pause_label, GNOME_STOCK_PIXMAP_TIMER_STOP);
		gnome_dialog_set_sensitive(play->dia, play->pause_index, FALSE);
		gnome_dialog_button_connect(play->dia, play->pause_index,
					    pause_cb, play);
	}

	if (stop_label) {
		play->stop_index = i++;
		gnome_dialog_append_button_with_pixmap(play->dia, stop_label,
						       GNOME_STOCK_PIXMAP_STOP);
		gnome_dialog_set_sensitive(play->dia, play->stop_index, FALSE);
		gnome_dialog_button_connect(play->dia, play->stop_index,
					    stop_cb, play);
	}

	play->cancel_index = i++;
	gnome_dialog_append_button(play->dia,GNOME_STOCK_BUTTON_CANCEL);
	gnome_dialog_button_connect(play->dia, play->cancel_index,
				    cancel_cb, play);

	if(modal)
		gtk_window_set_modal(GTK_WINDOW(play->dia),TRUE);

	/* Finally register cleanup stuff. */
	gtk_signal_connect(GTK_OBJECT(play->dia), "destroy",
			   (GtkSignalFunc)play_destroy_cb, play);

	gtk_widget_show(GTK_WIDGET(play->dia));

	return 0;
}




/*
 * Widget creation helpers. GNOME/gtk+ suck...
 */


/* Generic callback to update a 256 chars size destination buffer from
 * a GtkEditable (such as gnome_entry's gtk_edit widget). */
static void update_string_from_editable_cb(GtkEditable *w, char *buf)
{
	char *chars = gtk_editable_get_chars(w, 0, -1);
        strncpy(buf, chars, 255);
	g_free(chars);
}

void create_label_edit_pair(GtkWidget *vbox,
			    const char *label, const char *history,
			    char *result)
{
	GtkWidget *whbox, *wlabel, *wentry;

	whbox = gtk_hbox_new(TRUE, 5);

	wlabel = gtk_label_new(label);

	wentry = gnome_entry_new(history);
	gtk_entry_set_text(GTK_ENTRY(gnome_entry_gtk_entry(GNOME_ENTRY(wentry))), result);
	gtk_signal_connect(GTK_OBJECT(gnome_entry_gtk_entry(GNOME_ENTRY(wentry))), "changed", update_string_from_editable_cb, result);

	gtk_container_add(GTK_CONTAINER(whbox), wlabel);
	gtk_container_add(GTK_CONTAINER(whbox), wentry);
	gtk_container_add(GTK_CONTAINER(vbox), whbox);
	gtk_widget_show(wlabel);
	gtk_widget_show(wentry);
	gtk_widget_show(whbox);
}


static void glame_dialog_file_request_browse_cb(GnomeFileEntry *fileentry,
						const char *pattern)
{
	/* Well, this f*** widget is NULL here... */
	gtk_file_selection_complete(GTK_FILE_SELECTION(fileentry->fsw),
				    pattern);
}
GtkWidget *glame_dialog_file_request(const char *windowtitle,
				     const char *history_id, const char *label,
				     const char *pattern,
				     char *returnbuffer)
{
	GtkWidget *dialog;
	GtkWidget *fileEntry;
	GtkWidget *dialogVbox;	
	
	dialog = gnome_dialog_new(windowtitle, GNOME_STOCK_BUTTON_CANCEL,
				  GNOME_STOCK_BUTTON_OK, NULL);
        dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));

        fileEntry = gnome_file_entry_new(history_id, label);
        gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fileEntry))),
                           "changed", update_string_from_editable_cb,
			   returnbuffer);
	if (pattern)
		gtk_signal_connect_after(GTK_OBJECT(fileEntry), "browse-clicked",
					 glame_dialog_file_request_browse_cb,
					 pattern);
        create_label_widget_pair(dialogVbox, "Filename", fileEntry);
	return dialog;
}



struct gpi_s {
	GtkWidget *widget;
	filter_param_t *pos;
	long size;
	glsig_handler_t *handler;
	guint tid;
};
static void gpi_widget_destroy(GtkWidget *w, struct gpi_s *gpi)
{
	gtk_timeout_remove(gpi->tid);
	glsig_delete_handler(gpi->handler);
	free(gpi);
}
static void gpi_param_delete(glsig_handler_t *handler, long sig, va_list va)
{
	struct gpi_s *gpi = glsig_handler_private(handler);
	gtk_object_destroy(GTK_OBJECT(gpi->widget));
}
static gint gpi_timeout(struct gpi_s *gpi)
{
	gfloat state = (float)filterparam_val_pos(gpi->pos)/(float)gpi->size;
	gtk_progress_bar_update(GTK_PROGRESS_BAR(gpi->widget), state);
	return TRUE;
}
GtkWidget *glame_progress_indicator(filter_param_t *pos, long size)
{
	struct gpi_s *gpi;

	if (!pos)
		return NULL;

	gpi = ALLOC(struct gpi_s);
	gpi->pos = pos;
	gpi->size = size;

	gpi->widget = gtk_progress_bar_new();
	gtk_progress_bar_set_bar_style(GTK_PROGRESS_BAR(gpi->widget),
				       GTK_PROGRESS_CONTINUOUS);
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(gpi->widget),
					 GTK_PROGRESS_LEFT_TO_RIGHT);
	gpi->tid = gtk_timeout_add(200, (GtkFunction)gpi_timeout, gpi);

	gpi->handler = glsig_add_handler(&pos->emitter, GLSIG_PARAM_DELETED,
					 gpi_param_delete, gpi);
	gtk_signal_connect(GTK_OBJECT(gpi->widget), "destroy",
			   gpi_widget_destroy, gpi);

	return gpi->widget;
}



struct garn_s {
	filter_t *net;
	guint tid;
	GtkFunction callback;
	gpointer data;
};
gint garn_poll(struct garn_s *garn)
{
	if (!filter_is_ready(garn->net))
		return TRUE;
	filter_terminate(garn->net);
	filter_delete(garn->net);
	if (garn->callback)
		garn->callback(garn->data);
	free(garn);
	return FALSE;
}
int glame_async_run_network(filter_t *net, GtkFunction callback, gpointer data)
{
	struct garn_s *garn;

	if (filter_launch(net) == -1)
		return -1;
	garn = ALLOC(struct garn_s);
	garn->net = net;
	garn->callback = callback;
	garn->data = data;
	garn->tid = gtk_timeout_add(500, (GtkFunction)garn_poll, garn);
	if (filter_start(net) == -1) {
		gtk_timeout_remove(garn->tid);
		filter_terminate(net);
		free(garn);
		return -1;
	}
	return 0;
}
