
/*
 * glame_gui_utils.c
 *
 * $Id: glame_gui_utils.c,v 1.9 2001/04/22 14:24:57 richi Exp $
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

static void create_label_edit_pair_string_cb(GtkEditable *w, char *buf)
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
	gtk_signal_connect(GTK_OBJECT(gnome_entry_gtk_entry(GNOME_ENTRY(wentry))), "changed", create_label_edit_pair_string_cb, result);

	gtk_container_add(GTK_CONTAINER(whbox), wlabel);
	gtk_container_add(GTK_CONTAINER(whbox), wentry);
	gtk_container_add(GTK_CONTAINER(vbox), whbox);
	gtk_widget_show(wlabel);
	gtk_widget_show(wentry);
	gtk_widget_show(whbox);
}
