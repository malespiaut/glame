
/*
 * glame_gui_utils.c
 *
 * $Id: glame_gui_utils.c,v 1.7 2001/04/17 12:43:21 richi Exp $
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

struct foo{
	GnomeDialog * dia;
	filter_t *net;
	gui_network *gui;
	gboolean playing;
	guint handler_id;
	GtkFunction atExitFunc;
	gpointer data;
};

typedef struct foo play_struct_t;

static gint network_finish_check_cb(play_struct_t *play)
{
	// FIXME
	//	if(!(FILTER_IS_RUNNING(play->net))){
	if((!FILTER_IS_LAUNCHED(play->net)) && play->playing){
		gtk_timeout_remove(play->handler_id);
		DPRINTF("Network finished\n");
		filter_terminate(play->net);
		filter_wait(play->net);
		gnome_dialog_close(play->dia);
		return FALSE;
	}else{
		return TRUE;
	}
}

static void play_cb(GnomeDialog * dia, play_struct_t* play)
{
	if(play->gui)
		network_error_reset(play->gui);
	filter_launch(play->net);
	if(filter_start(play->net) == -1) {
		if (play->gui)
			network_draw_error(play->gui);
		if (!play->gui) {
			gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog("Error processing network")));
			gnome_dialog_close(play->dia);
		}
		return;
	}
	play->playing = TRUE;
	gnome_dialog_set_sensitive(play->dia,0,FALSE);
	gnome_dialog_set_sensitive(play->dia,1,TRUE);
	gnome_dialog_set_sensitive(play->dia,2,TRUE);
	play->handler_id = gtk_timeout_add(100,(GtkFunction)network_finish_check_cb,play);
}

static void pause_cb(GnomeDialog * dia, play_struct_t * play)
{
	filter_t* net = play->net;
	if(FILTER_IS_LAUNCHED(net)){
		// FIXME ..
		//if(FILTER_IS_RUNNING(play->net)){
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
	if(FILTER_IS_LAUNCHED(play->net)){
		filter_terminate(play->net);
		filter_wait(play->net);
		play->playing = FALSE;
		gnome_dialog_set_sensitive(play->dia,0,TRUE);
		gnome_dialog_set_sensitive(play->dia,1,FALSE);
		gnome_dialog_set_sensitive(play->dia,2,FALSE);
	}
	if(play->handler_id)
		gtk_timeout_remove(play->handler_id);
}

static void cancel_cb(GnomeDialog *dia, play_struct_t *play)
{
	if(FILTER_IS_LAUNCHED(play->net)){
		filter_terminate(play->net);
		filter_wait(play->net);
	}
	if(play->handler_id)
		gtk_timeout_remove(play->handler_id);
	gnome_dialog_close(play->dia);
}


static void play_destroy_cb(GtkWidget *widget, play_struct_t *play)
{
	DPRINTF("destroying widget\n");
	if (play->atExitFunc)
		play->atExitFunc(play->data);
	free(play);
}

int glame_gui_play_network(filter_t *network, gui_network *gui, int modal,
			   GtkFunction atExit, gpointer data)
{
	play_struct_t *play;

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

	gnome_dialog_append_button_with_pixmap(play->dia,"Play",GNOME_STOCK_PIXMAP_FORWARD);
	gnome_dialog_append_button_with_pixmap(play->dia,"Pause",GNOME_STOCK_PIXMAP_TIMER_STOP);
	gnome_dialog_append_button_with_pixmap(play->dia,"Stop",GNOME_STOCK_PIXMAP_STOP);	
	gnome_dialog_append_button(play->dia,GNOME_STOCK_BUTTON_CANCEL);
	
	gnome_dialog_set_default(play->dia,0);
	gnome_dialog_set_sensitive(play->dia,1,FALSE);
	gnome_dialog_set_sensitive(play->dia,2,FALSE);

	gnome_dialog_button_connect(play->dia,0,play_cb,play);
	gnome_dialog_button_connect(play->dia,1,pause_cb,play);
	gnome_dialog_button_connect(play->dia,2,stop_cb,play);
	gnome_dialog_button_connect(play->dia,3,cancel_cb,play);
	
	if(modal)
		gtk_window_set_modal(GTK_WINDOW(play->dia),TRUE);

	/* Finally register cleanup stuff. */
	gtk_signal_connect(GTK_OBJECT(play->dia), "destroy",
			   (GtkSignalFunc)play_destroy_cb, play);

	gtk_widget_show(GTK_WIDGET(play->dia));

	return 0;
}
