
/*
 * glame_gui_utils.c
 *
 * $Id: glame_gui_utils.c,v 1.6 2001/04/16 20:08:19 xwolf Exp $
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
	va_list va;
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
		if(!play->gui)
			filter_delete(play->net);
		gnome_dialog_close(play->dia);
		if(play->atExitFunc)
			(*(play->atExitFunc))(play->va);
		free(play);
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
	if(filter_start(play->net)<0){
		if(play->gui){
			network_draw_error(play->gui);
		}else
			DPRINTF("Some error in network occured\n");
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
		if(!play->gui)
			filter_delete(play->net);
	}
	if(play->handler_id)
		gtk_timeout_remove(play->handler_id);
	gnome_dialog_close(play->dia);
	free(play);
}

static int _glame_gui_play_network(play_struct_t * play, int modal)
{
	// Sanity Checks
	if(!play->net){
		DPRINTF("NULL pointer as network!\n");
		return -1;
	}
	
	if(!FILTER_IS_NETWORK(play->net)){
		DPRINTF("Trying to play non-network!\n");
		return -1;
	}
	
	play->dia = GNOME_DIALOG(gnome_dialog_new(filter_name(play->net),NULL));
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
	gtk_widget_show(GTK_WIDGET(play->dia));
	return 0;
}

int glame_gui_play_network_modal(filter_t * network, gui_network * gui_net)
{

        play_struct_t* play;

        play = malloc(sizeof(play_struct_t));

        play->net = network;
        play->gui = gui_net;
        play->atExitFunc = NULL;

        return _glame_gui_play_network(play,TRUE);
}

	
	
int glame_gui_play_network(filter_t * network, gui_network * gui_net)
{
	
	play_struct_t* play;
		
	play = malloc(sizeof(play_struct_t));
	
	play->net = network;
	play->gui = gui_net;
	play->atExitFunc = NULL;

	return _glame_gui_play_network(play,FALSE);
}

