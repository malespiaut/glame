
/*
 * glame_gui_utils.c
 *
 * $Id: glame_gui_utils.c,v 1.1 2001/03/25 20:48:30 xwolf Exp $
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


#include "glame_gui_utils.h"

struct foo{
	GnomeDialog * dia;
	filter_t *net;
	gui_network *gui;
	gboolean playing;
};

typedef struct foo play_struct_t;

static void play_cb(GnomeDialog * dia, play_struct_t* play)
{
	filter_launch(play->net);
	filter_start(play->net);
	play->playing = TRUE;
	gnome_dialog_set_sensitive(play->dia,0,FALSE);
	gnome_dialog_set_sensitive(play->dia,1,TRUE);
	gnome_dialog_set_sensitive(play->dia,2,TRUE);
}

static void pause_cb(GnomeDialog * dia, play_struct_t * play)
{
	if(FILTER_IS_LAUNCHED(play->net)){
		// FIXME .. C sux. I don't get it 
		//if(FILTER_IS_RUNNING(play->net))
		if(play->playing){
			filter_pause(play->net);
			play->playing = FALSE;
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
		gnome_dialog_set_sensitive(play->dia,0,TRUE);
		gnome_dialog_set_sensitive(play->dia,1,FALSE);
		gnome_dialog_set_sensitive(play->dia,2,FALSE);
	}
}

static void cancel_cb(GnomeDialog *dia, play_struct_t *play)
{
	if(FILTER_IS_LAUNCHED(play->net)){
		filter_terminate(play->net);
		filter_delete(play->net);
	}
	gnome_dialog_close(play->dia);
	gtk_object_destroy(GTK_OBJECT(play->dia));
}

int glame_gui_play_network(filter_t * network, gui_network * gui_net)
{
	/* FIXME The window should close itself after the network finishes. 
	         If a canvas is present it should set the error states of the items
		 aso... blabla 
	*/
	
	GnomeDialog * dialog;
	play_struct_t* play;
	/* sanity checks */
	
	if(!network){
		DPRINTF("NULL pointer as network!\n");
		return -1;
	}
	
	if(!FILTER_IS_NETWORK(network)){
		DPRINTF("Trying to play non-network!\n");
		return -1;
	}
	play = malloc(sizeof(play_struct_t));
	
	dialog = gnome_dialog_new(filter_name(network),NULL);

	play->net = network;
	play->dia = dialog;
	play->gui = gui_net;
	play->playing = FALSE;

	gnome_dialog_append_button_with_pixmap(dialog,"Play",GNOME_STOCK_PIXMAP_FORWARD);
	gnome_dialog_append_button_with_pixmap(dialog,"Pause",GNOME_STOCK_PIXMAP_TIMER_STOP);
	gnome_dialog_append_button_with_pixmap(dialog,"Stop",GNOME_STOCK_PIXMAP_STOP);	
	gnome_dialog_append_button(dialog,GNOME_STOCK_BUTTON_CANCEL);
	
	gnome_dialog_set_default(dialog,0);
	gnome_dialog_set_sensitive(dialog,1,FALSE);
	gnome_dialog_set_sensitive(dialog,2,FALSE);

	gnome_dialog_button_connect(dialog,0,play_cb,play);
	gnome_dialog_button_connect(dialog,1,pause_cb,play);
	gnome_dialog_button_connect(dialog,2,stop_cb,play);
	gnome_dialog_button_connect(dialog,3,cancel_cb,play);
	

	gtk_widget_show(dialog);
	
	
}
