/*
 * waveeditgui.h
 *
 * Copyright (C) 2001, 2002 Richard Guenther
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

#ifndef _WAVEEDITGUI_H
#define _WAVEEDITGUI_H

#include <gtk/gtk.h>
#include <gnome.h>
#include "libgtkwaveform/gtkwaveview.h"
#include "libgtkwaveform/gtkswapfilebuffer.h"
#include "filter.h"
#include "gpsm.h"

G_BEGIN_DECLS

struct _WaveeditGuiClass;
struct _WaveeditGui;
typedef struct _WaveeditGuiClass WaveeditGuiClass;
typedef struct _WaveeditGui WaveeditGui;

#define WAVEEDIT_GUI_TYPE (waveedit_gui_get_type())
#define WAVEEDIT_GUI(object) (GTK_CHECK_CAST((object), WAVEEDIT_GUI_TYPE, WaveeditGui))
#define WAVEEDIT_GUI_CLASS(object) (GTK_CHECK_CLASS_CAST((object), WAVEEDIT_GUI_TYPE, WaveeditGuiClass))
#define IS_WAVEEDIT_GUI(object) (GTK_CHECK_TYPE((object), WAVEEDIT_GUI_TYPE))
#define IS_WAVEEDIT_GUI_CLASS(object) (GTK_CHECK_CLASS_TYPE((object), WAVEEDIT_GUI_TYPE))
#define WAVEEDIT_GUI_GET_CLASS(object) ((WaveeditGuiClass*) (((GtkObject*) (obj))->klass))

struct _WaveeditGuiClass {
	GnomeAppClass parent_class;
};

struct _WaveeditGui {
	GnomeApp parent_object;

	gpsm_item_t *root;
	gpsm_grp_t *swfiles;
	GtkWidget *waveview;
	GtkWidget *playbutton, *stopbutton;
	GtkWidget *rec_stopped_button, *rec_live_button;
	GtkSwapfileBuffer *wavebuffer;
	GtkWidget *toolbar;

	/* wether we need to block further operations (async. one in work) */
	int locked;

	/* state for playmarker functionality */
	filter_t *pm_net, *pm_ain;
	filter_param_t *pm_param;
	long pm_start;
	long pm_size;
	long pm_marker;
	gboolean pm_loop;

	/* keep loop and scroll button state */
	int loop_play;
	int scroll_play;
	
	/* flag, if we are modified */
	int modified;
};



/* Initializes the waveeditgui subsystem. */
void glame_waveeditgui_init();


GtkType waveedit_gui_get_type();


/* Create a new waveedit widget out of a gpsm-item which can be
 * either a gpsm-grp or a gpsm-swfile. All gpsm-grp members have
 * to be gpsm-swfiles of the same hposition and hsize. */
WaveeditGui *glame_waveedit_gui_new(const char *title, gpsm_item_t *item);

G_END_DECLS
#endif
