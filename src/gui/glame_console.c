/*
 * glame_console.c
 *
 * $Id: glame_console.c,v 1.1 2001/06/11 08:44:12 richi Exp $
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
#include "util.h"
#include "glame_console.h"


static GtkWidget *console = NULL;
static GtkWidget *text = NULL;
static GdkColor fore, back;


static gint hide_cb(GtkWidget *window, GdkEventAny *event)
{
	gtk_widget_hide(window);
	return TRUE;
}


int glame_console_init()
{
	GtkWidget *scroll;
	GdkColormap *colormap;

	if (console)
		return 0;

	console = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(console), "GLAME "VERSION" console");
	gtk_window_set_default_size(GTK_WINDOW(console), 400, 200);
	scroll = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_ALWAYS);
	text = gtk_text_new(NULL, NULL);
	gtk_text_set_editable(GTK_TEXT(text), FALSE);

	gtk_signal_connect(GTK_OBJECT(console), "delete_event",
			   hide_cb, NULL);
	gtk_signal_connect(GTK_OBJECT(console), "destroy_event",
			   hide_cb, NULL);

	gtk_container_add(GTK_CONTAINER(scroll), text);
	gtk_container_add(GTK_CONTAINER(console), scroll);
	gtk_widget_show(text);
	gtk_widget_show(scroll);

	colormap = gdk_colormap_get_system();
	gdk_color_white(colormap, &back);
	gdk_color_black(colormap, &fore);

	return 0;
}

void glame_console_hide()
{
	if (console)
		gtk_widget_hide(console);
}

void glame_console_show()
{
	if (console)
		gtk_widget_show(console);
}

int glame_console_printf(const char *format, ...)
{
	char buf[1024];
	va_list va;
	int res;

	if (!text)
		return -1;

	va_start(va, format);
	res = vsnprintf(buf, 1023, format, va);
	buf[1023] = '\0';
	va_end(va);

	gtk_text_insert(GTK_TEXT(text), NULL, &fore, &back,
			buf, strlen(buf));

	return res;
}