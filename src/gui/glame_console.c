/*
 * glame_console.c
 *
 * $Id: glame_console.c,v 1.5 2001/07/13 09:01:43 richi Exp $
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
#include <guile/gh.h>
#include <libguile/ports.h>
#include "util.h"
#include "glscript.h"
#include "glame_console.h"


static GtkWidget *console = NULL;
static GtkWidget *text = NULL;
static GdkColor fore, back;
struct history_entry {
	struct list_head list;
	char *cmd;
};
static LIST_HEAD(history_list);
static int history_cnt = 0;
static struct history_entry *history = NULL;
static char *history_edited = NULL;

static void port_write(SCM port, void *data, size_t size)
{
	glame_console_printf("%.*s", (int)size, data);
}
static void port_register()
{
	SCM s_port;
	long port;
	scm_port *pt;

	/* Register glame-console port type (output only) */
	port = scm_make_port_type("glame-console", NULL, port_write);

	/* Create new port - shamelessly copied from libguile/strports.c */
	SCM_NEWCELL(s_port);
	SCM_DEFER_INTS;
	pt = scm_add_to_port_table(s_port);
#ifdef SCM_SET_CELL_TYPE /* guile >= 1.4 */
	SCM_SET_CELL_TYPE(s_port, port|SCM_WRTNG|SCM_OPN);
#else
	SCM_SETCAR(s_port, port|SCM_WRTNG|SCM_OPN);
#endif
	SCM_SETPTAB_ENTRY(s_port, pt);
	pt->rw_random = 0;
	SCM_ALLOW_INTS;

	scm_set_current_output_port(s_port);
	scm_set_current_error_port(s_port);
}

static gint hide_cb(GtkWidget *window, GdkEventAny *event)
{
	gtk_widget_hide(window);
	return TRUE;
}

static gboolean entry_cb(GtkWidget *entry, GdkEventKey *event)
{
	char *cmd;
	SCM s_res;
	struct history_entry *hentry;

	if (event->type != GDK_KEY_PRESS)
		return FALSE;

	if (event->keyval == GDK_KP_Enter
	    || event->keyval == GDK_ISO_Enter
	    || event->keyval == GDK_3270_Enter
	    || event->keyval == GDK_Return) {
		/* Catch ENTER - execute command. */
		cmd = gtk_entry_get_text(GTK_ENTRY(entry));
		if (!cmd)
			return FALSE;

		/* add cmd to history and adjust current history pointer */
		hentry = ALLOC(struct history_entry);
		INIT_LIST_HEAD(&hentry->list);
		hentry->cmd = strdup(cmd);
		list_add(&hentry->list, &history_list);
		if (++history_cnt > 128) {
			hentry = list_gettail(&history_list,
					      struct history_entry, list);
			list_del(&hentry->list);
			free(hentry->cmd);
			free(hentry);
		}
		history = NULL;
		if (history_edited) {
			free(history_edited);
			history_edited = NULL;
		}

		glame_console_printf("GLAME> %s\n", cmd);
		s_res = glame_gh_safe_eval_str(cmd);
		if (!SCM_UNBNDP(s_res)
#ifdef SCM_EQ_P
		    && !SCM_EQ_P(s_res, SCM_UNSPECIFIED)
#else
		    && !(SCM_UNSPECIFIED==s_res)
#endif
			) {
			gh_display(s_res);
			gh_newline();
		}
		gtk_entry_set_text(GTK_ENTRY(entry), "");
		return FALSE;

	} else if (event->keyval == GDK_Up
		   || event->keyval == GDK_Down) {
		/* Catch history - remember edited text */
		cmd = gtk_entry_get_text(GTK_ENTRY(entry));
		if (!history || strcmp(cmd, history->cmd) != 0) {
			if (history_edited)
				free(history_edited);
			history_edited = strdup(cmd);
		}
		hentry = NULL;
		if (!history && event->keyval == GDK_Up)
			hentry = list_gethead(&history_list,
					      struct history_entry, list);
		else if (history && event->keyval == GDK_Up)
			hentry = list_getnext(&history_list, history,
					      struct history_entry, list);
		else if (history && event->keyval == GDK_Down)
			hentry = list_getprev(&history_list, history,
					      struct history_entry, list);
		if (hentry || event->keyval == GDK_Down)
			history = hentry;
		if (history)
			gtk_entry_set_text(GTK_ENTRY(entry), history->cmd);
		else if (event->keyval == GDK_Down)
			gtk_entry_set_text(GTK_ENTRY(entry), history_edited);
		gtk_signal_emit_stop_by_name(GTK_OBJECT(entry),
                                             "key_press_event");
		return TRUE;
	} else if (event->keyval == GDK_Tab) {
		/* Autocompletion */
		/* FIXME */
		gtk_signal_emit_stop_by_name(GTK_OBJECT(entry),
                                             "key_press_event");
		return TRUE;
	}

	return FALSE;
}

int glame_console_init()
{
	GtkWidget *scroll, *label, *entry, *vbox, *hbox;
	GdkColormap *colormap;

	if (console)
		return 0;

	console = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(console), "GLAME "VERSION" console");
	gtk_window_set_default_size(GTK_WINDOW(console), 400, 200);
	vbox = gtk_vbox_new(FALSE, 10);
	scroll = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_ALWAYS);
	text = gtk_text_new(NULL, NULL);
	gtk_text_set_editable(GTK_TEXT(text), FALSE);
	hbox = gtk_hbox_new(FALSE, 10);
	label = gtk_label_new("GLAME>");
	entry = gtk_entry_new();

	gtk_signal_connect(GTK_OBJECT(entry), "key_press_event",
			   (GtkSignalFunc)entry_cb, NULL);
	gtk_signal_connect(GTK_OBJECT(console), "delete_event",
			   (GtkSignalFunc)hide_cb, NULL);
	gtk_signal_connect(GTK_OBJECT(console), "destroy_event",
			   (GtkSignalFunc)hide_cb, NULL);

	gtk_container_add(GTK_CONTAINER(scroll), text);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 5);
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 5);
	gtk_box_pack_start(GTK_BOX(vbox), scroll, TRUE, TRUE, 5);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 5);
	gtk_container_add(GTK_CONTAINER(console), vbox);
	gtk_widget_show(label);
	gtk_widget_show(entry);
	gtk_widget_show(hbox);
	gtk_widget_show(vbox);
	gtk_widget_show(text);
	gtk_widget_show(scroll);

	colormap = gdk_colormap_get_system();
	gdk_color_white(colormap, &back);
	gdk_color_black(colormap, &fore);

	port_register();

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
