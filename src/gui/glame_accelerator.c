/*
 * glame_accelerator.c
 *
 * $Id: glame_accelerator.c,v 1.1 2001/06/04 16:02:50 richi Exp $
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

#include <string.h>
#include "util.h"
#include "list.h"
#include "hash.h"
#include "glscript.h"
#include "glame_accelerator.h"


/* Note that accelerator handling is strictly single-threaded,
 * so no locking is required and state passing can be done via
 * global variables.
 */ 

struct accel;
struct accel {
	struct accel **pprev_accel_hash;
	struct accel *next_accel_hash;
	struct list_head list;
	char *spec;
	char *action;
};

struct accel_cb_data {
	GtkWidget *widget;
	char *scope;
};

static LIST_HEAD(accel_list);
static int stringhash(const char *spec);
HASH(accel, struct accel, 8,
     (strcmp(accel->spec, spec) == 0),
     (stringhash(spec)),
     (stringhash(accel->spec)),
     const char *spec)

static GtkWidget *accel_widget;


/*
 * Helpers.
 */

static int stringhash(const char *spec)
{
	int val = 0;
	while (*spec)
		val += *(spec++);
	return val;
}

static SCM accel_get_widget()
{
	return gh_long2scm((long)accel_widget);
}

static SCM delete_widget(SCM s_widget)
{
	GtkWidget *widget = (GtkWidget *)gh_scm2long(s_widget);
	gtk_widget_destroy(widget);
}

static gint accel_cb(GtkWidget *widget, GdkEventKey *event,
		     struct accel_cb_data *data)
{
	struct accel *accel;
	char spec[256];

	snprintf(spec, 255, "%s/%x-%s",
		 data->scope,
		 event->state & (GDK_SHIFT_MASK|GDK_CONTROL_MASK|GDK_MOD1_MASK|GDK_MOD2_MASK),
		 gdk_keyval_name(event->keyval));
	DPRINTF("Event %s\n", spec);
	if (!(accel = hash_find_accel(spec))) {
		DPRINTF("No accelerator.\n");
		return FALSE;
	}

	DPRINTF("Found accelerator, will execute\n%s\n", accel->action);
	accel_widget = widget;
	glame_gh_safe_eval_str(accel->action);

	return TRUE;
}

static void accel_cb_cleanup(GtkObject *object, struct accel_cb_data *data)
{
	free(data->scope);
	free(data);
}


/*
 * API.
 */

int glame_accel_init()
{
	gh_new_procedure0_0("glame-accel-get-widget", accel_get_widget);
	gh_new_procedure1_0("glame-delete-widget", delete_widget);
	return 0;
}


int glame_accel_from_xml(const xmlDocPtr xml)
{
	/* FIXME */
	return -1;
}

xmlDocPtr glame_accel_to_xml()
{
	/* FIXME */
	return NULL;
}



int glame_accel_add(const char *spec, const char *action)
{
	struct accel *accel;

	if (!spec || !action)
		return -1;

	if (!(accel = ALLOC(struct accel)))
		return -1;
	hash_init_accel(accel);
	INIT_LIST_HEAD(&accel->list);
	accel->spec = strdup(spec);
	accel->action = strdup(action);

	if (hash_find_accel(spec))
		/* FIXME - delete found accel */ ;

	list_add(&accel->list, &accel_list);
	hash_add_accel(accel);

	return 0;
}

void glame_accel_del(const char *spec)
{
	/* FIXME */
}

void glame_accel_del_all(const char *scope)
{
	/* FIXME */
}


int glame_accel_install(GtkWidget *widget,
			const char *scope, ...)
{
	struct accel_cb_data *data;
	va_list va;

	if (!widget || !scope)
		return -1;

	va_start(va, scope);
	if (va_arg(va, const char *) != NULL) {
		va_end(va);
		return -1; /* FIXME: later... */
	}

	if (!(data = ALLOC(struct accel_cb_data)))
		return -1;
	data->widget = widget;
	data->scope = strdup(scope);

	gtk_signal_connect(GTK_OBJECT(widget), "key_press_event",
			   accel_cb, data);
	gtk_signal_connect(GTK_OBJECT(widget), "destroy",
			   accel_cb_cleanup, data);

	return 0;
}
