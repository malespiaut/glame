/*
 * glame_accelerator.c
 *
 * $Id: glame_accelerator.c,v 1.4 2001/06/11 08:41:46 richi Exp $
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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <xmlmemory.h>
#include <parser.h>
#include <gdk/gdk.h>
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
	guint state_mask;
	guint state;
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
     (strcmp(accel->spec, spec) == 0
      && accel->state == (state & accel->state_mask)),
     (stringhash(spec)),
     (stringhash(accel->spec)),
     const char *spec, guint state)



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

static gint accel_cb(GtkWidget *widget, GdkEventKey *event,
		     struct accel_cb_data *data)
{
	struct accel *accel;
	char spec[256];

	snprintf(spec, 255, "%s/%s",
		 data->scope,
		 gdk_keyval_name(event->keyval));
	if (!(accel = hash_find_accel(spec, event->state))) {
		DPRINTF("No accelerator for %s (state %x)\n",
			spec, event->state);
		return FALSE;
	}

	DPRINTF("Found accelerator for %s (state %x):\n%s\n",
		spec, event->state, accel->action);
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
	char fname[256];

	glame_add_accels_from_file(PKGDATADIR "/default-accels");
	glame_add_accels_from_file("./gui/default-accels");
	snprintf(fname, 255, "%s/.glame-accels", getenv("HOME"));
	glame_add_accels_from_file(fname);

	return 0;
}

static int add_accels(const char *scope, xmlNodePtr node)
{
	DPRINTF("Recursing for scope %s\n", scope);

#ifndef xmlChildrenNode
        node = node->childs;
#else
        node = node->xmlChildrenNode;
#endif
        if (!node)
		return 0;

	while (node) {
		if (strcmp(node->name, "scope") == 0) {
			char combined_scope[256];
			char *node_scope;

			node_scope = xmlGetProp(node, "scope");
			if (!node_scope)
				return -1;
			snprintf(combined_scope, 255,
				 "%s%s/", scope, node_scope);
			add_accels(combined_scope, node);

		} else if (strcmp(node->name, "accel") == 0) {
			char *spec, *action, *s;
			guint state, state_mask;
			char full_spec[256];

			state = 0;
			s = xmlGetProp(node, "state");
			if (s)
				if (sscanf(s, "%i", &state) != 1)
					return -1;
			state_mask = GDK_SHIFT_MASK|GDK_CONTROL_MASK
				|GDK_MOD1_MASK;
			s = xmlGetProp(node, "mask");
			if (s)
				if (sscanf(s, "%i", &state_mask) != 1)
					return -1;
			spec = xmlGetProp(node, "spec");
			if (!spec)
				return -1;
			action = xmlNodeGetContent(node);
			if (!action || strlen(action) == 0)
				return -1;

			snprintf(full_spec, 255, "%s%s", scope, spec);
			DPRINTF("Accel for %s (0x%x, 0x%x) is %s\n",
				full_spec, state_mask, state, action);
			glame_accel_add(full_spec, state_mask, state, action);

		} else
			return -1;

		node = node->next;
	}

	return 0;
}
int glame_add_accels_from_xml(const xmlDocPtr xml)
{
	char scope[8] = "";
	return add_accels(scope, xmlDocGetRootElement(xml));
}

int glame_add_accels_from_file(const char *filename)
{
	int fd, res;
	FILE *f;
	struct stat st;
	char *xml;
	xmlDocPtr doc;

	if ((fd = open(filename, O_RDONLY)) == -1)
		return -1;
	fstat(fd, &st);

	if (!(f = fdopen(fd, "r"))) {
		close(fd);
		return -1;
	}
	xml = malloc(st.st_size+1);
	fread(xml, st.st_size, 1, f);
	xml[st.st_size] = '\0';
	DPRINTF("Processing file %s\n", filename);
	if ((doc = xmlParseMemory(xml, st.st_size))) {
		res = glame_add_accels_from_xml(doc);
		xmlFreeDoc(doc);
	} else
		res = -1;
	free(xml);
	fclose(f);

	return res;
}

xmlDocPtr glame_accels_to_xml()
{
	/* FIXME */
	return NULL;
}


static void _free_accel(struct accel *accel)
{
	hash_remove_accel(accel);
	list_del(&accel->list);
	free(accel->spec);
	free(accel->action);
	free(accel);
}

int glame_accel_add(const char *spec, guint state_mask, guint state,
		    const char *action)
{
	struct accel *accel, *old;

	if (!spec || !action)
		return -1;

	if (!(accel = ALLOC(struct accel)))
		return -1;
	hash_init_accel(accel);
	INIT_LIST_HEAD(&accel->list);
	accel->state_mask = state_mask;
	accel->state = state;
	accel->spec = strdup(spec);
	accel->action = strdup(action);

	if ((old = hash_find_accel(spec, state)))
		_free_accel(old);

	list_add(&accel->list, &accel_list);
	hash_add_accel(accel);

	return 0;
}

void glame_accel_del(const char *spec, guint state)
{
	struct accel *accel;

	if (!(accel = hash_find_accel(spec, state)))
		return;
	_free_accel(accel);
}

void glame_accel_del_all(const char *scope)
{
	struct accel *accel, *dummy;
	int len;

	len = strlen(scope);
	list_safe_foreach(&accel_list, struct accel, list, dummy, accel) {
		if (strncmp(accel->spec, scope, len) == 0)
			_free_accel(accel);
	}
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
