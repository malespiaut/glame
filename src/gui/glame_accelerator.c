/*
 * glame_accelerator.c
 *
 * $Id: glame_accelerator.c,v 1.14 2001/12/16 22:26:16 richi Exp $
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
#include <ctype.h>
#include <string.h>
#include <xmlmemory.h>
#include <parser.h>
#include <gdk/gdk.h>
#include <gnome.h>
#include "util.h"
#include "list.h"
#include "hash.h"
#include "glscript.h"
#include "glame_accelerator.h"


/* Note that accelerator handling is strictly single-threaded,
 * so no locking is required and state passing can be done via
 * global variables.
 */ 

struct accel_cb_data {
	GtkWidget *widget;
	char *scope;
};

GLAME_LIST_HEAD(_glame_accel_list);
static int stringhash(const char *spec);
HASH(accel, struct accel, 8,
     (strcmp(accel->spec, spec) == 0
      && accel->state == (state & accel->state_mask)),
     (stringhash(spec)),
     (stringhash(accel->spec)),
     const char *spec, guint state)


static SCM gls_glame_accel_edit_dialog_new(SCM s_scope, SCM s_edit);


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
	if (!(accel = hash_find_accel(spec, event->state)))
		return FALSE;

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

	gh_new_procedure2_0("glame-accel-edit-dialog",
			    gls_glame_accel_edit_dialog_new);

	return 0;
}

void glame_accel_sync()
{
	xmlDocPtr doc;
	xmlChar *xml;
	int size;
	char fname[256];
	FILE *f;

	snprintf(fname, 255, "%s/.glame-accels", getenv("HOME"));
	if (!(f = fopen(fname, "w")))
		return;

	doc = glame_accels_to_xml();
	xmlDocDumpMemory(doc, &xml, &size);
	fwrite(xml, size, 1, f);
	fclose(f);
	free(xml);
	xmlFreeDoc(doc);
}

static int add_accels(const char *scope, xmlNodePtr node)
{
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
			glame_accel_add(full_spec, state_mask, state, action);

		} else {
			/* with libxml2 we need to handle extra
  			 * whitespace. */
			if (strcmp(node->name, "text") != 0)
				return -1;
		}

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
	if ((doc = xmlParseMemory(xml, st.st_size))) {
		res = glame_add_accels_from_xml(doc);
		xmlFreeDoc(doc);
	} else
		res = -1;
	free(xml);
	fclose(f);

	return res;
}

static xmlNodePtr getScopeNode(xmlNodePtr root, const char *scope,
			       struct accel *accel)
{
	char accel_scope[256];
	char combined_scope[256];
	char *node_scope, *p;
	xmlNodePtr node;

	strncpy(accel_scope, accel->spec, 255);
	if ((p = strrchr(accel_scope, '/')))
		p[1] = '\0';
	if (strcmp(scope, accel_scope) == 0)
		return root;

#ifndef xmlChildrenNode
        node = root->childs;
#else
        node = root->xmlChildrenNode;
#endif
	while (node) {
		if (strcmp(node->name, "scope") == 0) {
			node_scope = xmlGetProp(node, "scope");
			if (!node_scope)
				return NULL;
			snprintf(combined_scope, 255,
				 "%s%s/", scope, node_scope);
			if (strncmp(combined_scope, accel_scope,
				    strlen(combined_scope)) == 0)
				break;
		} else
			/* Ignore */ ;

		node = node->next;
	}

	if (!node) {
		/* Need to create the scope node. */
		node = xmlNewChild(root, NULL, "scope", NULL);
		node_scope = &accel_scope[strlen(scope)];
		*strchr(node_scope, '/') = '\0';
		xmlSetProp(node, "scope", node_scope);
		snprintf(combined_scope, 255, "%s%s/", scope,
			 node_scope);
	}

	return getScopeNode(node, combined_scope, accel);
}

xmlDocPtr glame_accels_to_xml()
{
	xmlDocPtr doc;
	xmlNodePtr docroot, scope, entry;
	struct accel *accel, *dummy;
	char s[256];

	doc = xmlNewDoc("1.0");
	docroot = xmlNewNode(NULL, "glame-accels");
	glame_accel_safe_foreach(dummy, accel) {
		scope = getScopeNode(docroot, "", accel);
		entry = xmlNewChild(scope, NULL, "accel", NULL);
		snprintf(s, 255, "%i", accel->state);
		xmlSetProp(entry, "state", s);
		xmlSetProp(entry, "spec", strrchr(accel->spec, '/')+1);
		xmlNodeSetContent(entry, accel->action);
	}
	xmlDocSetRootElement(doc, docroot);

	return doc;
}


static void _free_accel(struct accel *accel)
{
	hash_remove_accel(accel);
	glame_list_del(&accel->list);
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
	GLAME_INIT_LIST_HEAD(&accel->list);
	accel->state_mask = state_mask;
	accel->state = state;
	accel->spec = strdup(spec);
	accel->action = strdup(action);

	if ((old = hash_find_accel(spec, state)))
		_free_accel(old);

	glame_list_add(&accel->list, &_glame_accel_list);
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
	glame_list_safe_foreach(&_glame_accel_list, struct accel, list, dummy, accel) {
		if (strncmp(accel->spec, scope, len) == 0)
			_free_accel(accel);
	}
}


guint glame_accel_install(GtkWidget *widget,
			  const char *scope, ...)
{
	struct accel_cb_data *data;
	va_list va;
	guint handler;

	if (!widget || !scope)
		return 0;

	va_start(va, scope);
	if (va_arg(va, const char *) != NULL) {
		va_end(va);
		return 0; /* FIXME: later... */
	}

	if (!(data = ALLOC(struct accel_cb_data)))
		return 0;
	data->widget = widget;
	data->scope = strdup(scope);

	handler = gtk_signal_connect(GTK_OBJECT(widget), "key_press_event",
				     (GtkSignalFunc)accel_cb, data);
	gtk_signal_connect(GTK_OBJECT(widget), "destroy",
			   (GtkSignalFunc)accel_cb_cleanup, data);

	return handler;
}


guint glame_accel_widget_data_cb(GtkWidget *widget, gpointer spec)
{
	struct accel *accel;

	if (!(accel = hash_find_accel(spec, 0)))
		return FALSE;

	glame_gh_safe_eval_str(accel->action);

	return TRUE;
}


GtkWidget *glame_accel_edit_widget(const char *scope, int edit)
{
	GtkWidget *sw, *clist;
	static char *labels[] = { "Scope", "Key", "Binding" };
	struct accel *accel, *dummy;

	clist = gtk_clist_new_with_titles(3, labels);
	gtk_clist_set_column_auto_resize(GTK_CLIST(clist), 0, TRUE);
	gtk_clist_set_column_auto_resize(GTK_CLIST(clist), 1, TRUE);
	gtk_clist_set_column_auto_resize(GTK_CLIST(clist), 2, TRUE);

	glame_accel_safe_foreach(dummy, accel) {
		char e_scope[1024];
		char e_key[1024];
		char *line[3], *p, *pp;

		if (strncmp(accel->spec, scope, strlen(scope)) != 0) {
			DPRINTF("Ignoring %s (for %s)\n", accel->spec, scope);
			continue;
		}

		/* Create scope */
		strncpy(e_scope, accel->spec + strlen(scope), 1024);
		if ((p = strrchr(e_scope, '/'))) {
			if (p[1] == '\0')
				p--;
		} else
			p = e_scope;
		*p = '\0';

		/* Create key */
		p = e_key;
		if (accel->state & GDK_SHIFT_MASK)
			p += sprintf(p, "SHIFT-");
		if (accel->state & GDK_CONTROL_MASK)
			p += sprintf(p, "CTRL-");
		if (accel->state & GDK_MOD1_MASK)
			p += sprintf(p, "MOD1-");
		if (accel->state & GDK_MOD2_MASK)
			p += sprintf(p, "MOD2-");
		if (accel->state & GDK_MOD3_MASK)
			p += sprintf(p, "MOD3-");
		if (accel->state & GDK_MOD4_MASK)
			p += sprintf(p, "MOD4-");
		if (accel->state & GDK_MOD5_MASK)
			p += sprintf(p, "MOD5-");
		if (!(pp = strrchr(accel->spec, '/')))
			pp = accel->spec-1;
		else {
			if (pp[1] == '\0')
				pp--;
		}
		sprintf(p, "%s", pp+1);

		line[0] = e_scope;
		line[1] = e_key;
		line[2] = accel->action;

		gtk_clist_append(GTK_CLIST(clist), line);
	}

	sw = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(sw), clist);
	gtk_widget_set_usize(sw, 500, 300);

	return sw;
}

GtkWidget *glame_accel_edit_dialog(const char *scope, int edit,
				   GtkWindow *parent)
{
	GtkWidget *dialog, *accel_widget;

	accel_widget = glame_accel_edit_widget(scope, edit);
	if (!accel_widget)
		return NULL;

	dialog = gtk_type_new(gnome_dialog_get_type());
	gnome_dialog_append_button_with_pixmap(
		GNOME_DIALOG(dialog), "Close", GNOME_STOCK_PIXMAP_CLOSE);
	gnome_dialog_set_sensitive(GNOME_DIALOG(dialog), 0, TRUE);
	if (parent)
		gnome_dialog_set_parent(GNOME_DIALOG(dialog), parent);

	gtk_container_add(GTK_CONTAINER(GNOME_DIALOG(dialog)->vbox), accel_widget);
	gtk_widget_show_all(accel_widget);

	return dialog;
}

static SCM gls_glame_accel_edit_dialog_new(SCM s_scope, SCM s_edit)
{
	char *scope;
	int len;
	SCM_ASSERT(gh_string_p(s_scope), s_scope, SCM_ARG1,
		   "glame-accel-edit-dialog-new");
	SCM_ASSERT(gh_boolean_p(s_edit), s_edit, SCM_ARG2,
		   "glame-accel-edit-dialog-new");

	scope = gh_scm2newstr(s_scope, &len);
	gnome_dialog_run_and_close(GNOME_DIALOG(
		glame_accel_edit_dialog(scope, gh_scm2bool(s_edit), NULL)));
	free(scope);

	return SCM_UNSPECIFIED;
}
