#ifndef _GLAME_ACCELERATOR_H
#define _GLAME_ACCELERATOR_H

/*
 * glame_accelerator.h
 *
 * $Id: glame_accelerator.h,v 1.10 2001/12/16 16:14:01 richi Exp $
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

#include <gtk/gtk.h>
#include <xmlmemory.h>
#include <tree.h>
#include "list.h"


/* Initialize the accelerator subsystem. Returns 0 on success, -1
 * on error. */
int glame_accel_init();

/* Sync the accelerator table to disk (~.glame-accels). */
void glame_accel_sync();


/* Constructs the accelerator table from the provided xml document.
 * Returns 0 on success, -1 on error. */
int glame_add_accels_from_xml(const xmlDocPtr xml);

/* Adds all accelerators found in the specified file. Returns 0
 * on success, -1 on error. */
int glame_add_accels_from_file(const char *filename);

/* Constructs an xml document out of the accelerator table.
 * Returns the document on success, NULL on error. You have to free
 * the document yourself. */
xmlDocPtr glame_accels_to_xml();


/* Adds the binding spec -> action to the accelerator table replacing
 * an already existing one. Returns 0 on success, -1 on error. */
int glame_accel_add(const char *spec, guint state_mask, guint state,
		    const char *action);

/* Deletes all bindings to spec from the accelerator table. */
void glame_accel_del(const char *spec, guint state);

/* Deletes all bindings to specifications inside the specified scope. */
void glame_accel_del_all(const char *scope);


struct accel;
struct accel {
	struct accel **pprev_accel_hash;
	struct accel *next_accel_hash;
	struct glame_list_head list;
	guint state_mask;
	guint state;
	char *spec;
	char *action;
};

/* Iterates (safe to delete actual item) through all available
 * bindings. */
extern struct glame_list_head _glame_accel_list;
#define glame_accel_safe_foreach(dummy, entry) glame_list_safe_foreach(&_glame_accel_list, struct accel, list, dummy, entry)


/* Installs a gtk signal handler to the specified widget which binds
 * to the accelerators inside the specified scope(s). 
 * Returns the gtk signal identifier on success, 0 on error. */
guint glame_accel_install(GtkWidget *widget,
			  const char *scope, ...);



/* Accelerator edit/list widget. Operations are restricted to the
 * specified scope. Pass TRUE to allow editing, FALSE sets read-only. */
GtkWidget *glame_accel_edit_widget(const char *scope, int edit);

/* Accelerator edit/list dialog. See glame_accel_edit_widget for
 * options. Parent is the dialog parent window. */
GtkWidget *glame_accel_edit_dialog(const char *scope, int edit,
				   GtkWindow *parent);


#endif
