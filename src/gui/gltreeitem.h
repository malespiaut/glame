#ifndef _GLTREEITEM_H
#define _GLTREEITEM_H

/*
 * gltreeitem.h
 *
 * $Id: gltreeitem.h,v 1.11 2003/05/26 20:50:25 richi Exp $
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

#define GTK_ENABLE_BROKEN
#include <gtk/gtktree.h>
#include <gtk/gtktreeitem.h>
#undef GTK_ENABLE_BROKEN
#include "glsignal.h"
#include "gpsm.h"


#define GLAME_TYPE_TREE_ITEM (glame_tree_item_get_type())
#define GLAME_TREE_ITEM(obj) (GTK_CHECK_CAST((obj), GLAME_TYPE_TREE_ITEM, GlameTreeItem))
#define GLAME_TREE_ITEM_CLASS(klass) (GTK_CHECK_CLASS_CAST((klass), GLAME_TYPE_TREE_ITEM, GlameTreeItemClass))
#define GLAME_IS_TREE_ITEM(obj) (GTK_CHECK_TYPE((obj), GLAME_TYPE_TREE_ITEM))
#define GLAME_IS_TREE_ITEM_CLASS(klass) (GTK_CHECK_CLASS_TYPE((klass), GLAME_TYPE_TREE_ITEM))

typedef struct _GlameTreeItem GlameTreeItem;
typedef struct _GlameTreeItemClass GlameTreeItemClass;

struct _GlameTreeItem {
	GtkTreeItem parent_object;

	/* internal linkage */
	GtkTree *tree;

	/* gpsm linkage and our signal handler */
	gpsm_item_t *item;
	glsig_handler_t *handler;

	/* widget stuff */
	GtkWidget *hbox;
	GtkLabel *label;
	GtkAdjustment *pos_adj;
};

struct _GlameTreeItemClass {
	GtkTreeItemClass parent_class;

	/* no own signals */
};


GtkType    glame_tree_item_get_type(void);
GtkWidget* glame_tree_item_new(gpsm_item_t *item);
void       glame_tree_item_update(GlameTreeItem *item);
GtkTree*   glame_tree_item_parent(GlameTreeItem *item);

void       glame_tree_append(GtkObject *tree, GlameTreeItem *item);
void       glame_tree_insert(GtkObject *tree, GlameTreeItem *item, gint pos);

/* Remove the glame tree item from the tree and destroy the widget
 * (we cannot do this w/o destroying it -- gtk is broken). */
void       glame_tree_remove(GlameTreeItem *item);

GlameTreeItem *glame_tree_find_gpsm_item(GtkObject *tree, gpsm_item_t *item);


#endif
