/*
 * gltreeitem.c
 *
 * $Id: gltreeitem.c,v 1.1 2001/03/05 15:02:09 richi Exp $
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

#include "gltreeitem.h"


static void glame_tree_item_class_init(GlameTreeItemClass *class)
{
	/* ? */
}

static void glame_tree_item_init(GlameTreeItem *item)
{
	/* defaults? */
}

GtkType glame_tree_item_get_type(void)
{
	static GtkType glame_tree_item_type = 0;

	if (!glame_tree_item_type) {
		GtkTypeInfo glame_tree_item_info = {
			"GlameTreeItem",
			sizeof(GlameTreeItem),
			sizeof(GlameTreeItemClass),
			(GtkClassInitFunc)glame_tree_item_class_init,
			(GtkObjectInitFunc)glame_tree_item_init,
			NULL, NULL, (GtkClassInitFunc)NULL,
		};
		glame_tree_item_type = gtk_type_unique(GTK_TYPE_TREE_ITEM,
						       &glame_tree_item_info);
		gtk_type_set_chunk_alloc(glame_tree_item_type, 8);
	}

	return glame_tree_item_type;
}

GtkWidget* glame_tree_item_new(void)
{
	GlameTreeItem *item;
	item = gtk_type_new(glame_tree_item_get_type());
	return GTK_WIDGET(item);
}

void glame_tree_item_update_label(GlameTreeItem *item)
{
	GtkBin *c;

	/* Create the label out of the GlameTreeItem data. */
	char buf[256];
	snprintf(buf, 255, "FIXME");

	/* Update/create the GtkLabel contained in the GtkBin
	 * (superclass of GtkItem/GtkTreeItem/GlameTreeItem) */
	/* FIXME - no method for this!? -> access child directly */
	c = GTK_BIN(item);
	if (!c->child)
		c->child = gtk_label_new(strdup(buf));
	else
		gtk_label_set_text(GTK_LABEL(c->child), strdup(buf));
	/* FIXME - need to raise some "update" signal? */
}
