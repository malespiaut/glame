/*
 * gltreeitem.c
 *
 * $Id: gltreeitem.c,v 1.2 2001/03/12 09:41:51 richi Exp $
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

#include <stdio.h>
#include <string.h>
#include <gnome.h>
#include "gltreeitem.h"

static void glame_tree_item_destroy(GtkObject *object)
{
	GlameTreeItem *item = GLAME_TREE_ITEM(object);
	free(item->label);
	GTK_OBJECT_CLASS(gtk_type_class(GTK_TYPE_TREE_ITEM))->destroy(object);
}

static void glame_tree_item_class_init(GlameTreeItemClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = glame_tree_item_destroy;
}

static void glame_tree_item_init(GlameTreeItem *item)
{
	item->type = -1;
	item->label = NULL;
	item->swapfile_name = -1;
	item->sample_rate = -1;
	item->size = -1;
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

GtkWidget* glame_tree_item_new_file(const char *label, long swapfile_name,
				    int sample_rate, int size)
{
        GlameTreeItem *item;

        if (!(item = GLAME_TREE_ITEM(glame_tree_item_new())))
		return NULL;
        item->type = GLAME_TREE_ITEM_FILE;
	item->label = strdup(label);
        item->swapfile_name = swapfile_name;
        item->sample_rate = sample_rate;
        item->size = size;
	glame_tree_item_update(item);

	return GTK_WIDGET(item);
}

GtkWidget* glame_tree_item_new_group(const char *label)
{
        GlameTreeItem *item;

        if (!(item = GLAME_TREE_ITEM(glame_tree_item_new())))
		return NULL;
        item->type = GLAME_TREE_ITEM_GROUP;
	item->label = strdup(label);
	glame_tree_item_update(item);

	return GTK_WIDGET(item);
}

void glame_tree_item_update(GlameTreeItem *item)
{
	GtkBin *c;
	GtkWidget *l;

	/* Create the label out of the GlameTreeItem data. */
	char buf[256];
	if (item->type == GLAME_TREE_ITEM_GROUP)
		snprintf(buf, 255, "%s", item->label);
	else if (item->type == GLAME_TREE_ITEM_FILE)
		snprintf(buf, 255, "%s [%li] - %iHz, %.3fs",
			 item->label, item->swapfile_name, item->sample_rate,
			 (float)item->size/(float)item->sample_rate);

	/* Update/create the GtkLabel contained in the GtkBin
	 * (superclass of GtkItem/GtkTreeItem/GlameTreeItem) */
	/* FIXME - no method for this!? -> access child directly */
	c = GTK_BIN(item);
	if (!c->child) {
		l = gtk_label_new(buf);
		gtk_label_set_justify(GTK_LABEL(l), GTK_JUSTIFY_LEFT);
		gtk_container_add(GTK_CONTAINER(item), l);
		gtk_widget_show(l);
	} else
		gtk_label_set_text(GTK_LABEL(c->child), buf);
}



/*
 * Helpers for managing a tree of GlameTreeItems.
 */


GlameTreeItem* glame_tree_find_group(GtkObject *t, const char *label)
{
	GList *childs;
	GlameTreeItem *item;
	GtkTree *tree;

	/* Handle both, GtkTree and group GlameTreeItem. */
	if (GLAME_IS_TREE_ITEM(t)
	    && GLAME_TREE_ITEM(t)->type == GLAME_TREE_ITEM_GROUP)
		tree = GTK_TREE(GTK_TREE_ITEM_SUBTREE(GTK_TREE_ITEM(t)));
	else if (GTK_IS_TREE(t))
		tree = GTK_TREE(t);
	else
		return NULL;

        /* First check direct children. */
	childs = gtk_container_children(GTK_CONTAINER(tree));
	while (childs) {
		item = GLAME_TREE_ITEM(childs->data);
		if (item->type == GLAME_TREE_ITEM_GROUP
		    && strcmp(label, item->label) == 0)
			return item;
		childs = g_list_next(childs);
	}

        /* Then recurse. */
	childs = gtk_container_children(GTK_CONTAINER(tree));
	while (childs) {
		item = GLAME_TREE_ITEM(childs->data);
		if (item->type == GLAME_TREE_ITEM_GROUP
                    && (item = glame_tree_find_group(GTK_OBJECT(item), label)))
			return item;
		childs = g_list_next(childs);
	}

        return NULL;
}

GlameTreeItem* glame_tree_find_filename(GtkObject *t, long name)
{
	GList *childs;
	GlameTreeItem *item;
	GtkTree *tree;

	/* Handle both, GtkTree and group GlameTreeItem. */
	if (GLAME_IS_TREE_ITEM(t)
	    && GLAME_TREE_ITEM(t)->type == GLAME_TREE_ITEM_GROUP)
		tree = GTK_TREE(GTK_TREE_ITEM_SUBTREE(GTK_TREE_ITEM(t)));
	else if (GTK_IS_TREE(t))
		tree = GTK_TREE(t);
	else
		return NULL;

        /* First check direct children. */
	childs = gtk_container_children(GTK_CONTAINER(tree));
	while (childs) {
		item = GLAME_TREE_ITEM(childs->data);
		if (item->type == GLAME_TREE_ITEM_FILE
                    && item->swapfile_name == name)
			return item;
		childs = g_list_next(childs);
	}

        /* Then recurse. */
	childs = gtk_container_children(GTK_CONTAINER(tree));
	while (childs) {
		item = GLAME_TREE_ITEM(childs->data);
		if (item->type == GLAME_TREE_ITEM_GROUP
                    && (item = glame_tree_find_filename(GTK_OBJECT(item), name)))
			return item;
		childs = g_list_next(childs);
	}

        return NULL;
}

GtkObject* glame_tree_copy(GtkObject *t)
{
	/* Handle both, GtkTree and group GlameTreeItem. */
	if (GLAME_IS_TREE_ITEM(t)) {
		GlameTreeItem *item = GLAME_TREE_ITEM(t);
		GtkTree *tree;
		if (item->type == GLAME_TREE_ITEM_FILE)
			return GTK_OBJECT(
				glame_tree_item_new_file(item->label,
				     item->swapfile_name, item->sample_rate,
				     item->size));
		item = GLAME_TREE_ITEM(glame_tree_item_new_group(item->label));
		tree = GTK_TREE(glame_tree_copy(GTK_OBJECT(GTK_TREE_ITEM_SUBTREE(item))));
		gtk_tree_item_set_subtree(GTK_TREE_ITEM(item),
					  GTK_WIDGET(tree));
		return GTK_OBJECT(item);

	} else if (GTK_IS_TREE(t)) {
		GtkWidget *tree = GTK_WIDGET(t);
		GList *children = gtk_container_children(GTK_CONTAINER(tree));
		tree = gtk_tree_new();
		while (children) {
			GlameTreeItem *item = GLAME_TREE_ITEM(children->data);
			item = GLAME_TREE_ITEM(glame_tree_copy(GTK_OBJECT(item)));
			gtk_tree_append(GTK_TREE(tree), GTK_WIDGET(item));
			children = g_list_next(children);
		}
		return GTK_OBJECT(tree);
	}

	return NULL;
}

void glame_tree_append(GtkObject *t, GlameTreeItem *item)
{
	GtkTree *tree;

	/* Handle both, GtkTree and group GlameTreeItem. */
	if (GLAME_IS_TREE_ITEM(t)
	    && GLAME_TREE_ITEM(t)->type == GLAME_TREE_ITEM_GROUP) {
		if (!GTK_TREE_ITEM_SUBTREE(t))
			gtk_tree_item_set_subtree(GTK_TREE_ITEM(t),
						  gtk_tree_new());
		tree = GTK_TREE(GTK_TREE_ITEM_SUBTREE(t));
	} else if (GTK_IS_TREE(t))
		tree = GTK_TREE(t);
	else
		return;

	gtk_tree_append(tree, GTK_WIDGET(item));
	gtk_widget_show(GTK_WIDGET(item));
}
