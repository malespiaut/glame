/*
 * gltreeitem.c
 *
 * $Id: gltreeitem.c,v 1.12 2001/05/05 14:36:13 richi Exp $
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

#include <sys/param.h>
#include <stdio.h>
#include <string.h>
#include <gnome.h>
#include "glame_types.h"
#include "swapfile.h"
#include "glmid.h"
#include "gltreeitem.h"

static void glame_tree_item_destroy(GtkObject *object)
{
	GlameTreeItem *item = GLAME_TREE_ITEM(object);

	if (item->handler)
		glsig_delete_handler(item->handler);
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
	item->tree = NULL;
	item->item = NULL;
	item->handler = NULL;
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

GtkWidget* glame_tree_item_new(gpsm_item_t *item)
{
	GlameTreeItem *itemw;

	itemw = gtk_type_new(glame_tree_item_get_type());
        itemw->item = (gpsm_item_t *)item;
	glame_tree_item_update(itemw);

	return GTK_WIDGET(itemw);
}


void glame_tree_item_update(GlameTreeItem *item)
{
	GtkBin *c;
	GtkWidget *l;
	char buf[256];

	/* Create the label out of the gpsm item data. */
	if (GPSM_ITEM_IS_GRP(item->item)) {
#ifdef DEBUG
		snprintf(buf, 255, "%s [%li %li %li %li]",
			 gpsm_item_label(item->item),
			 gpsm_item_hposition(item->item),
			 gpsm_item_vposition(item->item),
			 gpsm_item_hsize(item->item),
			 gpsm_item_vsize(item->item));
#else
		snprintf(buf, 255, "%s",
			 gpsm_item_label(item->item));
#endif
	} else if (GPSM_ITEM_IS_SWFILE(item->item)) {
		swfd_t fd = sw_open(gpsm_swfile_filename(item->item),
				    O_RDONLY);
		struct sw_stat st;
		long size = -1;
		if (fd != -1 && sw_fstat(fd, &st) != -1)
			size = st.size/SAMPLE_SIZE;
		sw_close(fd);
#ifdef DEBUG
		snprintf(buf, 255, "%s [%li] - %iHz, %.3fs [%li %li]",
			 gpsm_item_label(item->item),
			 gpsm_swfile_filename(item->item),
			 gpsm_swfile_samplerate(item->item),
			 (float)size/(float)gpsm_swfile_samplerate(item->item),
			 gpsm_item_hposition(item->item),
			 gpsm_item_hsize(item->item));
#else
		snprintf(buf, 255, "%s [%li] - %iHz, %.3fs",
			 gpsm_item_label(item->item),
			 gpsm_swfile_filename(item->item),
			 gpsm_swfile_samplerate(item->item),
			 (float)size/(float)gpsm_swfile_samplerate(item->item));
#endif
	}

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

GtkTree* glame_tree_item_parent(GlameTreeItem *item)
{
	if (!item)
		return NULL;

	return item->tree;
}


/*
 * Helpers for managing a tree of GlameTreeItems.
 */

GlameTreeItem *glame_tree_find_gpsm_item(GtkObject *t, gpsm_item_t *i)
{
	GList *childs;
	GlameTreeItem *item;
	GtkTree *tree;

	/* Simple check, is t == i? */
	if (GLAME_IS_TREE_ITEM(t)
	    && GLAME_TREE_ITEM(t)->item == i)
		return GLAME_TREE_ITEM(t);

	/* Handle both, GtkTree and group GlameTreeItem. */
	if (GLAME_IS_TREE_ITEM(t)
	    && GTK_TREE_ITEM_SUBTREE(t))
		tree = GTK_TREE(GTK_TREE_ITEM_SUBTREE(t));
	else if (GTK_IS_TREE(t))
		tree = GTK_TREE(t);
	else
		return NULL;

        /* First check direct children. */
	childs = gtk_container_children(GTK_CONTAINER(tree));
	while (childs) {
		item = GLAME_TREE_ITEM(childs->data);
		if (item->item == i)
			return item;
		childs = g_list_next(childs);
	}

        /* Then recurse. */
	childs = gtk_container_children(GTK_CONTAINER(tree));
	while (childs) {
		item = GLAME_TREE_ITEM(childs->data);
		if (GPSM_ITEM_IS_GRP(item->item)
                    && (item = glame_tree_find_gpsm_item(GTK_OBJECT(item), i)))
			return item;
		childs = g_list_next(childs);
	}

        return NULL;
}

void glame_tree_append(GtkObject *t, GlameTreeItem *item)
{
	GtkTree *tree;

	/* Handle both, GtkTree and group GlameTreeItem. */
	if (GLAME_IS_TREE_ITEM(t)
	    && GPSM_ITEM_IS_GRP(GLAME_TREE_ITEM(t)->item)) {
		if (!GTK_TREE_ITEM_SUBTREE(t))
			gtk_tree_item_set_subtree(GTK_TREE_ITEM(t),
						  gtk_tree_new());
		tree = GTK_TREE(GTK_TREE_ITEM_SUBTREE(t));
	} else if (GTK_IS_TREE(t))
		tree = GTK_TREE(t);
	else
		return;

	gtk_tree_append(tree, GTK_WIDGET(item));
	item->tree = tree;
}

void glame_tree_remove(GlameTreeItem *item)
{
	if (!item || !item->tree)
		return;
	gtk_container_remove(GTK_CONTAINER(item->tree), GTK_WIDGET(item));
}
