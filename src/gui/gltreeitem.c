/*
 * gltreeitem.c
 *
 * $Id: gltreeitem.c,v 1.22 2003/04/11 22:18:16 xwolf Exp $
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
#include "math.h"

#include <sys/param.h>
#include <stdio.h>
#include <string.h>
#include <gnome.h>
#include "util/gtknob.h"
#include "glame_types.h"
#include "swapfile.h"
#include "glmid.h"
#include "gltreeitem.h"

static void glame_tree_item_destroy(GtkObject *object)
{
	GlameTreeItem *item = GLAME_TREE_ITEM(object);

	if (item->handler){
		glsig_delete_handler(item->handler);
		item->handler = NULL;
	}
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
	}

	return glame_tree_item_type;
}

static void update_pos(GtkAdjustment *adj, gpsm_swfile_t *swfile)
{
	gpsm_swfile_set_position(swfile, adj->value);
}

static gchar *knob_formatter(gfloat val, gpointer data)
{
	char buf[16];
	if ((int)(val*10) == (int)(FILTER_PIPEPOS_LEFT*10))
		return g_strdup("Left");
	else if ((int)(val*10) == (int)(FILTER_PIPEPOS_RIGHT*10))
		return g_strdup("Right");
	else if ((int)(val*10) == (int)(FILTER_PIPEPOS_CENTRE*10))
		return g_strdup("Centre");
	snprintf(buf, 16, "%.1f", val);
	return g_strdup(buf);
}

GtkWidget* glame_tree_item_new(gpsm_item_t *item)
{
	GlameTreeItem *itemw;

	itemw = gtk_type_new(glame_tree_item_get_type());
        itemw->item = (gpsm_item_t *)item;
	itemw->hbox = gtk_hbox_new(FALSE, 5);
	itemw->label = GTK_LABEL(gtk_label_new(NULL));
	gtk_misc_set_alignment(GTK_MISC(itemw->label), 0.0, 0.5);
	itemw->pos_adj = NULL;
	if (GPSM_ITEM_IS_SWFILE(item)) {
		GtkWidget *slider;
		itemw->pos_adj = GTK_ADJUSTMENT(
			gtk_adjustment_new(gpsm_swfile_position(item),
					   -M_PI, M_PI,
					   M_PI/8.0, M_PI/2.0, 0.0));
#if 0
		slider = gtk_hscale_new(itemw->pos_adj);
		gtk_scale_set_value_pos(GTK_SCALE(slider), GTK_POS_RIGHT);
		gtk_scale_set_digits(GTK_SCALE(slider), 2);
		gtk_container_add(GTK_CONTAINER(itemw->hbox), slider);
#else
		slider = gtk_knob_new(itemw->pos_adj);
		gtk_knob_set_formatter(GTK_KNOB(slider), knob_formatter, NULL);
		gtk_knob_add_tick(GTK_KNOB(slider), FILTER_PIPEPOS_LEFT);
		gtk_knob_add_tick(GTK_KNOB(slider), FILTER_PIPEPOS_RIGHT);
		gtk_knob_add_tick(GTK_KNOB(slider), FILTER_PIPEPOS_CENTRE);
		gtk_box_pack_start(GTK_BOX(itemw->hbox), slider, FALSE, FALSE, 5);
#endif
		gtk_signal_connect(GTK_OBJECT(itemw->pos_adj), "value_changed",
				   update_pos, item);
	}
	gtk_container_add(GTK_CONTAINER(itemw->hbox), GTK_WIDGET(itemw->label));
	gtk_container_add(GTK_CONTAINER(itemw), itemw->hbox);
	gtk_widget_show_all(itemw->hbox);

	glame_tree_item_update(itemw);

	return GTK_WIDGET(itemw);
}


void glame_tree_item_update(GlameTreeItem *item)
{
	char buf[256];

	/* Create the label out of the gpsm item data. */
	if (GPSM_ITEM_IS_GRP(item->item)) {
		if (strcmp(gpsm_item_label(item->item), GPSM_GRP_UNRECOGNIZED_LABEL) == 0)
			snprintf(buf, 255, "%s", GPSM_GRP_UNRECOGNIZED_LABEL);
		else if (strcmp(gpsm_item_label(item->item), GPSM_GRP_DELETED_LABEL) == 0)
			snprintf(buf, 255, "%s", GPSM_GRP_DELETED_LABEL);
		else if (gpsm_item_vsize(item->item) == 0)
			snprintf(buf, 255, "%s",
				 gpsm_item_label(item->item));
		else {
			int hbox, vbox;
			hbox = gpsm_grp_is_hbox((gpsm_grp_t *)item->item);
			vbox = gpsm_grp_is_vbox((gpsm_grp_t *)item->item);
			snprintf(buf, 255, "%s - %li tracks, %li samples - (%li, %li)%s",
				 gpsm_item_label(item->item),
				 gpsm_item_vsize(item->item),
				 gpsm_item_hsize(item->item),
				 gpsm_item_hposition(item->item),
				 gpsm_item_vposition(item->item),
				 hbox && !vbox ? " [HBOX]"
				 : !hbox && vbox ? " [VBOX]"
				 : hbox && vbox ? " [BOX]" : " [XXXX]");
		}
	} else if (GPSM_ITEM_IS_SWFILE(item->item)) {
		swfd_t fd = sw_open(gpsm_swfile_filename(item->item),
				    O_RDONLY);
		struct sw_stat st;
		long size = -1;
		if (fd != -1 && sw_fstat(fd, &st) != -1)
			size = st.size/SAMPLE_SIZE;
		sw_close(fd);
		snprintf(buf, 255, "%s - %iHz, %.3fs - (%.3fs, %li)",
			 gpsm_item_label(item->item),
			 gpsm_swfile_samplerate(item->item),
			 (float)size/(float)gpsm_swfile_samplerate(item->item),
			 (float)gpsm_item_hposition(item->item)/(float)gpsm_swfile_samplerate(item->item),
			 gpsm_item_vposition(item->item));
	}

	/* Update/create the GtkLabel contained in the GtkBin
	 * (superclass of GtkItem/GtkTreeItem/GlameTreeItem) */
	/* FIXME - no method for this!? -> access child directly */
	gtk_label_set_text(GTK_LABEL(item->label), buf);
	if (GPSM_ITEM_IS_SWFILE(item->item)) {
		gtk_adjustment_set_value(item->pos_adj,
					 gpsm_swfile_position(item->item));
	}
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

void glame_tree_insert(GtkObject *t, GlameTreeItem *item, gint pos)
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

	gtk_tree_insert(tree, GTK_WIDGET(item), pos);
	item->tree = tree;
}

void glame_tree_remove(GlameTreeItem *item)
{
	if (!item || !item->tree)
		return;
	gtk_container_remove(GTK_CONTAINER(item->tree), GTK_WIDGET(item));
}
