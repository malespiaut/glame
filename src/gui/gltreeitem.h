#ifndef _GLTREEITEM_H
#define _GLTREEITEM_H

/*
 * gltreeitem.h
 *
 * $Id: gltreeitem.h,v 1.1 2001/03/05 15:02:09 richi Exp $
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

#include <gtk/gtktreeitem.h>


#define GLAME_TYPE_TREE_ITEM (glame_tree_item_get_type())
#define GLAME_TREE_ITEM(obj) (GTK_CHECK_CAST((obj), GLAME_TYPE_TREE_ITEM, GlameTreeItem))
#define GLAME_TREE_ITEM_CLASS(klass) (GTK_CHECK_CLASS_CAST((klass), GLAME_TYPE_TREE_ITEM, GlameTreeItemClass))
#define GLAME_IS_TREE_ITEM(obj) (GTK_CHECK_TYPE((obj), GLAME_TYPE_TREE_ITEM))
#define GLAME_IS_TREE_ITEM_CLASS(klass) (GTK_CHECK_CLASS_TYPE((klass), GLAME_TYPE_TREE_ITEM))

typedef struct _GlameTreeItem GlameTreeItem;
typedef struct _GlameTreeItemClass GlameTreeItemClass;

struct _GlameTreeItem {
	GtkTreeItem parent_object;

	/* data - from glame_editor.c:struct gledit_buffer */
	char *name;
	long swapfile_name;
	int  sample_rate;
	long size; /* in samples, useful for consistency check with swapfile */
};

struct _GlameTreeItemClass {
	GtkTreeItemClass parent_class;

	/* no own signals */
};


GtkType glame_tree_item_get_type(void);
GtkWidget* glame_tree_item_new(void);
void glame_tree_item_update_label(GlameTreeItem *item);



#endif
