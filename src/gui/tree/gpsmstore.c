/*
 * gpsmstore.c
 *
 * Copyright (C) 2004 Richard Guenther
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>
#include <string.h>
#include "gpsmstore.h"
#include "swapfile.h"
#include "glame_types.h"

static void glame_gpsm_store_init(GlameGpsmStore * tree_store);
static void glame_gpsm_store_class_init(GlameGpsmStoreClass * tree_store_class);
static void glame_gpsm_store_tree_model_init(GtkTreeModelIface * iface);
#if 0
static void gtk_tree_store_drag_source_init(GtkTreeDragSourceIface * iface);
static void gtk_tree_store_drag_dest_init(GtkTreeDragDestIface * iface);
#endif
static void glame_gpsm_store_finalize(GObject * object);

/* Tree model interface.  */

static GtkTreeModelFlags glame_gpsm_store_get_flags(GtkTreeModel * tree_model);
static gint glame_gpsm_store_get_n_columns(GtkTreeModel * tree_model);
static GType glame_gpsm_store_get_column_type(GtkTreeModel * tree_model,
					      gint index);
static gboolean glame_gpsm_store_get_iter(GtkTreeModel * tree_model,
					  GtkTreeIter * iter,
					  GtkTreePath * path);
static GtkTreePath *glame_gpsm_store_get_path(GtkTreeModel * tree_model,
					      GtkTreeIter * iter);
static void glame_gpsm_store_get_value(GtkTreeModel * tree_model,
				       GtkTreeIter * iter,
				       gint column, GValue * value);
static gboolean glame_gpsm_store_iter_next(GtkTreeModel * tree_model,
					   GtkTreeIter * iter);
static gboolean glame_gpsm_store_iter_children(GtkTreeModel * tree_model,
					       GtkTreeIter * iter,
					       GtkTreeIter * parent);
static gboolean glame_gpsm_store_iter_has_child(GtkTreeModel * tree_model,
						GtkTreeIter * iter);
static gint glame_gpsm_store_iter_n_children(GtkTreeModel * tree_model,
					     GtkTreeIter * iter);
static gboolean glame_gpsm_store_iter_nth_child(GtkTreeModel * tree_model,
						GtkTreeIter * iter,
						GtkTreeIter * parent, gint n);
static gboolean glame_gpsm_store_iter_parent(GtkTreeModel * tree_model,
					     GtkTreeIter * iter,
					     GtkTreeIter * child);

#if 0
/* DND interfaces */
static gboolean real_gtk_tree_store_row_draggable(GtkTreeDragSource *
						  drag_source,
						  GtkTreePath * path);
static gboolean gtk_tree_store_drag_data_delete(GtkTreeDragSource * drag_source,
						GtkTreePath * path);
static gboolean gtk_tree_store_drag_data_get(GtkTreeDragSource * drag_source,
					     GtkTreePath * path,
					     GtkSelectionData * selection_data);
static gboolean gtk_tree_store_drag_data_received(GtkTreeDragDest * drag_dest,
						  GtkTreePath * dest,
						  GtkSelectionData *
						  selection_data);
static gboolean gtk_tree_store_row_drop_possible(GtkTreeDragDest * drag_dest,
						 GtkTreePath * dest_path,
						 GtkSelectionData *
						 selection_data);
#endif

static GObjectClass *parent_class = NULL;

GType glame_gpsm_store_get_type(void)
{
	static GType gpsm_store_type = 0;

	if (!gpsm_store_type) {
		static const GTypeInfo gpsm_store_info = {
			sizeof(GlameGpsmStoreClass),
			NULL,	/* base_init */
			NULL,	/* base_finalize */
			(GClassInitFunc) glame_gpsm_store_class_init,
			NULL,	/* class_finalize */
			NULL,	/* class_data */
			sizeof(GlameGpsmStore),
			0,	/* n_preallocs */
			(GInstanceInitFunc) glame_gpsm_store_init
		};

		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) glame_gpsm_store_tree_model_init,
			NULL,
			NULL
		};

#if 0
		static const GInterfaceInfo drag_source_info = {
			(GInterfaceInitFunc) gtk_tree_store_drag_source_init,
			NULL,
			NULL
		};

		static const GInterfaceInfo drag_dest_info = {
			(GInterfaceInitFunc) gtk_tree_store_drag_dest_init,
			NULL,
			NULL
		};

#endif

		gpsm_store_type =
		    g_type_register_static(G_TYPE_OBJECT, "GlameGpsmStore",
					   &gpsm_store_info, 0);

		g_type_add_interface_static(gpsm_store_type,
					    GTK_TYPE_TREE_MODEL,
					    &tree_model_info);
#if 0
		g_type_add_interface_static(tree_store_type,
					    GTK_TYPE_TREE_DRAG_SOURCE,
					    &drag_source_info);
		g_type_add_interface_static(tree_store_type,
					    GTK_TYPE_TREE_DRAG_DEST,
					    &drag_dest_info);
#endif
	}

	return gpsm_store_type;
}

static void glame_gpsm_store_class_init(GlameGpsmStoreClass * class)
{
	GObjectClass *object_class;

	parent_class = g_type_class_peek_parent(class);
	object_class = (GObjectClass *) class;

	object_class->finalize = glame_gpsm_store_finalize;
}

static void glame_gpsm_store_finalize(GObject * object)
{
	(*parent_class->finalize) (object);
}

static void glame_gpsm_store_tree_model_init(GtkTreeModelIface * iface)
{
	iface->get_flags = glame_gpsm_store_get_flags;
	iface->get_n_columns = glame_gpsm_store_get_n_columns;
	iface->get_column_type = glame_gpsm_store_get_column_type;
	iface->get_iter = glame_gpsm_store_get_iter;
	iface->get_path = glame_gpsm_store_get_path;
	iface->get_value = glame_gpsm_store_get_value;
	iface->iter_next = glame_gpsm_store_iter_next;
	iface->iter_children = glame_gpsm_store_iter_children;
	iface->iter_has_child = glame_gpsm_store_iter_has_child;
	iface->iter_n_children = glame_gpsm_store_iter_n_children;
	iface->iter_nth_child = glame_gpsm_store_iter_nth_child;
	iface->iter_parent = glame_gpsm_store_iter_parent;
}

#if 0
static void gtk_tree_store_drag_source_init(GtkTreeDragSourceIface * iface)
{
	iface->row_draggable = real_gtk_tree_store_row_draggable;
	iface->drag_data_delete = gtk_tree_store_drag_data_delete;
	iface->drag_data_get = gtk_tree_store_drag_data_get;
}

static void gtk_tree_store_drag_dest_init(GtkTreeDragDestIface * iface)
{
	iface->drag_data_received = gtk_tree_store_drag_data_received;
	iface->row_drop_possible = gtk_tree_store_row_drop_possible;
}
#endif

static void glame_gpsm_store_init(GlameGpsmStore * tree_store)
{
	tree_store->root = NULL;
}

static void handle_item_change(glsig_handler_t * handler, long sig, va_list va)
{
	GlameGpsmStore *store =
	    GLAME_GPSM_STORE(glsig_handler_private(handler));
	GtkTreePath *path;
	GtkTreeIter iter;
	gpsm_item_t *item;

	GLSIGH_GETARGS1(va, item);
	iter.user_data = item;
	path = glame_gpsm_store_get_path(GTK_TREE_MODEL(store), &iter);
	gtk_tree_model_row_changed(GTK_TREE_MODEL(store), path, &iter);
	gtk_tree_path_free(path);
}

static void handle_item_addremove_1(GlameGpsmStore * store, gpsm_item_t * item,
				    int sig);
static void handle_item_addremove(glsig_handler_t * handler, long sig,
				  va_list va);
static void handle_item_addremove_r(GlameGpsmStore * store, gpsm_item_t * grp,
				    int sig)
{
	gpsm_item_t *item;
	gpsm_grp_foreach_item(grp, item)
	    handle_item_addremove_1(store, item, sig);
}
static void handle_item_addremove_1(GlameGpsmStore * store, gpsm_item_t * item,
				    int sig)
{
	GtkTreePath *path;
	GtkTreeIter iter;

	iter.user_data = item;
	path = glame_gpsm_store_get_path(GTK_TREE_MODEL(store), &iter);
	if (sig == GPSM_SIG_GRP_NEWITEM) {
		gtk_tree_model_row_inserted(GTK_TREE_MODEL(store), path, &iter);
		glsig_add_handler(gpsm_item_emitter(item),
				  GPSM_SIG_ITEM_CHANGED,
				  handle_item_change, store);
		if (GPSM_ITEM_IS_GRP(item)) {
			handle_item_addremove_r(store, item, sig);
			glsig_add_handler(gpsm_item_emitter(item),
					  GPSM_SIG_GRP_NEWITEM |
					  GPSM_SIG_GRP_REMOVEITEM,
					  handle_item_addremove, store);
		}
	} else if (sig == GPSM_SIG_GRP_REMOVEITEM) {
		glsig_handler_t *h;
		struct glame_list_head *dummy;
		if (GPSM_ITEM_IS_GRP(item))
			handle_item_addremove_r(store, item, sig);
		gtk_tree_model_row_deleted(GTK_TREE_MODEL(store), path);
		glame_list_safe_foreach(&item->emitter.handlers,
					glsig_handler_t, list, dummy, h)
		    if (h->handler == handle_item_addremove
			|| h->handler == handle_item_change)
			glsig_delete_handler(h);
	}
}
static void handle_item_addremove(glsig_handler_t * handler, long sig,
				  va_list va)
{
	GlameGpsmStore *store =
	    GLAME_GPSM_STORE(glsig_handler_private(handler));
	gpsm_grp_t *grp;
	gpsm_item_t *item;

	GLSIGH_GETARGS2(va, grp, item);
	handle_item_addremove_1(store, item, sig);
}

static void process_grp(gpsm_grp_t * grp, GlameGpsmStore * store)
{
	gpsm_item_t *item;
	gpsm_grp_foreach_item(grp, item)
	    glsig_add_handler(gpsm_item_emitter(item),
			      GPSM_SIG_ITEM_CHANGED, handle_item_change, store);
	gpsm_grp_foreach_item(grp, item)
	    if (GPSM_ITEM_IS_GRP(item))
		process_grp((gpsm_grp_t *)item, store);
	glsig_add_handler(gpsm_item_emitter(grp),
			  GPSM_SIG_GRP_NEWITEM | GPSM_SIG_GRP_REMOVEITEM,
			  handle_item_addremove, store);
}

GlameGpsmStore *glame_gpsm_store_new(gpsm_grp_t * root)
{
	GlameGpsmStore *retval;

	retval = g_object_new(GLAME_TYPE_GPSM_STORE, NULL);
	retval->root = root;
	process_grp(root, retval);

	return retval;
}

gpsm_item_t *glame_gpsm_store_get_item(GtkTreeIter * iter)
{
	g_return_val_if_fail(iter != NULL, NULL);
	g_return_val_if_fail(iter->user_data != NULL, NULL);

	return (gpsm_item_t *) iter->user_data;
}

/* fulfill the GtkTreeModel requirements */

static GtkTreeModelFlags glame_gpsm_store_get_flags(GtkTreeModel * tree_model)
{
	g_return_val_if_fail(GLAME_IS_GPSM_STORE(tree_model), 0);

	return GTK_TREE_MODEL_ITERS_PERSIST;
}

static gint glame_gpsm_store_get_n_columns(GtkTreeModel * tree_model)
{
	GlameGpsmStore *tree_store = (GlameGpsmStore *) tree_model;

	g_return_val_if_fail(GLAME_IS_GPSM_STORE(tree_model), 0);

	return GPSM_STORE_COL_N;
}

static GType
glame_gpsm_store_get_column_type(GtkTreeModel * tree_model, gint index)
{
	GlameGpsmStore *tree_store = (GlameGpsmStore *) tree_model;

	g_return_val_if_fail(GLAME_IS_GPSM_STORE(tree_model), G_TYPE_INVALID);
	g_return_val_if_fail(index < GPSM_STORE_COL_N &&
			     index >= 0, G_TYPE_INVALID);

	switch (index) {
	case GPSM_STORE_LABEL:
		return G_TYPE_STRING;
	case GPSM_STORE_SIZE:
		return G_TYPE_STRING;
		/* return G_TYPE_LONG; */
	case GPSM_STORE_NRTRACKS:
		return G_TYPE_STRING;
	case GPSM_STORE_TRACK_DURATION:
	        return G_TYPE_STRING;
	case GPSM_STORE_TRACK_SR:	
	        return G_TYPE_STRING;
	}
	return G_TYPE_INVALID;
}

static gboolean
glame_gpsm_store_get_iter(GtkTreeModel * tree_model,
			  GtkTreeIter * iter, GtkTreePath * path)
{
	GlameGpsmStore *tree_store = (GlameGpsmStore *) tree_model;
	gint *indices, depth;
	gpsm_item_t *item;
	int i;

	g_return_val_if_fail(GLAME_IS_GPSM_STORE(tree_store), FALSE);

	indices = gtk_tree_path_get_indices(path);
	depth = gtk_tree_path_get_depth(path);
	item = (gpsm_item_t *)GLAME_GPSM_STORE(tree_store)->root;
	for (i = 0; i < depth; ++i) {
		gpsm_grp_t *grp = (gpsm_grp_t *)item;
		int ii = 0;
		if (!grp) {
			DPRINTF("not enough depth?\n");
			return FALSE;
		}
		gpsm_grp_foreach_item(grp, item)
		    if (indices[i] == ii++)
			break;
		if (ii != indices[i] + 1) {
			DPRINTF("not enough children?\n");
			return FALSE;
		}
	}
	iter->user_data = item;

	return TRUE;
}

static GtkTreePath *glame_gpsm_store_get_path(GtkTreeModel * tree_model,
					      GtkTreeIter * iter)
{
	GtkTreePath *retval;
	gpsm_item_t *item;

	g_return_val_if_fail(GLAME_IS_GPSM_STORE(tree_model), NULL);
	g_return_val_if_fail(iter != NULL, NULL);

	retval = gtk_tree_path_new();
	item = iter->user_data;
	while (gpsm_item_parent(item)) {
		gpsm_item_t *it;
		int i = 0;
		gpsm_grp_foreach_item(gpsm_item_parent(item), it) {
			if (it == item)
				break;
			++i;
		}
		DPRINTF("Path element %i\n", i);
		gtk_tree_path_prepend_index(retval, i);
		item = (gpsm_item_t *)gpsm_item_parent(item);
	}

	return retval;
}

static void
glame_gpsm_store_get_value(GtkTreeModel * tree_model,
			   GtkTreeIter * iter, gint column, GValue * value)
{
       static char sizetext[32], nrtrackstext[32], durationtext[32], srtext[32];
       long f_rate;
       g_return_if_fail(GLAME_IS_GPSM_STORE(tree_model));
       g_return_if_fail(iter != NULL && iter->user_data != NULL);
       g_return_if_fail(column < GPSM_STORE_COL_N);

       switch (column) {
       case GPSM_STORE_LABEL:
	 g_value_init(value, G_TYPE_STRING);
	 g_value_set_string(value, gpsm_item_label(iter->user_data));
	 break;
       case GPSM_STORE_SIZE:
	 /* We don't have a G_TYPE_LONG renderer, so use a text one.
	  * We don't like memleaks, so use one static buffer.  Ugh.  */
	 snprintf(sizetext, 32, "%li", gpsm_item_hsize(iter->user_data));
	 g_value_init(value, G_TYPE_STRING);
	 g_value_set_string(value, sizetext);
	 /* g_value_init(value, G_TYPE_LONG);
	    g_value_set_long(value, gpsm_item_hsize(iter->user_data)); */
	 break;

       case GPSM_STORE_NRTRACKS:
	 if (GPSM_ITEM_IS_GRP(iter->user_data)){
	   snprintf(nrtrackstext, 32, "%li", gpsm_item_vsize(iter->user_data)); 
	   g_value_init(value, G_TYPE_STRING);
	   g_value_set_string(value, nrtrackstext);
	 }
	 else {
	   snprintf(nrtrackstext, 32, "-"); 
	   g_value_init(value, G_TYPE_STRING);
	   g_value_set_string(value, nrtrackstext);
	 }
	 break;
	  
       case GPSM_STORE_TRACK_DURATION:
	 if (GPSM_ITEM_IS_SWFILE(iter->user_data)){
	   swfd_t fd ;
	   struct sw_stat st;
	   long size = -1;
	   fd = sw_open(gpsm_swfile_filename(iter->user_data),
			O_RDONLY);
	   if (fd != -1 && sw_fstat(fd, &st) != -1)
	     size = st.size/SAMPLE_SIZE;
	   sw_close(fd);
	   snprintf(durationtext, 32, "%.0f", (float)size/(float)gpsm_swfile_samplerate(iter->user_data)); 
	   g_value_init(value, G_TYPE_STRING);
	   g_value_set_string(value,durationtext);
	 }
	 else {    
	   snprintf(durationtext, 32, "-"); 
	   g_value_init(value, G_TYPE_STRING);
	   g_value_set_string(value, durationtext);
	 }
	 break;

       case GPSM_STORE_TRACK_SR:
	 if (GPSM_ITEM_IS_SWFILE(iter->user_data)){
	   snprintf(srtext, 32, "%i", gpsm_swfile_samplerate(iter->user_data)); 
	   g_value_init(value, G_TYPE_STRING);
	   g_value_set_string(value,srtext);
	 }
	 else {    
	   snprintf(srtext, 32, "-"); 
	   g_value_init(value, G_TYPE_STRING);
	   g_value_set_string(value, srtext);
	 }
	 break;
       }
}

static gboolean
glame_gpsm_store_iter_next(GtkTreeModel * tree_model, GtkTreeIter * iter)
{
	g_return_val_if_fail(iter != NULL, FALSE);
	g_return_val_if_fail(iter->user_data != NULL, FALSE);

	iter->user_data =
	    gpsm_grp_next(gpsm_item_parent(iter->user_data), iter->user_data);
	if (iter->user_data)
		return TRUE;
	else
		return FALSE;
}

static gboolean
glame_gpsm_store_iter_children(GtkTreeModel * tree_model,
			       GtkTreeIter * iter, GtkTreeIter * parent)
{
	g_return_val_if_fail(parent == NULL
			     || (parent->user_data != NULL
				 && GPSM_ITEM_IS_GRP(parent->user_data)),
			     FALSE);

	if (parent)
		iter->user_data = gpsm_grp_first(parent->user_data);
	else
		iter->user_data =
		    gpsm_grp_first(GLAME_GPSM_STORE(tree_model)->root);

	if (iter->user_data)
		return TRUE;
	else
		return FALSE;
}

static gboolean
glame_gpsm_store_iter_has_child(GtkTreeModel * tree_model, GtkTreeIter * iter)
{
	g_return_val_if_fail(GLAME_IS_GPSM_STORE(tree_model), FALSE);
	g_return_val_if_fail(iter->user_data != NULL, FALSE);

	return GPSM_ITEM_IS_GRP(iter->user_data)
	    && gpsm_grp_first(iter->user_data) != NULL;
}

static gint
glame_gpsm_store_iter_n_children(GtkTreeModel * tree_model, GtkTreeIter * iter)
{
	g_return_val_if_fail(GLAME_IS_GPSM_STORE(tree_model), 0);
	g_return_val_if_fail(iter == NULL || iter->user_data != NULL, 0);

	if (iter == NULL)
		return gpsm_grp_nritems(GLAME_GPSM_STORE(tree_model)->root);
	else
		return GPSM_ITEM_IS_GRP(iter->user_data) &&
		    gpsm_grp_nritems(iter->user_data);
}

static gboolean
glame_gpsm_store_iter_nth_child(GtkTreeModel * tree_model,
				GtkTreeIter * iter,
				GtkTreeIter * parent, gint n)
{
	gpsm_grp_t *parent_node;
	gpsm_item_t *item;

	g_return_val_if_fail(GLAME_IS_GPSM_STORE(tree_model), FALSE);
	g_return_val_if_fail(parent == NULL
			     || (parent->user_data != NULL
				 && GPSM_ITEM_IS_GRP(parent->user_data)),
			     FALSE);

	if (parent == NULL)
		parent_node = (gpsm_grp_t *)GLAME_GPSM_STORE(tree_model)->root;
	else
		parent_node = parent->user_data;

	iter->user_data = NULL;
	gpsm_grp_foreach_item(parent_node, item) {
		if (!n--) {
			iter->user_data = item;
			break;
		}
	}
	return iter->user_data != NULL;
}

static gboolean
glame_gpsm_store_iter_parent(GtkTreeModel * tree_model,
			     GtkTreeIter * iter, GtkTreeIter * child)
{
	g_return_val_if_fail(iter != NULL, FALSE);
	g_return_val_if_fail(child != NULL, FALSE);
	g_return_val_if_fail(child->user_data != NULL, FALSE);

	iter->user_data = gpsm_item_parent(child->user_data);

	return iter->user_data != NULL;
}

#if 0
/* DND */

#define G_NODE(node) ((GNode *)node)

static gboolean real_gtk_tree_store_row_draggable(GtkTreeDragSource *
						  drag_source,
						  GtkTreePath * path)
{
	return TRUE;
}

static gboolean
gtk_tree_store_drag_data_delete(GtkTreeDragSource * drag_source,
				GtkTreePath * path)
{
	GtkTreeIter iter;

	g_return_val_if_fail(GTK_IS_TREE_STORE(drag_source), FALSE);

	if (gtk_tree_model_get_iter(GTK_TREE_MODEL(drag_source), &iter, path)) {
		gtk_tree_store_remove(GTK_TREE_STORE(drag_source), &iter);
		return TRUE;
	} else {
		return FALSE;
	}
}

static gboolean
gtk_tree_store_drag_data_get(GtkTreeDragSource * drag_source,
			     GtkTreePath * path,
			     GtkSelectionData * selection_data)
{
	g_return_val_if_fail(GTK_IS_TREE_STORE(drag_source), FALSE);

	/* Note that we don't need to handle the GTK_TREE_MODEL_ROW
	 * target, because the default handler does it for us, but
	 * we do anyway for the convenience of someone maybe overriding the
	 * default handler.
	 */

	if (gtk_tree_set_row_drag_data(selection_data,
				       GTK_TREE_MODEL(drag_source), path)) {
		return TRUE;
	} else {
		/* FIXME handle text targets at least. */
	}

	return FALSE;
}

static void
copy_node_data(GtkTreeStore * tree_store,
	       GtkTreeIter * src_iter, GtkTreeIter * dest_iter)
{
	GtkTreeDataList *dl = G_NODE(src_iter->user_data)->data;
	GtkTreeDataList *copy_head = NULL;
	GtkTreeDataList *copy_prev = NULL;
	GtkTreeDataList *copy_iter = NULL;
	GtkTreePath *path;
	gint col;

	col = 0;
	while (dl) {
		copy_iter =
		    _gtk_tree_data_list_node_copy(dl,
						  tree_store->
						  column_headers[col]);

		if (copy_head == NULL)
			copy_head = copy_iter;

		if (copy_prev)
			copy_prev->next = copy_iter;

		copy_prev = copy_iter;

		dl = dl->next;
		++col;
	}

	G_NODE(dest_iter->user_data)->data = copy_head;

	path = gtk_tree_store_get_path(GTK_TREE_MODEL(tree_store), dest_iter);
	gtk_tree_model_row_changed(GTK_TREE_MODEL(tree_store), path, dest_iter);
	gtk_tree_path_free(path);
}

static void
recursive_node_copy(GtkTreeStore * tree_store,
		    GtkTreeIter * src_iter, GtkTreeIter * dest_iter)
{
	GtkTreeIter child;
	GtkTreeModel *model;

	model = GTK_TREE_MODEL(tree_store);

	copy_node_data(tree_store, src_iter, dest_iter);

	if (gtk_tree_model_iter_children(model, &child, src_iter)) {
		/* Need to create children and recurse. Note our
		 * dependence on persistent iterators here.
		 */
		do {
			GtkTreeIter copy;

			/* Gee, a really slow algorithm... ;-) FIXME */
			gtk_tree_store_append(tree_store, &copy, dest_iter);

			recursive_node_copy(tree_store, &child, &copy);
		}
		while (gtk_tree_model_iter_next(model, &child));
	}
}

static gboolean
gtk_tree_store_drag_data_received(GtkTreeDragDest * drag_dest,
				  GtkTreePath * dest,
				  GtkSelectionData * selection_data)
{
	GtkTreeModel *tree_model;
	GtkTreeStore *tree_store;
	GtkTreeModel *src_model = NULL;
	GtkTreePath *src_path = NULL;
	gboolean retval = FALSE;

	g_return_val_if_fail(GTK_IS_TREE_STORE(drag_dest), FALSE);

	tree_model = GTK_TREE_MODEL(drag_dest);
	tree_store = GTK_TREE_STORE(drag_dest);

	validate_tree(tree_store);

	if (gtk_tree_get_row_drag_data(selection_data,
				       &src_model,
				       &src_path) && src_model == tree_model) {
		/* Copy the given row to a new position */
		GtkTreeIter src_iter;
		GtkTreeIter dest_iter;
		GtkTreePath *prev;

		if (!gtk_tree_model_get_iter(src_model, &src_iter, src_path)) {
			goto out;
		}

		/* Get the path to insert _after_ (dest is the path to insert _before_) */
		prev = gtk_tree_path_copy(dest);

		if (!gtk_tree_path_prev(prev)) {
			GtkTreeIter dest_parent;
			GtkTreePath *parent;
			GtkTreeIter *dest_parent_p;

			/* dest was the first spot at the current depth; which means
			 * we are supposed to prepend.
			 */

			/* Get the parent, NULL if parent is the root */
			dest_parent_p = NULL;
			parent = gtk_tree_path_copy(dest);
			if (gtk_tree_path_up(parent) &&
			    gtk_tree_path_get_depth(parent) > 0) {
				gtk_tree_model_get_iter(tree_model,
							&dest_parent, parent);
				dest_parent_p = &dest_parent;
			}
			gtk_tree_path_free(parent);
			parent = NULL;

			gtk_tree_store_prepend(tree_store,
					       &dest_iter, dest_parent_p);

			retval = TRUE;
		} else {
			if (gtk_tree_model_get_iter
			    (tree_model, &dest_iter, prev)) {
				GtkTreeIter tmp_iter = dest_iter;

				gtk_tree_store_insert_after(tree_store,
							    &dest_iter, NULL,
							    &tmp_iter);

				retval = TRUE;
			}
		}

		gtk_tree_path_free(prev);

		/* If we succeeded in creating dest_iter, walk src_iter tree branch,
		 * duplicating it below dest_iter.
		 */

		if (retval) {
			recursive_node_copy(tree_store, &src_iter, &dest_iter);
		}
	} else {
		/* FIXME maybe add some data targets eventually, or handle text
		 * targets in the simple case.
		 */

	}

      out:

	if (src_path)
		gtk_tree_path_free(src_path);

	return retval;
}

static gboolean
gtk_tree_store_row_drop_possible(GtkTreeDragDest * drag_dest,
				 GtkTreePath * dest_path,
				 GtkSelectionData * selection_data)
{
	GtkTreeModel *src_model = NULL;
	GtkTreePath *src_path = NULL;
	GtkTreePath *tmp = NULL;
	gboolean retval = FALSE;

	g_return_val_if_fail(GTK_IS_TREE_STORE(drag_dest), FALSE);

	/* don't accept drops if the tree has been sorted */
	if (GTK_TREE_STORE_IS_SORTED(drag_dest))
		return FALSE;

	if (!gtk_tree_get_row_drag_data(selection_data, &src_model, &src_path))
		goto out;

	/* can only drag to ourselves */
	if (src_model != GTK_TREE_MODEL(drag_dest))
		goto out;

	/* Can't drop into ourself. */
	if (gtk_tree_path_is_ancestor(src_path, dest_path))
		goto out;

	/* Can't drop if dest_path's parent doesn't exist */
	{
		GtkTreeIter iter;

		if (gtk_tree_path_get_depth(dest_path) > 1) {
			tmp = gtk_tree_path_copy(dest_path);
			gtk_tree_path_up(tmp);

			if (!gtk_tree_model_get_iter(GTK_TREE_MODEL(drag_dest),
						     &iter, tmp))
				goto out;
		}
	}

	/* Can otherwise drop anywhere. */
	retval = TRUE;

      out:

	if (src_path)
		gtk_tree_path_free(src_path);
	if (tmp)
		gtk_tree_path_free(tmp);

	return retval;
}

#endif
