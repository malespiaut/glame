
#include "gltree.h"
#include <string.h>


glTree::glTree(gpsm_grp_t * newroot)
{
	root=newroot;
	store = gtk_tree_store_new(N_ITEMS,
				   //				   G_TYPE_OBJECT,
				   G_TYPE_STRING);
	GtkTreeIter iter,iter2;
	
	gpsm_item_t *item;
	gpsm_grp_foreach_item(newroot,item){
		gtk_tree_store_append(store, &iter,NULL);
		gtk_tree_store_set(store, &iter,INFO,strdup(item->label),-1);
		if(GPSM_ITEM_IS_GRP(item)){
			gpsm_item_t *it;
			gpsm_grp_foreach_item(item,it){
				gtk_tree_store_append(store,&iter2, &iter);
				gtk_tree_store_set(store,&iter2,INFO,strdup(it->label),-1);
			}
		}
	}
		
		tree = gtk_tree_view_new_with_model (GTK_TREE_MODEL (store));
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("label",
							   renderer,
							   "text", INFO,
							   NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);
}
				   
	
