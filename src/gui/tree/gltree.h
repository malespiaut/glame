#ifndef _GLTREEITEM_H
#define _GLTREEITEM_H

#include <gtk/gtk.h>
#include "gpsm.h"

enum {
	//	PROJECT,
	//	KNOB,
	INFO,
	N_ITEMS
};
	

class glTree{
public:
	GtkTreeStore *store;
	gpsm_grp_t *root;
	GtkWidget *tree;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
public:
	glTree(){
		store = NULL;
		root=NULL;
	}
	glTree(gpsm_grp_t * newroot);
	
	

};
#endif
