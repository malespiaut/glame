#ifndef _TREE_H
#define _TREE_H

#include <gtk/gtk.h>


GtkWidget *tree_deserialize(const char *xml);
char *tree_serialize();
void tree_delete();


int tree_add_file(long swname, const char *url, const char *name);


#endif
