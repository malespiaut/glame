/*
 * canvasitem.c
 *
 * $Id: glamecanvas.c,v 1.22 2001/07/11 08:36:53 xwolf Exp $
 *
 * Copyright (C) 2001 Johannes Hirche
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
 *
 */

#include <sys/param.h>
#include <stdio.h>
#include <math.h>
#include <gnome.h>
#include "glamecanvas.h"
#include "canvasitem.h"
#include "hash.h"
#include "util/glame_gui_utils.h"
#include "glame_accelerator.h"

     

/*  Forward decls */



/*Yukky gtk class stuff    
 * Ignore these
 */


static void
glame_canvas_destroy (GtkObject *object)
{
	GnomeCanvasClass *parent_class;
	if (!GLAME_CANVAS(object)->openedUp)
		filter_delete(GLAME_CANVAS(object)->net);
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS);
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
}


static void
glame_canvas_class_init(GlameCanvasClass *class)
{
	GtkObjectClass *object_class;
	
	object_class = GTK_OBJECT_CLASS (class);
	object_class->destroy = glame_canvas_destroy;
}


static void
glame_canvas_init (GlameCanvas *item)
{
	item->net = NULL;
	item->openedUp = FALSE;
	item->paused = FALSE;
	item->font_size = 1.0;
	item->selectedItems = NULL;
}


GtkType
glame_canvas_get_type(void)
{
        static GtkType canvas_type = 0;
        
        if(!canvas_type){
                GtkTypeInfo canvas_info={
                        "GlameCanvas",
                        sizeof(GlameCanvas),
                        sizeof(GlameCanvasClass),
                        (GtkClassInitFunc) glame_canvas_class_init,
                        (GtkObjectInitFunc) glame_canvas_init,
                        NULL,NULL,(GtkClassInitFunc)NULL,};
                canvas_type = gtk_type_unique(GNOME_TYPE_CANVAS,
					      &canvas_info);
                gtk_type_set_chunk_alloc(canvas_type,8);
        }
        
        return canvas_type;
}
//////////////
static void
glame_canvas_group_destroy (GtkObject *object)
{
	GnomeCanvasGroupClass *parent_class;
	if(GLAME_IS_CANVAS_GROUP(GCI(object)->parent)){
		GlameCanvasGroup* parent = GCI(object)->parent;
		parent->groups = g_list_remove(parent->groups,GLAME_CANVAS_GROUP(object));
	}
	//else{
	//GlameCanvas* canvas = CANVAS_ITEM_GLAME_CANVAS(object);
	//canvas->selectedItems = g_list_remove(canvas->selectedItems,GLAME_CANVAS_GROUP(object));
	//}	
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS_GROUP);
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
}


static void
glame_canvas_group_class_init(GlameCanvasGroupClass *class)
{
	GtkObjectClass *object_class;
	
	object_class = GTK_OBJECT_CLASS (class);
	object_class->destroy = glame_canvas_group_destroy;
}


static void
glame_canvas_group_init (GlameCanvasGroup *item)
{
	static guint id_count = 0;
	item->type = GROUP_TYPE_UNION;
	item->children = NULL;
	item->groups = NULL;
	item->id = id_count++;
}


GtkType
glame_canvas_group_get_type(void)
{
        static GtkType canvas_type = 0;
        
        if(!canvas_type){
                GtkTypeInfo canvas_info={
                        "GlameCanvasGroup",
                        sizeof(GlameCanvasGroup),
                        sizeof(GlameCanvasGroupClass),
                        (GtkClassInitFunc) glame_canvas_group_class_init,
                        (GtkObjectInitFunc) glame_canvas_group_init,
                        NULL,NULL,(GtkClassInitFunc)NULL,};
                canvas_type = gtk_type_unique(GNOME_TYPE_CANVAS_GROUP,
					      &canvas_info);
                gtk_type_set_chunk_alloc(canvas_type,8);
        }
        
        return canvas_type;
}

/*****************************
 * Whew...
 * now for the real work
 *****************************/

GlameCanvas* glame_canvas_new(filter_t * net)
{
	GlameCanvas *g;
	GnomeCanvasGroup *group;
	filter_t * node;
	filter_t * filter;
	filter_port_t * port;
	filter_pipe_t *pipe;
	char * buffer;
	filter_t * network;
	if(!net){
		network = filter_creat(NULL);
	} else {
		if(!FILTER_IS_NETWORK(net)){
			DPRINTF("Not network\n");
			/* HACK maybe empty? */
			if(filter_nrnodes(net))
				return NULL; /* something seriously wrong here! */
			
		}
		network = net;
	}
		

	g = gtk_type_new(glame_canvas_get_type());
	if(!g)
		return NULL;
	g->net = network;
	
	if(!filter_nrnodes(g->net))
		return g;
	group = gnome_canvas_root(GNOME_CANVAS(g));
	
	/* create nodes */
	filter_foreach_node(network,filter){
		glame_canvas_filter_new(group,filter);
	}
	
	/* create pipes */
	filter_foreach_node(network, node){
	  /* FIXME this doesn't work?
	  struct fconnection *c;
	  list_foreach(&node->connections, struct fconnection, list, c) {
	    filter_t *f;
	    
	    if (strcmp(filter_name(node), c->source_filter) != 0) {
	      DPRINTF("Filter not the one promised\n");
	      continue;
	    }
	    if (!(f = filter_get_node(filter, c->dest_filter))) {
	      DPRINTF("No such filter\n");
	      continue;
	    }
	    glame_canvas_pipe_new(group,c->pipe);
	    
	    }*/
	  
	  filterportdb_foreach_port(filter_portdb(node),port){
	    filterport_foreach_pipe(port,pipe){
	      if (!filter_get_node(network, pipe->source_filter)
	          || !filter_get_node(network, pipe->dest_filter))
		continue;
	      glame_canvas_pipe_new(group,pipe);
	    }
	  }
	}
	
	/* map external ports */
	
	filterportdb_foreach_port(filter_portdb(network),port){
		node = filter_get_node(network,filterport_get_property(port,FILTERPORT_MAP_NODE));
		buffer = filterport_get_property(port,FILTERPORT_MAP_LABEL);
		if (!node || !buffer )
			continue;
		glame_canvas_port_set_external(glame_canvas_find_port(filterportdb_get_port(filter_portdb(node),buffer)),TRUE);
	}
	
	/*canvas_update_scroll_region(GLAME_CANVAS(canv));*/
	glame_canvas_view_all(g);

	return g;
}



/* uuuuh :-\  */
void glame_canvas_marshal_NONE__DOUBLE_DOUBLE(GtkObject* obj,
						  GtkSignalFunc func,
						  gpointer func_data,
						  GtkArg* args)
{

	GtkSignal_NONE__DOUBLE_DOUBLE rfunc;
	//	gint *return_val;
	
	//return_val = GTK_RETLOC_BOOL(args[2]);
	rfunc = (GtkSignal_NONE__DOUBLE_DOUBLE)func;
	
	//*return_val =
 	(*rfunc)(obj,
		 GTK_VALUE_DOUBLE(args[0]),
		 GTK_VALUE_DOUBLE(args[1]),
		 func_data);
}
	

/* returns a newly allocated string identifier. */
char* 
glame_gui_get_font(GlameCanvas* canv)
{
	
	char * fontbuffer = malloc(256);
	sprintf(fontbuffer,"-adobe-helvetica-medium-r-normal-*-%d-120-*-*-p-*-iso8859-1",
		(int)(GNOME_CANVAS(canv)->pixels_per_unit*12.0));
	return fontbuffer;
}


GdkImlibImage*
glame_gui_get_icon_from_filter(filter_t *filter)
{
	return glame_load_icon(plugin_query(filter->plugin, PLUGIN_PIXMAP),0,0);

}

GlameCanvasFilter*
glame_canvas_add_filter(GlameCanvas *canv, filter_t * filter)
{
	if(filter_add_node(canv->net,filter,plugin_name(filter->plugin)) == -1) {
		DPRINTF("Error adding node!\n");
		return NULL;
	}
	
	return glame_canvas_filter_new(gnome_canvas_root(GNOME_CANVAS(canv)), filter);
}


GlameCanvasFilter*
glame_canvas_add_filter_by_plugin(GlameCanvas *canv, plugin_t * plug)
{
	filter_t *filter;

	filter = filter_instantiate(plug);
	if(!filter){
		DPRINTF("Error in instantiate\n");
		return NULL;
	}
	return glame_canvas_add_filter(canv, filter);
}

void glame_canvas_redraw(GlameCanvas *canv)
{
	filter_t *node;
	filter_foreach_node(canv->net,node){
		glame_canvas_filter_redraw(glame_canvas_find_filter(node));
	}
}


/* FIXME! these two are completely useless */

void glame_canvas_draw_errors(GlameCanvas *canv)
{
	filter_t *node;
	filter_foreach_node(canv->net,node){
		glame_canvas_filter_redraw(glame_canvas_find_filter(node));
	}
}

void glame_canvas_reset_errors(GlameCanvas *canv)
{
	filter_t *node;
	filter_foreach_node(canv->net,node){
		glame_canvas_filter_redraw(glame_canvas_find_filter(node));
	}
}

GlameCanvasGroup* glame_canvas_group_root(GlameCanvasGroup* group)
{
	if(!GLAME_IS_CANVAS_GROUP(GNOME_CANVAS_ITEM(group)->parent))
		return group;
	else glame_canvas_group_root(GNOME_CANVAS_ITEM(group)->parent);
}

guint glame_canvas_group_root_id(GlameCanvasGroup* group)
{
	return (glame_canvas_group_root(group))->id;
}

void glame_canvas_group_move(GlameCanvasGroup* group, double x, double y)
{
	GList* list = g_list_first(group->children);
	while(list){
		_glame_canvas_filter_move(list->data, x,y);
		list = g_list_next(list);
	}
	list = g_list_first(group->groups);
	while(list){
		if(GLAME_IS_CANVAS_GROUP(list->data))
			glame_canvas_group_move(list->data,x,y);
		list = g_list_next(list);
	}
}
void glame_canvas_group_item_moved_cb(GlameCanvasFilter* item, double x, double y, GlameCanvasGroup* group)
{
	
	GList *list;
	GlameCanvasGroup* root = glame_canvas_group_root(group);
	
	glame_canvas_group_move(root,x,y);
}
	

void glame_canvas_group_dissolve(GlameCanvasGroup* group)
{
	GList * list,*next;
	GnomeCanvasGroup* root = CANVAS_ITEM_ROOT(group);

	DPRINTF("List length: %d\n",g_list_length(group->children));
	list = g_list_first(group->children);
	list = g_list_next(list);
	while(list){
		GlameCanvasGroup *newgrp = GLAME_CANVAS_GROUP(gnome_canvas_item_new(root,
										    glame_canvas_group_get_type(),
										    NULL));
		DPRINTF("reparenting item\n");
		next = g_list_next(list);
		glame_canvas_group_reparent_item(newgrp,GLAME_CANVAS_FILTER(list->data));
		glame_canvas_filter_redraw(GLAME_CANVAS_FILTER(list->data));
		list = next;
	}
	
	list = g_list_first(group->groups);
	while(list){
		if(GLAME_IS_CANVAS_GROUP(list->data)){
			gnome_canvas_item_reparent(GNOME_CANVAS_ITEM(list->data),root);
			group->groups = g_list_remove(group->groups, list->data);
			list = g_list_first(group->groups);
		}else{
			DPRINTF("This can't happen\n");
			list = g_list_next(list);
		}
	}
	g_list_free(group->groups);
}

int glame_canvas_group_remove_item(GlameCanvasGroup* group, GlameCanvasFilter* item)
{
	if(!group)
		return;
	group->children = g_list_remove(group->children,item);
	if(!group->children)
		if(!group->groups){
			/* schedule for killing */
			return 1;
			/* glame_canvas_group_destroy(GTO(group)); */
		}
	return 0;
}

void glame_canvas_group_remove_item_cb(GlameCanvasFilter* item, GlameCanvasGroup* group)
{
	glame_canvas_group_remove_item(group,item);
} 

void glame_canvas_group_delete(GlameCanvasGroup* group)
{
	GList * list;

	list = g_list_first(group->children);
	while(list){
		if(GLAME_IS_CANVAS_FILTER(list->data)){
			filter_delete(GLAME_CANVAS_FILTER(list->data)->filter);
			
		}
		list = g_list_next(list);
	}
	list = g_list_first(group->groups);
	while(list){
		if(GLAME_IS_CANVAS_GROUP(list->data)){
			glame_canvas_group_delete(list->data);
		}
		list = g_list_next(list);
	}
}
			
	
void glame_canvas_group_deleted_cb(GlameCanvasFilter* item, GlameCanvasGroup* group)
{
	static int rec_level = 0;
	DPRINTF("delete: %s\n",filter_name(item->filter));
	if(!rec_level){
		GList * list = g_list_first(group->children);
		rec_level++;
		while(list){
			if(item!= GLAME_CANVAS_FILTER(list->data)){
				filter_delete(GLAME_CANVAS_FILTER(list->data)->filter);
			}		
			list = g_list_next(list);
		}
	}
}


void _glame_canvas_group_select(GlameCanvasGroup* group)
{

	GlameCanvas* canvas;
	GList* list;

	if(!group)
		return;
	if(!GLAME_IS_CANVAS_GROUP(group))
		return;
	list = g_list_first(group->children);
	
	DPRINTF("bla\n");
	canvas = CANVAS_ITEM_GLAME_CANVAS(GCI(group));
	while(list){
		glame_canvas_select_add(canvas,GLAME_CANVAS_FILTER(list->data));
		list = g_list_next(list);
	}
	list = g_list_first(group->groups);
	while(list){
		_glame_canvas_group_select(list->data);
		list = g_list_next(list);
	}
}

void glame_canvas_group_select(GlameCanvasGroup* group)
{
	_glame_canvas_group_select(glame_canvas_group_root(group));
	glame_canvas_group_raise(glame_canvas_group_root(group));
	glame_canvas_redraw(CANVAS_ITEM_GLAME_CANVAS(group));
}
void _glame_canvas_group_unselect(GlameCanvasGroup* group)
{

	GlameCanvas* canvas;
	GList* list;

	if(!group)
		return;
	if(!GLAME_IS_CANVAS_GROUP(group))
		return;
	list = g_list_first(group->children);
	canvas = CANVAS_ITEM_GLAME_CANVAS(GCI(group));

	while(list){
		glame_canvas_filter_set_selected(GLAME_CANVAS_FILTER(list->data),FALSE);
		list = g_list_next(list);
	}
	list = g_list_first(group->groups);
	while(list){
		_glame_canvas_group_unselect(list->data);
		list = g_list_next(list);
	}
}

void glame_canvas_group_unselect(GlameCanvasGroup* group)
{
	GlameCanvasGroup* root;
	GlameCanvas* canvas;

	if(!root)return;
	root = glame_canvas_group_root(group);
	canvas = CANVAS_ITEM_GLAME_CANVAS(group);
	_glame_canvas_group_unselect(root);
	canvas->selectedItems = g_list_remove(canvas->selectedItems,glame_canvas_group_root(group));
	glame_canvas_redraw(canvas);
}



void _glame_canvas_group_raise(GlameCanvasGroup* group)
{
		
	if(!group)
		return;
	else{
		GList* list = g_list_first(group->children);
		
		while(list){
			_glame_canvas_filter_raise_to_top(list->data);
			list = g_list_next(list);
		}
		list = g_list_first(group->groups);
		while(list){
			_glame_canvas_group_raise(list->data);
			list = g_list_next(list);
		}
	}
}
void glame_canvas_group_raise(GlameCanvasGroup* group)
{
	_glame_canvas_group_raise(glame_canvas_group_root(group));
}

void glame_canvas_group_set_item(GlameCanvasGroup* glameGroup, GlameCanvasFilter* gItem)
{
	gnome_canvas_item_reparent(GNOME_CANVAS_ITEM(gItem), GNOME_CANVAS_GROUP(glameGroup));
	glameGroup->children = g_list_append(glameGroup->children,gItem);
}

static guint glame_canvas_group_children_count(GlameCanvasGroup* group)
{
	guint count = g_list_length(group->children) + g_list_length(group->groups);
	return count;
}
		

void glame_canvas_group_reparent_item(GlameCanvasGroup* glameGroup, GlameCanvasFilter* gItem)
{
	GlameCanvasGroup* oldParent = GLAME_CANVAS_GROUP(GCI(gItem)->parent);
	gnome_canvas_item_reparent(gItem, GNOME_CANVAS_GROUP(glameGroup));
	glame_canvas_group_remove_item(oldParent,gItem);
	
	glameGroup->children = g_list_append(glameGroup->children,gItem);
}

void glame_canvas_group_add(GlameCanvasGroup* parent, GlameCanvasGroup* child)
{
	if(glame_canvas_group_children_count(child)>1){
		GlameCanvasGroup* oldparent = GCI(child)->parent;
		DPRINTF("adding subgroup\n");
		gnome_canvas_item_reparent(GNOME_CANVAS_ITEM(child),GNOME_CANVAS_GROUP(parent));
		parent->groups = g_list_append(parent->groups,child);
		if(GLAME_IS_CANVAS_GROUP(oldparent))
			oldparent->groups = g_list_remove(oldparent->groups,child);		
	}else{
		DPRINTF("adding item\n");
		glame_canvas_group_reparent_item(parent, (g_list_first(child->children))->data); // FIXME!
	}
}

void glame_canvas_set_zoom(GlameCanvas * canv, double pixelperpoint)
{
	gnome_canvas_set_pixels_per_unit(GNOME_CANVAS(canv),pixelperpoint);
	glame_canvas_redraw(canv);	
}

void glame_canvas_view_all(GlameCanvas* canv)
{
	filter_t *filter;
	GlameCanvasFilter* item;
	double minX,minY,maxX,maxY;
	double x1,x2,y1,y2;
	
	minX=minY=99999.0;
	maxX=maxY=-99999.0;

	filter_foreach_node(canv->net,filter){
		item = glame_canvas_find_filter(filter);
		if(item){
			gnome_canvas_item_get_bounds(GNOME_CANVAS_ITEM(item),
						     &x1,&y1,&x2,&y2);
			minX=(minX>x1)?x1:minX;
			minY=(minY>y1)?y1:minY;
			maxX=(maxX<x2)?x2:maxX;
			maxY=(maxY<y2)?y2:maxY;
		}
	}
	minX-=30.0;
	minY-=30.0;
	maxX+=30.0;
	maxY+=30.0;

/*	gnome_canvas_get_scroll_region(GNOME_CANVAS(canv),&x1,&y1,&x2,&y2);
	x1=(x1>minX)?minX:x1;
	y1=(y1>minY)?minY:y1;
	x2=(x2<maxX)?maxX:x2;
	y2=(y2<maxY)?maxY:y2;
*/
	gnome_canvas_set_scroll_region(GNOME_CANVAS(canv),minX,minY,maxX,maxY);
	gnome_canvas_update_now(GNOME_CANVAS(canv));
	filter_foreach_node(canv->net,filter){
		// FIXME gnomebug?
		glame_canvas_filter_move(glame_canvas_find_filter(filter),0.0,0.0);
		glame_canvas_filter_redraw(glame_canvas_find_filter(filter));
		
	}

}


void glame_canvas_select_add(GlameCanvas* canvas, GlameCanvasFilter* filter)
{
	GlameCanvasGroup* root = glame_canvas_group_root(filter);
	glame_canvas_filter_set_selected(filter,TRUE);
	if(!g_list_find(canvas->selectedItems,root)){
		canvas->selectedItems = g_list_append(canvas->selectedItems,root);
	}
}

void glame_canvas_select_exclusive(GlameCanvas* canvas, GlameCanvasFilter* filter)
{
	glame_canvas_select_clear(canvas);
	glame_canvas_select_add(canvas,filter);
}


void glame_canvas_select_clear(GlameCanvas *canvas)
{
	GList* iter;
	GList* removed = NULL;

	iter = g_list_first(canvas->selectedItems);
	while(iter){
		if(GLAME_IS_CANVAS_GROUP(iter->data))
			glame_canvas_group_unselect(GLAME_CANVAS_GROUP(iter->data));
		iter = g_list_next(iter);
	}
	canvas->selectedItems = NULL;
}
	
void glame_canvas_select_unselect(GlameCanvas* canvas, GlameCanvasFilter* filter)
{
	canvas->selectedItems = g_list_remove(canvas->selectedItems,filter);
	glame_canvas_filter_set_selected(filter,FALSE);
}

void glame_canvas_group_selected(GlameCanvas* canvas)
{
	GList * iter, *list;
	GlameCanvasGroup* group;
	gboolean found = FALSE;
	GlameCanvasGroup* newgroup;
	group = GLAME_CANVAS_GROUP(gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(canvas)),
							 glame_canvas_group_get_type(),
							 NULL));
	list = g_list_first(canvas->selectedItems);
	while(list){
		newgroup = GLAME_CANVAS_GROUP(list->data);
		list = g_list_next(list);
		glame_canvas_group_add(group, newgroup);
	}
	glame_canvas_select_clear(canvas);
	glame_canvas_redraw(canvas);
}

void glame_canvas_ungroup_selected(GlameCanvas* canvas)
{
	GList *list = g_list_first(canvas->selectedItems);
	
/* Yuck! FIXME */
	while(list){
		glame_canvas_group_dissolve(GLAME_CANVAS_GROUP(list->data));
		list = g_list_next(list);
	}
	glame_canvas_redraw(canvas);
}
