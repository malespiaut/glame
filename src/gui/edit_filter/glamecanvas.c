/*
 * canvasitem.c
 *
 * $Id: glamecanvas.c,v 1.50 2003/04/20 21:56:02 richi Exp $
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/param.h>
#include <stdio.h>
#include <math.h>
#include <gnome.h>
#include "glamecanvas.h"
#include "canvasitem.h"
#include "hash.h"
#include "util/glame_gui_utils.h"
#include "util/glame_dnd.h"
#include "glame_accelerator.h"

#define in_between(x1,x2,test) ((x1)>(test)?0:((x2)<(test)?0:1))

/*  Forward decls */



/*Yukky gtk class stuff    
 * Ignore these
 */


HASH(gcanvas, GlameCanvas, 8,
     (gcanvas->net == key),
     ((long)key/4),
     ((long)gcanvas->net/4),
     filter_t * key)

static void
glame_canvas_destroy (GtkObject *object)
{
	GnomeCanvasClass *parent_class;
	hash_remove_gcanvas(GLAME_CANVAS(object));
	DPRINTF("Destroying glamecanvas\n");
	if(GLAME_CANVAS(object)->net){
		if (!GLAME_CANVAS(object)->openedUp) {
			filter_terminate(GLAME_CANVAS(object)->net->launch_context);
			filter_delete(GLAME_CANVAS(object)->net);
			GLAME_CANVAS(object)->net = NULL;
		}
	}
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS);
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
}

static void
glame_canvas_class_init(GlameCanvasClass *klass)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS (klass);
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
	item->last = NULL;
	hash_init_gcanvas(item);
	item->clipBoard = NULL;
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
        }
        
        return canvas_type;
}
//////////////
static void
glame_canvas_group_destroy (GtkObject *object)
{
	GnomeCanvasGroupClass *parent_class;

	//if(GLAME_IS_CANVAS_GROUP(GCI(object)->parent)){
	//GlameCanvasGroup* parent = GCI(object)->parent;
	//parent->groups = g_list_remove(parent->groups,GLAME_CANVAS_GROUP(object));
        //}
	//else{
	//GlameCanvas* canvas = CANVAS_ITEM_GLAME_CANVAS(object);
	//canvas->selectedItems = g_list_remove(canvas->selectedItems,GLAME_CANVAS_GROUP(object));
	//}	
	parent_class = gtk_type_class(GNOME_TYPE_CANVAS_GROUP);
	GTK_OBJECT_CLASS(parent_class)->destroy(object);
}


static void
glame_canvas_group_class_init(GlameCanvasGroupClass *klass)
{
	GtkObjectClass *object_class;
	
	object_class = GTK_OBJECT_CLASS (klass);
	object_class->destroy = glame_canvas_group_destroy;
}


static void
glame_canvas_group_init (GlameCanvasGroup *item)
{
	static guint id_count = 0;
	item->type = GROUP_TYPE_UNION;
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
	GtkTargetEntry desttargets[] = { GLAME_DND_TARGET_STRING_SCM, 
					 GLAME_DND_TARGET_STRING_SCM_NETWORK,
					 GLAME_DND_TARGET_POINTER_FILTER_T,
					 {"text/uri-list",0,0},
					 {"text/x-sh",0,0}
					 };

	filter_t * network;
	if(!net){
		network = filter_creat(NULL);
	} else {
		if(!FILTER_IS_NETWORK(net)){
			//DPRINTF("Not network\n");
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
	hash_add_gcanvas(g);
	
	if(!filter_nrnodes(g->net))
		return g;

	glame_canvas_redraw(g);
	glame_canvas_view_all(g);

	/* setup dnd */
	//	gtk_drag_source_set(GTK_WIDGET(g),
			    /*
			      gtk_signal_connect(GTO(g),"drag-data-received",
			   glame_canvas_drag_received,
			   NULL);
	gtk_signal_connect(GTO(g),"drag-data-get",
			   glame_canvas_drag_received,
			   NULL);
	gtk_signal_connect(GTO(g),"drag-motion",
			   glame_foo,NULL);
	*/
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
	

/* Returns a font identifier in a static buffer which is ok, as
 * the GUI is single threaded. */
static char fontbuffer[256];
char* glame_gui_get_font(GlameCanvas* canv)
{
	snprintf(fontbuffer, 256, "-adobe-helvetica-medium-r-normal-*-%d-120-*-*-p-*-iso8859-1",
		 (int)(GNOME_CANVAS(canv)->pixels_per_unit*12.0));
	return fontbuffer;
}


GdkPixbuf*
glame_gui_get_icon_from_filter(filter_t *filter)
{
	return glame_load_icon(plugin_query(filter->plugin, PLUGIN_PIXMAP),64,64);

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
	canv->last = plug;
	return glame_canvas_add_filter(canv, filter);
}

/* kills all items and performs a full redraw 
   use only when really necessary!!!          */
void glame_canvas_full_redraw(GlameCanvas *canv)
{
	glame_canvas_pipe_destroy_all(GNOME_CANVAS(canv));
	glame_canvas_port_destroy_all(GNOME_CANVAS(canv));
	glame_canvas_filter_destroy_all(GNOME_CANVAS(canv));
	glame_canvas_redraw(canv);
}


void glame_canvas_redraw(GlameCanvas *canv)
{
	filter_t *node;
	GlameCanvasFilter *gf;
	GnomeCanvasGroup* root;
	filter_pipe_t *pipe;
	filter_port_t *port;
	filter_t *network;
	GList *newNodes = NULL, *iter = NULL;
	char * buffer;
	root = gnome_canvas_root(GNOME_CANVAS(canv));
	network = canv->net;

	filter_foreach_node(canv->net, node){
		gf = glame_canvas_find_filter(node);
		if (gf) {
			glame_canvas_filter_redraw(gf);
		} else {
			glame_canvas_filter_new(root,node);
			newNodes = g_list_append(newNodes,node);
		}
	}
	if (!newNodes)
		return;

	iter = g_list_first(newNodes);
	while(iter){
		node = (filter_t*)iter->data;
		glame_list_foreach(&node->connections, filter_pipe_t, list, pipe)
			glame_canvas_pipe_new(root, pipe);
		iter = g_list_next(iter);
	}
	/* external ports */
	filterportdb_foreach_port(filter_portdb(network),port){
		node = filter_get_node(network,filterport_get_property(port,FILTERPORT_MAP_NODE));
		buffer = filterport_get_property(port,FILTERPORT_MAP_LABEL);
		if (!node || !buffer )
			continue;
		glame_canvas_port_set_external(glame_canvas_find_port(filterportdb_get_port(filter_portdb(node),buffer)),TRUE);
	}
	g_list_free(newNodes);
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

GlameCanvasGroup* glame_canvas_group_root(GnomeCanvasItem* group)
{
	if(!GLAME_IS_CANVAS_GROUP(group->parent))
		return GLAME_CANVAS_GROUP(group);
	else 
		return glame_canvas_group_root(
			group->parent);
}

guint glame_canvas_group_root_id(GlameCanvasGroup* group)
{
	return (glame_canvas_group_root(GCI(group)))->id;
}

void glame_canvas_group_move(GlameCanvasGroup* group, double x, double y)
{

	GList * iter;
	iter = g_list_first(GNOME_CANVAS_GROUP(group)->item_list);
	while(iter){
		if(GLAME_IS_CANVAS_FILTER(iter->data))
			_glame_canvas_filter_move(GLAME_CANVAS_FILTER(iter->data),x,y);
		else if(GLAME_IS_CANVAS_GROUP(iter->data))
			glame_canvas_group_move(GLAME_CANVAS_GROUP(iter->data),x,y);
		iter = g_list_next(iter);
	}

}

void glame_canvas_group_item_moved_cb(GlameCanvasFilter* item, double x, double y, GlameCanvasGroup* group)
{
	GlameCanvasGroup* root = glame_canvas_group_root(GCI(group));
	
	glame_canvas_group_move(root,x,y);
}
	

void glame_canvas_group_dissolve(GlameCanvasGroup* group)
{

	GList * iter;
	GnomeCanvasGroup* root = CANVAS_ITEM_ROOT(group);
	

	iter = g_list_first(GNOME_CANVAS_GROUP(group)->item_list);
	while(iter){
		if(GLAME_IS_CANVAS_FILTER(iter->data)){
			GlameCanvasGroup *newgrp;
			//DPRINTF("found filter: %s\n",filter_name(GLAME_CANVAS_FILTER(iter->data)->filter));
			newgrp = GLAME_CANVAS_GROUP(gnome_canvas_item_new(
				root,
				glame_canvas_group_get_type(),
				NULL));
			gnome_canvas_item_reparent(GNOME_CANVAS_ITEM(iter->data),GNOME_CANVAS_GROUP(newgrp));
		}else if(GLAME_IS_CANVAS_GROUP(iter->data)){
			//DPRINTF("Reparenting group %d to root\n",GLAME_CANVAS_GROUP(iter->data)->id);
			gnome_canvas_item_reparent(GNOME_CANVAS_ITEM(iter->data),root);
			glame_canvas_group_unselect(GLAME_CANVAS_GROUP(iter->data));
		}
		iter = g_list_first(GNOME_CANVAS_GROUP(group)->item_list);
	}
	
}

void glame_canvas_add_last(GlameCanvas* canvas)
{
	if(canvas->last){
		glame_canvas_add_filter_by_plugin(canvas,canvas->last);
	}
}

void glame_canvas_group_delete(GlameCanvasGroup* group)
{

	GList* iter;

	iter = g_list_first(GNOME_CANVAS_GROUP(group)->item_list);
	while(iter){
		if(GLAME_IS_CANVAS_FILTER(iter->data)){
			filter_delete(GLAME_CANVAS_FILTER(iter->data)->filter);
		}else if(GLAME_IS_CANVAS_GROUP(iter->data)){
			glame_canvas_group_delete(GLAME_CANVAS_GROUP(iter->data));
		}
		iter = g_list_next(iter);
	}
}


void _glame_canvas_group_select(GlameCanvasGroup* group)
{
	GlameCanvas* canvas;
	GList* iter;

	canvas = CANVAS_ITEM_GLAME_CANVAS(group);

	iter = g_list_first(GNOME_CANVAS_GROUP(group)->item_list);
	while(iter){
		if(GLAME_IS_CANVAS_FILTER(iter->data)){
			glame_canvas_select_add(canvas,GLAME_CANVAS_FILTER(iter->data));
		}else if(GLAME_IS_CANVAS_GROUP(iter->data)){
			_glame_canvas_group_select(GLAME_CANVAS_GROUP(iter->data));
		}
		iter = g_list_next(iter);
	}
}

void glame_canvas_group_select(GlameCanvasGroup* group)
{
	_glame_canvas_group_select(glame_canvas_group_root(GCI(group)));
	glame_canvas_group_raise(glame_canvas_group_root(GCI(group)));
	glame_canvas_redraw(CANVAS_ITEM_GLAME_CANVAS(group));
}

void _glame_canvas_group_unselect(GlameCanvasGroup* group)
{
	GlameCanvas* canvas;
	GList * iter;

	canvas = CANVAS_ITEM_GLAME_CANVAS(GCI(group));
	iter = g_list_first(GNOME_CANVAS_GROUP(group)->item_list);
	
	while(iter){
		if(GLAME_IS_CANVAS_FILTER(iter->data)){
			glame_canvas_filter_set_selected(GLAME_CANVAS_FILTER(iter->data),FALSE);
		}else if(GLAME_IS_CANVAS_GROUP(iter->data)){
			_glame_canvas_group_unselect(GLAME_CANVAS_GROUP(iter->data));
		}
		iter = g_list_next(iter);
	}
}

void glame_canvas_group_unselect(GlameCanvasGroup* group)
{
	GlameCanvasGroup* root;
	GlameCanvas* canvas;

	if(!group)return;
	root = glame_canvas_group_root(GCI(group));
	canvas = CANVAS_ITEM_GLAME_CANVAS(group);
	_glame_canvas_group_unselect(root);
	canvas->selectedItems = g_list_remove(canvas->selectedItems,glame_canvas_group_root(GCI(group)));
	glame_canvas_redraw(canvas);
}

void _glame_canvas_group_raise(GlameCanvasGroup* group)
{
		

	if(!group)
		return;
	else{
		GList *iter;

		iter = g_list_first(GNOME_CANVAS_GROUP(group)->item_list);
		
		while(iter){
			if(GLAME_IS_CANVAS_FILTER(iter->data)){
				_glame_canvas_filter_raise_to_top(GLAME_CANVAS_FILTER(iter->data));
			}else if(GLAME_IS_CANVAS_GROUP(iter->data)){
				_glame_canvas_group_raise(GLAME_CANVAS_GROUP(iter->data));
			}
			iter = g_list_next(iter);
		}
	}
}

void glame_canvas_group_raise(GlameCanvasGroup* group)
{
	_glame_canvas_group_raise(glame_canvas_group_root(GCI(group)));
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
		item = glame_canvas_find_filter(filter);
		if(item){
			glame_canvas_filter_move(item,0.0,0.0);
			glame_canvas_filter_redraw(item);
		}
		
	}

}

static gint compareItemPointers(gconstpointer a, gconstpointer b)
{
	if(a>b)return 1;
	return 0;
}

void glame_canvas_select_add(GlameCanvas* canvas, GlameCanvasFilter* filter)
{
        GlameCanvasGroup* root = glame_canvas_group_root(GCI(filter));
	glame_canvas_filter_set_selected(filter,TRUE);
	if(!g_list_find(canvas->selectedItems,root)){
		canvas->selectedItems = g_list_insert_sorted(canvas->selectedItems,root,compareItemPointers);
	}
}

void glame_canvas_select_exclusive(GlameCanvas* canvas, GlameCanvasFilter* filter)
{
	glame_canvas_select_clear(canvas);
	glame_canvas_select_add(canvas,filter);
}


void glame_canvas_select_clear(GlameCanvas *canvas)
{
	GList* iter, *temp;
	iter = g_list_first(canvas->selectedItems);
	while(iter){
		if(GLAME_IS_CANVAS_GROUP(iter->data)){
			temp = iter;
			iter = g_list_next(iter);
			glame_canvas_group_unselect(GLAME_CANVAS_GROUP(temp->data));
		}
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
	GList *list;
	GlameCanvasGroup* group;

	group = GLAME_CANVAS_GROUP(gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(canvas)),
							 glame_canvas_group_get_type(),
							 NULL));
	list = g_list_first(canvas->selectedItems);
	while(list){
		//DPRINTF("reparenting %d to %d\n",glame_canvas_group_root_id(list->data),glame_canvas_group_root_id(group));
		gnome_canvas_item_reparent(GNOME_CANVAS_ITEM(list->data),
					   GNOME_CANVAS_GROUP(group));
		list = g_list_next(list);
	}
	glame_canvas_select_clear(canvas);
	glame_canvas_redraw(canvas);
}



GList* glame_canvas_get_selected_items(GlameCanvas* canvas)
{
	GList* list = NULL;
	filter_t* net;
	filter_t* filter;

	/* not nice.. */
	net = canvas->net;
	filter_foreach_node(net, filter){
		if(glame_canvas_find_filter(filter)->selected)
			list = g_list_append(list,filter);
	}
	return list;
}
	
void glame_canvas_ungroup_selected(GlameCanvas* canvas)
{
	GList *list = g_list_first(canvas->selectedItems);
	
/* Yuck! FIXME */
	while(list){
		//DPRINTF("dissolving group: %d\n",glame_canvas_group_root_id(list->data));
		glame_canvas_group_dissolve(GLAME_CANVAS_GROUP(list->data));
		list = g_list_next(list);
	}
	glame_canvas_redraw(canvas);
}


GlameCanvas* glame_canvas_find_canvas(filter_t *f)
{
	return hash_find_gcanvas(f);
}

void glame_canvas_copy_selected(GlameCanvas* canv)
{
        GList *items = glame_canvas_get_selected_items(canv), *n;
        filter_t *net, **nodes;
        int i;

        if (!items)
                return;

        nodes = alloca(sizeof(filter_t *)*(g_list_length(items)+1));
        for (i=0, n = g_list_first(items); n != NULL; n = g_list_next(n), i++) {
                nodes[i] = (filter_t *)(n->data);
        }
        nodes[i] = NULL;

        net = filter_collapse("Clipboard", nodes);

	if (canv->clipBoard)
		filter_delete(canv->clipBoard);
	canv->clipBoard = filter_creat(net);
	filter_expand(net);
	filter_delete(net);
}

void glame_canvas_paste_selection(GlameCanvas* canv)
{
	filter_t *newnode;
	if (!canv->clipBoard)
		return;
	newnode = filter_creat(canv->clipBoard);
	filter_add_node(canv->net, newnode, "foo");
	filter_expand(newnode);
	filter_delete(newnode);
	glame_canvas_full_redraw(canv);
}


GList* glame_canvas_find_items_in_region(GlameCanvas *canv, gdouble x1,gdouble y1,gdouble x2,gdouble y2)
{
	GList * retlist=NULL;
	GlameCanvasFilter* gcf;
	filter_t *f;
	double ix1,ix2,iy1,iy2;
	filter_foreach_node(canv->net, f){
		gcf = glame_canvas_find_filter(f);
		if(gcf){
			gnome_canvas_item_get_bounds(GNOME_CANVAS_ITEM(gcf),
                                                     &ix1,&iy1,&ix2,&iy2);

			if(in_between(x1,x2,ix1))
				if(in_between(y1,y2,iy1))
					retlist = g_list_append(retlist,gcf);
		}
	}
	return retlist;
}


void glame_canvas_select_item(GlameCanvas* canv, GlameCanvasFilter* filter)
{
	glame_canvas_group_select(GLAME_CANVAS_GROUP(GNOME_CANVAS_ITEM(filter)->parent));
}

filter_t* glame_canvas_collapse_selection(GlameCanvas * canv)
{

	GList *items = glame_canvas_get_selected_items(canv), *n;
	filter_t *net, **nodes;
	int i;
	GlameCanvasFilter *filter;
	
	gdouble x1,y1,x2,y2;
	char buffer[10];
	if (!items)
		return NULL;
	if(g_list_length(items)<2)
		return NULL;
	
	nodes = alloca(sizeof(filter_t *)*(g_list_length(items)+1));
	for (i=0, n = g_list_first(items); n != NULL; n = g_list_next(n), i++) {
		nodes[i] = (filter_t *)(n->data);
	}
	nodes[i] = NULL;
	
	filter = glame_canvas_find_filter(nodes[0]);
	
	net = filter_collapse("Collapsed", nodes);
	if (!net) {
		DPRINTF("Error collapsing selection\n");
		return NULL;
	}
	
	if(filter){
		gnome_canvas_item_get_bounds(GCI(filter),&x1,&y1,&x2,&y2);
		
		snprintf(buffer,9,"%.1f",x1); 
		filter_set_property(net, "canvas_x", buffer);
		
		snprintf(buffer,9,"%.1f",y1); 
		filter_set_property(net, "canvas_y", buffer);
	}
	glame_canvas_full_redraw(canv);
	return net;
}


