/*
 * canvasitem.c
 *
 * $Id: glamecanvas.c,v 1.3 2001/05/08 21:54:01 xwolf Exp $
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

#include <gnome.h>
#include "glamecanvas.h"
#include "canvasitem.h"
#include "hash.h"


     

/*  Forward decls */



/*Yukky gtk class stuff    
 * Ignore these
 */


static void
glame_canvas_destroy (GtkObject *object)
{
	gtk_object_destroy(object);
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

/*****************************
 * Whew...
 * now for the real work
 *****************************/


GlameCanvas* glame_canvas_new(filter_t * network)
{
	GlameCanvas *g;
	GnomeCanvasGroup *group;
	filter_t * node;
	filter_t * filter;
	filter_port_t * port;
	filter_pipe_t *pipe;
	char * buffer;

	if(!network){
		network = filter_creat(NULL);
	} else {
		if(!FILTER_IS_NETWORK(network)){
			DPRINTF("Not network\n");
			return NULL;
		}
	}

	g = gtk_type_new(glame_canvas_get_type());
	if(!g)
		return NULL;

	g->net = network;
	
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
			
		}
		*/
		filterportdb_foreach_port(filter_portdb(node),port){
			filterport_foreach_pipe(port,pipe){
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
		glame_canvas_port_set_external(filterportdb_get_port(filter_portdb(node),buffer),TRUE);
	}
	
	/*canvas_update_scroll_region(GLAME_CANVAS(canv));*/
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
	

char* 
glame_gui_get_font(GlameCanvas* canv)
{
	/* HACK */
	return strdup("-adobe-helvetica-medium-r-normal-*-12-120-*-*-p-*-iso8859-1");
}


GdkImlibImage*
glame_gui_get_icon_from_filter(filter_t *filter)
{
	/* HACK */

	return gdk_imlib_load_image("test.png");
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
