/*
 * canvaspipe.c
 *
 * $Id: canvaspipe.c,v 1.1 2001/05/07 00:45:36 xwolf Exp $
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



HASH(gcpipe, GlameCanvasPipe, 8,
	(gcpipe->pipe == key ),
	((long)key/4),
	((long)gcpipe->pipe/4),
	filter_pipe_t * key)

     
     /*	GlameCanvasFilter* hash_find_GCfilter(filter_t*);
	void hash_add_GCFilter(GlameCanvasFilter*);
     */


     /*  Forward decls */



/*Yukky gtk class stuff    
 * Ignore these
 */


enum {
	DELETED,
	LAST_SIGNAL
};
static guint pipe_signals[LAST_SIGNAL] = { 0 };



static void
glame_canvas_pipe_destroy (GtkObject *object)
{
	GTK_OBJECT_CLASS(gtk_type_class(GLAME_CANVAS_PIPE_TYPE))->destroy(object);
}


static void
glame_canvas_pipe_class_init(GlameCanvasPipeClass* class)
{
	
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = glame_canvas_pipe_destroy;

	pipe_signals[DELETED] = gtk_signal_new("deleted",
					       GTK_RUN_LAST,
					       object_class->type,
					       GTK_SIGNAL_OFFSET(GlameCanvasPipeClass,deleted),
					       gtk_marshal_NONE__NONE,
					       GTK_TYPE_NONE,
					       0);
	gtk_object_class_add_signals(object_class,pipe_signals,LAST_SIGNAL);
	class->deleted = NULL;
}

static void
glame_canvas_pipe_init(GlameCanvasPipe *p)
{
	p->pipe = NULL;
	p->property_texts = NULL;
	p->points = NULL;
	p->line = NULL;
	p->circle = NULL;
	p->sourceId = 0;
	p->destId = 0;
}

GtkType
glame_canvas_pipe_get_type(void)
{
	static GtkType canvas_pipe_type = 0;
	
	if(!canvas_pipe_type){
		GtkTypeInfo canvas_pipe_info={
			"GlameCanvasPipe",
			sizeof(GlameCanvasPipe),
			sizeof(GlameCanvasPipeClass),
			(GtkClassInitFunc) glame_canvas_pipe_class_init,
			(GtkObjectInitFunc) glame_canvas_pipe_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		canvas_pipe_type = gtk_type_unique(GNOME_TYPE_CANVAS_GROUP,
						   &canvas_pipe_info);
		gtk_type_set_chunk_alloc(canvas_pipe_type,8);
	}
	
	return canvas_pipe_type;
}


GlameCanvasPipe* 
glame_canvas_find_pipe(filter_pipe_t *p)
{
	return hash_find_gcpipe(p);
}

static void
glame_canvas_pipe_reroute(GlameCanvasPipe *p)
{
	/* draw connection depending on start and end coords  */
	double xs, ys, xd,yd;
	double xOffset, dist;
	
	xs = p->points->coords[0];
	ys = p->points->coords[1];
	xd = p->points->coords[10];
	yd = p->points->coords[11];
	

	dist = xd-xs;
	if(dist<50.0)
		xOffset = dist/2.0;
	xOffset=(xOffset<5.0)?5.0:xOffset;
	
	
	p->points->coords[2]=xs+xOffset+(p->sourceId*4);
	p->points->coords[3]=ys;
	
	p->points->coords[4]=xs+xOffset+(p->sourceId*4);
	p->points->coords[5]=(ys+yd)/2.0;
	
	p->points->coords[6]=xd-25.0-(p->destId*4);
	p->points->coords[7]=(ys+yd)/2.0;
	
	p->points->coords[8]=xd-25.0-(p->destId*4);
	p->points->coords[9]=yd;
	
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(p->line),
			      "points",p->points,
			      NULL);
	
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(p->circle),
			      "x1",((xs+xd)/2.0)-6.0,
			      "x2",((xs+xd)/2.0)+6.0,
			      "y1",((ys+yd)/2.0)-6.0,
			      "y2",((ys+yd)/2.0)+6.0,
			      NULL);
}
	

void
glame_canvas_pipe_redraw(GlameCanvasPipe *p)
{
	double sourcex,sourcey,destx,desty;
	GlameCanvasPort *port;
	int pipes;

	/* find coords */
	
	port = glame_canvas_find_port(filterpipe_source(p->pipe));
	pipes = filterport_nrpipes(filterpipe_source(p->pipe));

	sourcex = GNOME_CANVAS_RE(port)->x1 + 16.0;
	sourcey = ((GNOME_CANVAS_RE(port)->y2 - GNOME_CANVAS_RE(port)->y1)/(float)(pipes+1))*p->sourceId;

	port = glame_canvas_find_port(filterpipe_dest(p->pipe));
	pipes = filterport_nrpipes(filterpipe_dest(p->pipe));

	destx = GNOME_CANVAS_RE(port)->x1;
	desty = ((GNOME_CANVAS_RE(port)->y2 - GNOME_CANVAS_RE(port)->y1)/(float)(pipes+1))*p->destId;
	
	p->points->coords[0] = sourcex;
	p->points->coords[1] = sourcey;
	p->points->coords[10] = destx;
	p->points->coords[11] = destx;
	glame_canvas_pipe_reroute(p);
}


static void
glame_canvas_pipe_end_moved_cb(GlameCanvasPort* p,double dx, double dy, GlameCanvasPipe* pipe)
{
	pipe->points->coords[10] += dx;
	pipe->points->coords[11] += dy;
	glame_canvas_pipe_reroute(pipe);
}

static void
glame_canvas_pipe_begin_moved_cb(GlameCanvasPort* p,double dx, double dy, GlameCanvasPipe* pipe)
{
	pipe->points->coords[0] += dx;
	pipe->points->coords[1] += dy;
	glame_canvas_pipe_reroute(pipe);
}

static void
glame_canvas_pipe_port_changed_cb(GlameCanvasPort* p, GlameCanvasPipe* pipe)
{
	glame_canvas_pipe_redraw(pipe);
}
static void
glame_canvas_pipe_deleted_cb(glsig_handler_t* foo,long sig,va_list va)
{
	
	GlameCanvasPipe* gPipe = glsig_handler_private(foo);
	filter_pipe_t* pipe = gPipe->pipe;
	
	gtk_signal_emit_by_name(GTK_OBJECT(glame_canvas_find_port(filterpipe_dest(pipe))),
				"connections_changed");
	gtk_signal_emit_by_name(GTK_OBJECT(glame_canvas_find_port(filterpipe_source(pipe))),
				"connections_changed");
	gtk_object_destroy(GTK_OBJECT(gPipe));
}
	
static void
glame_canvas_pipe_event_cb(GnomeCanvasItem* i, GdkEvent* event, GlameCanvasPipe* p)
{}

GlameCanvasPipe* glame_canvas_pipe_new(GnomeCanvasGroup *group, filter_pipe_t * pipe)
{
	GlameCanvasPipe* gPipe;
	int i;

	gPipe = GLAME_CANVAS_PIPE(gnome_canvas_item_new(group, 
							glame_canvas_pipe_get_type(),
							NULL));

	/* create and clear line coords */
	gPipe->points = gnome_canvas_points_new(6);
	for(i=0;i<12;i++)
		gPipe->points->coords[i] = 0.0;
	
	gPipe->line = GNOME_CANVAS_LINE(gnome_canvas_item_new(group,
								   gnome_canvas_line_get_type(),
								   "points",gPipe->points,
								   "fill_color","black",
								   "width_units",2.0,
								   "arrow_shape_a",18.0,
								   "arrow_shape_b",20.0,
								   "arrow_shape_c",5.0,
								   "last_arrowhead",TRUE,
								   NULL));
	gPipe->circle = GNOME_CANVAS_ELLIPSE(gnome_canvas_item_new(group,
								   gnome_canvas_ellipse_get_type(),
								   "x1",0.0,
								   "x2",0.0,
								   "y1",0.0,
								   "y2",0.0,
								   "fill_color","black",
								   "width_pixels",5,
								   NULL));
	gPipe->pipe = pipe;
	
	hash_add_gcpipe(gPipe);
	
	gtk_signal_connect(GTK_OBJECT(gPipe->circle),
			   "event",
			   glame_canvas_pipe_event_cb,
			   gPipe);
	
	/* connect to movement of ports */
	gtk_signal_connect(GTK_OBJECT(glame_canvas_find_port(filterpipe_dest(pipe))),
			   "moved",
			   glame_canvas_pipe_end_moved_cb,
			   gPipe);
	
	gtk_signal_connect(GTK_OBJECT(glame_canvas_find_port(filterpipe_source(pipe))),
			   "moved",
			   glame_canvas_pipe_begin_moved_cb,
			   gPipe);
	
	/* connect to reordering of connections at port */
	gtk_signal_connect(GTK_OBJECT(glame_canvas_find_port(filterpipe_dest(pipe))),
			   "connections_changed",
			   glame_canvas_pipe_port_changed_cb,
			   gPipe);
	
	gtk_signal_connect(GTK_OBJECT(glame_canvas_find_port(filterpipe_source(pipe))),
			   "connections_changed",
			   glame_canvas_pipe_port_changed_cb,
			   gPipe);

	/* connect to backend destroy sig */
	glsig_add_handler(filterpipe_emitter(pipe),GLSIG_PIPE_DELETED,glame_canvas_pipe_deleted_cb,gPipe);

	/* reorder all pipes */
	gtk_signal_emit_by_name(GTK_OBJECT(glame_canvas_find_port(filterpipe_dest(pipe))),
				"connections_changed");
	gtk_signal_emit_by_name(GTK_OBJECT(glame_canvas_find_port(filterpipe_source(pipe))),
				"connections_changed");
	
	return gPipe;
}
	
