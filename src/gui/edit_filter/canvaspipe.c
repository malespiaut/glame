/*
 * canvaspipe.c
 *
 * $Id: canvaspipe.c,v 1.30 2003/04/21 12:16:06 richi Exp $
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
#include "util/glame_gui_utils.h"
#include "hash.h"
#include "edit_filter_marshal.h"

extern long bMac;
extern long nPopupTimeout;

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
	GnomeCanvasGroupClass* parent_class;
	hash_remove_gcpipe(GLAME_CANVAS_PIPE(object));
	parent_class = gtk_type_class (GNOME_TYPE_CANVAS_GROUP);
	GTK_OBJECT_CLASS (parent_class)->destroy (object);
}


static void
glame_canvas_pipe_class_init(GlameCanvasPipeClass* klass)
{
	
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(klass);
	object_class->destroy = glame_canvas_pipe_destroy;

	pipe_signals[DELETED] = g_signal_new("deleted",
					     G_OBJECT_CLASS_TYPE(object_class),
					     GTK_RUN_LAST,
					     GTK_SIGNAL_OFFSET(GlameCanvasPipeClass,deleted),
					     NULL,NULL,
					     edit_filter_marshal_VOID__VOID,
					     GTK_TYPE_NONE,
					     0);
	//	gtk_object_class_add_signals(object_class,pipe_signals,LAST_SIGNAL);
	klass->deleted = NULL;
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
	p->dy = 0.0;
	hash_init_gcpipe(p);
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
	double dy = p->dy;
	xs = p->points->coords[0];
	ys = p->points->coords[1];
	xd = p->points->coords[10];
	yd = p->points->coords[11];
	
	xOffset = 25.0;
	dist = xd-xs;
	xOffset = 25.0;
	if(dist<50.0)
		xOffset = dist/2.0;
	xOffset=(xOffset<12.0)?12.0:xOffset;
	
	
	p->points->coords[2]=xs+xOffset+(p->sourceId*4);
	p->points->coords[3]=ys;
	
	p->points->coords[4]=xs+xOffset+(p->sourceId*4);
	p->points->coords[5]=(ys+yd)/2.0 + dy;
	
	p->points->coords[6]=xd-xOffset-(p->destId*4);
	p->points->coords[7]=(ys+yd)/2.0+dy;
	
	p->points->coords[8]=xd-xOffset-(p->destId*4);
	p->points->coords[9]=yd;
	
	
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(p->line),
			      "points",p->points,
			      NULL);
	
	gnome_canvas_item_set(GNOME_CANVAS_ITEM(p->circle),
			      "x1",((xs+xd)/2.0)-6.0,
			      "x2",((xs+xd)/2.0)+6.0,
			      "y1",((ys+yd)/2.0+dy)-6.0,
			      "y2",((ys+yd)/2.0+dy)+6.0,
			      NULL);
}
	

void
glame_canvas_pipe_redraw(GlameCanvasPipe *p)
{
	double sourcex,sourcey,destx,desty;
	GlameCanvasPort *port;
	int pipes;

	/* find coords */
	
	port = glame_canvas_find_port(filterpipe_connection_source(p->pipe));
	pipes = filterport_nrpipes(filterpipe_source(p->pipe));
	
	if(!port){
		DPRINTF("canvasport not found\n");
		return;
	}
	sourcex = GNOME_CANVAS_RE(port)->x1 + 16.0;
	sourcey = GNOME_CANVAS_RE(port)->y1 + ((GNOME_CANVAS_RE(port)->y2 - GNOME_CANVAS_RE(port)->y1)/(float)(pipes+1))*p->sourceId;
	gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(port),&sourcex,&sourcey);
	
	port = glame_canvas_find_port(filterpipe_connection_dest(p->pipe));
	pipes = filterport_nrpipes(filterpipe_dest(p->pipe));

	destx = GNOME_CANVAS_RE(port)->x1;
	
	desty = GNOME_CANVAS_RE(port)->y1 + ((GNOME_CANVAS_RE(port)->y2 - GNOME_CANVAS_RE(port)->y1)/(float)(pipes+1))*p->destId;
	gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(port),&destx,&desty);
	
	p->points->coords[0] = sourcex;
	p->points->coords[1] = sourcey;
	p->points->coords[10] = destx;
	p->points->coords[11] = desty;
	glame_canvas_pipe_reroute(p);
}


static void
glame_canvas_pipe_end_moved_cb(GlameCanvasPort* p,double dx, double dy, GlameCanvasPipe* pipe)
{
	pipe->points->coords[10] += dx;
	pipe->points->coords[11] += dy;
	glame_canvas_pipe_reroute(pipe);
}

static gboolean
glame_canvas_pipe_begin_moved_cb(GlameCanvasPort* p,double dx, double dy, GlameCanvasPipe* pipe)
{
	pipe->points->coords[0] += dx;
	pipe->points->coords[1] += dy;
	glame_canvas_pipe_reroute(pipe);
	return FALSE;
}

static void
glame_canvas_pipe_port_changed_cb(GlameCanvasPort* p, GlameCanvasPipe* pipe)
{
	glame_canvas_pipe_redraw(pipe);
}

static void
glame_canvas_pipe_deleted_cb(glsig_handler_t *handler,long sig,va_list va)
{
	GlameCanvasPipe* gPipe;
	filter_pipe_t *pipe;

	/* Ignore not existing pipes (and delete handler) */
	GLSIGH_GETARGS1(va, pipe);
	if (!hash_find_gcpipe(pipe)) {
		glsig_delete_handler(handler);
		return;
	}

	gPipe = GLAME_CANVAS_PIPE(glsig_handler_private(handler));
	gtk_signal_emit_by_name(GTK_OBJECT(gPipe),"deleted");
	gtk_object_destroy(GTK_OBJECT(gPipe));
}
	

static gboolean
glame_canvas_pipe_event_cb(GnomeCanvasItem* i, GdkEvent* event, GlameCanvasPipe* p);



/****************
 * menu stuff
 ****************/

static void canvas_pipe_source_properties_cb(GtkObject *foo,
					     filter_pipe_t *pipe)
{
	GtkWidget *p;
	p = glame_gui_filter_properties(filterpipe_sourceparamdb(pipe),
					filterport_label(filterpipe_connection_source(pipe)), NULL);
	gtk_widget_show(p);
}
static void canvas_pipe_dest_properties_cb(GtkObject *foo, filter_pipe_t *pipe)
{
	GtkWidget *p;
	p = glame_gui_filter_properties(filterpipe_destparamdb(pipe),
					filterport_label(filterpipe_connection_dest(pipe)), NULL);
	gtk_widget_show(p);
}
static void canvas_pipe_delete_cb(GtkObject* foo, filter_pipe_t* pipe)
{
	filterpipe_delete(pipe);
}

static GnomeUIInfo pipe_menu[]=
{
	GNOMEUIINFO_ITEM(N_("_Source properties..."), NULL, canvas_pipe_source_properties_cb, NULL),
	GNOMEUIINFO_ITEM(N_("D_estination properties..."), NULL, canvas_pipe_dest_properties_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM(N_("_Delete"), NULL, canvas_pipe_delete_cb,NULL),
	GNOMEUIINFO_END
};



static gboolean
glame_canvas_pipe_grabbing_cb(GnomeCanvasItem* i, GdkEvent* event, GlameCanvasPipe* p)
{
	GtkWidget* menu;
	switch(event->type){
	case GDK_MOTION_NOTIFY:
		/* do something here */
		p->dy -= p->last_y - event->button.y;
		p->last_y = event->button.y;
		glame_canvas_pipe_reroute(p);
		return TRUE;
		break;
	case GDK_BUTTON_RELEASE:
		gnome_canvas_item_ungrab(i,event->button.time);
		gtk_signal_disconnect_by_func(GTO(i),GTK_SIGNAL_FUNC(glame_canvas_pipe_grabbing_cb),p);
		gtk_signal_handler_unblock_by_func(GTO(i),GTK_SIGNAL_FUNC(glame_canvas_pipe_event_cb),p);
		return TRUE;
	case GDK_2BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			if(bMac){
				menu = gnome_popup_menu_new(pipe_menu);
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,p->pipe,GTK_WIDGET(CANVAS_ITEM_CANVAS(i)));
			}
			break;
		}
		gnome_canvas_item_ungrab(i,event->button.time);
		gtk_signal_disconnect_by_func(GTO(i),GTK_SIGNAL_FUNC(glame_canvas_pipe_grabbing_cb),p);
		gtk_signal_handler_unblock_by_func(GTO(i),GTK_SIGNAL_FUNC(glame_canvas_pipe_event_cb),p);
		return TRUE;
		break;

	default:
		return FALSE;
	}
}



gboolean 
glame_canvas_pipe_show_properties(GlameCanvasPipe* pipe)
{

	filter_param_t* param;
	GnomeCanvasText * text;
	GnomeCanvasGroup *group;
	GnomeCanvasRect* rect;
	char buffer[256];
	const char* font = glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(pipe)->canvas));
	float y;
	double xOffset,yOffset;
	double bmaxx,bminx,bminy,bmaxy, recx1,recx2,recy1,recy2;
	gboolean params = FALSE;
	if(pipe->timeout_id){
		gtk_timeout_remove(pipe->timeout_id);
		pipe->timeout_id = 0;
	}
	
	bmaxx=-999999999.0;
	bminx=9999999999.0;
	bmaxy=-999999999.0;
	bminy=999999999.0;


	xOffset = (GNOME_CANVAS_RE(pipe->circle)->x1+GNOME_CANVAS_RE(pipe->circle)->x2)/2.0;
	yOffset = GNOME_CANVAS_RE(pipe->circle)->y2;

	y = 10.0 + yOffset;
	
	group = GNOME_CANVAS_GROUP(CANVAS_ITEM_ROOT(pipe));
	group = GNOME_CANVAS_GROUP(gnome_canvas_item_new(group,
				      gnome_canvas_group_get_type(),
				      NULL));
	gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(group));
	params = TRUE;

	if (filterpipe_type(pipe->pipe) == FILTER_PIPETYPE_SAMPLE) {
		snprintf(buffer, 255, _("Samplerate: %d"),
			 filterpipe_sample_rate(pipe->pipe));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(
			group,
			gnome_canvas_text_get_type(),
			"x",xOffset,
			"y",y,
			"text",buffer,
			"font",font,
			"clip_width",94.0,
			"clip_height",16.0,
			"fill_color","blue",
			"anchor",GTK_ANCHOR_WEST,
			"justification",GTK_JUSTIFY_LEFT, 
			"clip",0,
			NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
		gnome_canvas_item_get_bounds(GCI(text), &recx1, &recy1,
					     &recx2, &recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
       		y +=16.0;

		snprintf(buffer, 255, _("Hangle: %f"),
			 filterpipe_sample_hangle(pipe->pipe));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(
			group,
			gnome_canvas_text_get_type(),
			"x",xOffset,
			"y",y,
			"text",buffer,
			"font",font,
			"clip_width",94.0,
			"clip_height",16.0,
			"fill_color","blue",
			"anchor",GTK_ANCHOR_WEST,
			"justification",GTK_JUSTIFY_LEFT, 
			"clip",0,
			NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
       		gnome_canvas_item_get_bounds(GCI(text), &recx1, &recy1,
					     &recx2, &recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
 		y +=16.0;
	}

	else if (filterpipe_type(pipe->pipe) == FILTER_PIPETYPE_FFT) {
		snprintf(buffer, 255, _("Samplerate: %d"),
			 filterpipe_fft_rate(pipe->pipe));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(
			group,
			gnome_canvas_text_get_type(),
			"x",xOffset,
			"y",y,
			"text",buffer,
			"font",font,
			"clip_width",94.0,
			"clip_height",16.0,
			"fill_color","blue",
			"anchor",GTK_ANCHOR_WEST,
			"justification",GTK_JUSTIFY_LEFT, 
			"clip",0,
			NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
		gnome_canvas_item_get_bounds(GCI(text), &recx1, &recy1,
					     &recx2, &recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
		y +=16.0;

		snprintf(buffer, 255, _("Hangle: %f"),
			 filterpipe_fft_hangle(pipe->pipe));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(
			group,
			gnome_canvas_text_get_type(),
			"x",xOffset,
			"y",y,
			"text",buffer,
			"font",font,
			"clip_width",94.0,
			"clip_height",16.0,
			"fill_color","blue",
			"anchor",GTK_ANCHOR_WEST,
			"justification",GTK_JUSTIFY_LEFT, 
			"clip",0,
			NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
       		gnome_canvas_item_get_bounds(GCI(text), &recx1, &recy1,
					     &recx2, &recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
 		y +=16.0;

		snprintf(buffer, 255, _("FFT blocksize: %i"),
			 filterpipe_fft_bsize(pipe->pipe));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(
			group,
			gnome_canvas_text_get_type(),
			"x",xOffset,
			"y",y,
			"text",buffer,
			"font",font,
			"clip_width",94.0,
			"clip_height",16.0,
			"fill_color","blue",
			"anchor",GTK_ANCHOR_WEST,
			"justification",GTK_JUSTIFY_LEFT, 
			"clip",0,
			NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
		gnome_canvas_item_get_bounds(GCI(text), &recx1, &recy1,
					     &recx2, &recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
		y +=16.0;

		snprintf(buffer, 255, _("FFT oversampling: %i"),
			 filterpipe_fft_osamp(pipe->pipe));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(
			group,
			gnome_canvas_text_get_type(),
			"x",xOffset,
			"y",y,
			"text",buffer,
			"font",font,
			"clip_width",94.0,
			"clip_height",16.0,
			"fill_color","blue",
			"anchor",GTK_ANCHOR_WEST,
			"justification",GTK_JUSTIFY_LEFT, 
			"clip",0,
			NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
		gnome_canvas_item_get_bounds(GCI(text), &recx1, &recy1,
					     &recx2, &recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
		y +=16.0;
	}

	else if (filterpipe_type(pipe->pipe) == FILTER_PIPETYPE_SSP) {
		snprintf(buffer, 255, _("Samplerate: %d"),
			 filterpipe_ssp_rate(pipe->pipe));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(
			group,
			gnome_canvas_text_get_type(),
			"x",xOffset,
			"y",y,
			"text",buffer,
			"font",font,
			"clip_width",94.0,
			"clip_height",16.0,
			"fill_color","blue",
			"anchor",GTK_ANCHOR_WEST,
			"justification",GTK_JUSTIFY_LEFT, 
			"clip",0,
			NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
		gnome_canvas_item_get_bounds(GCI(text), &recx1, &recy1,
					     &recx2, &recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
		y +=16.0;

		snprintf(buffer, 255, _("SSP blocksize: %i"),
			 filterpipe_ssp_bsize(pipe->pipe));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(
			group,
			gnome_canvas_text_get_type(),
			"x",xOffset,
			"y",y,
			"text",buffer,
			"font",font,
			"clip_width",94.0,
			"clip_height",16.0,
			"fill_color","blue",
			"anchor",GTK_ANCHOR_WEST,
			"justification",GTK_JUSTIFY_LEFT, 
			"clip",0,
			NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
       		gnome_canvas_item_get_bounds(GCI(text), &recx1, &recy1,
					     &recx2, &recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
 		y +=16.0;
	}
	
	if(filterparamdb_nrparams(filterpipe_sourceparamdb(pipe->pipe))){
		params = TRUE;
		snprintf(buffer,255, _("Source params:"));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(group,
							       gnome_canvas_text_get_type(),
							       "x",xOffset,
							       "y",y,
							       "text",buffer,
							       "font",font,
							       "clip_width",94.0,
							       "clip_height",16.0,
							       "fill_color","blue",
							       "anchor",GTK_ANCHOR_WEST,
							       "justification",GTK_JUSTIFY_LEFT, 
							       "clip",0,
							       NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
		
		gnome_canvas_item_get_bounds(GCI(text),&recx1,&recy1,&recx2,&recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
		
		y +=16.0;
		
		filterparamdb_foreach_param(filterpipe_sourceparamdb(pipe->pipe),param){
			char *str;
			str = filterparam_to_string(param);
			snprintf(buffer, 255, "%s: %s", filterparam_label(param), str);
			free(str);
			text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(group,
								       gnome_canvas_text_get_type(),
								       "x",xOffset,
					     "y",y,
								       "text",buffer,
								       "font",font,
								       "clip_width",94.0,
								       "clip_height",16.0,
								       "fill_color","black",
								       "anchor",GTK_ANCHOR_WEST,
								       "justification",GTK_JUSTIFY_LEFT, 
								       "clip",0,
								       NULL));
			gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
			
			gnome_canvas_item_get_bounds(GCI(text),&recx1,&recy1,&recx2,&recy2);
			bmaxx = bmaxx<recx2?recx2:bmaxx;
			bmaxy = bmaxy<recy2?recy2:bmaxy;
			bminx = bminx>recx1?recx1:bminx;
			bminy = bminy>recy1?recy1:bminy;
			
			y+=16.0;
		}
	}
	if(filterparamdb_nrparams(filterpipe_destparamdb(pipe->pipe))){
		params = TRUE;
		snprintf(buffer,255, _("Dest params:"));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(group,
							       gnome_canvas_text_get_type(),
							       "x",xOffset,
							       "y",y,
							       "text",buffer,
							       "font",font,		       
							       "clip_width",94.0,
							       "clip_height",16.0,
							       "fill_color","red",
							       "anchor",GTK_ANCHOR_WEST,
							       "justification",GTK_JUSTIFY_LEFT, 
							       "clip",0,
							       NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
	
		gnome_canvas_item_get_bounds(GCI(text),&recx1,&recy1,&recx2,&recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;
		
		y +=16.0;
		
		filterparamdb_foreach_param(filterpipe_destparamdb(pipe->pipe),param){
			char *str;
			str = filterparam_to_string(param);
			snprintf(buffer, 255, "%s: %s", filterparam_label(param), str);
			free(str);
			text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(group,
								       gnome_canvas_text_get_type(),
								       "x",xOffset,
								       "y",y,
								       "text",buffer,
								       "font",font,
								       "clip_width",94.0,
								       "clip_height",16.0,
								       "fill_color","black",
								       "anchor",GTK_ANCHOR_WEST,
								       "justification",GTK_JUSTIFY_LEFT, 
								       "clip",0,
								       NULL));
			gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));
			
			gnome_canvas_item_get_bounds(GCI(text),&recx1,&recy1,&recx2,&recy2);
			bmaxx = bmaxx<recx2?recx2:bmaxx;
			bmaxy = bmaxy<recy2?recy2:bmaxy;
			bminx = bminx>recx1?recx1:bminx;
			bminy = bminy>recy1?recy1:bminy;
			
			y+=16.0;
		}
	}
	if(params){
	rect = GNOME_CANVAS_RECT(gnome_canvas_item_new(group,
				     gnome_canvas_rect_get_type(),
				     "x1",bminx -5.0,
				     "y1",bminy -5.0,
				     "x2",bmaxx+5.0,
				     "y2",bmaxy+5.0,
				     "outline_color","black",
				     "width_units",1.0,
				     "fill_color_rgba",0xd0d0ff00,
				     NULL));
	gnome_canvas_item_lower_to_bottom(GNOME_CANVAS_ITEM(rect));
	}
	pipe->popupGroup = group;
	return FALSE;
}

void
glame_canvas_pipe_hide_properties(GlameCanvasPipe *pipe)
{
	if(!pipe->popupGroup){
		return;
	}
	gtk_object_destroy(GTO(pipe->popupGroup));
	pipe->popupGroup = NULL;

}


static void
glame_canvas_pipe_register_popup(GlameCanvasPipe* pipe)
{
	if(pipe->timeout_id){
		gtk_timeout_remove(pipe->timeout_id);
	}
	pipe->timeout_id = gtk_timeout_add(nPopupTimeout,(GtkFunction)glame_canvas_pipe_show_properties,pipe);
}
static void
glame_canvas_pipe_deregister_popup(GlameCanvasPipe* pipe)
{
	if(pipe->timeout_id){
		gtk_timeout_remove(pipe->timeout_id);
		pipe->timeout_id = 0;
	}else{
		glame_canvas_pipe_hide_properties(pipe);
	}
}

static gboolean
glame_canvas_pipe_event_cb(GnomeCanvasItem* i, GdkEvent* event, GlameCanvasPipe* p)
{
	GdkCursor * fleur;
	GtkWidget* menu;
	switch(event->type){
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			p->last_y = event->button.y;
			fleur = gdk_cursor_new(GDK_FLEUR);
			/* block other handlers (this one ;-) */
			gtk_signal_handler_block_by_func(GTK_OBJECT(i),GTK_SIGNAL_FUNC(glame_canvas_pipe_event_cb),p);
			gtk_signal_connect(GTK_OBJECT(i),"event", GTK_SIGNAL_FUNC(glame_canvas_pipe_grabbing_cb), p);
			gnome_canvas_item_grab(GNOME_CANVAS_ITEM(i),GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK|GDK_BUTTON_PRESS_MASK,fleur,
					       event->button.time);
			gdk_cursor_destroy(fleur);
			return TRUE;
			break;
		case 3:
				/* popup menu */
			menu = gnome_popup_menu_new(pipe_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,p->pipe,GTK_WIDGET(CANVAS_ITEM_CANVAS(i)));

			break;
		default:
			return FALSE;
			break;
		}
	case GDK_2BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			if(bMac){
				menu = gnome_popup_menu_new(pipe_menu);
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,p->pipe,GTK_WIDGET(CANVAS_ITEM_CANVAS(i)));
			}
			break;
		}
		break;
	case GDK_ENTER_NOTIFY:
		glame_canvas_pipe_register_popup(p);
		break;
	case GDK_LEAVE_NOTIFY:
		glame_canvas_pipe_deregister_popup(p);
		break;
	default:
		return FALSE;
		break;
	}
	return FALSE;
}

GlameCanvasPipe* glame_canvas_pipe_new(GnomeCanvasGroup *group, filter_pipe_t * pipe)
{
	GlameCanvasPipe* gPipe;
	GnomeCanvasGroup * gGroup;
	GlameCanvasPort *gSource, *gDest;
	int i;
	
	if(glame_canvas_find_pipe(pipe)){
		DPRINTF("Trying to add duplicate pipe!\n");
		return glame_canvas_find_pipe(pipe);
	}
	gPipe = GLAME_CANVAS_PIPE(gnome_canvas_item_new(group, 
							glame_canvas_pipe_get_type(),
							NULL));
	gGroup = GNOME_CANVAS_GROUP(gPipe);
	
	/* create and clear line coords */
	gPipe->points = gnome_canvas_points_new(6);
	for(i=0;i<12;i++)
		gPipe->points->coords[i] = 0.0;
	
	gPipe->line = GNOME_CANVAS_LINE(gnome_canvas_item_new(gGroup,
							      gnome_canvas_line_get_type(),
							      "points",gPipe->points,
							      "fill_color","black",
							      "width_units",2.0,
							      "arrow_shape_a",18.0,
							      "arrow_shape_b",20.0,
							      "arrow_shape_c",5.0,
							      "smooth",TRUE,
							      "spline_steps",100,
							      "last_arrowhead",TRUE,
							      NULL));
	gPipe->circle = GNOME_CANVAS_ELLIPSE(gnome_canvas_item_new(gGroup,
								   gnome_canvas_ellipse_get_type(),
								   "x1",0.0,
								   "x2",0.0,
								   "y1",0.0,
								   "y2",0.0,
								   "fill_color","black",
								   "width_pixels",5,
								   NULL));
	gPipe->pipe = pipe;

	/* connect to backend destroy sig */
	glsig_add_handler(filterpipe_emitter(pipe),GLSIG_PIPE_DELETED,glame_canvas_pipe_deleted_cb,gPipe);
	
	hash_add_gcpipe(gPipe);
	
	gtk_signal_connect_after(GTK_OBJECT(gPipe->circle),
				 "event",
				 GTK_SIGNAL_FUNC(glame_canvas_pipe_event_cb),
				 gPipe);
	
	gSource = glame_canvas_find_port(filterpipe_connection_source(pipe));
	gDest = glame_canvas_find_port(filterpipe_connection_dest(pipe));

	/* connect to movement of ports */
	gtk_signal_connect(GTK_OBJECT(gDest),
			   "moved",
			   GTK_SIGNAL_FUNC(glame_canvas_pipe_end_moved_cb),
			   gPipe);
	
	gtk_signal_connect(GTK_OBJECT(gSource),
			   "moved",
			   GTK_SIGNAL_FUNC(glame_canvas_pipe_begin_moved_cb),
			   gPipe);

	/* connect to reordering of connections at port */
	gtk_signal_connect(GTK_OBJECT(gDest),
			   "connections_changed",
			   GTK_SIGNAL_FUNC(glame_canvas_pipe_port_changed_cb),
			   gPipe);
	
	gtk_signal_connect(GTK_OBJECT(gSource),
			   "connections_changed",
			   GTK_SIGNAL_FUNC(glame_canvas_pipe_port_changed_cb),
			   gPipe);

	gtk_signal_connect(GTK_OBJECT(gPipe),"deleted",
			   GTK_SIGNAL_FUNC(glame_canvas_port_pipe_deleted_cb),
			   gSource);

	gtk_signal_connect(GTK_OBJECT(gPipe),"deleted",
			   GTK_SIGNAL_FUNC(glame_canvas_port_pipe_deleted_cb),
			   gDest);


	/* reorder all pipes */
	gtk_signal_emit_by_name(GTK_OBJECT(glame_canvas_find_port(filterpipe_connection_dest(pipe))),
				"connections_changed");
	gtk_signal_emit_by_name(GTK_OBJECT(glame_canvas_find_port(filterpipe_connection_source(pipe))),
				"connections_changed");
	
	return gPipe;
}



/* destroy all pipes in a canvas */
void glame_canvas_pipe_destroy_all(GnomeCanvas* canvas)
{
	GlameCanvasPipe * gcp;
	
	int i;
	for(i=0;i<(1<<8);i++){
		gcp = hash_getslot_gcpipe(i);
		
		while(gcp){
			if(CANVAS_ITEM_CANVAS(gcp) == canvas){
				/* FIXME doesnt take other windows into account! */
				gtk_object_destroy(GTO(gcp));
			}
			gcp = hash_next_gcpipe(gcp);
		}
	}
}
