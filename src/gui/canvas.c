


/*
 * canvas.c
 *
 * $Id: canvas.c,v 1.4 2000/02/23 11:32:40 xwolf Exp $
 *
 * Copyright (C) 2000 Johannes Hirche
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


#include "canvas.h"



static gpointer parent_class = NULL;
static gpointer canvas_parent_class = NULL;
static GlameCanvasItemClass* glame_canvas_item_class = NULL;
static GlameCanvasClass* glame_canvas_class = NULL;

static void glame_canvas_item_destroy (GtkObject *object);
static void glame_canvas_item_class_init(GlameCanvasItemClass *class);
static void glame_canvas_item_init (GlameCanvasItem *item);
static void glame_canvas_destroy(GtkObject *object);
static void glame_canvas_class_init(GlameCanvasClass *class);
static void glame_canvas_init(GlameCanvas *canv);


GtkType
glame_canvas_item_get_type(void)
{
	static GtkType canvas_item_type = 0;
	
	if(!canvas_item_type){
		GtkTypeInfo canvas_item_info={
			"GlameCanvasItem",
			sizeof(GlameCanvasItem),
			sizeof(GlameCanvasItemClass),
			(GtkClassInitFunc) glame_canvas_item_class_init,
			(GtkObjectInitFunc) glame_canvas_item_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		canvas_item_type = gtk_type_unique(GNOME_TYPE_CANVAS_GROUP,
						   &canvas_item_info);
		gtk_type_set_chunk_alloc(canvas_item_type,8);
	}
	
	return canvas_item_type;
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

static void
glame_canvas_item_class_init(GlameCanvasItemClass *class)
{
	GtkObjectClass *object_class;
	GnomeCanvasItemClass *canvas_item_class;
	GnomeCanvasGroupClass *canvas_group_class;
	
	object_class = GTK_OBJECT_CLASS (class);
	canvas_item_class = GNOME_CANVAS_ITEM_CLASS (class);
	canvas_group_class = GNOME_CANVAS_GROUP_CLASS (class);
	
	glame_canvas_item_class = class;
	parent_class = gtk_type_class (GNOME_TYPE_CANVAS_GROUP);
	
	object_class->destroy = glame_canvas_item_destroy;
	
	//canvas_item_class->event = glame_canvas_item_event;
	
}

static void
glame_canvas_class_init(GlameCanvasClass *class)
{
	// FIXME
	// hmmm... i don't really know what i'm doing here...
	
	GtkObjectClass *object_class;
	GnomeCanvasClass *canvas_item_class;
	//GnomeCanvasGroupClass *canvas_group_class;
	
	object_class = GTK_OBJECT_CLASS (class);
	canvas_item_class = GNOME_CANVAS_CLASS (class);
	//canvas_group_class = GNOME_CANVAS_GROUP_CLASS (class);
	
	//glame_canvas_item_class = class;
	canvas_parent_class = gtk_type_class (GNOME_TYPE_CANVAS);
	
	object_class->destroy = glame_canvas_destroy;
	
	//canvas_item_class->event = glame_canvas_item_event;
	
}



static void
glame_canvas_item_init (GlameCanvasItem *item)
{
	item->filter=NULL;
}

static void
glame_canvas_init (GlameCanvas *item)
{
	item->net=NULL;
}


static void
glame_canvas_item_destroy (GtkObject *object)
{
  //GlameCanvasItem *item = GLAME_CANVAS_ITEM (object);
  //	GnomeCanvasGroup *group = GNOME_CANVAS_GROUP (object);

	GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

static void
glame_canvas_destroy (GtkObject *object)
{
	GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

GtkWidget * glame_canvas_new(gui_network *n)
{

	GlameCanvas *g;
	g = gtk_type_new(glame_canvas_get_type());
	g->net = n;
	return GTK_WIDGET(g);
}

GlameCanvasItem*
glame_canvas_item_new(GnomeCanvasGroup *group,
		      gui_filter *gfilter,
		      gdouble x,
		      gdouble y)
{
	
	GnomeCanvasPoints* points;
	GdkImlibImage *image;

	GnomeCanvasItem *iitem;
	GlameCanvasItem *item;

	
	iitem = gnome_canvas_item_new(group,GLAME_TYPE_CANVAS_ITEM,NULL);
	item = GLAME_CANVAS_ITEM(iitem);
	item->filter = gfilter;
	

	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1",0.0,
			      "y1",0.0,
			      "x2",16.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0x55555550,
			      NULL);

	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1",80.0,
			      "y1",0.0,
			      "x2",96.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0x55555550,
			      NULL);
	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_rect_get_type(),
			      "x1",0.0,
			      "y1",64.0,
			      "x2",96.0,
			      "y2",84.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color","white",
			      NULL);

	points = gnome_canvas_points_new(2);
	points->coords[0] = 16.0;
	points->coords[1] = 0.0;
	points->coords[2] = 80.0;
	points->coords[3] = 0.0;
	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_line_get_type(),
			      "points",points,
			      "fill_color","black",
			      "width_units",1.0,
			      NULL);

	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_text_get_type(),
			      "x",48.0,
			      "y",67.0,
			      "clip_width",94.0,
			      "clip_height",19.0,
			      "fill_color","black",
			      "anchor",GTK_ANCHOR_NORTH,
			      "justification",GTK_JUSTIFY_CENTER,
			      "font", "-adobe-helvetica-medium-r-normal--12-*-72-72-p-*-iso8859-1",
			      "clip",0,
			      "text",gfilter->caption,
			      NULL);
	fprintf(stderr,"%s\n",gfilter->caption);
	

	image = gdk_imlib_load_image(gfilter->pixname);
	gnome_canvas_item_new(GNOME_CANVAS_GROUP(item),
			      gnome_canvas_image_get_type(),
			      "x",48.0,
			      "y",32.0,
			      "width",64.0,
			      "height",64.0,
			      "image",image,
			      NULL);
	
	create_ports(GNOME_CANVAS_GROUP(item),gfilter);
	
	return GLAME_CANVAS_ITEM(item);
	



}
		
static gint delete_canvas(GtkWidget*win,GdkEventAny*ev, gpointer data)
{
	gtk_object_destroy(GTK_OBJECT(win));
	return TRUE;
}


static void
dropped(GtkWidget*win, GdkDragContext*cont,gint x,gint y, GtkSelectionData *data, guint info, guint time)
{

	GnomeCanvasGroup*grp;
	int selected;
	gui_filter *gf;
	gui_filter *inst;
	double dx,dy;
	GlameCanvas* canv;
	char *buff;


	canv = GLAME_CANVAS(win);
	buff= malloc(40);
	inst = malloc(sizeof(gui_filter));
	selected = atoi(data->data);
	gf = g_array_index(gui->filters,gui_filter*,selected);
	fprintf(stderr,"received");
	fprintf(stderr,"%s\n",data->data);

	memcpy(inst,gf,sizeof(gui_filter));
	
	sprintf(buff,"%s%d",inst->caption,time);

	inst->instance=buff;
	

	gui_network_filter_add(canv->net,inst);
	gnome_canvas_window_to_world(GNOME_CANVAS(canv),x,y,&dx,&dy);
	grp = GNOME_CANVAS_GROUP(create_new_node(GNOME_CANVAS(canv),inst,dx,dy));
	
}

GtkWidget * 
create_new_canvas(const char *name, gui_network* net)
{
	GtkWidget *window, *canvas, *sw;


	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window),name);
	//gtk_signal_connect(GTK_OBJECT(window),
	//		   "delete_event",
	//		   GTK_SIGNAL_FUNC(delete_canvas),
	//		   NULL);

	sw = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_widget_push_visual(gdk_rgb_get_visual());
	gtk_widget_push_colormap(gdk_rgb_get_cmap());
	canvas = glame_canvas_new(net);
	gtk_widget_pop_colormap();
	gtk_widget_pop_visual();
	gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas),0,0,600,400);
	gtk_container_add(GTK_CONTAINER(sw),canvas);
	gtk_container_add(GTK_CONTAINER(window),sw);
	gtk_window_set_default_size(GTK_WINDOW(window),400,300);
	gtk_widget_show_all(window);
	gtk_drag_dest_set(GTK_WIDGET(canvas),GTK_DEST_DEFAULT_ALL,gui->target,1,GDK_ACTION_COPY);
	gtk_signal_connect(GTK_OBJECT(canvas),"drag-data-received",GTK_SIGNAL_FUNC(dropped),NULL);
	return canvas;
}


static gint
input_port_select(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{
	filter_portdesc_t *port = (filter_portdesc_t*)data;
//	filter_t *filter = (GLAME_CANVAS_ITEM(item->parent)->filter)->filter;
	if(event->type==GDK_BUTTON_PRESS){
		fprintf(stderr,"iport selected! %s: %s\n",filter_name(port->filter),filterportdesc_label(port));
		return TRUE;
	}
	return FALSE;
}
static gint
output_port_select(GnomeCanvasItem*item,GdkEvent* event, gpointer data)
{
	filter_portdesc_t *port = (filter_portdesc_t*)data;
	//filter_t *filter = GLAME_CANVAS_ITEM(item->parent)->filter;
	if(event->type==GDK_BUTTON_PRESS){
		fprintf(stderr,"oport selected! %s %s\n",filter_name(port->filter),filterportdesc_label(port));
		return TRUE;
	}
	return FALSE;
}

void
create_ports(GnomeCanvasGroup* grp,gui_filter*f)
{
	filter_t*filter =  filter_get(f->caption);
	filter_portdesc_t * port;
	GnomeCanvasItem*item;
//	GtkTooltips* tt;

	int portcount=filter_nrinputs(filter);
	double step = 64.0/(float)portcount;
	double border = 0.0;
	filter_foreach_inputdesc(filter,port){		
		item = gnome_canvas_item_new(grp,
					     gnome_canvas_rect_get_type(),
					     "x1",0.0,
					     "x2",16.0,
					     "y1",border,
					     "y2",border+step,
					     "outline_color","black",
					     "width_units",1.0,
					     "fill_color_rgba",0xff000090,
					     NULL);
		border+=step;
		//	tt = gtk_tooltips_new();
		//      gtk_tooltips_set_tip(tt,GTK_WIDGET(item),filterportdesc_label(port),NULL);
		gtk_signal_connect(GTK_OBJECT(item),
				   "event",GTK_SIGNAL_FUNC(input_port_select),
				   port);
				   
	}
	portcount=filter_nroutputs(filter);
	step = 64.0/(float)portcount;
	border=0.0;
	filter_foreach_outputdesc(filter,port){
		item = gnome_canvas_item_new(grp,
					     gnome_canvas_rect_get_type(),
					     "x1",80.0,
					     "x2",96.0,
					     "y1",border,
					     "y2",border+step,
				      "outline_color","black",
				      "width_units",1.0,
				      "fill_color_rgba",0x0000ff90,
				      NULL);
		border+=step;

		gtk_signal_connect(GTK_OBJECT(item),
				   "event",GTK_SIGNAL_FUNC(output_port_select),
				   port);
	}
}
	
				      

		
	



GtkWidget*
create_new_node(GnomeCanvas *canvas, gui_filter *filter,double x, double y)
{

	GlameCanvasItem *item;
	GnomeCanvasGroup* root;

	root = gnome_canvas_root(GNOME_CANVAS(canvas));

	item = glame_canvas_item_new(root,
				     filter,
				     0.0,0.0);
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(item),x,y);
	return GTK_WIDGET(item);
}
				     
	
	


	
