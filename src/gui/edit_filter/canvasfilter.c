/*
 * canvasfilter.c
 *
 * $Id: canvasfilter.c,v 1.17 2001/06/11 17:34:21 xwolf Exp $
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
#include "filtereditgui.h"
#include "glame_gui_utils.h"
#include "canvasitem.h"
#include "hash.h"

extern gboolean bMac;
extern guint nPopupTimeout;

HASH(gcfilter, GlameCanvasFilter, 8,
	(gcfilter->filter == key ),
	((long)key/4),
	((long)gcfilter->filter/4),
        filter_t * key)


     /*	GlameCanvasFilter* hash_find_GCfilter(filter_t*);
	void hash_add_GCFilter(GlameCanvasFilter*);
     */


     /*  Forward decls */

static gboolean glame_canvas_filter_event(GnomeCanvasItem* foo, GdkEvent* event, GlameCanvasFilter* filter);

/*Yukky gtk class stuff    
 * Ignore these
 */

enum {
	MOVED,
	DELETED,
	LAST_SIGNAL
};
static guint filter_signals[LAST_SIGNAL] = { 0 };



static void
glame_canvas_filter_destroy (GtkObject *object)
{
	GnomeCanvasGroupClass* parent_class;
	glame_canvas_filter_hide_properties(GLAME_CANVAS_FILTER(object));
	gtk_signal_emit(object,filter_signals[DELETED]);
	hash_remove_gcfilter(GLAME_CANVAS_FILTER(object));
	parent_class = gtk_type_class (GNOME_TYPE_CANVAS_GROUP);
	GTK_OBJECT_CLASS (parent_class)->destroy (object);
}


static void
glame_canvas_filter_class_init(GlameCanvasFilterClass* class)
{
	
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = glame_canvas_filter_destroy;
	
	/* now for the signal stuff */
	
	filter_signals[MOVED] = 
		gtk_signal_new("moved",
			       GTK_RUN_LAST,
			       object_class->type,
			       GTK_SIGNAL_OFFSET(GlameCanvasFilterClass, moved),
			       glame_canvas_marshal_NONE__DOUBLE_DOUBLE,
			       GTK_TYPE_NONE,
			       2,
			       GTK_TYPE_DOUBLE,
			       GTK_TYPE_DOUBLE);
	
	filter_signals[DELETED] = 
		gtk_signal_new("deleted",
			       GTK_RUN_LAST,
			       object_class->type,
			       GTK_SIGNAL_OFFSET(GlameCanvasFilterClass, deleted),
			       gtk_marshal_NONE__NONE,
			       GTK_TYPE_NONE,
			       0);

	gtk_object_class_add_signals(object_class,filter_signals, LAST_SIGNAL);
	
	/* default handlers */
	class->moved = NULL;
	class->deleted = NULL;
	
}

static void
glame_canvas_filter_init(GlameCanvasFilter* node)
{
	node->filter = NULL;
	node->x = 0.0;
	node->y = 0.0;
	node->label = NULL;
	node->labelBox = NULL;
	node->immutable = FALSE;
	node->undeletable = FALSE;
	node->connecting = FALSE;
	node->dragging = FALSE;
	node->last_x = 0.0;
	node->last_y = 0.0;
	node->popupGroup = NULL;
	node->timeout_id = 0;
	node->selected = FALSE;
}
		
	
	
GtkType
glame_canvas_filter_get_type(void)
{
	static GtkType canvas_filter_type = 0;
	
	if(!canvas_filter_type){
		GtkTypeInfo canvas_filter_info={
			"GlameCanvasFilter",
			sizeof(GlameCanvasFilter),
			sizeof(GlameCanvasFilterClass),
			(GtkClassInitFunc) glame_canvas_filter_class_init,
			(GtkObjectInitFunc) glame_canvas_filter_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		canvas_filter_type = gtk_type_unique(GNOME_TYPE_CANVAS_GROUP,
						   &canvas_filter_info);
		gtk_type_set_chunk_alloc(canvas_filter_type,8);
	}
	
	return canvas_filter_type;
}



/********************************
 *
 *  Whew... Now for the real stuff
 *
 ********************************/

static void glame_canvas_filter_destroy_cb(glsig_handler_t* foo,long sig,va_list va)
{
	glame_canvas_filter_hide_properties(glsig_handler_private(foo));
	glame_canvas_filter_destroy(glsig_handler_private(foo));
}



GlameCanvasFilter* glame_canvas_find_filter(filter_t *f)
{
	return hash_find_gcfilter(f);
}
     

void
glame_canvas_filter_create_ports(GlameCanvasFilter* filter)
{
	int iPorts = 0, oPorts = 0;
	filter_port_t* port;
	double portHeight;
	double portPos = 0.0;
	double filter_pos_x, filter_pos_y;
		
	GnomeCanvasGroup *group;
	GlameCanvasPort* gPort;
	
	/* count i/o ports */
	filterportdb_foreach_port(filter_portdb(filter->filter),port){
		if(filterport_is_input(port))
			iPorts++;
		else if(filterport_is_output(port))
			oPorts++;
	}

	/* find canvas root group */
	group = GNOME_CANVAS_GROUP(gnome_canvas_root(GNOME_CANVAS(GNOME_CANVAS_ITEM(filter)->canvas)));

	/* find filter coords */
	
	filter_pos_x = GNOME_CANVAS_ITEM(filter)->x1;
	filter_pos_y = GNOME_CANVAS_ITEM(filter)->y1;
	
	gnome_canvas_item_i2w(GNOME_CANVAS_ITEM(filter),&filter_pos_x, &filter_pos_y);
	
	DPRINTF("%f %f\n",filter_pos_x, filter_pos_y);
	portPos = filter_pos_y;
	
	/* input ports */
	
	if(iPorts){
		portHeight = 64.0/(float)iPorts;
		
		filterportdb_foreach_port(filter_portdb(filter->filter),port){
			if(filterport_is_input(port)){
				gPort = glame_canvas_port_new(group,
							      port,
							      filter_pos_x,
							      portPos,
							      portHeight);
				portPos += portHeight;
			}
		}
	}
	
	if(oPorts){
		
		/* reset upper coordinates */
		portPos = filter_pos_y;
		filter_pos_x += 80.0;
		portHeight = 64.0/(float)oPorts;
		
		filterportdb_foreach_port(filter_portdb(filter->filter),port){
			if(filterport_is_output(port)){
				gPort = glame_canvas_port_new(group,
							      port,
							      filter_pos_x,
							      portPos,
							      portHeight);
				portPos += portHeight;
			}
		}
	}
}
			   
			
	
	
	


GlameCanvasFilter* glame_canvas_filter_new(GnomeCanvasGroup *group,
					   filter_t* filter)
{
	GlameCanvasFilter * gItem;
	GnomeCanvasGroup * gGroup;
	GlameCanvasGroup * glameGroup;
	GnomeCanvasItem * item;
	double x,y;
	char numberbuffer[10];
	char * buffer;
	char* cimmutable;
	gboolean immutable;

	glameGroup = GLAME_CANVAS_GROUP(gnome_canvas_item_new(group, GLAME_CANVAS_GROUP_TYPE,
							      NULL));
	
	gItem = GLAME_CANVAS_FILTER(gnome_canvas_item_new(group,
							  GLAME_CANVAS_FILTER_TYPE,
							  NULL));
	gGroup = GNOME_CANVAS_GROUP(gItem);

	gItem->filter = filter;
	gItem->defaultGroup = glameGroup;

	buffer = filter_get_property(filter,"canvas_x");
	if(buffer)
		x = atoi(buffer);
	else
		x = 0.0;
	buffer = filter_get_property(filter,"canvas_y");
	if(buffer)
		y = atoi(buffer);
	else
		y = 0.0;

	cimmutable = filter_get_property(filter,"immutable");
	if(cimmutable)
		immutable = atoi(cimmutable);
	else 
		immutable = FALSE;
	gItem->immutable = immutable;

	cimmutable = filter_get_property(filter,"undeletable");
	if(cimmutable)
		immutable = atoi(cimmutable);
	else 
		immutable = FALSE;
	gItem->undeletable = immutable;
	
	/* add geometry stuff */
	
	gnome_canvas_item_new(gGroup,
			      gnome_canvas_rect_get_type(),
			      "x1",0.0,
			      "x2",16.0,
			      "y1",0.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0x55555500,
			      NULL);
	
	gnome_canvas_item_new(gGroup,
			      gnome_canvas_rect_get_type(),
			      "x1",80.0,
			      "y1",0.0,
			      "x2",96.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0x55555500,
			      NULL);
	
	gnome_canvas_item_new(gGroup,
			      gnome_canvas_rect_get_type(),
			      "x1",16.0,
			      "y1",0.0,
			      "x2",80.0,
			      "y2",64.0,
			      "outline_color","black",
			      "width_units",1.0,
			      "fill_color_rgba",0xffffff00,
			      NULL);
	
	gItem->labelBox = GNOME_CANVAS_RECT(gnome_canvas_item_new(gGroup,
								  gnome_canvas_rect_get_type(),
								  "x1",0.0,
								  "x2",96.0,
								  "y1",64.0,
								  "y2",84.0,
								  "outline_color","black",
								  "width_units",1.0,
								  "fill_color_rgba",0xffffff00,
								  NULL));
	
								  
	gItem->label = GNOME_CANVAS_TEXT(gnome_canvas_item_new(gGroup,
							       gnome_canvas_text_get_type(),
							       "x",48.0,
							       "y",67.0,
							       "clip_width",94.0,
							       "clip_height",19.0,
							       "fill_color",0x00000000,
							       "anchor",GTK_ANCHOR_NORTH,
							       "justification",GTK_JUSTIFY_CENTER,
							       "font",glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(group)->canvas)),
							       "clip",0,
							       "text",filter_name(filter),
							       NULL));
	
	item = gnome_canvas_item_new(gGroup,
				     gnome_canvas_image_get_type(),
				     "x",48.0,
				     "y",32.0,
				     "width",64.0,
				     "height",64.0,
				     "image",glame_gui_get_icon_from_filter(filter),
				     NULL);
	gItem->selbox = GNOME_CANVAS_RECT(gnome_canvas_item_new(gGroup,
					      gnome_canvas_rect_get_type(),
					      "x1",-5.0,
					      "x2",101.0,
					      "y1",-5.0,
					      "y2",89.0,
					      "fill_color_rgba",0xa5adff00,
					      NULL));
	gnome_canvas_item_lower_to_bottom(GCI(gItem->selbox));
	gnome_canvas_item_hide(GCI(gItem->selbox));

	/* register */
	
	hash_add_gcfilter(gItem);
	
	glsig_add_handler(filter_emitter(filter),GLSIG_FILTER_DELETED,glame_canvas_filter_destroy_cb,gItem);
	
	gtk_signal_connect(GTK_OBJECT(item),
			   "event",
			   GTK_SIGNAL_FUNC(glame_canvas_filter_event),gItem);
	
	
	glame_canvas_group_set_item(glameGroup,gItem);
	glame_canvas_filter_move(gItem, x,y);
	
	sprintf(numberbuffer,"%f",x);
	filter_set_property(filter,"canvas_x",numberbuffer);
	sprintf(numberbuffer,"%f",y);
	filter_set_property(filter,"canvas_y",numberbuffer);

	glame_canvas_filter_create_ports(gItem);

	return gItem;
}

void 
glame_canvas_filter_set_immutable(GlameCanvasFilter *filter, gboolean immutable)
{
	filter->immutable = immutable;
}

gboolean
glame_canvas_filter_is_immutable(GlameCanvasFilter *filter)
{
	return filter->immutable;
}

void 
glame_canvas_filter_set_undeletable(GlameCanvasFilter *filter, gboolean undeletable)
{
	filter->undeletable = undeletable;
}

gboolean
glame_canvas_filter_is_undeletable(GlameCanvasFilter *filter)
{
	return filter->undeletable;
}

void
_glame_canvas_filter_move(GlameCanvasFilter* filter,
			  gdouble dx,
			  gdouble dy)
{
	char buffer[10];
	/* don't recurse! */
	if(gtk_signal_n_emissions_by_name(GTO(filter),"moved"))
		return;

	gtk_signal_emit_by_name(GTK_OBJECT(filter),"moved",
				dx,dy);
	gnome_canvas_item_move(GNOME_CANVAS_ITEM(filter),
			       dx,dy);

	sprintf(buffer,"%f",GNOME_CANVAS_ITEM(filter)->x1);
	filter_set_property(filter->filter,
			    "canvas_x",buffer);
	
	sprintf(buffer,"%f",GNOME_CANVAS_ITEM(filter)->y1);
	filter_set_property(filter->filter,
			    "canvas_y",buffer);
}
void
glame_canvas_filter_move(GlameCanvasFilter *filter,
			 gdouble dx,
			 gdouble dy)
{
	glame_canvas_group_item_moved_cb(filter,dx,dy,GNOME_CANVAS_ITEM(filter)->parent);

}

void
glame_canvas_filter_redraw(GlameCanvasFilter *filter)
{
	

	/* redraw label with right size */

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(filter->label),
			      "font",glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(filter)->canvas)),
			      NULL);
	
	/* check for error */
	
	if(filter_has_error(filter->filter))
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(filter->labelBox),
				      "fill_color","red",
				      NULL);
	else
		gnome_canvas_item_set(GNOME_CANVAS_ITEM(filter->labelBox),
				      "fill_color","white",
				      NULL);
	if(filter->selected){
		gnome_canvas_item_show(GCI(filter->selbox));
	}else{
		gnome_canvas_item_hide(GCI(filter->selbox));
	}
}

void glame_canvas_filter_set_selected(GlameCanvasFilter* f, gboolean selected)
{
	f->selected = selected;
	glame_canvas_filter_redraw(f);
}

guint
glame_canvas_filter_show_properties(GlameCanvasFilter* filter)
{
	filter_param_t* param;
	GnomeCanvasText * text;
	GnomeCanvasGroup *group;
	GnomeCanvasRect* rect;
	
	char buffer[256];
	const char* font = glame_gui_get_font(GLAME_CANVAS(GNOME_CANVAS_ITEM(filter)->canvas));
	float y;
	double xOffset,yOffset,dummy1,dummy2;
	double bmaxx,bminx,bminy,bmaxy, recx1,recx2,recy1,recy2;
	
	bmaxx=-999999999.0;
	bminx=9999999999.0;
	bmaxy=-999999999.0;
	bminy=999999999.0;

	gnome_canvas_item_get_bounds(GNOME_CANVAS_ITEM(filter),&xOffset,&yOffset,&dummy1,&dummy2);
	
	y = 110.0 + yOffset;
	
	DPRINTF("foo\n");
	group = GNOME_CANVAS_GROUP(CANVAS_ITEM_ROOT(filter));
	group = GNOME_CANVAS_GROUP(gnome_canvas_item_new(group,
				      gnome_canvas_group_get_type(),
				      NULL));
	gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(group));
	
	filterparamdb_foreach_param(glame_canvas_filter_get_paramdb(filter),param){
		sprintf(buffer,"%s: %s",filterparam_label(param),filterparam_to_string(param));
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

	y+=10.0;
	if(filter_has_error(filter->filter)){
		sprintf(buffer,"ERROR: %s",filter_errstr(filter->filter));
		text = GNOME_CANVAS_TEXT(gnome_canvas_item_new(group,
							       gnome_canvas_text_get_type(),
							       "x",xOffset,
							       "y",y,
							       "text",buffer,
							       "clip_width",94.0,
							       "clip_height",16.0,
							       "fill_color","red",
							       "anchor",GTK_ANCHOR_WEST,
							       "justification",GTK_JUSTIFY_LEFT, 
							       "font", font,
							       "clip",0,
							       NULL));
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(text));

		gnome_canvas_item_get_bounds(GCI(text),&recx1,&recy1,&recx2,&recy2);
		bmaxx = bmaxx<recx2?recx2:bmaxx;
		bmaxy = bmaxy<recy2?recy2:bmaxy;
		bminx = bminx>recx1?recx1:bminx;
		bminy = bminy>recy1?recy1:bminy;

		
	}
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
	filter->popupGroup = group;

	gtk_timeout_remove(filter->timeout_id);
	filter->timeout_id = 0;
	return FALSE;
}

void
glame_canvas_filter_hide_properties(GlameCanvasFilter* filter)
{
	if(!filter->popupGroup){
		return;
	}
	gtk_object_destroy(GTO(filter->popupGroup));
	filter->popupGroup = NULL;

}

filter_paramdb_t*
glame_canvas_filter_get_paramdb(GlameCanvasFilter* filter)
{
	return filter_paramdb(filter->filter);
}

filter_portdb_t*
glame_canvas_filter_get_portdb(GlameCanvasFilter* filter)
{
	return filter_portdb(filter->filter);
}



/************************
 * menu handling
 ************************/



static void glame_canvas_filter_edit_properties_cb(GtkWidget* m,GlameCanvasFilter *filter)
{
	GtkWidget *p;

	if(filter->immutable)
		return;
	p = glame_gui_filter_properties(filter_paramdb(filter->filter),
						   filter_name(filter->filter));
	gnome_dialog_run_and_close(GNOME_DIALOG(p));
}


static void glame_canvas_filter_delete_cb(GtkWidget* foo, GlameCanvasFilter* filter)
{
	filter_delete(filter->filter);
}

static void glame_canvas_filter_open_node_cb(GtkWidget* foo, GlameCanvasFilter* filter)
{
	if(FILTER_IS_NETWORK(filter->filter))
		gtk_widget_show(glame_filtereditgui_new(filter->filter));
}

static void glame_canvas_filter_show_about(GtkWidget* foo, GlameCanvasFilter* filterItem)
{
		GtkWidget * dialog;
	GtkWidget * text;
	GtkWidget * vbox;
	GtkWidget * notebook;
	GtkWidget * tablabel;
	GtkCList * list;
	char * desc;
	int pos=0;

	filter_portdb_t * ports;
	filter_paramdb_t * params;
	filter_param_t * param;
	filter_t * filter;
	filter_port_t * port;
	
	char *labels[] = {"Name","Type","Description"};
	char *plabels[] = {"Name","Value","Description"};
	char ** line;
	char * buffer;
	filter = filterItem->filter;

	ports = filter_portdb(filter);
	
	notebook = gtk_notebook_new();
	
	dialog = gnome_dialog_new(plugin_name(filter->plugin),GNOME_STOCK_BUTTON_OK,NULL);
	

	desc = (char*)plugin_query(filter->plugin,PLUGIN_DESCRIPTION);
	text = gtk_text_new(NULL,NULL);
	gtk_widget_show(text);
	
	if(desc)
		gtk_editable_insert_text(GTK_EDITABLE(text),desc,strlen(desc),&pos);
	else
		gtk_editable_insert_text(GTK_EDITABLE(text),"This item does not have a description",38,&pos);
	tablabel = gtk_label_new("Description");
	gtk_widget_show(tablabel);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),text,tablabel);

	tablabel = gtk_label_new("Ports");
	list = GTK_CLIST(gtk_clist_new_with_titles(3,labels));
	gtk_clist_set_column_auto_resize(list,0,TRUE);
	gtk_clist_set_column_auto_resize(list,1,TRUE);
	gtk_clist_set_column_auto_resize(list,2,TRUE);

	filterportdb_foreach_port(ports,port){
		line = calloc(3,sizeof(char*));
		if(filterport_label(port))
			buffer = strdup(filterport_label(port));
		else
			buffer = strdup("Empty");
		line[0] = buffer;
		line[1] = (filterport_is_input(port)?"In":"Out");
		if( filterport_get_property(port,FILTERPORT_DESCRIPTION))
			buffer = filterport_get_property(port,FILTERPORT_DESCRIPTION);
		else
			buffer = strdup("Empty");
		line[2] = buffer;
		gtk_clist_append(list,line);
	}
	gtk_widget_show(GTK_WIDGET(list));
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),GTK_WIDGET(list),tablabel);

	tablabel = gtk_label_new("Properties");
	gtk_widget_show(tablabel);
	
	list = GTK_CLIST(gtk_clist_new_with_titles(3,plabels));
	gtk_clist_set_column_auto_resize(list,0,TRUE);
	gtk_clist_set_column_auto_resize(list,1,TRUE);
	gtk_clist_set_column_auto_resize(list,2,TRUE);
	
	params = filter_paramdb(filter);
	filterparamdb_foreach_param(params,param){
		line = calloc(3,sizeof(char*));
		if(filterparam_label(param))
			buffer = strdup(filterparam_label(param));
		else
			buffer = strdup("Empty");
		line[0] = buffer;
		if(filterparam_to_string(param))
			buffer = strdup(filterparam_to_string(param));
		else
			buffer = strdup("Empty");
		line[1] = buffer;
		if(filterparam_get_property(param,FILTERPARAM_DESCRIPTION))
			buffer = strdup(filterparam_get_property(param,FILTERPARAM_DESCRIPTION));
		else
			buffer = strdup("Empty");
		line[2] = buffer;
		gtk_clist_append(list,line);
	}
	gtk_widget_show(GTK_WIDGET(list));
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),GTK_WIDGET(list),tablabel);

	gtk_widget_show(notebook);
	vbox = GNOME_DIALOG(dialog)->vbox;
	gtk_container_add(GTK_CONTAINER(vbox),notebook);
	
	gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
}

static void glame_canvas_filter_help(GtkWidget *foo, GlameCanvasFilter* filter)
{
	char * helppath;
	char buffer[100];
	helppath = plugin_query(filter->filter->plugin,PLUGIN_GUI_HELP_PATH);
	if(!helppath)
		helppath = strdup("Plugin_Collection");
	sprintf(buffer,"info:glame#%s",helppath);
	gnome_help_goto(NULL,buffer);
}

static void update_entry_text(GtkListItem* item,GtkEntry* entry)
{
	// this is a little kludgy.. FIXME
	char *label;
	label = strdup(GTK_LABEL(GTK_BIN(item)->child)->label);
	gtk_entry_set_text(entry,label);
	free(label);  
}

static void update_string(GtkListItem* item,char ** returnbuffer)
{
	// this is a little kludgy.. FIXME
	char *label;
	
	label = strdup(GTK_LABEL(GTK_BIN(item)->child)->label);
	strncpy(*returnbuffer,label,100);
	free(label);  
}

static void update_string_from_editable(GtkEntry* entry, char** retbuffer)
{
	strncpy(*retbuffer,gtk_editable_get_chars(GTK_EDITABLE(entry),0,-1),100);
}

static void glame_canvas_filter_redirect_parameters(GtkWidget *bla, GlameCanvasFilter *item)
{
	GtkWidget * dialog;
	GtkWidget * vbox;
	GtkWidget * entry;
	GtkWidget * list;
	GtkWidget * listItems;
	filter_paramdb_t * db, *topleveldb; 
	filter_param_t *iter,*newparam;
	GList * items=NULL;
	char * paramnamebuffer;
	char * externnamebuffer;

	paramnamebuffer = calloc(100,sizeof(char));
	externnamebuffer = calloc(100,sizeof(char));

	dialog = gnome_dialog_new("Export parameters...",GNOME_STOCK_BUTTON_CANCEL,"Remap All",GNOME_STOCK_BUTTON_OK,NULL);
		
	vbox = GNOME_DIALOG(dialog)->vbox;
	
	list = gtk_list_new();
	entry = gtk_entry_new();
	
	db = filter_paramdb(item->filter);
	topleveldb = filter_paramdb(GLAME_CANVAS(GNOME_CANVAS_ITEM(item)->canvas)->net);

	filterparamdb_foreach_param(db,iter){
		listItems = gtk_list_item_new_with_label(filterparam_label(iter));
		gtk_signal_connect(GTK_OBJECT(listItems),"select",update_entry_text,entry);
		gtk_signal_connect(GTK_OBJECT(listItems),"select",update_string,&paramnamebuffer);
		gtk_signal_connect(GTK_OBJECT(entry),"changed",update_string_from_editable,&externnamebuffer);
		gtk_widget_show(listItems);
		items = g_list_append(items,listItems);
	}
	gtk_list_append_items(GTK_LIST(list),items);
	gtk_widget_show(list);
	create_label_widget_pair(vbox,"Select parameter:",list);
	create_label_widget_pair(vbox,"External name:",entry);
	switch(gnome_dialog_run_and_close(GNOME_DIALOG(dialog)))
	{
	case 2:
		DPRINTF("%s\n",paramnamebuffer);
		if(paramnamebuffer){
			iter = filterparamdb_get_param(db,paramnamebuffer);
			if(iter){
				if(*externnamebuffer)
					newparam = filterparamdb_add_param(topleveldb,externnamebuffer,filterparam_type(iter),filterparam_val(iter),FILTERPARAM_END);
				else
					newparam = filterparamdb_add_param(topleveldb,filterparam_label(iter),filterparam_type(iter),filterparam_val(iter),FILTERPARAM_END);
				if(newparam){
					if(filterparam_redirect(newparam,iter))
						DPRINTF("Failed to redirect: %s\n",filterparam_label(iter));
				}
				
			}
		}
			
		break;
	case 1:
		
		filterparamdb_foreach_param(db,iter){
			newparam = filterparamdb_add_param(topleveldb,filterparam_label(iter),filterparam_type(iter),filterparam_val(iter),FILTERPARAM_END);
			if(newparam){
				if(filterparam_redirect(newparam,iter))
					DPRINTF("Failed to redirect: %s\n",filterparam_label(iter));
			}
		}
		break;
	default:
		break;
	}
	
	free(paramnamebuffer);
	free(externnamebuffer);
								    
}


int inItem;
static GnomeUIInfo node_menu[]=
{
	GNOMEUIINFO_MENU_PROPERTIES_ITEM(glame_canvas_filter_edit_properties_cb,NULL),
	GNOMEUIINFO_ITEM("_Redirect parameter","redirect",glame_canvas_filter_redirect_parameters,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("_Delete","Delete node",glame_canvas_filter_delete_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("_About node...","bout",glame_canvas_filter_show_about,NULL),
	GNOMEUIINFO_ITEM("_Help","Show help",glame_canvas_filter_help,NULL),
//	GNOMEUIINFO_ITEM("Reroute","Reroute from this item",reroute_cb,NULL),
	GNOMEUIINFO_END
};

static GnomeUIInfo node_menu_network[]=
{
	GNOMEUIINFO_MENU_PROPERTIES_ITEM(glame_canvas_filter_edit_properties_cb,NULL),
	GNOMEUIINFO_ITEM("_Redirect parameter","redirect",glame_canvas_filter_redirect_parameters,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("_Delete","Delete node",glame_canvas_filter_delete_cb,NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("_Open Down","Open down",glame_canvas_filter_open_node_cb,NULL),
	GNOMEUIINFO_ITEM("_About node...","bout",glame_canvas_filter_show_about,NULL),
	GNOMEUIINFO_ITEM("_Help","Show help",glame_canvas_filter_help,NULL),
//	GNOMEUIINFO_ITEM("Reroute","Reroute from this item",reroute_cb,NULL),
	GNOMEUIINFO_END
};



static void glame_canvas_filter_deregister_popup(GlameCanvasFilter* filter);

static gboolean
glame_canvas_filter_grabbing_cb(GnomeCanvasItem* i, GdkEvent* event, GlameCanvasFilter* filter)
{
	GtkWidget * menu;
	switch(event->type){
	case GDK_MOTION_NOTIFY:
		/* first deregister timeouts */
		glame_canvas_filter_deregister_popup(filter);
		glame_canvas_filter_move(filter, (double)event->button.x-filter->last_x,(double)event->button.y-filter->last_y);
		filter->last_x = event->button.x;
		filter->last_y = event->button.y;
		
		return TRUE;
		break;
	case GDK_BUTTON_RELEASE:
		gnome_canvas_item_ungrab(i,event->button.time);
		gtk_signal_disconnect_by_func(GTO(i),GTK_SIGNAL_FUNC(glame_canvas_filter_grabbing_cb),filter);
		gtk_signal_handler_unblock_by_func(GTO(i),GTK_SIGNAL_FUNC(glame_canvas_filter_event),filter);
		return TRUE;
	case GDK_2BUTTON_PRESS:
		gnome_canvas_item_ungrab(i,event->button.time);
		gtk_signal_disconnect_by_func(GTO(i),GTK_SIGNAL_FUNC(glame_canvas_filter_grabbing_cb),filter);
		gtk_signal_handler_unblock_by_func(GTO(i),GTK_SIGNAL_FUNC(glame_canvas_filter_event),filter);
		switch(event->button.button){
		case 1:
			if(bMac){
				if(FILTER_IS_NETWORK(filter->filter))
					menu = gnome_popup_menu_new(node_menu_network);
				else
					menu = gnome_popup_menu_new(node_menu);
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,filter);
				break;		
			}
		default:
			break;
		}
		
		return TRUE;
	default:
		return FALSE;
	}
}

static void glame_canvas_filter_register_popup(GlameCanvasFilter* filter)
{
	if(filter->timeout_id){
		/* already a timeout present! kill it and register new one */
		gtk_timeout_remove(filter->timeout_id);
	}
	filter->timeout_id=gtk_timeout_add(nPopupTimeout,(GtkFunction)glame_canvas_filter_show_properties,filter);
}

static void glame_canvas_filter_deregister_popup(GlameCanvasFilter* filter)
{
	if(filter->timeout_id){
		gtk_timeout_remove(filter->timeout_id);
		filter->timeout_id = 0;
	}else{
		glame_canvas_filter_hide_properties(filter);
	}
}
		
void _glame_canvas_filter_raise_to_top(GlameCanvasFilter* filter)
{
	filter_port_t *port;
	gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(filter)->parent);
	filterportdb_foreach_port(glame_canvas_filter_get_portdb(filter),port){
		gnome_canvas_item_raise_to_top(GNOME_CANVAS_ITEM(glame_canvas_find_port(port)));
	}
}


void glame_canvas_filter_raise_to_top(GlameCanvasFilter* filter)
{
	glame_canvas_group_raise(GNOME_CANVAS_ITEM(filter)->parent);
}




static void glame_canvas_filter_do_select(GlameCanvasFilter* filter, GdkEvent* event)
{
	/* check for modifiers */
	if((GDK_SHIFT_MASK&event->button.state)||(GDK_CONTROL_MASK&event->button.state)){
		if(filter->selected)
			glame_canvas_group_unselect(GNOME_CANVAS_ITEM(filter)->parent);
		else
			glame_canvas_group_select(GNOME_CANVAS_ITEM(filter)->parent);
		return;
	}
	if(filter->selected)
		glame_canvas_group_unselect(GNOME_CANVAS_ITEM(filter)->parent);
	else{
		glame_canvas_select_clear(CANVAS_ITEM_GLAME_CANVAS(filter));
		glame_canvas_group_select(GNOME_CANVAS_ITEM(filter)->parent);
	}
}


	
static gboolean
glame_canvas_filter_event(GnomeCanvasItem* i, GdkEvent* event, GlameCanvasFilter* filter)
{
	GdkCursor * fleur;
	GtkWidget * menu;
	switch(event->type){
	case GDK_BUTTON_PRESS:
		switch(event->button.button){
		case 1:

			/* raise to top */
			glame_canvas_filter_raise_to_top(filter);
			/* perform selects */
			glame_canvas_filter_do_select(filter,event);

                        /* grab the thing */
			
			/* save coords */
 			filter->last_x = event->button.x;
			filter->last_y = event->button.y;
			
			fleur = gdk_cursor_new(GDK_FLEUR);
			/* block other handlers (this one ;-) */
			gtk_signal_handler_block_by_func(GTK_OBJECT(i),GTK_SIGNAL_FUNC(glame_canvas_filter_event),filter);
			gtk_signal_connect(GTK_OBJECT(i),"event", GTK_SIGNAL_FUNC(glame_canvas_filter_grabbing_cb), filter);
			gnome_canvas_item_grab(GNOME_CANVAS_ITEM(i),GDK_POINTER_MOTION_MASK|GDK_BUTTON_RELEASE_MASK|GDK_BUTTON_PRESS_MASK,fleur,
					       event->button.time);
			gdk_cursor_destroy(fleur);
			return TRUE;
			break;
		case 3:
			/* popup menu */
			/* check for network */
			if(FILTER_IS_NETWORK(filter->filter))
				menu = gnome_popup_menu_new(node_menu_network);
			else
				menu = gnome_popup_menu_new(node_menu);
			gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,filter);
			break;	
		default:
			return FALSE;
			break;
		}
	case GDK_2BUTTON_PRESS:
		switch(event->button.button){
		case 1:
			if(bMac){
				if(FILTER_IS_NETWORK(filter->filter))
					menu = gnome_popup_menu_new(node_menu_network);
				else
					menu = gnome_popup_menu_new(node_menu);
				gnome_popup_menu_do_popup(menu,NULL,NULL,&event->button,filter);
				break;		
			}
		default:
			break;
		}
		break;
	case GDK_ENTER_NOTIFY:
		glame_canvas_filter_register_popup(filter);
		break;
	case GDK_LEAVE_NOTIFY:
		glame_canvas_filter_deregister_popup(filter);
		break;
	default:
		return FALSE;
		break;
	}
	return FALSE;
}




