/*
 * gui.c
 *
 * $Id: gui.c,v 1.23 2001/04/23 18:18:41 xwolf Exp $
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
 */

#include <values.h>
#include "glmid.h"
#include "glame_gui_utils.h"
#include "canvas.h"


/* creates a hbox with two labels in it, adds it to the box box in window win   */

void create_label_val_pair(GtkWidget *win,GtkWidget *box,const char *lab, const char* val)
{
	GtkWidget *hbox, *propval, *propname;
	
	propname = gtk_label_new(_(lab));
	propval = gtk_label_new(_(val));


	hbox = gtk_hbox_new (TRUE, 3);
	gtk_widget_ref (hbox);
	gtk_object_set_data_full (GTK_OBJECT (win), "hbox", hbox,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (hbox);
	gtk_box_pack_start(GTK_BOX (box), hbox,TRUE,TRUE,3);
	gtk_container_set_border_width(GTK_CONTAINER(hbox),2);

	gtk_widget_ref (propname);
	gtk_object_set_data_full (GTK_OBJECT (win), "probname", propname,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (propname);
	gtk_box_pack_start (GTK_BOX (hbox), propname, FALSE, FALSE, 0);
	

	gtk_widget_ref (propval);
	gtk_object_set_data_full (GTK_OBJECT (win), "probval", propval,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (propval);
	gtk_box_pack_start (GTK_BOX (hbox), propval, FALSE, FALSE, 0);
	
}

/* dito, but lab is a frame label here, and the value is inside the box  */
void create_frame_label_val_pair(GtkWidget *win,GtkWidget *box,const char *lab, const char* val)
{
	GtkWidget *propval,*frame;
	
	propval = gtk_label_new(_(val));

	frame = gtk_frame_new (_(lab));
	gtk_widget_ref (frame);
	gtk_object_set_data_full (GTK_OBJECT (win), "frame", frame,
				  (GtkDestroyNotify) gtk_widget_unref);

	gtk_widget_show (frame);
	gtk_box_pack_start (GTK_BOX (box), frame, TRUE, TRUE, 3);

	gtk_widget_ref (propval);
	gtk_object_set_data_full (GTK_OBJECT (win), "probval", propval,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (propval);
	gtk_container_add (GTK_CONTAINER (frame), propval);
	
}



void
create_label_widget_pair(GtkWidget *vbox,const char *clabel, GtkWidget *w)
{
	GtkWidget*hbox,*label;
	hbox = gtk_hbox_new(TRUE,5);
	gtk_container_add(GTK_CONTAINER(vbox),hbox);
	label = gtk_label_new(clabel);
	gtk_container_add(GTK_CONTAINER(hbox),label);
	gtk_container_add(GTK_CONTAINER(hbox),w);
	gtk_widget_show(hbox);
	gtk_widget_show(label);
	gtk_widget_show(w);

}


void changeString(GtkEditable *wid, char ** returnbuffer)
{
	char * chars;
	chars = gtk_editable_get_chars(wid,0,-1);
        strncpy(*returnbuffer,chars,strlen(chars)+1);
}



gui_network*
gui_network_new(void)
{
	
	gui_network * net;       
	GtkWidget * canv;
	net = malloc(sizeof(gui_network));

	net->caption = NULL;
	net->pixname = NULL;
	net->descr = NULL;
	net->net = filter_creat(NULL);
	net->openedUp = FALSE;
	if(!(net->net))
		fprintf(stderr,"Error creating network!\n");
	canv=canvas_new_from_network(net);
	return net;
}




typedef struct {
	struct list_head list;
	int is_category;
	union {
		struct {
			const char *name;
			struct list_head list;
		} c;
		struct {
			plugin_t *plugin;
		} p;
	} u;
} ggbpm_entry;
static void
glame_gui_build_plugin_menu_add_item(struct list_head *cats, plugin_t *plugin)
{
	ggbpm_entry *entry;
	ggbpm_entry *cat;
	const char *catname;

	/* Create entry for plugin with catname */
	entry = (ggbpm_entry *)malloc(sizeof(ggbpm_entry));
	INIT_LIST_HEAD(&entry->list);
	entry->is_category = 0;
	entry->u.p.plugin = plugin;
	if (!(catname = plugin_query(plugin, PLUGIN_CATEGORY)))
		catname = "Default";

	/* Search for old cat. subtree */
	cat = list_gethead(cats, ggbpm_entry, list);
	while (cat) {
		if (strcmp(cat->u.c.name, catname) == 0) {
			list_add_tail(&entry->list, &cat->u.c.list);
			return;
		}
		cat = list_getnext(cats, cat, ggbpm_entry, list);
	}

	/* Need to create new one */
	cat = (ggbpm_entry *)malloc(sizeof(ggbpm_entry));
	INIT_LIST_HEAD(&cat->list);
	cat->is_category = 1;
	cat->u.c.name = catname;
	INIT_LIST_HEAD(&cat->u.c.list);
	list_add_tail(&cat->list, cats);
	list_add_tail(&entry->list, &cat->u.c.list);
}

/* Build a gtk menu from the registered plugins, selecting them
 * via callback. */
GtkMenu *glame_gui_build_plugin_menu(int (*select)(plugin_t *),
				     void (*gtksighand)(GtkWidget *, plugin_t *))
{
	GtkWidget *menu;
	plugin_t *plugin = NULL;
	LIST_HEAD(categories);
	ggbpm_entry *cat;

	/* Build tree of selected categories/plugins. */
	while ((plugin = plugin_next(plugin))) {
		if (!plugin_query(plugin, PLUGIN_FILTER))
			continue;
		if (select && !select(plugin))
			continue;
		glame_gui_build_plugin_menu_add_item(&categories, plugin);
	}

	/* Build the actual GtkMenu out of the tree (and free the tree
	 * while traversing it). */
	menu = gtk_menu_new();
	while ((cat = list_gethead(&categories, ggbpm_entry, list))) {
		GtkWidget *catmenu, *catmenuitem;
		ggbpm_entry *item;
		catmenu = gtk_menu_new();
		catmenuitem = gtk_menu_item_new_with_label(cat->u.c.name);
		while ((item = list_gethead(&cat->u.c.list, ggbpm_entry, list))) {
			GtkWidget *mitem = gtk_menu_item_new_with_label(plugin_name(item->u.p.plugin));
			gtk_widget_show(mitem);
			gtk_menu_append(GTK_MENU(catmenu), mitem);
			if (gtksighand)
				gtk_signal_connect(GTK_OBJECT(mitem), "activate",
						   gtksighand, item->u.p.plugin);
			list_del(&item->list);
			free(item);
		}
		gtk_widget_show(catmenu);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(catmenuitem), catmenu);
		gtk_widget_show(catmenuitem);
		gtk_menu_append(GTK_MENU(menu), catmenuitem);
		list_del(&cat->list);
		free(cat);
	}

	return GTK_MENU(menu);
}




enum {PINT,PFLOAT,PSTRING,PFILE};

typedef struct {
	GtkWidget *widget;
	filter_param_t* param;
	int widget_type;
} param_widget_t;

typedef struct {
	GList* paramList;
	char *caption;
} param_callback_t;


static void set_file_selection_filter(GnomeFileEntry* entry, const char * filter)
{
      gtk_file_selection_complete(GTK_FILE_SELECTION(entry->fsw),filter);
}

static gint
update_params(GnomePropertyBox *propertybox, param_callback_t* callback)
{
	GList* list = g_list_first(callback->paramList);
	char *strVal; 
	int iVal;
	float fVal;
	char *caption = callback->caption;
	param_widget_t* item;
	
	while(list){
		item = (param_widget_t*)(list->data);
		DPRINTF("param: %s\n", filterparam_label(item->param));
		switch(item->widget_type){
		case PINT:
			iVal = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(item->widget));
			DPRINTF("Setting %s::%s to %i", caption, filterparam_label(item->param), iVal);
			if(filterparam_set(item->param, &iVal) == -1)
				DPRINTF(" - failed!\n");
			else
				DPRINTF(" - success!\n");
			break;
		case PFLOAT:
			fVal = gtk_spin_button_get_value_as_float(GTK_SPIN_BUTTON(item->widget));
			DPRINTF("Setting %s::%s to %f", caption, filterparam_label(item->param), fVal);
			if(filterparam_set(item->param, &fVal) == -1)
				DPRINTF(" - failed!\n");
			else
				DPRINTF(" - success!\n");
			break;
		case PSTRING:
			strVal = gtk_editable_get_chars(GTK_EDITABLE(gnome_entry_gtk_entry(GNOME_ENTRY(item->widget))),0,-1);
			DPRINTF("Setting %s::%s to %s", caption, filterparam_label(item->param), strVal);
			if(filterparam_set(item->param, &strVal) == -1)
				DPRINTF(" - failed!\n");
			else
				DPRINTF(" - success!\n");
			g_free(strVal);
			break;
		case PFILE:
			strVal = gtk_editable_get_chars(GTK_EDITABLE(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(item->widget))),0,-1);
			DPRINTF("Setting %s::%s to %s", caption, filterparam_label(item->param), strVal);
			if(filterparam_set(item->param, &strVal) == -1)
				DPRINTF(" - failed!\n");
			else
				DPRINTF(" - success!\n");
			g_free(strVal);
			break;
		}
		list = g_list_next(list);
	}
	DPRINTF("Finished with update_params\n");
	return TRUE;
}

gint
static cancel_params(GtkWidget* wig,param_callback_t* callback)
{
	g_list_free(callback->paramList);
	// FIXME. does list_free kill the structs, too? mem leak
	return FALSE;
}

GtkWidget *
glame_gui_filter_properties(filter_paramdb_t *pdb, const char *caption)
{
	GtkWidget *vbox,*entry;
	GtkAdjustment *adjust;
	param_widget_t *pw;
	param_callback_t* cb;
	char * prop;
	GList * list=NULL;
	
	GtkWidget* propBox;
	GtkWidget* tablabel;
	int iVal;
	float fVal;
	char* cVal;
	filter_param_t* param;
	char label[256];

	propBox = gnome_property_box_new ();
	
	tablabel=gtk_label_new(_(caption));

	vbox = gtk_vbox_new(FALSE,3);
	
	gtk_widget_show(vbox);

	filterparamdb_foreach_param(pdb, param) {
		if (FILTER_PARAM_IS_INT(param)) {
			adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,(float)-MAXINT,(float)MAXINT,1.0,10.0,10.0));

			entry = gtk_spin_button_new(GTK_ADJUSTMENT(adjust),1.0,5);
			gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(entry),TRUE);
			gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry),0);
			iVal = filterparam_val_int(param);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry),(float)iVal);
			create_label_widget_pair(vbox,filterparam_label(param),entry);
			pw = malloc(sizeof(param_widget_t));
			pw->widget = entry;
			pw->param = param;
			pw->widget_type = PINT;
			list = g_list_append(list,pw);
		} else if (FILTER_PARAM_IS_FLOAT(param)
			   || FILTER_PARAM_IS_SAMPLE(param)) {
			adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,-MAXFLOAT,MAXFLOAT,1.0,10.0,10.0));
			entry = gtk_spin_button_new(GTK_ADJUSTMENT(adjust),1.0,5);
			gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(entry),TRUE);
			gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry),3);
			fVal = filterparam_val_float(param);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry),fVal);
			if (filterparam_type(param) == FILTER_PARAMTYPE_TIME_S)
				snprintf(label, 255, "%s [s]",
					 filterparam_label(param));
			else if (filterparam_type(param) == FILTER_PARAMTYPE_TIME_MS)
				snprintf(label, 255, "%s [ms]",
					 filterparam_label(param));
			else
				snprintf(label, 255, "%s",
					 filterparam_label(param));
			create_label_widget_pair(vbox, label, entry);
			pw = malloc(sizeof(param_widget_t));
			pw->widget = entry;
			pw->param = param;
			pw->widget_type = PFLOAT;
			list = g_list_append(list,pw);
		} else if (FILTER_PARAM_IS_STRING(param)) {
			switch(filterparam_type(param)){
			case FILTER_PARAMTYPE_FILENAME:
				entry = gnome_file_entry_new("blahh",filterparam_label(param));
				if((prop = filterparam_get_property(param,FILTER_PARAM_PROPERTY_FILE_FILTER)))
				      gtk_signal_connect_after(GTK_OBJECT(entry),"browse_clicked",GTK_SIGNAL_FUNC(set_file_selection_filter),prop);
				
				create_label_widget_pair(vbox,filterparam_label(param),entry);
				pw = malloc(sizeof(param_widget_t));
				pw->widget = entry;
				pw->param = param;
				pw->widget_type = PFILE;
				list = g_list_append(list,pw);
				break;
			default:
			        entry = gnome_entry_new("blubb");
				create_label_widget_pair(vbox,filterparam_label(param),entry);
				cVal =  filterparam_val_string(param);
				gtk_entry_set_text(GTK_ENTRY(gnome_entry_gtk_entry(GNOME_ENTRY(entry))),cVal);
				free(cVal);
				pw = malloc(sizeof(param_widget_t));
				pw->widget = entry;
				pw->param = param;
				pw->widget_type = PSTRING;
				list = g_list_append(list,pw);
				break;
			}
		} else {
			entry = gnome_entry_new("blubb");
			create_label_widget_pair(vbox,filterparam_label(param),entry);
			pw = malloc(sizeof(param_widget_t));
			pw->widget = entry;
			pw->param = param;
			pw->widget_type = PSTRING;
			list = g_list_append(list,pw);
			break;
		}
	}
	gnome_property_box_append_page(GNOME_PROPERTY_BOX(propBox),vbox,tablabel);

	gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->apply_button));
	gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->help_button));
	gtk_window_set_modal(GTK_WINDOW(propBox),TRUE);
	cb = malloc(sizeof(param_callback_t));
	cb->paramList=list;
	cb->caption = strdup(caption);
	
	gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->ok_button),"clicked",(GtkSignalFunc)update_params,cb);
	gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->cancel_button),"clicked",(GtkSignalFunc)cancel_params,cb);	
	
	return propBox;
}
