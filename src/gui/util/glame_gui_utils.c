/*
 * glame_gui_utils.c
 *
 * $Id: glame_gui_utils.c,v 1.4 2001/07/05 13:57:52 mag Exp $
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
 */

#include <sys/param.h>
#include <values.h>
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifdef HAVE_LIBGLADE
#include <glade/glade.h>
#endif
#include "glmid.h"
#include "gpsm.h"
#include "glame_curve.h"
#include "edit_filter/filtereditgui.h"
#include "glame_gui_utils.h"




/*
 * The generic play bar.
 */

typedef struct {
	GnomeDialog * dia;
	filter_t *net;
	GlameCanvas *gui;
	gboolean playing;
	guint handler_id;
	gint play_index, pause_index, stop_index, cancel_index;
	int close_on_stop;
	GtkFunction atExitFunc;
	gpointer data;
} play_struct_t;

static gint network_finish_check_cb(play_struct_t *play)
{
	if (filter_is_ready(play->net) && play->playing) {
		gtk_timeout_remove(play->handler_id);
		filter_terminate(play->net);
		gnome_dialog_close(play->dia);
		return FALSE;
	}
	return TRUE;
}

static void play_cb(GnomeDialog * dia, play_struct_t* play)
{
	if (play->gui)
		glame_filtereditgui_reset_error(play->gui);
	if (filter_launch(play->net) == -1
	    || filter_start(play->net) == -1) {
		if (play->gui)
			glame_filtereditgui_draw_error(play->gui);
		else {
			char msg[256] = "Error processing network";
			filter_t *node;
			filter_foreach_node(play->net, node) {
				if (filter_errstr(node)) {
					snprintf(msg, 256, "Error processing network:\n%s: %s\n", filter_name(node), filter_errstr(node));
					break;
				}
			}
			gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(msg)));
		}
		gnome_dialog_close(play->dia);
		return;
	}
	play->playing = TRUE;
	gnome_dialog_set_sensitive(play->dia, play->play_index, FALSE);
	if (play->pause_index != -1)
		gnome_dialog_set_sensitive(play->dia, play->pause_index, TRUE);
	if (play->stop_index != -1)
		gnome_dialog_set_sensitive(play->dia, play->stop_index, TRUE);
	play->handler_id = gtk_timeout_add(100, (GtkFunction)network_finish_check_cb, play);
}

static void pause_cb(GnomeDialog * dia, play_struct_t * play)
{
	if(FILTER_IS_LAUNCHED(play->net)){
		if(play->playing){
			play->playing = FALSE;
			filter_pause(play->net);
		} else {
			filter_start(play->net);
			play->playing = TRUE;
		}
	}
}

static void stop_cb(GnomeDialog *dia, play_struct_t* play)
{
	if(play->handler_id)
		gtk_timeout_remove(play->handler_id);
	if(FILTER_IS_LAUNCHED(play->net)){
		if (play->pause_index != -1)
			gnome_dialog_set_sensitive(play->dia,
						   play->pause_index, FALSE);
		gnome_dialog_set_sensitive(play->dia, play->stop_index, FALSE);
		filter_terminate(play->net);
		play->playing = FALSE;
		gnome_dialog_set_sensitive(play->dia, play->play_index, TRUE);
		if (play->close_on_stop)
			gnome_dialog_close(play->dia);
	}
}

static void cancel_cb(GnomeDialog *dia, play_struct_t *play)
{
	if(play->handler_id)
		gtk_timeout_remove(play->handler_id);
	if(FILTER_IS_LAUNCHED(play->net))
		filter_terminate(play->net);
	gnome_dialog_close(play->dia);
}


static void play_destroy_cb(GtkWidget *widget, play_struct_t *play)
{
	if (FILTER_IS_LAUNCHED(play->net))
		filter_terminate(play->net);
	if (play->atExitFunc)
		play->atExitFunc(play->data);
	free(play);
}

int glame_gui_play_network(filter_t *network, GlameCanvas *gui, int modal,
			   GtkFunction atExit, gpointer data,
			   const char *start_label,
			   const char *pause_label,
			   const char *stop_label, int close_on_stop)
{
	play_struct_t *play;
	int i;
	if (!network || !FILTER_IS_NETWORK(network))
		return -1;

	if (!(play = malloc(sizeof(play_struct_t))))
		return -1;

	play->net = network;
	play->gui = gui;
	play->atExitFunc = atExit;
	play->data = data;
	play->dia = GNOME_DIALOG(gnome_dialog_new(filter_name(network),NULL));
	play->playing = FALSE;
	play->handler_id = 0;
	play->play_index = -1;
	play->pause_index = -1;
	play->stop_index = -1;
	play->cancel_index = -1;
	play->close_on_stop = close_on_stop;

	i = 0;
	if (!start_label)
		start_label = "Play";
	play->play_index = i++;
	gnome_dialog_append_button_with_pixmap(play->dia, start_label,
					       GNOME_STOCK_PIXMAP_FORWARD);
	gnome_dialog_set_default(play->dia, play->play_index);
	gnome_dialog_button_connect(play->dia, play->play_index,
				    play_cb, play);

	if (pause_label) {
		play->pause_index = i++;
		gnome_dialog_append_button_with_pixmap(play->dia, pause_label, GNOME_STOCK_PIXMAP_TIMER_STOP);
		gnome_dialog_set_sensitive(play->dia, play->pause_index, FALSE);
		gnome_dialog_button_connect(play->dia, play->pause_index,
					    pause_cb, play);
	}

	if (stop_label) {
		play->stop_index = i++;
		gnome_dialog_append_button_with_pixmap(play->dia, stop_label,
						       GNOME_STOCK_PIXMAP_STOP);
		gnome_dialog_set_sensitive(play->dia, play->stop_index, FALSE);
		gnome_dialog_button_connect(play->dia, play->stop_index,
					    stop_cb, play);
	}

	play->cancel_index = i++;
	gnome_dialog_append_button(play->dia,GNOME_STOCK_BUTTON_CANCEL);
	gnome_dialog_button_connect(play->dia, play->cancel_index,
				    cancel_cb, play);

	if(modal)
		gtk_window_set_modal(GTK_WINDOW(play->dia),TRUE);

	/* Finally register cleanup stuff. */
	gtk_signal_connect(GTK_OBJECT(play->dia), "destroy",
			   (GtkSignalFunc)play_destroy_cb, play);

	gtk_widget_show(GTK_WIDGET(play->dia));

	return 0;
}




/*
 * Widget creation helpers. GNOME/gtk+ suck...
 */


/* Generic callback to update a 256 chars size destination buffer from
 * a GtkEditable (such as gnome_entry's gtk_edit widget). */
static void update_string_from_editable_cb(GtkEditable *w, char *buf)
{
	char *chars = gtk_editable_get_chars(w, 0, -1);
        strncpy(buf, chars, 255);
	g_free(chars);
}

void create_label_edit_pair(GtkWidget *vbox,
			    const char *label, const char *history,
			    char *result)
{
	GtkWidget *whbox, *wlabel, *wentry;

	whbox = gtk_hbox_new(TRUE, 5);

	wlabel = gtk_label_new(label);

	wentry = gnome_entry_new(history);
	gtk_entry_set_text(GTK_ENTRY(gnome_entry_gtk_entry(GNOME_ENTRY(wentry))), result);
	gtk_signal_connect(GTK_OBJECT(gnome_entry_gtk_entry(GNOME_ENTRY(wentry))), "changed", update_string_from_editable_cb, result);

	gtk_container_add(GTK_CONTAINER(whbox), wlabel);
	gtk_container_add(GTK_CONTAINER(whbox), wentry);
	gtk_container_add(GTK_CONTAINER(vbox), whbox);
	gtk_widget_show(wlabel);
	gtk_widget_show(wentry);
	gtk_widget_show(whbox);
}


static void glame_dialog_file_request_browse_cb(GnomeFileEntry *fileentry,
						const char *pattern)
{
	/* Well, this f*** widget is NULL here... */
	gtk_file_selection_complete(GTK_FILE_SELECTION(fileentry->fsw),
				    pattern);
}
GtkWidget *glame_dialog_file_request(const char *windowtitle,
				     const char *history_id, const char *label,
				     const char *pattern,
				     char *returnbuffer)
{
	GtkWidget *dialog;
	GtkWidget *fileEntry;
	GtkWidget *dialogVbox;	
	
	dialog = gnome_dialog_new(windowtitle, GNOME_STOCK_BUTTON_CANCEL,
				  GNOME_STOCK_BUTTON_OK, NULL);
        dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));

        fileEntry = gnome_file_entry_new(history_id, label);
        gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fileEntry))),
                           "changed", update_string_from_editable_cb,
			   returnbuffer);
	if (pattern)
		gtk_signal_connect_after(GTK_OBJECT(fileEntry), "browse-clicked",
					 glame_dialog_file_request_browse_cb,
					 pattern);
        create_label_widget_pair(dialogVbox, "Filename", fileEntry);
	return dialog;
}





/*
 * Plugin menu builder
 */

typedef struct {
	struct list_head list;
	int is_category;
	union {
		struct {
			char *name;
			struct list_head list;
		} c;
		struct {
			plugin_t *plugin;
		} p;
	} u;
} ggbpm_entry;
static ggbpm_entry *
glame_gui_build_plugin_menu_get_cat(struct list_head *cats,
				    const char *catname)
{
	ggbpm_entry *cat;
	ggbpm_entry *before;
	char catn[256], *p;

	strncpy(catn, catname, 256);
	if ((p = strchr(catn, '/')))
		*p = '\0';

	/* Search for old category. */
	cat = list_gethead(cats, ggbpm_entry, list);
	while (cat) {
		if (strcmp(cat->u.c.name, catn) == 0)
			goto recurse;
		cat = list_getnext(cats, cat, ggbpm_entry, list);
	}

	/* Ok, need to create new one. */
	cat = (ggbpm_entry *)malloc(sizeof(ggbpm_entry));
	INIT_LIST_HEAD(&cat->list);
	cat->is_category = 1;
	cat->u.c.name = strdup(catn);
	INIT_LIST_HEAD(&cat->u.c.list);

	before = list_gethead(cats, ggbpm_entry, list);
	while (before
	       && strcasecmp(cat->u.c.name, before->u.c.name) > 0)
		before = list_getnext(cats, before, ggbpm_entry, list);
	if (!before)
		list_add_tail(&cat->list, cats);
	else
		list_add_tail(&cat->list, &before->list);

 recurse:
	if (!p)
		return cat;
	return glame_gui_build_plugin_menu_get_cat(&cat->u.c.list, p+1);
}
static void
glame_gui_build_plugin_menu_add_item(struct list_head *cats, plugin_t *plugin)
{
	ggbpm_entry *entry;
	ggbpm_entry *cat;
	ggbpm_entry *before;
	const char *catname;

	/* Create entry for plugin with catname */
	entry = (ggbpm_entry *)malloc(sizeof(ggbpm_entry));
	INIT_LIST_HEAD(&entry->list);
	entry->is_category = 0;
	entry->u.p.plugin = plugin;
	if (!(catname = plugin_query(plugin, PLUGIN_CATEGORY)))
		catname = "Default";

	/* Get cat. subtree */
	cat = glame_gui_build_plugin_menu_get_cat(cats, catname);

	/* Add the entry. */
	before = list_gethead(&cat->u.c.list, ggbpm_entry, list);
	while (before
	       && strcasecmp(plugin_name(entry->u.p.plugin),
		             before->is_category
			       ? before->u.c.name
			       : plugin_name(before->u.p.plugin)) > 0)
		before = list_getnext(&cat->u.c.list, before, ggbpm_entry, list);
	if (!before)
		list_add_tail(&entry->list, &cat->u.c.list);
	else
		list_add_tail(&entry->list, &before->list);
}
static void
glame_gui_build_plugin_menu_genmenu(struct list_head *entries, GtkMenu *menu,
				    void (*gtksighand)(GtkWidget *, plugin_t *))
{
	ggbpm_entry *entry;

	while ((entry = list_gethead(entries, ggbpm_entry, list))) {
		if (entry->is_category) {
			GtkWidget *citem = gtk_menu_item_new_with_label(
				entry->u.c.name);
			GtkWidget *cmenu = gtk_menu_new();
			gtk_menu_item_set_submenu(GTK_MENU_ITEM(citem), cmenu);
			gtk_menu_append(GTK_MENU(menu), citem);
			glame_gui_build_plugin_menu_genmenu(
				&entry->u.c.list, GTK_MENU(cmenu), gtksighand);
			gtk_widget_show(cmenu);
			gtk_widget_show(citem);
			free(entry->u.c.name);
		} else {
			const char *label;
			GtkWidget *mitem;
			label = plugin_query(entry->u.p.plugin, PLUGIN_LABEL);
			if (!label)
				label = plugin_name(entry->u.p.plugin);
			mitem = gtk_menu_item_new_with_label(label);
			gtk_widget_show(mitem);
			gtk_menu_append(GTK_MENU(menu), mitem);
			if (gtksighand)
				gtk_signal_connect(
					GTK_OBJECT(mitem), "activate",
					gtksighand, entry->u.p.plugin);
			gtk_widget_show(mitem);
		}
		list_del(&entry->list);
		free(entry);
	}
}

/* Build a gtk menu from the registered plugins, selecting them
 * via callback. */
GtkMenu *glame_gui_build_plugin_menu(int (*select)(plugin_t *),
				     void (*gtksighand)(GtkWidget *, plugin_t *))
{
	GtkWidget *menu;
	plugin_t *plugin = NULL;
	LIST_HEAD(categories);

	/* Build tree of selected categories/plugins. */
	while ((plugin = plugin_next(plugin))) {
		if (!select && !plugin_query(plugin, PLUGIN_FILTER))
			continue;
		if (select && !select(plugin))
			continue;
		glame_gui_build_plugin_menu_add_item(&categories, plugin);
	}

	/* Build the actual GtkMenu out of the tree (and free the tree
	 * while traversing it). */
	menu = gtk_menu_new();
	glame_gui_build_plugin_menu_genmenu(&categories, GTK_MENU(menu),
					    gtksighand);
	return GTK_MENU(menu);
}





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

enum {PINT,PFLOAT,PSAMPLE,PSTRING,PFILE,PGLADE,PSBUF};

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
	SAMPLE sVal;
	char *caption = callback->caption;
	param_widget_t* item;
	filter_buffer_t *sbuf;
	
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
		case PSAMPLE:
			sVal = gtk_spin_button_get_value_as_float(GTK_SPIN_BUTTON(item->widget));
			DPRINTF("Setting %s::%s to %f", caption, filterparam_label(item->param), (float)sVal);
			if(filterparam_set(item->param, &sVal) == -1)
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
		case PSBUF: {
			int numpoints,i;
			gfloat (*ctlpoints)[2];
			char* ctlbuffer;
			GtkCurve* gCurve;
			GlameCurve *curve;
			sbuf = sbuf_alloc(1000, NULL);
			sbuf_make_private(sbuf);
			gtk_curve_get_vector(GTK_CURVE(item->widget), sbuf_size(sbuf), sbuf_buf(sbuf));
			filterparam_set(item->param, &sbuf);
			sbuf_unref(sbuf);
			curve = GLAME_CURVE(item->widget);
			gCurve = GTK_CURVE(item->widget);
			glame_curve_get_control_vector(curve,&numpoints,&ctlpoints);
			ctlbuffer = calloc(numpoints*2*12,sizeof(gfloat));
			sprintf(ctlbuffer,"%d",numpoints);
			filterparam_set_property(item->param,"curve-control-points",ctlbuffer);
			i=0;
			while(i<numpoints){
				sprintf(&ctlbuffer[i*28],"%13.8f %13.8f ",(ctlpoints)[i][0],(ctlpoints)[i][1]);
				i++;
			}
			fprintf(stderr,"ctl_strin in: %s\n",ctlbuffer);
			filterparam_set_property(item->param,"curve-control-points-data",ctlbuffer);
			g_free(ctlpoints);
			break;
		}
		case PGLADE:
			if (GTK_IS_OPTION_MENU(item->widget)) {
				GtkMenu *menu = GTK_MENU(gtk_option_menu_get_menu(GTK_OPTION_MENU(item->widget)));
				GtkWidget *act = gtk_menu_get_active(menu);
				/* Doh - gtk suxx again. */
				GList *list;
				DPRINTF("Menu %p - Active %p\n", menu, act);
				list = gtk_container_children(GTK_CONTAINER(menu));
				iVal = 0;
				while (list) {
					DPRINTF("%i - %p\n", iVal, list->data);
					if ((GtkWidget *)(list->data) == act)
						break;
					list = g_list_next(list);
					iVal++;
				}
				DPRINTF("Setting %s::%s to %i", caption, filterparam_label(item->param), iVal);
				if(!list || filterparam_set(item->param, &iVal) == -1)
					DPRINTF(" - failed!\n");
				else
					DPRINTF(" - success!\n");
			} else
				/* FIXME */;
			break;
		}
		list = g_list_next(list);
	}
	DPRINTF("Finished with update_params\n");
	return TRUE;
}


GtkWidget *
glame_gui_filter_properties(filter_paramdb_t *pdb, const char *caption)
{
	GtkWidget *vbox;
	param_callback_t* cb;
	GList * list=NULL;
	GtkWidget* propBox;
	GtkWidget* tablabel;

	propBox = gnome_property_box_new ();
	
	tablabel=gtk_label_new(_(caption));

	vbox = glame_gui_from_paramdb(pdb, &list);
	gtk_widget_show(vbox);
	gnome_property_box_append_page(GNOME_PROPERTY_BOX(propBox),vbox,tablabel);

	gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->apply_button));
	gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->help_button));
	gtk_window_set_modal(GTK_WINDOW(propBox),TRUE);
	cb = malloc(sizeof(param_callback_t));
	cb->paramList=list;
	cb->caption = strdup(caption);
	
	gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->ok_button),"clicked",(GtkSignalFunc)update_params,cb);
	
	return propBox;
}





static gint cleanup_params(GtkWidget *widget, GList *list)
{
	g_list_free(list);
	// FIXME. does list_free kill the structs, too? mem leak
	return FALSE;
}

GtkWidget *glame_gui_from_paramdb(filter_paramdb_t *pdb, GList **list)
{
	GtkWidget *vbox,*entry;
	GtkAdjustment *adjust;
	param_widget_t *pw;
	char * prop;
	
	int iVal;
	float fVal;
	char* cVal;
	filter_param_t* param;
	char label[256];
	char *xml;

	/* the vbox holds the param widgets */
	vbox = gtk_vbox_new(FALSE,3);
	*list = NULL;

	filterparamdb_foreach_param(pdb, param) {
		pw = malloc(sizeof(param_widget_t));
		pw->param = param;
#ifdef HAVE_LIBGLADE
		if ((xml = filterparam_get_property(param, FILTERPARAM_GLADEXML))) {
			GladeXML *gxml;
			gxml = glade_xml_new_from_memory(xml, strlen(xml), NULL, NULL);
			entry = glade_xml_get_widget(gxml, "widget");
			if (GTK_IS_OPTION_MENU(entry)) {
				gtk_option_menu_set_history(GTK_OPTION_MENU(entry), filterparam_val_int(param));
			} else
                                /* FIXME */;
			create_label_widget_pair(vbox,filterparam_label(param),entry);
			pw->widget = entry;
			pw->widget_type = PGLADE;
		} else
#endif
		if (FILTER_PARAM_IS_INT(param)) {
			adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,(float)-MAXINT,(float)MAXINT,1.0,10.0,10.0));

			entry = gtk_spin_button_new(GTK_ADJUSTMENT(adjust),1.0,5);
			gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(entry),TRUE);
			gtk_spin_button_set_digits(GTK_SPIN_BUTTON(entry),0);
			iVal = filterparam_val_int(param);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry),(float)iVal);
			create_label_widget_pair(vbox,filterparam_label(param),entry);
			pw->widget = entry;
			pw->widget_type = PINT;
		} else if (FILTER_PARAM_IS_SAMPLE(param)) {
			adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,-1.0,1.0,0.05,1.0,1.0));
			entry = gtk_spin_button_new(adjust, 0.05, 3);
			snprintf(label, 255, "%s",
				 filterparam_label(param));
			gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(entry),TRUE);
			fVal = filterparam_val_sample(param);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry),fVal);
			create_label_widget_pair(vbox, label, entry);
			pw->widget = entry;
			pw->widget_type = PSAMPLE;
		} else if (FILTER_PARAM_IS_FLOAT(param)
			   || FILTER_PARAM_IS_SAMPLE(param)) {
			if (filterparam_type(param) == FILTER_PARAMTYPE_TIME_S) {
				adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,0.0,MAXFLOAT,0.1,1.0,1.0));
				entry = gtk_spin_button_new(adjust, 0.1, 3);
				snprintf(label, 255, "%s [s]",
					 filterparam_label(param));
			} else if (filterparam_type(param) == FILTER_PARAMTYPE_TIME_MS) {
				adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,0.0,MAXFLOAT,1.0,10.0,10.0));
				entry = gtk_spin_button_new(adjust, 1, 1);
				snprintf(label, 255, "%s [ms]",
					 filterparam_label(param));
			} else {
				adjust = GTK_ADJUSTMENT(gtk_adjustment_new(0.0,-MAXFLOAT,MAXFLOAT,0.1,10.0,10.0));
				entry = gtk_spin_button_new(adjust, 0.1, 3);
				snprintf(label, 255, "%s",
					 filterparam_label(param));
			}
			gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(entry),TRUE);
			fVal = filterparam_val_float(param);
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(entry),fVal);
			create_label_widget_pair(vbox, label, entry);
			pw->widget = entry;
			pw->widget_type = PFLOAT;
		} else if (FILTER_PARAM_IS_STRING(param)) {
			switch(filterparam_type(param)){
			case FILTER_PARAMTYPE_FILENAME:
				entry = gnome_file_entry_new("editfilter::param::filename",filterparam_label(param));
				if((prop = filterparam_get_property(param,FILTER_PARAM_PROPERTY_FILE_FILTER)))
				      gtk_signal_connect_after(GTK_OBJECT(entry),"browse_clicked",GTK_SIGNAL_FUNC(set_file_selection_filter),prop);
				
				create_label_widget_pair(vbox,filterparam_label(param),entry);
				pw->widget = entry;
				pw->widget_type = PFILE;
				break;
			default:
			        entry = gnome_entry_new("editfilter::param::string");
				create_label_widget_pair(vbox,filterparam_label(param),entry);
				cVal =  filterparam_val_string(param);
				gtk_entry_set_text(GTK_ENTRY(gnome_entry_gtk_entry(GNOME_ENTRY(entry))),cVal);
				pw->widget = entry;
				pw->widget_type = PSTRING;
				break;
			}
		} else if (FILTER_PARAM_IS_BUF(param)) {
			char *ctl_vec_string, *num_string;
			char * string_begin,*string_next;
			gfloat (*ctl_points)[2];
			int num_points,i;
			entry = glame_curve_new();
			gtk_curve_set_range(GTK_CURVE(entry),0.0,1.0,-1.0,1.0);
			gtk_widget_set_usize(GTK_WIDGET(entry),200,200);
			create_label_widget_pair(vbox,filterparam_label(param),entry);
			if((num_string = filterparam_get_property(param,"curve-control-points")))
				if((ctl_vec_string=filterparam_get_property(param,"curve-control-points-data"))){

					num_points = atoi(num_string);
					fprintf(stderr,"num: %s %d\n%s\n",num_string,num_points,ctl_vec_string);
					ctl_points = malloc(num_points*sizeof(gfloat[2]));
					string_begin = ctl_vec_string;
					for(i=0;i<num_points;i++){
						ctl_points[i][0] = strtod(string_begin,&string_next);
						string_begin=string_next;
						ctl_points[i][1] = strtod(string_begin,&string_next);
						string_begin=string_next;
						//sscanf(ctl_vec_string,"%f %f",&(ctl_points[i][0]),&(ctl_points[i][1]));
						fprintf(stderr,"set_param: %f %f\n",ctl_points[i][0],ctl_points[i][1]);
					}
					glame_curve_set_control_vector(GLAME_CURVE(entry),num_points,ctl_points);
				}
			pw->widget=entry;
			pw->widget_type = PSBUF;
		} else {
			/* default - ignore */
			free(pw);
			continue;
		}

		*list = g_list_append(*list,pw);
	}

	gtk_signal_connect(GTK_OBJECT(vbox), "destroy",
			   (GtkSignalFunc)cleanup_params, *list);

	return vbox;
}

int glame_gui_update_paramdb(filter_paramdb_t *pdb, GList *list)
{
	param_callback_t cb;

	cb.caption = "blah";
	cb.paramList = list;
	update_params(NULL, &cb);
	return 0;
}




GdkImlibImage* glame_load_icon(const char *filename, int x, int y)
{
	GdkImlibImage* image = NULL;
	GdkImlibImage* imagetmp = NULL;
	const char * file;
	char * filepath;

	/* no filename given, ->default */
	if(!filename)
		file = GLAME_DEFAULT_ICON;
	else
		file = filename;

	/* check if stock gnome */
	if ((filepath = gnome_pixmap_file(file))) {
		image = gdk_imlib_load_image(filepath);
		g_free(filepath);
	}

	/* maybe in Glamepixmappath? */
	filepath = g_concat_dir_and_file(GLAME_PIXMAP_PATH,file);
	if (!g_file_test(filepath, G_FILE_TEST_ISFILE)) {
		g_free(filepath);
		/* maybe cvs? */
		filepath = g_concat_dir_and_file("../data/pixmaps", file);
	}
	if (!g_file_test(filepath, G_FILE_TEST_ISFILE)) {
		g_free(filepath);
		/* default! */
		filepath = gnome_pixmap_file(GLAME_DEFAULT_ICON);
	}
	if(!filepath)
		filepath = g_concat_dir_and_file("../data/pixmaps", GLAME_DEFAULT_ICON);
	
	imagetmp = gdk_imlib_load_image(filepath);
	g_free(filepath);
	if(x && y){
		image = gdk_imlib_clone_scaled_image(imagetmp, x,y);
		gdk_imlib_destroy_image(imagetmp);
		return image;
	}else{
		return imagetmp;
	}
}

GtkWidget* glame_load_icon_widget(const char* filename,int x, int y)
{
	return gnome_pixmap_new_from_imlib(glame_load_icon(filename,x,y));
}




/*
 * Async. network <-> GUI interaction and cleanup.
 */

struct network_notificator {
	glsig_emitter_t emitter;
	guint timeout_handler_id;
	filter_t *net;
};

static gint network_notificator_timeout(struct network_notificator *n)
{
	if (!filter_is_ready(n->net)) {
		/* Run tick handlers. */
		glsig_emit(&n->emitter, GLSIG_NETWORK_TICK, n->net);
		return TRUE;
	}

	/* Run cleanup handlers. */
	glsig_emit(&n->emitter, GLSIG_NETWORK_DONE, n->net);

	/* Cleanup notificator. */
	glsig_delete_all(&n->emitter);
	free(n);

	/* Remove timeout handler. */
	return FALSE;
}

glsig_emitter_t *glame_network_notificator_creat(filter_t *net)
{
	struct network_notificator *n;

	/* Init notification stuff. */
	n = ALLOC(struct network_notificator);
	INIT_GLSIG_EMITTER(&n->emitter);
	n->net = net;

	return &n->emitter;
}

int glame_network_notificator_run(glsig_emitter_t *emitter, int timeout)
{
	struct network_notificator *n = (struct network_notificator *)emitter;
	/* First launch & start the network. Cleanup in case of
	 * errors. */
	if (filter_launch(n->net) == -1
	    || filter_start(n->net) == -1) {
		free(n);
		return -1;
	}

	/* Everything went ok, install gtk_timeout handler for polling. */
	n->timeout_handler_id = gtk_timeout_add(
		timeout, (GtkFunction)network_notificator_timeout, n);

	return 0;
}

void glame_network_notificator_delete_network(glsig_handler_t *handler,
					      long sig, va_list va)
{
	filter_t *net;

	GLSIGH_GETARGS1(va, net);

	filter_terminate(net);
	filter_delete(net);
}

void glame_network_notificator_destroy_gpsm(glsig_handler_t *handler,
					    long sig, va_list va)
{
	gpsm_item_destroy((gpsm_item_t *)glsig_handler_private(handler));
}