/*
 * gui.c
 *
 * $Id: gui.c,v 1.7 2001/02/08 01:10:08 xwolf Exp $
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

#include <glmid.h>
#include "gui.h"

void handle_about(GtkWidget *menuitem,gpointer bla)
{
	gtk_widget_show(gui_create_about());
}
void handle_properties(GtkWidget *menuitem, gpointer bla)
{

}
void 
handle_new_filter_net(GtkWidget *menuitem, gpointer bla)
{
	gui_network_new_wizard();

}

void handle_filter_net_open(GtkWidget *menuitem, gpointer bla){}
void handle_load_filter_plugin(GtkWidget *menuitem,gpointer bla){}


void on_preferences_activate(GtkWidget *m,gpointer bla){}
void on_cut_activate(GtkWidget *m,gpointer bla){}
void on_copy_activate(GtkWidget *m, gpointer bla){}
void on_paste_activate(GtkWidget *m, gpointer bla){}
void on_clear_activate(GtkWidget *m, gpointer bla){}


/* Allocates a new filter type for the gui */ 
/* gui_filter*  */
/* gui_filter_new(plugin_t* plugin) */
/* { */
/* 	gui_filter * newFilter; */
/* 	newFilter = malloc(sizeof(gui_filter)); */
/* 	newFilter->plugin = plugin; */
/* 	return newFilter; */
/* } */



/* creates about popup */
GtkWidget* 
gui_create_about(void)
{
	const gchar *authors[]={
		"Richard Guenther [Richi]",
		"Alexander Ehlert [OzMag]",
		"Daniel Kobras [*nold]",
		"Johannes Hirche [XWolf]",
		"and others",
		NULL
	};
	
	GtkWidget *about;
	
	about = gnome_about_new ("GLAME", VERSION, 
				 _("Copyright (C) 1999,2000 Alexander Ehlert, Richard Guenther."),
				 authors,
				 _("GLAME comes with ABSOLUTELY NO WARRANTY. \nThis is free software."),
				 GLAME_LOGO);
	gtk_object_set_data (GTK_OBJECT (about), "about", about);
	gtk_window_set_modal (GTK_WINDOW (about), TRUE);
	gtk_window_set_wmclass (GTK_WINDOW (about), "Glameabout", "Glame");
	
	gtk_widget_show(about);
	return about;
}			 

void gui_exit(GtkWidget *w,GdkEvent *e, gpointer d)
{
	gtk_main_quit();
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


/* browses the registered filtrs and returns a list of strings */
GSList* gui_browse_registered_filters(void)
{
      GSList *ret=NULL;
      plugin_t *plugin = NULL;

	/* browse registered plugins */
      while((plugin = plugin_next(plugin))){
	      if(plugin_query(plugin,PLUGIN_FILTER)){
		      ret=g_slist_append(ret,(gpointer)plugin);
	      }
      }
      return ret;
	    
}


gui_network* 
gui_network_new(const char * caption, const char * pixname)
{
	gui_network *net = malloc(sizeof(gui_network));
	net->net = filter_creat(NULL);
	if(!(net->net))
		fprintf(stderr,"Error creating network!\n");
	return net;
}
	


GtkWidget*
create_label_edit_pair(GtkWidget *vbox,const char *clabel)
{
	GtkWidget*hbox,*label,*edit;
	hbox = gtk_hbox_new(TRUE,5);
	gtk_container_add(GTK_CONTAINER(vbox),hbox);
	label = gtk_label_new(clabel);
	gtk_container_add(GTK_CONTAINER(hbox),label);
	edit = gtk_entry_new_with_max_length(12);
	gtk_container_add(GTK_CONTAINER(hbox),edit);
	gtk_widget_show(hbox);
	gtk_widget_show(label);
	gtk_widget_show(edit);
	return edit;
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


gui_network*
gui_network_new_wizard(void)
{
	
	gui_network * net;       
	GtkWidget * canv;
	net = malloc(sizeof(gui_network));

	net->caption = NULL;
	net->pixname = NULL;
	net->descr = NULL;
	net->net = filter_creat(NULL);
	if(!(net->net))
		fprintf(stderr,"Error creating network!\n");
	canv=create_new_canvas(net);
	gtk_signal_connect(GTK_OBJECT(canv),"delete-event",GTK_SIGNAL_FUNC(gui_exit),NULL);
	
	return net;
}

