/*
 * gui.c
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

#include <gnome.h>
#include "filter.h"
#include "gui.h"



/* Allocates a new filter type for the gui */


gui_filter* 
gui_filter_new(const char *pixname,filter_t* filter)
{
	gui_filter * newFilter = malloc(sizeof(gui_filter));
	newFilter->caption = filter->name;
	newFilter->pixname = malloc(strlen(pixname));
	//newFilter->filter = filter;
	strcpy(newFilter->pixname,pixname);
	
	return newFilter;
}

/* Adds a filter type to the gui */
int
gui_filter_add(gui_filter *filter)
{
	g_array_append_val(gui->filters,filter);
	return gnome_icon_list_append(GNOME_ICON_LIST(gui->iconlist),filter->pixname,filter->caption);
}


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
				 "pixmaps/glame-logo.jpg");
	gtk_object_set_data (GTK_OBJECT (about), "about", about);
	gtk_window_set_modal (GTK_WINDOW (about), TRUE);
	gtk_window_set_wmclass (GTK_WINDOW (about), "Glameabout", "Glame");
	
	return about;
}			 

void gui_exit(GtkWidget *w,GdkEvent *e, gpointer d)
{
	gtk_main_quit();
}


/* creates main window */
GtkWidget*
gui_create_commandwin(void)
{
	GtkWidget *commandwin;
	GtkWidget *commanddock;
	
	GtkWidget *buttontable;
	GtkWidget *butt[GUI_BUTTONS_X*GUI_BUTTONS_Y];
	

	GtkWidget *scroller;
	
	int i,j;
	char lab[20];

	commandwin = gnome_app_new("glame",_("glame"));
	gtk_object_set_data(GTK_OBJECT(commandwin),"commandwindow",commandwin);
	
	commanddock = GNOME_APP(commandwin)->dock;
	gtk_widget_ref(commanddock);
	gtk_object_set_data_full(GTK_OBJECT(commanddock),"commanddock",commanddock, 
				 (GtkDestroyNotify) gtk_widget_unref);
	
	gtk_widget_show(commanddock);
	
	gui->app=commandwin;
	gui->dock=commanddock;
	
	buttontable = gtk_table_new(GUI_BUTTONS_X,GUI_BUTTONS_Y,FALSE);
	gtk_widget_ref(buttontable);
	gtk_object_set_data_full(GTK_OBJECT(commandwin),"buttontable",buttontable,
				 (GtkDestroyNotify)gtk_widget_unref);
	gtk_widget_show(buttontable);
	//  gnome_app_set_contents(GNOME_APP(commandwin),buttontable);
	gnome_app_add_docked(GNOME_APP(commandwin),buttontable,"buttontable",
			     GNOME_DOCK_ITEM_BEH_NORMAL,GNOME_DOCK_TOP,1,0,0);
	gui->table=buttontable;
	for(i=0;i<GUI_BUTTONS_Y;i++)
		for(j=0;j<GUI_BUTTONS_X;j++){
			butt[j+i*GUI_BUTTONS_X]=gtk_button_new_with_label(_(gui->buttonlabels[j+i*GUI_BUTTONS_X]));
			
			gtk_widget_ref(butt[j+i*GUI_BUTTONS_X]);
			sprintf(lab,"butt%d",j+i*GUI_BUTTONS_X);
			gtk_object_set_data_full(GTK_OBJECT(commandwin),lab,butt[j+i*GUI_BUTTONS_X],
						 (GtkDestroyNotify)gtk_widget_unref);
			gtk_widget_show(butt[j+i*GUI_BUTTONS_X]);
			gtk_table_attach(GTK_TABLE(buttontable),butt[j+i*GUI_BUTTONS_X],j,j+1,i,i+1,
					 (GtkAttachOptions)(0),
					 (GtkAttachOptions)(0),0,0);
		}
	scroller = gtk_scrolled_window_new(NULL,NULL);
	gtk_widget_ref(scroller);
	gtk_widget_set_usize(scroller,640,480);
	gtk_object_set_data_full(GTK_OBJECT(commandwin),"scroller",scroller,
				 (GtkDestroyNotify)gtk_widget_unref);
	gnome_app_set_contents(GNOME_APP(commandwin),scroller);
	
	gui->iconlist = gnome_icon_list_new_flags(64,NULL,0);
	
	gtk_widget_ref(gui->iconlist);
	gtk_object_set_data_full(GTK_OBJECT(commandwin),"iconlist",gui->iconlist,
				 (GtkDestroyNotify)gtk_widget_unref);
	gtk_widget_show(gui->iconlist);
	gtk_container_add(GTK_CONTAINER(scroller),gui->iconlist);
	
	gtk_signal_connect(GTK_OBJECT(gui->iconlist),
			   "select-icon",
			   GTK_SIGNAL_FUNC(gui_handle_icon_sel),
			   NULL);

	gui->filters = g_array_new(TRUE,TRUE,sizeof(gui_filter*));
	
	
	return commandwin;
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
	gtk_container_set_border_width(hbox,2);

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

/* handels the filter-properties popup  */

void
icon_prop_activate                (gpointer user_data)
{
	int index=(int)user_data;


/*************************
 * This is veeeery badly made and hardcoded...
 * Will switch to libglade/xml descriptions later on
 *************************/

	GtkWidget* propBox;
	GtkWidget* notebook;
	GtkWidget* vbox,*vbox2,*vbox3;
	GtkWidget* tablabel;
	GtkWidget* frame,*frame2;
	filter_portdesc_t *port;
	filter_paramdesc_t *param;
	filter_t * filter = hash_find_filter(g_array_index(gui->filters,gui_filter*,index)->caption);
	
	propBox = gnome_property_box_new ();
	gtk_object_set_data (GTK_OBJECT (propBox), "propBox", propBox);
	notebook=GNOME_PROPERTY_BOX(propBox)->notebook;
	gtk_object_set_data (GTK_OBJECT (propBox), "notebook", notebook);

	vbox = gtk_vbox_new(TRUE,3);
	
	gtk_widget_ref(vbox);
	gtk_object_set_data_full (GTK_OBJECT (propBox), "vbox", vbox,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show(vbox);
	gtk_container_add (GTK_CONTAINER (notebook), vbox);

	
	create_frame_label_val_pair(propBox,vbox,"Name",filter->name);

	create_frame_label_val_pair(propBox,vbox,"Description",filter->description);
	
	tablabel=gtk_label_new(_("Info"));
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), 0), tablabel);
	vbox = gtk_vbox_new(FALSE,3);
	
	gtk_widget_ref(vbox);
	gtk_object_set_data_full (GTK_OBJECT (propBox), "vbox2", vbox,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show(vbox);
	gtk_container_add (GTK_CONTAINER (notebook), vbox);

	frame = gtk_frame_new(_("Input ports"));
	gtk_widget_ref (frame);
	
	gtk_object_set_data_full (GTK_OBJECT (propBox), "frame", frame,
				  (GtkDestroyNotify) gtk_widget_unref);
	
	vbox2 = gtk_vbox_new(FALSE,3);
	
	gtk_widget_ref(vbox2);
	gtk_object_set_data_full (GTK_OBJECT (propBox), "vbox3", vbox2,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show(vbox2);
	gtk_container_add (GTK_CONTAINER (frame), vbox2);
	gtk_widget_show (frame);
	gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 3);

	list_foreach_inputdesc(filter,port){
		frame2=gtk_frame_new(NULL);
		gtk_widget_ref(frame2);
		gtk_object_set_data(GTK_OBJECT(propBox),"frame4",frame2);
		gtk_widget_show(frame2);
		gtk_box_pack_start(GTK_BOX(vbox2),frame2,TRUE,TRUE,3);
		vbox3 = gtk_vbox_new(TRUE,3);
		gtk_widget_ref(vbox3);
		gtk_object_set_data(GTK_OBJECT(propBox),"vbox4",vbox3);
		gtk_widget_show(vbox3);
		gtk_container_add(GTK_CONTAINER(frame2),vbox3);
		create_label_val_pair(propBox,vbox3,"Label:",port->label);
		create_label_val_pair(propBox,vbox3,"Description:",port->description);
	}

	frame = gtk_frame_new(_("Output ports"));
	gtk_widget_ref (frame);
	
	gtk_object_set_data_full (GTK_OBJECT (propBox), "frame", frame,
				  (GtkDestroyNotify) gtk_widget_unref);
	
	vbox2 = gtk_vbox_new(TRUE,3);
	
	gtk_widget_ref(vbox2);
	gtk_object_set_data_full (GTK_OBJECT (propBox), "vbox3", vbox2,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show(vbox2);
	gtk_container_add (GTK_CONTAINER (frame), vbox2);
	gtk_widget_show (frame);
	gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, TRUE, 3);

	list_foreach_outputdesc(filter,port){
		frame2=gtk_frame_new(NULL);
		gtk_widget_ref(frame2);
		gtk_object_set_data(GTK_OBJECT(propBox),"frame4",frame2);
		gtk_widget_show(frame2);
		gtk_box_pack_start(GTK_BOX(vbox2),frame2,TRUE,TRUE,3);
		vbox3 = gtk_vbox_new(TRUE,3);
		gtk_widget_ref(vbox3);
		gtk_object_set_data(GTK_OBJECT(propBox),"vbox4",vbox3);
		gtk_widget_show(vbox3);
		gtk_container_add(GTK_CONTAINER(frame2),vbox3);
		create_label_val_pair(propBox,vbox3,"Label:",port->label);
		create_label_val_pair(propBox,vbox3,"Description:",port->description);
	}

	tablabel=gtk_label_new(_("Ports"));
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), 1), tablabel);


	vbox2 = gtk_vbox_new(FALSE,3);
	
	gtk_widget_ref(vbox2);
	gtk_object_set_data_full (GTK_OBJECT (propBox), "vbox3", vbox2,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show(vbox2);
	gtk_container_add (GTK_CONTAINER (notebook), vbox2);


	list_foreach_paramdesc(filter,param){
		frame2=gtk_frame_new(NULL);
		gtk_widget_ref(frame2);
		gtk_object_set_data(GTK_OBJECT(propBox),"frame4",frame2);
		gtk_widget_show(frame2);
		gtk_box_pack_start(GTK_BOX(vbox2),frame2,TRUE,TRUE,3);
		vbox3 = gtk_vbox_new(TRUE,3);
		gtk_widget_ref(vbox3);
		gtk_object_set_data(GTK_OBJECT(propBox),"vbox4",vbox3);
		gtk_widget_show(vbox3);
		gtk_container_add(GTK_CONTAINER(frame2),vbox3);
		create_label_val_pair(propBox,vbox3,"Label:",param->label);
		create_label_val_pair(propBox,vbox3,"Description:",param->description);
	}
	tablabel=gtk_label_new(_("Parameter"));
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (notebook), gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), 2), tablabel);
	

	gtk_widget_show(notebook);
	gtk_notebook_set_tab_hborder (GTK_NOTEBOOK (notebook), 5);
	gtk_notebook_popup_enable (GTK_NOTEBOOK (notebook));
	gtk_widget_show(propBox);

	
	fprintf(stderr,"%d: %s \n",index,filter->description);

	


}

/* handels an icon select */

void gui_handle_icon_sel (GnomeIconList *iconlist,
			  gint index,
			  GdkEvent *event,
			  gpointer user_data)
{
	GtkWidget *men;
	GtkWidget *prop;
	
	if(event){
		if(((GdkEventButton*)event)->button==3){
		

			men=gtk_menu_new();
			prop=gtk_menu_item_new_with_label("Properties");
			gtk_menu_append(GTK_MENU(men),prop);
			gtk_signal_connect_object(GTK_OBJECT(prop),"activate",GTK_SIGNAL_FUNC(icon_prop_activate),index);
			gtk_widget_show(prop);
			gtk_widget_show(men);
		
			gtk_menu_popup(GTK_MENU(men),NULL,NULL,NULL,NULL,((GdkEventButton*)event)->button,GDK_CURRENT_TIME);
		}
	
		
	}else
		fprintf(stderr,"unhandled event in gui_handle_icon_sel\n");
}

/* browses the registered filtrs and adds them to the gui */
int gui_browse_registered_filters(void)
{
	filter_t * fil=NULL;
	gui_filter* gfilt;
	int i=0;
	
	while((fil=filter_next(fil))){
		gfilt=gui_filter_new("pixmaps/default.png",fil);
		gui_filter_add(gfilt);
	}
}


/* inits filter stuff */
int gui_filter_init(void)
{
	if (hash_alloc() == -1) {
                fprintf(stderr, "error in initting global hash\n");
                return -1;
        }
        
        if (filter_init() == -1) {
                fprintf(stderr, "error in filter_init()\n");
                return -1;
        }
}
