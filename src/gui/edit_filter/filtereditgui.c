/*
 * filtereditgui.c
 *
 * $Id: filtereditgui.c,v 1.1 2001/05/07 00:45:36 xwolf Exp $
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



static gboolean 
root_event(GnomeCanvas * canvas, GdkEvent *event, GlameCanvas* glCanv)
{
	plugin_t *plug;
	filter_t * filter;
	filter_t* filter2;
	filter_pipe_t *pipe;
	GlameCanvasFilter* f;
	int eventx, eventy;
	GdkEventButton *event_button;
	if(event->type == GDK_BUTTON_PRESS){
		plug = plugin_get("one2n");
		
		filter = filter_instantiate(plug);
		fprintf(stderr,"%s\n",filter_name(filter));
		event_button = (GdkEventButton *) event;
		//			gnome_canvas_c2w(canv,event->button.x,event->button.y,&event_x,&event_y);
		eventx = event->button.x;
		eventy = event->button.y;
		
		f = glame_canvas_filter_new(gnome_canvas_root(canvas),filter);
		glame_canvas_filter_move(f,(double)eventx,(double)eventy);

		filter2 = filter_instantiate(plug);
		f = glame_canvas_filter_new(gnome_canvas_root(canvas),filter2);
		glame_canvas_filter_move(f,(double)eventx+200, (double)eventy+100);
		pipe = filterport_connect(filterportdb_get_port(filter_portdb(filter),PORTNAME_OUT),
					  filterportdb_get_port(filter_portdb(filter2),PORTNAME_IN));
		glame_canvas_pipe_new(gnome_canvas_root(canvas),pipe);


	}
		
		

}






GtkWidget * 
glame_filtereditgui_new(filter_t *net)
{
	GtkWidget *window, *canvas, *sw, *toolbar;

	GnomeDock *dock;
	
	const char *name;

	
	if(net && filter_name(net))
		name = strdup(filter_name(net));
	else
		name = strdup("Untitled");
	


	window = gnome_app_new(name,_(name));
	dock = GNOME_DOCK(GNOME_APP(window)->dock);
	gtk_widget_ref(GTK_WIDGET(dock));
	
	gtk_widget_show(GTK_WIDGET(dock));
	toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,GTK_TOOLBAR_BOTH);


	sw = gtk_scrolled_window_new(NULL,NULL);
	gtk_widget_show(sw);
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
	/*
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Execute","Executes Filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_EXEC),network_play,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Register","Registers actual filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CONVERT),register_filternetwork_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Save","Saves Filternetwork","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_SAVE_AS),canvas_save_as,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Properties","Edit Filternetwork Properties","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_PROPERTIES),canvas_network_property_dialog_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom in","Zooms in","foo",
				gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_DOWN),
				//gnome_pixmap_new_from_file("plus.png"),
				canvas_zoom_in_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Zoom out","Zooms out","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_UP),canvas_zoom_out_cb,canvas);
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"View all","Adjusts scroll region","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_REFRESH),canvas_update_scroll_region_cb,canvas);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Close","Close","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_CLOSE),canvas_close,window);
	gtk_toolbar_append_space(GTK_TOOLBAR(toolbar));
	gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),"Help","Help","foo",gnome_stock_new_with_icon(GNOME_STOCK_PIXMAP_HELP),gnome_help_goto,"info:glame#The_Filternetwork_Editor");
	gnome_app_add_toolbar(GNOME_APP(window), GTK_TOOLBAR(toolbar),
			      "canvas::toolbar",
			      GNOME_DOCK_ITEM_BEH_EXCLUSIVE|GNOME_DOCK_ITEM_BEH_NEVER_FLOATING,
			      GNOME_DOCK_TOP, 0, 0, 0);
	*/
	gnome_app_set_contents(GNOME_APP(window),sw);	
	gtk_widget_show(GTK_WIDGET(dock));
	
	gtk_window_set_default_size(GTK_WINDOW(window),400,300);
	gtk_widget_show_all(window);
	gtk_signal_connect_after(GTK_OBJECT(canvas),"event",GTK_SIGNAL_FUNC(root_event),canvas);
	return window;
}


