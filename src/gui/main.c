/*
 * main.c
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gnome.h>
#include "gui.h"





int main(int argc, char *argv[])
{
	GtkWidget *about;
	GtkWidget *commandwin;

	/* dummy buttons */
	gui_filter * bla;
	char *labels[]={
		"bla1","bla2","bla3","bla4"};
	
	/* setup gnome/gtk  */
	
	gnome_init("glame",VERSION,argc,argv);
	
	/* splash screen */
	about = gui_create_about();
	
	gtk_widget_show(about);
	
	gui=malloc(sizeof(glame_gui));
	
	gui->buttonlabels =labels;
	
	
	/* fire up main dispatch */
	commandwin = gui_create_commandwin();
	gtk_widget_show(commandwin);


	gui_filter_init();
	gui_browse_registered_filters();

	gtk_signal_connect(GTK_OBJECT(gui->app),"delete-event",GTK_SIGNAL_FUNC(gui_exit),NULL);
	
	/* main loop */
       	gtk_main();
	return 0;
}
  
