/*
 * main.c
 *
 * $Id: main.c,v 1.8 2001/05/17 22:38:36 xwolf Exp $
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "glmid.h"
#include "filtereditgui.h"


static void gui_main()
{
	GtkWidget *commandwin;

	/* fire up main dispatch */
	commandwin = glame_filtereditgui_new(NULL); 
	gtk_widget_show(commandwin);


	
	/* main loop */
       	gtk_main();
}


int main(int argc, char *argv[])
{
	/* setup gnome/gtk  */
	gnome_init("glame", VERSION, argc, argv);

	/* init glame */
	glame_init(gui_main);

	/* not reached */
	return 0;
}
  
