/*
 * edit_filter.c
 *
 * $Id: main.c,v 1.1 2000/12/07 14:28:31 xwolf Exp $
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

#include "swapfile.h"
#include "glmid.h"
#include "gui.h"



static void gui_main()
{

  gui_network_new_wizard();
  
  //  gtk_signal_connect(GTK_OBJECT(gui->app),"delete-event",GTK_SIGNAL_FUNC(gui_exit),NULL);
  
	/* main loop */
  gtk_main();
}


int main(int argc, char *argv[])
{
	/* swapfile setup */
	if (argc >= 2) {
		if (swapfile_open(argv[1], 0) == 0)
			fprintf(stderr, "swapfile %s opened.\n", argv[1]);
	}

	/* setup gnome/gtk  */
	gnome_init("glame_filter_edit", VERSION, argc, argv);

	/* init glame */
	glame_init_with_guile(gui_main);

	/* not reached */
	return 0;
}
  
