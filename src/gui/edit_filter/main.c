/*
 * edit_filter.c
 *
 * $Id: main.c,v 1.3 2001/01/29 11:40:01 richi Exp $
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
		if (swapfile_open(argv[1], 0) == -1) {
			if (errno != EBUSY) {
				perror("ERROR: Unable to open swap");
				exit(1);
			}
			fprintf(stderr, "WARNING: Unclean swap - running fsck\n");
			if (swapfile_fsck(argv[1]) == -1) {
				perror("ERROR: Fsck failed");
				exit(1);
			}
			fprintf(stderr, "WARNING: Fsck successful\n");
			if (swapfile_open(argv[1], 0) == -1) {
				perror("ERROR: Still cannot open swap");
				exit(1);
			}
		}
	} else {
		fprintf(stderr, "WARNING: starting without a swapfile.\n");
	}

	/* setup gnome/gtk  */
	gnome_init("glame_filter_edit", VERSION, argc, argv);

	/* init glame */
	glame_init_with_guile(gui_main);

	/* not reached */
	return 0;
}
  
