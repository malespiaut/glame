/*
 * main.c
 *
 * $Id: main.c,v 1.10 2001/03/15 12:54:11 richi Exp $
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

#include <stdlib.h>
#include "swapfile.h"
#include "glmid.h"
#include "swapfilegui.h"
#include "waveeditgui.h"


static char *swname;
static GtkWidget *swapfile;

static void gui_quit(GtkWidget *widget, gpointer data)
{
	glame_waveedit_cleanup();
	glame_swapfile_gui_destroy();
	gtk_main_quit();
}

static void gui_main()
{
	GtkWidget * mainwin;
	/* create swapfile gui */
	swapfile = glame_swapfile_gui_new(swname);
	if (!swapfile)
		return;
	mainwin = gui_main_new();
	gtk_signal_connect(GTK_OBJECT(mainwin), "delete-event",
			   GTK_SIGNAL_FUNC(gui_quit), NULL);
	gtk_widget_show(swapfile);
	gnome_app_set_contents(GNOME_APP(mainwin),swapfile);
	gtk_widget_show(mainwin);

	/* cleanup handler */
	atexit((void (*)(void))gui_quit);

	/* main loop */
       	gtk_main();
}


int main(int argc, char **argv)
{
        if (argc < 2)
		goto err;

	/* setup gnome/gtk  */
	gnome_init("glame", VERSION, argc, argv);

	/* init glame */
	swname = argv[1];
	glame_init_with_guile(gui_main);

 err:	/* reached on error only */
	fprintf(stderr, "Usage: %s swapfile\n", argv[0]);

	return 1;
}
