#ifndef _SWAPFILEGUI_H
#define _SWAPFILEGUI_H

/* The swapfile widget is responsible for opening and closing the
 * swapfile, i.e. it gets constructed out of a swapfile.
 * As there can be only one swapfile open at a time, only one
 * swapfile widget can be constructed (i.e. there is a global
 * containing the widget).
 */

/* Opens the swapfile and creates the tree widget out of the information
 * stored in file 0 in xml format. FIXME: policy wrt inconsistencies
 * of xml/swapfile state, not existing swapfile, etc.
 * Returns the swapfile gui widget (not shown). */
GtkWidget *glame_swapfile_gui_new(const char *swapfile);

/* Destroys the swapfile widget (so you can create another one) and
 * updates the xml representation of the meta information in swapfile
 * 0 and closes the swapfile. */
void glame_swapfile_gui_destroy();


/* FIXME: everything internal? We may f.i. have an */
int glame_swapfile_gui_import(const char *filename);
/* or something like that... */


#endif
