/*
 * wavetest.c
 *
 * Author: Joe Navratil
 *
 * Description: Crappy little test program.  See the (sparse) comments
 *              for hints on getting it to do what you want.
 */

#include <stdlib.h>
#include <time.h>
#include <gtk/gtk.h>
#include "gtkwavedraw.h"

static gfloat *
generate_random_data(int n_vals)
{
  int i;
  gfloat *fltary;
  static int entry = 1;
  double scale;

  fltary = (gfloat *)g_malloc(n_vals * sizeof(float));
  scale = 2 / (gfloat)RAND_MAX;

  if (entry) {
    srandom((unsigned int)time(NULL));
    entry = 0;
  }
  for (i=0; i<n_vals; i++) {
    fltary[i] = (gfloat)random() * scale - 1;
  }
  return fltary;
}

gint CloseAppWindow (GtkWidget *widget, gpointer *data)
{
  gtk_main_quit();

  return FALSE;
}

int main(int argc, char **argv)
{
  GtkWidget *window;
  GtkWidget *wavedraw;
  gfloat *fltary;
  gint npoints;

  gtk_init(&argc, &argv);
  
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title(GTK_WINDOW(window), "Test Wave Widget");

  gtk_signal_connect(GTK_OBJECT(window), "delete-event",
		     GTK_SIGNAL_FUNC (CloseAppWindow), NULL);

  gtk_container_border_width(GTK_CONTAINER(window), 20);


  /*
   * Everything's based off of npoints right now.  npoints random
   * points are generated; the widget is zoomed to display 0 to
   * npoints, etc.
   */

  npoints = 2000;

  wavedraw = gtk_wave_draw_new();
  gtk_widget_show(wavedraw);

  /* Some basic starting stuff */
  gtk_wave_draw_zoom(GTK_WAVE_DRAW(wavedraw), 0, npoints);
  gtk_wave_draw_set_resolution(GTK_WAVE_DRAW(wavedraw),10);
  
  /* Add a wave */
  fltary = generate_random_data(npoints);
  gtk_wave_draw_add_wave(GTK_WAVE_DRAW(wavedraw), fltary, npoints, 0);
  
  /* Add another wave */
  fltary = generate_random_data(npoints);
  gtk_wave_draw_add_wave(GTK_WAVE_DRAW(wavedraw), fltary, npoints, 0);
  
  g_free(fltary);

  /* Draw the widget, this populates the backbuffer */
  gtk_widget_draw(wavedraw, NULL);
  
  gtk_container_add(GTK_CONTAINER(window), wavedraw);
  gtk_widget_show(window);

  gtk_main();
  exit(0);
}
