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
#include "swapfile.h"


/* Simple and very stupid wave callback using the
 * swapfile API (just file 0). For more intelligent
 * usage we might need more "clever" handling of
 * the callback result. A different callback might
 * have the following prototype:
 *   struct gliovec *callback(void *handle,
 *                            glong from, glong to, gint step);
 * where struct gliovec is similar to the readv/writev one:
 *   struct gliovec {
 *       gfloat *data;
 *       glong from;
 *       glong to;
 *       gint step;
 *   };
 *
 * The gtk_wave_draw_add_by_callback() would take an additional
 * void *handle argument which is passed along to the callback.
 */

static fileid_t fid = 1; /* HACK */
static gfloat *wave = NULL; /* HACK, too */

/* umm, I dont understand semantics of step...
 */
static gfloat *swapfile_callback(gint wave_idx,
				 glong from, glong to, gint step)
{
	gfloat *s, *w;
	filecluster_t *fc;

	printf("callback with from %li to %li step %i\n", from, to, step);
	if (wave)
		free(wave);
	wave = (gfloat *)malloc(sizeof(gfloat)*(to-from+1+step-1)/step);

	w = wave;
	while (from <= to) {
		if (!(fc = filecluster_get(fid, from*sizeof(gfloat)))) {
			printf("no filecluster at %li\n", from);
			return NULL;
		}
		s = (gfloat *)filecluster_mmap(fc);
		s = s + from - filecluster_start(fc)/sizeof(gfloat);
		while (from <= filecluster_end(fc)/sizeof(gfloat)
		       && from <= to) {
			*w++ = *s;
			s += step;
			from += step;
		}
		filecluster_munmap(fc);
	}

	return wave;
}




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

  if (argc > 1 && swap_open(argv[1], 0) == 0) {
	  /* Some basic starting stuff */
	  gtk_wave_draw_zoom(GTK_WAVE_DRAW(wavedraw), 0, 10000);
	  gtk_wave_draw_set_resolution(GTK_WAVE_DRAW(wavedraw), 10);

	  /* Add a swapfile wave - HACK */
	  gtk_wave_draw_add_wave_by_call(GTK_WAVE_DRAW(wavedraw),
					 swapfile_callback,
					 file_size(fid)/sizeof(gfloat), 0);
  } else {
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
  }

  /* Draw the widget, this populates the backbuffer */
  gtk_widget_draw(wavedraw, NULL);
  
  gtk_container_add(GTK_CONTAINER(window), wavedraw);
  gtk_widget_show(window);

  gtk_main();
  exit(0);
}
