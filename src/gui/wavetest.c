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

static swfd_t fd = -1; /* HACK */
static gfloat *wave = NULL; /* HACK, too */

/* umm, I dont understand semantics of step...
 */
static gfloat *swapfile_callback(gint wave_idx,
				 glong from, glong to, gint step)
{
	int cnt = sizeof(gfloat)*(to-from+1+step-1)/step;
	int res;

	printf("callback with from %li to %li step %i\n", from, to, step);
	if (wave)
		free(wave);
	wave = (gfloat *)malloc(cnt);

	sw_lseek(fd, from, SEEK_SET);
	res = sw_read(fd, wave, cnt);
	if (res == -1) {
		perror("sw_read");
	} else if (res < cnt) {
		printf("short read - %i\n", res);
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
  GtkWidget *window, *hbox, *vbox, *hsb, *vsb;
  GtkWidget *wavedraw;
  gfloat *fltary;
  gint npoints;
  int fname;
  struct sw_stat sbuf;

  gtk_init(&argc, &argv);

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title(GTK_WINDOW(window), "Test Wave Widget");

  gtk_signal_connect(GTK_OBJECT(window), "delete-event",
		     GTK_SIGNAL_FUNC (CloseAppWindow), NULL);

  gtk_container_border_width(GTK_CONTAINER(window), 20);

  hbox = gtk_hbox_new(FALSE, 0);
  vbox = gtk_vbox_new(FALSE, 0);
  hsb = gtk_hscrollbar_new(NULL);
  vsb = gtk_vscrollbar_new(NULL);

  gtk_box_pack_start(hbox, vsb, FALSE, FALSE, 10);
  gtk_box_pack_end(vbox, hsb, FALSE, FALSE, 10);
  gtk_box_pack_end(vbox, hbox, TRUE, TRUE, 10);
  gtk_container_add(GTK_CONTAINER(window), vbox);
  gtk_widget_show(hsb);
  gtk_widget_show(vsb);
  gtk_widget_show(hbox);
  gtk_widget_show(vbox);

  /*
   * Everything's based off of npoints right now.  npoints random
   * points are generated; the widget is zoomed to display 0 to
   * npoints, etc.
   */

  npoints = 2000;

  wavedraw = gtk_wave_draw_new();
  gtk_widget_show(wavedraw);

  if (argc > 2) {
	  if (swapfile_open(argv[1], 0) == -1) {
		  fprintf(stderr, "Error opening swapfile %s\n", argv[1]);
		  exit(1);
	  }

	  fname = atoi(argv[2]);
	  if ((fd = sw_open(fname, O_RDONLY, TXN_NONE)) == -1) {
		  fprintf(stderr, "Error opening file %i\n", fname);
		  swapfile_close();
		  exit(1);
	  }

	  /* Some basic starting stuff */
	  gtk_wave_draw_zoom(GTK_WAVE_DRAW(wavedraw), 0, 10000);
	  gtk_wave_draw_set_resolution(GTK_WAVE_DRAW(wavedraw), 10);

	  /* Add a swapfile wave - HACK */
	  sw_fstat(fd, &sbuf);
	  gtk_wave_draw_add_wave_by_call(GTK_WAVE_DRAW(wavedraw),
					 swapfile_callback,
					 sbuf.size/sizeof(gfloat), 0);

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
  
  gtk_box_pack_start(hbox, wavedraw, TRUE, TRUE, 10);
  gtk_widget_show(window);

  gtk_main();

  swapfile_close();
  exit(0);
}
