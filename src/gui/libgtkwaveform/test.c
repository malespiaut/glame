#include <stdio.h>
#include <math.h>
#include <gtk/gtk.h>
#include "gtkwaveview.h"
#include "gtkswapfilebuffer.h"
#include "glame_types.h"
#include "swapfile.h"

/* Quit callback. */
static void
quit (GtkWidget *widget, gpointer user_data)
{
  gtk_main_quit ();
}

int
main (int argc, char *argv[])
{
  GtkWidget *window, *waveview;
  GtkObject *wavebuffer;
  gint16    *ptr;
  gint       i;
  long fname;

  /* Init swapfile. */
  if (argc < 3) {
	  fprintf(stderr, "Usage: %s swap file\n", argv[0]);
	  exit(1);
  }
  
  if (swapfile_open(argv[1], 0) == -1) {
	  fprintf(stderr, "Unable to open swapfile\n");
	  exit(1);
  }

  /* Initialize Gtk+. */
  gtk_init (&argc, &argv);

  for(i=2;i<argc;i++) {
  /* Create a Gtk+ window. */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  
  fname = atoi(argv[i]);

  /* Create a GtkWaveView widget. */
  waveview = gtk_wave_view_new ();
  gtk_wave_view_set_select_channels (GTK_WAVE_VIEW (waveview), 0x03);
  gtk_widget_set_usize (waveview, 300, 200);

  /* Set the zoom factor such that 1 pixel = 5 frames.  A frame is equal
     to n samples at one point in time where n = number of channels. */
  gtk_wave_view_set_zoom (GTK_WAVE_VIEW (waveview), 1.0);

  /* Set the cache size to hold 8192 pixel columns of data.  This means
     the user can scroll the widget's contents back and forth and we
     will cache the most recently displayed 8192 columns of data. */
  gtk_wave_view_set_cache_size (GTK_WAVE_VIEW (waveview), 8192);

  /* Create a data source object. */
  wavebuffer = gtk_swapfile_buffer_new (fname, GLAME_DEFAULT_SAMPLERATE);
  if (!wavebuffer) {
	  fprintf(stderr, "Unable to create wavebuffer\n");
	  goto cleanup;
  }

  /* Add GtkWaveView to the window. */
  gtk_container_add (GTK_CONTAINER (window), waveview);

  /* Set the Waveform widget's data stream to point to our wavebuffer. */
  gtk_wave_view_set_buffer (GTK_WAVE_VIEW (waveview),
                            GTK_WAVE_BUFFER (wavebuffer));

  /* Call the usual Gtk+ cruft. */
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
                      (GtkSignalFunc) quit, NULL);
  gtk_widget_show_all (window);
  
  }

  gtk_main ();

  /* cleanup. */
 cleanup:
  swapfile_close();

  return 0;
}
