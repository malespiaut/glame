#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <gtk/gtk.h>
#include <gnome.h>
#include "gtkwaveview.h"
#include "gtkswapfilebuffer.h"
#include "glame_types.h"
#include "swapfile.h"



/* Quit callback. */
static void quit (GtkWidget *widget, gpointer user_data)
{
	gtk_main_quit ();
}



static long temp_fname = -1;
static swfd_t temp_fd;

static void reset_temp()
{
	if (temp_fname == -1) {
		for (temp_fname = 1000; ; temp_fname++) {
			temp_fd = sw_open(temp_fname,
					  O_CREAT|O_EXCL|O_RDWR, TXN_NONE);
			if (temp_fd != -1)
				break;
		}
	}

	sw_ftruncate(temp_fd, 0);
	sw_lseek(temp_fd, 0, SEEK_SET);
}

/* Menu event - Copy. */
static void copy_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	g_print ("Copying selection from %i of length %i\n", start, length);

	reset_temp();
	fd = sw_open(gtk_swapfile_buffer_get_filename(swapfile), O_RDONLY, TXN_NONE);
	sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
	if (sw_sendfile(temp_fd, fd, length*SAMPLE_SIZE, SWSENDFILE_INSERT) == -1)
		fprintf(stderr, "*** sw_sendfile failed\n");
	sw_close(fd);
}

/* Menu event - Paste. */
static void paste_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start;
	struct sw_stat st;
	swfd_t fd;

	start = gtk_wave_view_get_marker (waveview);
	if (start < 0)
		return;

	if (sw_fstat(temp_fd, &st) == -1)
		return;

	g_print ("Pasting to %i size %i\n", start, st.size/SAMPLE_SIZE);

	fd = sw_open(gtk_swapfile_buffer_get_filename(swapfile), O_RDWR, TXN_NONE);
	if (sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET) != start*SAMPLE_SIZE)
		fprintf(stderr, "*** sw_lseek(swapfile->fd) failed\n");
	if (sw_lseek(temp_fd, 0, SEEK_SET) != 0)
		fprintf(stderr, "*** sw_lseek(temp_fd, 0) failed\n");
	if (sw_sendfile(fd, temp_fd, st.size, SWSENDFILE_INSERT) == -1)
		fprintf(stderr, "*** sw_sendfile failed\n");
	sw_close(fd);

	gtk_editable_wave_buffer_insert(editable, start, st.size/SAMPLE_SIZE);
}

/* Menu event - Delete. */
static void delete_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	g_print ("Deleting selection from %i of length %i\n", start, length);

	reset_temp();
	fd = sw_open(gtk_swapfile_buffer_get_filename(swapfile), O_RDWR, TXN_NONE);
	sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
	if (sw_sendfile(temp_fd, fd, length*SAMPLE_SIZE, SWSENDFILE_CUT|SWSENDFILE_INSERT) == -1)
		fprintf(stderr, "*** sw_sendfile failed\n");
	sw_close(fd);

	gtk_editable_wave_buffer_delete (editable, start, length);
}

/* Menu event - Cut. */
static void cut_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	g_print ("Cutting selection from %i of length %i\n", start, length);

	fd = sw_open(gtk_swapfile_buffer_get_filename(swapfile), O_RDWR, TXN_NONE);
	sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
	if (sw_sendfile(SW_NOFILE, fd, length*SAMPLE_SIZE, SWSENDFILE_CUT) == -1)
		fprintf(stderr, "*** sw_sendfile failed\n");
	sw_close(fd);

	gtk_editable_wave_buffer_delete (editable, start, length);
}



static GnomeUIInfo rmb_menu[] = {
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Copy", "copy", copy_cb, NULL),
	GNOMEUIINFO_ITEM("Paste", "paste", paste_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Delete", "delete", delete_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_ITEM("Cut", "cut", cut_cb, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_END
};

/* Button press event. */
static void press (GtkWidget *widget, GdkEventButton *event,
		   gpointer user_data) 
{
	GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
	GtkWaveBuffer *wavebuffer;
	GtkWidget *menu;
  
	if (event->button != 3)
		return;

	wavebuffer = gtk_wave_view_get_buffer (waveview);
	if (!GTK_IS_EDITABLE_WAVE_BUFFER (wavebuffer)) {
		fprintf(stderr, "not editable\n");
		return;
	}

	menu = gnome_popup_menu_new(rmb_menu);
	gnome_popup_menu_do_popup(menu, NULL, NULL, event, waveview);
}


int main (int argc, char *argv[])
{
	GtkWidget *window, *waveview;
	GtkObject *wavebuffer;
	long fname;
	int i;

	/* Init swapfile. */
	if (argc < 4) {
		fprintf(stderr, "Usage: %s swap zoom file...\n", argv[0]);
		exit(1);
	}
  
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

	/* Initialize Gtk+. */
	gtk_init (&argc, &argv);

	for(i=3;i<argc;i++) {
		/* Create a Gtk+ window. */
		window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  
		fname = atoi(argv[i]);

		/* Create a GtkWaveView widget. */
		waveview = gtk_wave_view_new ();
		gtk_wave_view_set_select_channels (GTK_WAVE_VIEW (waveview), 0x03);
		gtk_widget_set_usize (waveview, 300, 200);

		/* Set the zoom factor such that 1 pixel = 5 frames.
		 * A frame is equal to n samples at one point in time
		 * where n = number of channels. */
		gtk_wave_view_set_zoom (GTK_WAVE_VIEW (waveview), atoi(argv[2]));

		/* Set the cache size to hold 8192 pixel columns of data.
		 * This means the user can scroll the widget's contents
		 * back and forth and we will cache the most recently
		 * displayed 8192 columns of data. */
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
		gtk_signal_connect (GTK_OBJECT (waveview), "button_press_event",
				    (GtkSignalFunc) press, NULL);
		gtk_widget_show_all (window);
	}

	gtk_main ();

	/* cleanup. */
 cleanup:
	if (temp_fname != -1) {
		sw_close(temp_fd);
		sw_unlink(temp_fname);
	}
	swapfile_close();

	return 0;
}
