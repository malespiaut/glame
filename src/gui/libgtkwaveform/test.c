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


static int temp_nrtracks = 0;
static long temp_fname[GTK_SWAPFILE_BUFFER_MAX_TRACKS];
static swfd_t temp_fd[GTK_SWAPFILE_BUFFER_MAX_TRACKS];

static void reset_temp(int nrtracks)
{
	int i;

	/* Kill unnecessary temporary tracks. */
	for (i=nrtracks; i<temp_nrtracks; i++) {
		sw_close(temp_fd[i]);
		sw_unlink(temp_fname[i]);
	}

	/* Generate additionally needed tracks. */
	for (i=temp_nrtracks; i<nrtracks; i++) {
		while ((temp_fd[i] = sw_open(rand(), O_CREAT|O_EXCL|O_RDWR, TXN_NONE)) == -1)
			;
	}
	temp_nrtracks = nrtracks;

	/* Reset temporary tracks. */
	for (i=0; i<temp_nrtracks; i++) {
		sw_ftruncate(temp_fd[i], 0);
		sw_lseek(temp_fd[i], 0, SEEK_SET);
	}
}

/* Menu event - Copy. */
static void copy_cb(GtkWidget *bla, GtkWaveView *waveview)
{
	GtkWaveBuffer *wavebuffer = gtk_wave_view_get_buffer (waveview);
	GtkEditableWaveBuffer *editable = GTK_EDITABLE_WAVE_BUFFER (wavebuffer);
	GtkSwapfileBuffer *swapfile = GTK_SWAPFILE_BUFFER(editable);
	gint32 start, length;
	swfd_t fd;
	long *names, nrtracks;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	DPRINTF("Copying selection from %i of length %i\n", start, length);

	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	reset_temp(nrtracks);
	for (i=0; i<nrtracks; i++) {
		fd = sw_open(names[i], O_RDONLY, TXN_NONE);
		sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
		if (sw_sendfile(temp_fd[i], fd, length*SAMPLE_SIZE, SWSENDFILE_INSERT) == -1)
			fprintf(stderr, "*** sw_sendfile failed\n");
		sw_close(fd);
	}
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
	long *names, nrtracks;
	int i;

	start = gtk_wave_view_get_marker (waveview);
	if (start < 0)
		return;

	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	if (nrtracks != temp_nrtracks) {
		DPRINTF("clipboard nrtracks do not match\n");
		return;
	}

	if (sw_fstat(temp_fd[0], &st) == -1)
		return;

	g_print ("Pasting to %i size %i\n", start, st.size/SAMPLE_SIZE);

	for (i=0; i<nrtracks; i++) {
		fd = sw_open(names[i], O_RDWR, TXN_NONE);
		if (sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET) != start*SAMPLE_SIZE)
			fprintf(stderr, "*** sw_lseek(swapfile->fd) failed\n");
		if (sw_lseek(temp_fd[i], 0, SEEK_SET) != 0)
			fprintf(stderr, "*** sw_lseek(temp_fd, 0) failed\n");
		if (sw_sendfile(fd, temp_fd[i], st.size, SWSENDFILE_INSERT) == -1)
			fprintf(stderr, "*** sw_sendfile failed\n");
		sw_close(fd);
	}

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
	long *names, nrtracks;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	g_print ("Deleting selection from %i of length %i\n", start, length);

	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	reset_temp(nrtracks);
	for (i=0; i<nrtracks; i++) {
		fd = sw_open(names[i], O_RDWR, TXN_NONE);
		sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
		if (sw_sendfile(temp_fd[i], fd, length*SAMPLE_SIZE, SWSENDFILE_CUT|SWSENDFILE_INSERT) == -1)
			fprintf(stderr, "*** sw_sendfile failed\n");
		sw_close(fd);
	}

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
	long *names, nrtracks;
	int i;

	gtk_wave_view_get_selection (waveview, &start, &length);
	if (length <= 0)
		return;

	g_print ("Cutting selection from %i of length %i\n", start, length);

	nrtracks = gtk_swapfile_buffer_get_filenames(swapfile, &names);
	for (i=0; i<nrtracks; i++) {
		fd = sw_open(names[i], O_RDWR, TXN_NONE);
		sw_lseek(fd, start*SAMPLE_SIZE, SEEK_SET);
		if (sw_sendfile(SW_NOFILE, fd, length*SAMPLE_SIZE, SWSENDFILE_CUT) == -1)
			fprintf(stderr, "*** sw_sendfile failed\n");
		sw_close(fd);
	}

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
	int nrtracks;

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

	/* Create a Gtk+ window. */
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  
	/* Create a GtkWaveView widget. */
	nrtracks = argc - 3;
	waveview = gtk_wave_view_new ();
	gtk_wave_view_set_select_channels (GTK_WAVE_VIEW (waveview), 0x03);
	gtk_widget_set_usize (waveview, 300+nrtracks*20, 150*nrtracks+50);

	/* Set the zoom factor such that 1 pixel = 5 frames.
	 * A frame is equal to n samples at one point in time
	 * where n = number of channels. */
	gtk_wave_view_set_zoom (GTK_WAVE_VIEW (waveview), atoi(argv[2]));

	/* Set the cache size to hold 8192 pixel columns of data.
	 * This means the user can scroll the widget's contents
	 * back and forth and we will cache the most recently
	 * displayed 8192 columns of data. */
	gtk_wave_view_set_cache_size (GTK_WAVE_VIEW (waveview), 8192);

	/* Create a data source object. - FIXME (API) */
	wavebuffer = NULL;
	if (nrtracks == 1)
		wavebuffer = gtk_swapfile_buffer_new (1, GLAME_DEFAULT_SAMPLERATE, atoi(argv[3]));
	else if (nrtracks == 2)
		wavebuffer = gtk_swapfile_buffer_new (2, GLAME_DEFAULT_SAMPLERATE, atoi(argv[3]), atoi(argv[4]));
	else if (nrtracks == 3)
		wavebuffer = gtk_swapfile_buffer_new (3, GLAME_DEFAULT_SAMPLERATE, atoi(argv[3]), atoi(argv[4]), atoi(argv[5]));
	else if (nrtracks == 4)
		wavebuffer = gtk_swapfile_buffer_new (4, GLAME_DEFAULT_SAMPLERATE, atoi(argv[3]), atoi(argv[4]), atoi(argv[5]), atoi(argv[6]));
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

	gtk_main ();

	/* cleanup. */
 cleanup:
	reset_temp(0);
	swapfile_close();

	return 0;
}
