#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include <gnome.h>

struct timeline {
	GtkWidget *table;
        GtkWidget *sw1, *sw2;
	GtkWidget *cv1, *cv2;
	GtkWidget *ruler;
};

struct timeline *timeline_new()
{
	struct timeline *tl;
	GtkAdjustment *va, *ha;
	int i;

	/* Alloc the timeline. */
	tl = g_malloc(sizeof(struct timeline));

	/* table that holds both scrolling windows. */
	tl->table = gtk_table_new(2, 2, FALSE);

	/* Scrolling window that will held the main canvas. We share
	 * the vertical adjustment with the scrolled window below. */
	tl->sw2 = gtk_scrolled_window_new(NULL, NULL);
	va = gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(tl->sw2));
	ha = gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(tl->sw2));

	/* Scrolling window that will held the track labels canvas.
	 * This should be implicitly scrolled with the canvas. */
	tl->sw1 = gtk_scrolled_window_new(NULL, va);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(tl->sw1),
		GTK_POLICY_NEVER, GTK_POLICY_NEVER);

	/* The two canvas. */
	tl->cv1 = gnome_canvas_new();
	tl->cv2 = gnome_canvas_new();
	gnome_canvas_set_scroll_region(tl->cv1, 0.0, 0.0, 100.0, 20.0);
	gnome_canvas_set_scroll_region(tl->cv2, 0.0, 0.0, 1000.0, 20.0);
	gnome_canvas_set_pixels_per_unit(tl->cv1, 20);
	gnome_canvas_set_pixels_per_unit(tl->cv2, 20);

	/* Fill the left canvas with 20 gtk_entry. */
	for (i=0; i<20; i++) {
	    	char str[64];
		GtkWidget *entry;
		GnomeCanvasItem *item;
		
	    	entry = gtk_entry_new();
		sprintf(str, "track%i", i);
		gtk_entry_set_text(entry, str);
		item = gnome_canvas_item_new(gnome_canvas_root(tl->cv1),
			gnome_canvas_widget_get_type(),
			"x", (double)0.0, "y", (double)i,
			"widget", entry,
			"size_pixels", TRUE,
			NULL);
		gtk_widget_show(entry);
	}

	/* a ruler */
	tl->ruler = gtk_widget_new(gtk_ruler_get_type(), NULL);
	gtk_ruler_set_range(tl->ruler, 0.0, 1000.0, 0.0, 1000.0);
	gtk_ruler_set_metric(tl->ruler, GTK_PIXELS);
	//gtk_ruler_draw_pos(tl->ruler);
	//gtk_ruler_draw_ticks(tl->ruler);

	/* Pack and show the whole stuff. */
	gtk_container_add(GTK_CONTAINER(tl->sw1), tl->cv1);
	gtk_container_add(GTK_CONTAINER(tl->sw2), tl->cv2);
	gtk_table_attach(GTK_TABLE(tl->table), tl->sw1, 0, 1, 1, 2,
		GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);
	gtk_table_attach(GTK_TABLE(tl->table), tl->sw2, 1, 2, 1, 2,
		GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);
	gtk_table_attach(GTK_TABLE(tl->table), tl->ruler, 1, 2, 0, 1,
		GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 0);
	gtk_widget_show(tl->cv1);
	gtk_widget_show(tl->cv2);
	gtk_widget_show(tl->sw1);
	gtk_widget_show(tl->sw2);
	gtk_widget_show(tl->ruler);

	return tl;
}

void canvas_line(GnomeCanvasGroup *group,
	                      double x1, double y1, double x2, double y2)
{
        GnomeCanvasPoints *points;
	points = gnome_canvas_points_new(2);
	points->coords[0] = x1;
	points->coords[1] = y1;
	points->coords[2] = x2;
	points->coords[3] = y2;
	gnome_canvas_item_new(group,
		gnome_canvas_line_get_type(),
	        "points", points,
	        "fill_color", "black",
		"width_units", 1.0,
		NULL);
	gnome_canvas_points_free(points);
}


void fill_timeline(struct timeline *tl)
{
    	canvas_line(gnome_canvas_root(tl->cv2), 0, 0, 100, 20);
    	canvas_line(gnome_canvas_root(tl->cv2), 50, 0, -10, 20);
}


/* Dummy main for testing. */
int main(int argc, char **argv)
{
	GtkWidget *w;
	struct timeline *tl;

	gnome_init("timeline test", "no version", argc, argv);

	w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	tl = timeline_new();
	gtk_container_add(GTK_CONTAINER(w), tl->table);
	gtk_widget_show(tl->table);
	gtk_widget_show(w);

	fill_timeline(tl);

	gtk_main();
	return 0;
}
