#ifndef _GTK_WAVE_VIEW_H
#define _GTK_WAVE_VIEW_H

/*
 * GtkWaveView is supposed to present a stacked view of GWaveCaches.
 * Control is done through a custom GtkAdjustment and a view mode
 * specification. The stack of waves can be modified by adding,
 * removing, raising and lowering of waves.
 */

#include <gdk/gdk.h>
#include <gtk/gtkvbox.h>
#include "gwavecache.h"


#ifdef __cplusplus
extern "C" {
#endif


#define GTK_TYPE_WAVEFORM            (gtk_wave_view_get_type ())
#define GTK_WAVE_VIEW(obj)           (GTK_CHECK_CAST ((obj), GTK_TYPE_WAVEFORM, GtkWaveView))
#define GTK_WAVE_VIEW_CLASS(klass)   (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_WAVEFORM, GtkWaveViewClass))
#define GTK_IS_WAVEFORM(obj)         (GTK_CHECK_TYPE ((obj), GTK_TYPE_WAVEFORM))
#define GTK_IS_WAVEFORM_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_WAVEFORM))


typedef struct _GtkWaveView            GtkWaveView;
typedef struct _GtkWaveViewClass       GtkWaveViewClass;


struct _GtkWaveView
{
	GtkObject parent;

	GtkAdjustment *adjustment;

	GtkDrawingArea *drawing_area;
	GdkColor *color;
};

struct _GtkWaveViewClass
{
	GtkObjectClass parent_class;
};


enum GtkWaveViewMode {
	GTK_WAVE_VIEW_MODE_AMPLITUDE,
	GTK_WAVE_VIEW_MODE_VALUE
};

enum GtkWaveViewStyle {
	GTK_WAVE_VIEW_STYLE_LINES,
	GTK_WAVE_VIEW_STYLE_DOTS,
	GTK_WAVE_VIEW_STYLE_FILLED
};


GtkType     gtk_wave_view_get_type          (void);
GtkWidget  *gtk_wave_view_new               (GWaveCache *wavecache);
GWaveCache *gtk_wave_view_get_cache         (GtkWaveView *waveview);

void gtk_wave_view_set_adjustment           (GtkWaveView *waveview,
					     GtkAdjustment *adjustment);
GtkAdjustment *gtk_wave_view_get_adjustment (GtkWaveView *waveview);

void gtk_wave_view_set_mode              (GtkWaveView *waveview,
					  GtkWaveViewMode mode);
GtkWaveViewMode gtk_wave_view_get_mode   (GtkWaveView *waveview);
void gtk_wave_view_set_style             (GtkWaveView *waveview,
					  GtkWaveViewStyle style);
GtkWaveViewStyle gtk_wave_view_get_style (GtkWaveView *waveview);
void gtk_wave_view_set_color             (GtkWaveView *waveview,
					  GdkColor *color);
GdkColor *gtk_wave_view_get_color        (GtkWaveView *waveview);


#ifdef __cplusplus
}
#endif


#endif
