/* gtkwaveview.h: Gtk+ widget for displaying data
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __GTK_WAVE_VIEW_H__
#define __GTK_WAVE_VIEW_H__


#include <gdk/gdk.h>
#include <gtk/gtkvbox.h>
#include "gtkwavebuffer.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_TYPE_WAVEFORM            (gtk_wave_view_get_type ())
#define GTK_WAVE_VIEW(obj)           (GTK_CHECK_CAST ((obj), GTK_TYPE_WAVEFORM, GtkWaveView))
#define GTK_WAVE_VIEW_CLASS(klass)   (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_WAVEFORM, GtkWaveViewClass))
#define GTK_IS_WAVEFORM(obj)         (GTK_CHECK_TYPE ((obj), GTK_TYPE_WAVEFORM))
#define GTK_IS_WAVEFORM_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_WAVEFORM))


typedef struct _GtkWaveView            GtkWaveView;
typedef struct _GtkWaveViewClass       GtkWaveViewClass;
typedef struct _GtkWaveViewChannelInfo GtkWaveViewChannelInfo;
typedef struct _GtkWaveViewCacheEntry  GtkWaveViewCacheEntry;

struct _GtkWaveViewCacheEntry
{
  gint16        min, max;
};

struct _GtkWaveViewChannelInfo
{
  guint32                top;
  guint32                height;
  GtkWaveViewCacheEntry *cache;
};

struct _GtkWaveView
{
  GtkVBox parent;

  /* Cache is direct mapped. */
  gboolean        invalidate;
  gint32          invalidate_start;
  gint32          invalidate_stop;
  gint32          cache_size;
  gint32         *cache_tag;
  GtkWaveViewChannelInfo *channels;

  /* n_channels = gtk_wave_buffer_get_num_channels () */
  guint32         n_channels;

  /* Last sample offset set by the scroll bar. */
  /* Stored in units of pels relative to data start. */
  gint32          drawn_offset;

  /* Exposure accumulation stuff. */
  gint            expose_count;
  GdkRectangle    expose_area;
  gint            drawing, destroyed;

  /* Keep pointers to some useful sub-widgets. */
  GtkObject      *adjust;
  GtkWidget      *hruler;
  GtkWidget      *hscroll;
  GtkWidget      *area;

  /* Data source object. */
  GtkWaveBuffer  *wavebuffer;
  guint           modified_connection;
  guint           insert_data_connection;
  guint           delete_data_connection;

  /* Which button is pressed down during a drag. */
  guint           drag_flags;       /* bit mask */
  gint32          drag_start_point; /* in units of samples */
  GdkCursor      *drag_cursor;
  GdkCursor      *normal_cursor;

  /* Auto scroll information. */
  gint32          mouse_x;        /* in units of window relative pixels */
  guint           scroll_timeout; /* gtk_timeout handle */

  /* Selection information. */
  guint32         select_channels; /* bit mask */
  gint32          select_left;     /* in units of samples */
  gint32          select_right;    /* in units of samples */

  /* Zoom factor, in units of samples per pixel. */
  gdouble         zoom;

  /* Amplitude divisor and zoom.
   * The apml_zoom is the amount by which the amplitude should be multiplied.
   * Default is 1.0.
   */
  gdouble         ampl_zoom;   

  /* Marker position. */
  gint32          marker;     /* < 0 means no marker. in units of samples */
  GdkGC          *marker_gc;  /* the gc to draw the marker */
  gdouble         marker_scroll_stop;
  gdouble         marker_scroll_start;

	GtkWidget *vbox1;
};

struct _GtkWaveViewClass
{
  GtkVBoxClass parent_class;
};

#define GTK_WAVE_VIEW_FLAG_RECORD 0
#define GTK_WAVE_VIEW_FLAG_MUTE 1


GtkType    gtk_wave_view_get_type               (void);
GtkWidget *gtk_wave_view_new                    (void);
GtkWaveBuffer *gtk_wave_view_get_buffer         (GtkWaveView *waveview);
void       gtk_wave_view_set_buffer             (GtkWaveView *waveview,
                                                 GtkWaveBuffer *wavebuffer);
gdouble    gtk_wave_view_get_zoom               (GtkWaveView *waveview);
void       gtk_wave_view_set_zoom               (GtkWaveView *waveview,
                                                 gdouble      zoom);
void       gtk_wave_view_set_zoom_all           (GtkWaveView *waveview);
void       gtk_wave_view_set_zoom_selection     (GtkWaveView *waveview);
void       gtk_wave_view_get_selection          (GtkWaveView *waveview,
                                                 gint32      *start,
                                                 gint32      *length);
void       gtk_wave_view_set_selection          (GtkWaveView *waveview,
                                                 gint32       start,
                                                 gint32       length);
guint32    gtk_wave_view_get_select_channels    (GtkWaveView *waveview);
void       gtk_wave_view_set_select_channels    (GtkWaveView *waveview,
                                                 guint32      sel_mask);
void       gtk_wave_view_set_cache_size         (GtkWaveView *waveview,
                                                 guint32      size);
gdouble    gtk_wave_view_get_amplitude_zoom     (GtkWaveView *waveview);
void       gtk_wave_view_set_amplitude_zoom     (GtkWaveView *waveview,
                                                 gdouble      amplzoom);
gint32     gtk_wave_view_get_marker             (GtkWaveView *waveview);
void       gtk_wave_view_set_marker             (GtkWaveView *waveview,
                                                 gint32       frame);
void       gtk_wave_view_set_marker_scrolling_boundaries
                                                (GtkWaveView *waveview,
                                                 gdouble      stop_threshold,
                                                 gdouble      start_offset);
void       gtk_wave_view_set_marker_and_scroll  (GtkWaveView *waveview,
                                                 gint32       frame);
gboolean   gtk_wave_view_get_flag(GtkWaveView *waveview, int track, int flag);
void       gtk_wave_view_set_flag(GtkWaveView *waveview, int track, int flag,
				  gboolean value);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_WAVE_VIEW_H__ */
