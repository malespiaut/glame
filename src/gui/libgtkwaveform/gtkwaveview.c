/* gtkwaveview.c: Gtk+ widget for displaying data
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

/* Amplitude zoom, selection, and marker code by
 * Ben De Rydt <ben.de.rydt@pandora.be>
 *
 * Documentation for the selection resize algorithm is in
 * docs/drag_algorithm.txt
 */

#include <gtk/gtk.h>
#include "gtkwaveview.h"
#include "gtkeditablewavebuffer.h"
#include "grange.h"


static void gtk_wave_view_class_init    (GtkWaveViewClass *klass);
static void gtk_wave_view_init          (GtkWaveView      *waveview);


static GtkVBoxClass *parent_class;


/*
   Info on units used in this widget:

   There are three major units:
    * Frame units (frame_pos)
        Used by select_left, select_right and for keeping track of
        precise data locations.  Also used by the scrollbar.

    * Pel x units relative to left side of window (win_pel_pos)
        Given to us by Gdk for motion events.  Used by us to tell
        Gdk where to draw stuff.

    * Pel x units relative to start of data stream (ext_pel_pos)
        Used by the cache to keep track of recently calculated
        data.

   Mathematical representation of how to convert between unit forms:
     frame units =
       pel x units relative to start of data stream * zoom

   Where zoom >= 1.0.  zoom is in units of samples per pixel

   There is a need to convert back and forth between the units,
   however, you must be careful to round exactly the same way,
   regardless of which direction you want to convert.  Rounding
   errors occur when converting floating point #s to integers.
   That happens a lot in this code.   Or when performing integer
   divison (not used much here).  If you don't watch how you
   round, subtle one-off problems will develop.  Be careful!
*/


GtkType
gtk_wave_view_get_type (void)
{
  static GtkType waveview_type = 0;
  
  if (!waveview_type)
    {
      static const GtkTypeInfo waveview_info =
      {
        "GtkWaveView",
        sizeof (GtkWaveView),
        sizeof (GtkWaveViewClass),
        (GtkClassInitFunc) gtk_wave_view_class_init,
        (GtkObjectInitFunc) gtk_wave_view_init,
        /* reserved_1 */ NULL,
        /* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL,
      };
      
      waveview_type = gtk_type_unique (GTK_TYPE_VBOX, &waveview_info);
    }
  
  return waveview_type;
}


GtkWidget *
gtk_wave_view_new (void)
{
  GtkWaveView *waveview;
  
  waveview = gtk_type_new (GTK_TYPE_WAVEFORM);
  
  return GTK_WIDGET (waveview);
}


static void
gtk_wave_view_real_destroy (GtkObject *obj)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (obj);

  if (waveview->wavebuffer != NULL)
    gtk_wave_view_set_buffer (waveview, NULL);

  gdk_cursor_destroy (waveview->drag_cursor);
  gdk_cursor_destroy (waveview->normal_cursor);

  if (waveview->marker_gc != NULL)
    gdk_gc_unref (waveview->marker_gc);

  GTK_OBJECT_CLASS (parent_class)->destroy (obj); 
}


static void
gtk_wave_view_class_init (GtkWaveViewClass *klass)
{
  GtkObjectClass *object_class = GTK_OBJECT_CLASS (klass);

  parent_class = gtk_type_class (gtk_vbox_get_type ());
  object_class->destroy = gtk_wave_view_real_destroy;
}


/* Unit conversion functions */

static guint32
calc_scrn_y (gint32 top, gint32 height, gint16 mag, gdouble ampl_zoom)
{
  gint32 pos;

  pos = (guint32) (top + (height * 0.5) + \
                  (height * mag * ampl_zoom) / 65535.0);

  pos = MIN (MAX (pos, top), top + height - 1);

  return pos;
}


static gint32
calc_win_pel_pos (GtkWaveView *waveview, gint32 frame_pos)
{
  return ((gint32) (frame_pos / waveview->zoom)) - ((gint32) (GTK_ADJUSTMENT (waveview->adjust)->value / waveview->zoom));
}


static gint32
calc_ext_pel_pos (GtkWaveView *waveview, gint32 frame_pos)
{
  return (gint32) (frame_pos / waveview->zoom);
}


#if 0
static gint32
calc_win_pel_ext (GtkWaveView *waveview, gint32 ext_pel_pos)
{
  return ext_pel_pos - ((gint32)(GTK_ADJUSTMENT (waveview->adjust)->value / waveview->zoom));
}

static gint32
calc_ext_pel_win (GtkWaveView *waveview, gint32 win_pel_pos)
{
  return win_pel_pos + ((gint32)(GTK_ADJUSTMENT (waveview->adjust)->value / waveview->zoom));
}
#endif


static gint32
calc_frame_pos_ext (GtkWaveView *waveview, gint32 ext_pel_pos)
{
  return (gint32) (ext_pel_pos * waveview->zoom);
}


static gint32
calc_frame_pos_win (GtkWaveView *waveview, gint32 win_pel_pos)
{
  return (((gint32) GTK_ADJUSTMENT (waveview->adjust)->value / waveview->zoom) + win_pel_pos) * waveview->zoom;
}


/* Cache functions */

static void gtk_wave_view_cache_free (GtkWaveView *waveview);
static void gtk_wave_view_cache_create (GtkWaveView *waveview);
static void gtk_wave_view_cache_add (GtkWaveView *waveview, guint32 x, gint16 *min, gint16 *max);
static gint gtk_wave_view_cache_paint (GtkWaveView *waveview, gint32 offset, gint32 x);
static void gtk_wave_view_cache_invalidate (GtkWaveView *waveview);
static void gtk_wave_view_cache_invalidate_range (GtkWaveView *waveview, gint32 start, gint32 length);


static void
gtk_wave_view_cache_free (GtkWaveView *waveview)
{
  gint32 i;

  for (i = 0; i < waveview->n_channels; i++)
    {
      g_free (waveview->channels [i].cache);
      waveview->channels [i].cache = NULL;
    }

  g_free (waveview->cache_tag);
  waveview->cache_tag = NULL;
}


static void
gtk_wave_view_cache_create (GtkWaveView *waveview)
{
  gint32 i;

  if (waveview->wavebuffer == NULL)
    return;

  waveview->cache_tag = g_new (gint32, waveview->cache_size);

  for (i = 0; i < waveview->n_channels; i++)
    waveview->channels[i].cache = g_new (GtkWaveViewCacheEntry, waveview->cache_size);

  gtk_wave_view_cache_invalidate (waveview);
}


static void
gtk_wave_view_cache_add (GtkWaveView *waveview, guint32 x, gint16 *min, gint16 *max)
{
  guint32 i, size, offset;
  GtkWaveViewCacheEntry *e;

  size = waveview->cache_size;
  offset = x % size;

  /* Mmmm... direct mapped cache. */
  waveview->cache_tag [offset] = x;
  
  for (i = 0; i < waveview->n_channels; i++)
    {
      e = &waveview->channels [i].cache [offset];
      e->min = min [i];
      e->max = max [i];

      min [i] = max [i] = 0;
    }
}


static gint
gtk_wave_view_cache_paint (GtkWaveView *waveview, gint32 offset, gint32 x)
{
  GdkGC *gc;
  guint32 size, cache_offset;

  if (waveview->wavebuffer == NULL)
    return 0;

  size = waveview->cache_size;
  cache_offset = (offset + x) % size;

  /* Is this x coordinate cached? */
  if (waveview->cache_tag [cache_offset] == offset + x)
    {
      guint32 i;
      gint l_pos, r_pos;

      l_pos = calc_win_pel_pos (waveview, waveview->select_left);
      r_pos = calc_win_pel_pos (waveview, waveview->select_right);

      for (i = 0; i < waveview->n_channels; i++)
        {
          GtkWaveViewCacheEntry *e;
          gint32        min, max;
          guint32       top, height;

          /* Retrieve cache entry for this channel. */
          e = &waveview->channels [i].cache [cache_offset];
          top = waveview->channels [i].top;
          height = waveview->channels [i].height;

          /* Calc y coordinates. */
          min = calc_scrn_y (top, height, e->min, waveview->ampl_zoom);
          max = calc_scrn_y (top, height, e->max, waveview->ampl_zoom);

          /* Draw black line if not selected, white otherwise. */
          if (((1 << i) & waveview->select_channels) && x >= l_pos && x <= r_pos && waveview->select_left <= waveview->select_right)
            gc = waveview->area->style->fg_gc [GTK_STATE_SELECTED];
          else
            gc = waveview->area->style->fg_gc [GTK_STATE_ACTIVE];

          gdk_draw_line (waveview->area->window, gc, x, min, x, max);
        }

      return 1;
    }

  return 0;
}


static void
gtk_wave_view_cache_invalidate (GtkWaveView *waveview)
{
  gint32 i;

  if (waveview->wavebuffer == NULL)
    return;

  for (i = 0; i < waveview->cache_size; i++)
    waveview->cache_tag [i] = -1;
}


static void
gtk_wave_view_cache_invalidate_range (GtkWaveView *waveview, gint32 start, gint32 length)
{
  gint32 i;

  if (waveview->wavebuffer == NULL)
    return;

  for (i = 0; i < waveview->cache_size; i++)
    if (waveview->cache_tag [i] >= start &&
        waveview->cache_tag [i] <= start + length)
      waveview->cache_tag [i] = -1;
}


/* Update ruler and scrollbar units. */
static void
gtk_wave_view_update_units (GtkWaveView *waveview)
{
  GtkAdjustment *adj;
  gdouble length;
  guint32 width;
  gdouble mult;
  gint32 j;

  adj = GTK_ADJUSTMENT (waveview->adjust);

  if (waveview->wavebuffer == NULL)
    {
      adj->lower = 0.0;
      adj->upper = 0.0;
      adj->value = 0.0;
      gtk_adjustment_changed (adj);
      gtk_ruler_set_range (GTK_RULER (waveview->hruler), 0.0, 0.0, 0.0, 20.0);
      return;
    }

  length = (gdouble) gtk_wave_buffer_get_length (waveview->wavebuffer);
  width = GTK_WIDGET (waveview)->allocation.width;

  /* Set scrollbar in pixel units. */
  adj->lower = 0.0;
  adj->upper = length;
  adj->step_increment = waveview->zoom * 32.0;
  adj->page_size = width * waveview->zoom;
  adj->page_increment = adj->page_size;

  if (adj->value > adj->upper - adj->page_size)
    {
      adj->value = adj->upper - adj->page_size;
      if (adj->value < 0.0)
        adj->value = 0.0;
    }

  gtk_adjustment_changed (adj);

  /* Scale our values down a few decimal places to keep the ruler happy. */
  if (waveview->zoom <= 1000.0)
    mult = 0.01;
  else
    mult = 0.00001;

  /* This is a bit convoluted, but I want to round the units properly.
     First I convert the sample position to a pel position, and then
     convert it back into a sample position. */

  j = -calc_win_pel_pos (waveview, 0);
  gtk_ruler_set_range (GTK_RULER (waveview->hruler),
                       calc_frame_pos_ext (waveview, j) * mult,
                       calc_frame_pos_ext (waveview, j + width) * mult,
                       0.0, 100.0);
}


/* The crusty stuff to redraw all the channels of a waveview. */
static void
gtk_wave_view_redraw_wave (GtkWaveView *waveview)
{
  GWavefileType datatype;
  guint32   n_samples, n_channels;
  guint32   i, j, width, start_x, offset;
  gint16    max_val, min_val;

  if (waveview->wavebuffer == NULL)
    return;

  if (waveview->invalidate)
    {
      waveview->invalidate = FALSE;   
      gtk_wave_view_cache_invalidate_range (waveview,
        calc_ext_pel_pos (waveview, waveview->invalidate_start),
        calc_ext_pel_pos (waveview, waveview->invalidate_stop));
    }

  min_val = -1;

  offset = -calc_win_pel_pos (waveview, 0);
  waveview->drawn_offset = offset;
  width = waveview->expose_area.width;
  start_x = waveview->expose_area.x;

  /* First, paint all cached x coords. */
  /* Keep a range min_val -> max_val that contains all uncached x coords. */
  for (i = start_x; i < start_x + width; i++)
    if (gtk_wave_view_cache_paint (waveview, offset, i) == 0) {
      if (min_val == -1)
        min_val = max_val = i;
      else
        max_val = i;
    }

  if (min_val != -1)
    {
      gint16    max [waveview->n_channels], min [waveview->n_channels];
      guint32   sample_offset, last_sample_offset, pos, size;
      guint32   accum, count, delta;
      guint32   pos16;
      gpointer  data;
      gint16   *data16;

      /* Now here comes the fun part, if some x coordinates weren't cached, */
      /* we gotta do a lot of calculations to figure out their values.      */
      n_channels = waveview->n_channels;

      datatype = gtk_wave_buffer_get_datatype (waveview->wavebuffer);

      /* Allocate some temp space. */
      data = g_malloc (g_wavefile_type_width (datatype) * n_channels * 2048);

      /* Allocate more temp space if the data source datatype is not s16.
         We then have to convert it to s16 data for cache use. */
      if (datatype != G_WAVEFILE_TYPE_S16)
        data16 = g_new (gint16, n_channels * 2048);
      else
        data16 = (gint16*) data;

      /* Approximate sample # range we need to recalc for the cache. */
      sample_offset = calc_frame_pos_win (waveview, min_val);
      last_sample_offset = calc_frame_pos_win (waveview, max_val + 1);

      n_samples = gtk_wave_buffer_get_length (waveview->wavebuffer);

      /* Perform clipping. */
      if (sample_offset >= n_samples)
        return;
      if (last_sample_offset >= n_samples)
        last_sample_offset = n_samples - 1;

      for (j = 0; j < waveview->n_channels; j++)
        min[j] = max[j] = 0;

      pos = sample_offset;
      accum = 1073741824U; /* 2147483648 / 2, round to nearest unit */
      count = 0;
      delta = (guint32) (2147483648.0 / waveview->zoom);
      while (pos <= last_sample_offset)
        {
          size = MIN (2048, last_sample_offset - pos + 1);

          /* Read data chunk. */
          gtk_wave_buffer_get_samples (waveview->wavebuffer, pos, size, 0xffffffff, data);

          /* Convert data to s16 if it's not already in that format. */
          if (datatype != G_WAVEFILE_TYPE_S16)
            g_wavefile_type_convert (waveview->n_channels, size, G_WAVEFILE_TYPE_S16, data16, datatype, data);

          pos16 = 0;
          for (i = 0; i < size; i++)
            {
              /* For each sample of a track, keep min and max values. */
              for (j = 0; j < n_channels; j++)
                {
                  gint16 val = data16[pos16++];

                  if (val < min[j]) min[j] = val;
                  if (val > max[j]) max[j] = val;
                }

              /* Do Bresenham's algorithm */
              accum += delta;

              if (accum >= 2147483648U)
                {
                  accum -= 2147483648U;

                  gtk_wave_view_cache_add (waveview, offset + min_val + count, min, max);
                  gtk_wave_view_cache_paint (waveview, offset, min_val + count);

                  count++;
                }
            }

          /* Increment position in data source. */
          pos += size;
        }

      if (data16 != (void*) data)
        g_free (data16);

      g_free (data);
    }
}


/* Fill rectangle (x1, y) -> (x2, y + height - 1), clip if necessary. */
static void
fill_rect (GdkWindow *win, GdkGC *gc, gint32 win_width, gint32 x1, gint32 x2, gint32 y, gint32 height)
{
  /* Clipping and stuff.. */
  if (x1 > x2 || x2 < 0 || x1 >= win_width) return;

  if (x1 < 0) x1 = 0;
  if (x2 >= win_width) x2 = win_width - 1;

  gdk_draw_rectangle (win, gc, TRUE, x1, y, x2 - x1 + 1, height);
}

static void
draw_marker_from_frame_pos (GtkWaveView *waveview, gint32 frame)
{
  gint32 win_pos;
  
  win_pos = calc_win_pel_pos (waveview, frame);
  if (win_pos >= 0 && win_pos < waveview->area->allocation.width)
    {
      gint32 i;
      for (i = 0; i < waveview->n_channels; i++)
        {
          gint32 top, height;

          top = waveview->channels[i].top;
          height = waveview->channels[i].height;

          gdk_draw_line (waveview->area->window, waveview->marker_gc,
                         win_pos, top, win_pos, top + height - 1);
        }
    }
}


static void
gtk_wave_view_draw_marker (GtkWaveView *waveview)
{
  if (waveview->marker < 0)
    return;
  if (!GTK_WIDGET_REALIZED (waveview->area))
    return;
  draw_marker_from_frame_pos (waveview, waveview->marker);
}


/* Redraw the region between two window x coordinates inclusive. */
static void
gtk_wave_view_redraw_area (GtkWaveView *waveview, gint32 x1, gint32 x2)
{
  /* Reverse x1 and x2, if backwards. */
  if (x1 > x2)
    {
      guint32 t;
      t = x1; x1 = x2; x2 = t;
    }

  /* Perform clipping. */
  if (x2 < 0) return;
  if (x1 >= waveview->area->allocation.width) return;

  if (x1 < 0)
    x1 = 0;
  if (x2 >= waveview->area->allocation.width)
    x2 = waveview->area->allocation.width - 1;

  /* Query a redraw of the area. */
  gtk_widget_queue_draw_area (GTK_WIDGET (waveview->area),
                              x1, 0, x2 - x1 + 1, waveview->area->allocation.height);
}


/* Redraw the range between two *sample* #s, inclusive. */
static void
gtk_wave_view_redraw_sample_area (GtkWaveView *waveview, gint32 pos1, gint32 pos2)
{
  gint32 x1, x2;

  x1 = calc_win_pel_pos (waveview, pos1);
  x2 = calc_win_pel_pos (waveview, pos2);

  gtk_wave_view_redraw_area (waveview, x1, x2);
}


static void
on_area_realize (GtkWidget *widget, gpointer userdata)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (userdata);
  if (waveview->marker_gc == NULL)
    {
      waveview->marker_gc = gdk_gc_new (waveview->area->window);
      gdk_gc_copy (waveview->marker_gc, waveview->area->style->white_gc);
      gdk_gc_set_function (waveview->marker_gc, GDK_XOR);
    }
}


/* Paint selection areas then call a function to paint the wave data. */
static void
on_area_expose_event (GtkWidget *widget, GdkEventExpose *event, gpointer userdata)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (userdata);
  static gint16 horiz_lines [] = { -24576, -16384, -8192, 0, 8191, 16383, 24575, -999 };
  GdkGC *sel_bg_gc, *unsel_bg_gc;

  /* Accumulate expose events. */

  if (waveview->expose_count == 0)
    waveview->expose_area = event->area;
  else
    {
      GdkRectangle temp;
      gdk_rectangle_union (&waveview->expose_area, &event->area, &temp);
      waveview->expose_area = temp;
    }

  waveview->expose_count++;

  if (event->count > 0)
    return;

  /* Done accumulating sequential expose events, now process them. */

  sel_bg_gc = widget->style->bg_gc [GTK_STATE_SELECTED];
  unsel_bg_gc = widget->style->white_gc;

  if (waveview->wavebuffer != NULL)
    {
      gint32 i, j, k;
      guint32 n_channels;
      guint32 top, height, width;
      GdkWindow *window;

      /* Set clipping to expose region. */
      gdk_gc_set_clip_rectangle (widget->style->fg_gc [GTK_STATE_NORMAL], &waveview->expose_area);
      gdk_gc_set_clip_rectangle (widget->style->fg_gc [GTK_STATE_SELECTED], &waveview->expose_area);
      gdk_gc_set_clip_rectangle (widget->style->white_gc, &waveview->expose_area);
      gdk_gc_set_clip_rectangle (widget->style->bg_gc [GTK_STATE_SELECTED], &waveview->expose_area);
      gdk_gc_set_clip_rectangle (widget->style->dark_gc [GTK_STATE_NORMAL], &waveview->expose_area);
      gdk_gc_set_clip_rectangle (waveview->marker_gc, &waveview->expose_area);

      n_channels = gtk_wave_buffer_get_num_channels (waveview->wavebuffer);
      width = widget->allocation.width;
      window = widget->window;

      /* Draw seperator bars. */
      for (i = 1; i < n_channels; i++)
        {
          j = waveview->channels[i].top;
          gdk_draw_line (window, waveview->area->style->mid_gc [GTK_STATE_NORMAL], 0, j - 3, width, j - 3);
          gdk_draw_line (window, waveview->area->style->dark_gc [GTK_STATE_NORMAL], 0, j - 2, width, j - 2);
          gdk_draw_line (window, waveview->area->style->black_gc, 0, j - 1, width, j - 1);
        }

      for (i = 0; i < n_channels; i++)
        {
          top = waveview->channels [i].top;
          height = waveview->channels [i].height;

          /* Draw filled regions. */
          if (waveview->select_right < waveview->select_left ||
              !(waveview->select_channels & (1 << i)))
            {
              fill_rect (window, unsel_bg_gc, width, 0, width - 1, top, height);
            }
          else
            {
              gint32 l_pos, r_pos;

              l_pos = calc_win_pel_pos (waveview, waveview->select_left);
              r_pos = calc_win_pel_pos (waveview, waveview->select_right);

              fill_rect (window, unsel_bg_gc, width, 0, l_pos - 1, top, height);
              fill_rect (window, sel_bg_gc, width, l_pos, r_pos, top, height);
              fill_rect (window, unsel_bg_gc, width, r_pos + 1, width - 1, top, height);
            }

          /* Draw horizontal lines. */
          for (j = 0; horiz_lines [j] != -999; j++)
            {
              k = calc_scrn_y (top, height, horiz_lines [j], 
                               waveview->ampl_zoom);
              gdk_draw_line (window, waveview->area->style->dark_gc [GTK_STATE_NORMAL], 0, k, width, k);
            }

          /* Basically, this is just a fast, convoluted version of:
             j = (x * 50) + offset; where x is an integer selected such that
             j is a modulus of 50 in the range of: [0,50) */

          /* calc_win_pel_pos returns <= 0 */
          j = 49 - ((49 - calc_win_pel_pos (waveview, 0)) % 50);

          /* Draw vertical lines. */
          for (; j < width; j += 50)
            gdk_draw_line (window, waveview->area->style->dark_gc [GTK_STATE_NORMAL], j, top, j, top + height - 1);
        }

      /* Paint all channels of the waveview. */
      gtk_wave_view_redraw_wave (waveview);
      gtk_wave_view_draw_marker (waveview);

      /* Reset clipping. */
      gdk_gc_set_clip_mask (widget->style->fg_gc [GTK_STATE_NORMAL], NULL);
      gdk_gc_set_clip_mask (widget->style->fg_gc [GTK_STATE_SELECTED], NULL);
      gdk_gc_set_clip_mask (widget->style->white_gc, NULL);
      gdk_gc_set_clip_mask (widget->style->bg_gc [GTK_STATE_SELECTED], NULL);
      gdk_gc_set_clip_mask (widget->style->dark_gc [GTK_STATE_NORMAL], NULL);
      gdk_gc_set_clip_mask (waveview->marker_gc, NULL);
    }

  waveview->expose_count = 0;
}


/* Calculate the top and bottom window y coordinates for each channel. */
static void
gtk_wave_view_calc_channel_locs (GtkWaveView *waveview)
{
  guint32 i, n_channels, height;
  guint32 top;
  guint32 accum;

  if (waveview->wavebuffer == NULL)
    return;

  n_channels = gtk_wave_buffer_get_num_channels (GTK_WAVE_BUFFER (waveview->wavebuffer));
  height = waveview->area->allocation.height - (n_channels - 1) * 3;

  top = 0;
  accum = 0;
  for (i = 0; i < n_channels; i++)
    {
      waveview->channels[i].top = top;
      accum += height;
      waveview->channels[i].height = accum / n_channels;
      accum %= n_channels;

      top += waveview->channels[i].height + 3;
    }
}


/* Update ruler and repaint screen on resize event. */
static void
gtk_wave_view_resize_event (GtkWidget *widget, gpointer data)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (widget);

  gtk_wave_view_calc_channel_locs (waveview);
  gtk_wave_view_update_units (waveview);
}


static void
gtk_wave_view_scroll (GtkWidget *widget, gpointer data)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (data);
  guint32 offset, width, height;
  guint32 shift;

  offset = -calc_win_pel_pos (waveview, 0);

  width = waveview->area->allocation.width;
  height = waveview->area->allocation.height;

  gtk_wave_view_update_units (waveview);

  if (!GTK_WIDGET_REALIZED (waveview->area))
    return;

  if (offset > waveview->drawn_offset)
    {
      /* Find out how many pixels we moved. */
      shift = offset - waveview->drawn_offset;

      /* Copy and redraw as needed. */
      if (shift >= width)
        gtk_widget_queue_draw (GTK_WIDGET (waveview->area));
      else
        {
          gdk_window_copy_area (waveview->area->window, waveview->area->style->bg_gc [GTK_STATE_NORMAL], 0, 0, waveview->area->window, shift, 0, width - shift, height);
          gtk_wave_view_redraw_area (waveview, width - 1, width - shift);
        }
    }
  else
    {
      /* Find out how many pixels we moved. */
      shift = waveview->drawn_offset - offset;

      /* Copy and redraw as needed. */
      if (shift >= width)
        gtk_widget_queue_draw (GTK_WIDGET (waveview->area));
      else
        {
          gdk_window_copy_area (waveview->area->window, waveview->area->style->bg_gc [GTK_STATE_NORMAL], shift, 0, waveview->area->window, 0, 0, width - shift, height);
          gtk_wave_view_redraw_area (waveview, 0, shift);
        }
    }
}

/* The meaning of the different flags. */
#define FLAG_DRAGGING_MARKER    (1)
#define FLAG_DRAGGING_SELECTION (1 << 1)
#define FLAG_DID_MOVE           (1 << 2)
#define FLAG_CURSOR_SET         (1 << 3)

#define FLAGS_CLEAR(flags)        do { (flags) = 0; } while(0)
#define FLAGS_IS_CLEAR(flags)     ( (flags) == 0)
#define FLAG_SET(flags, flag)     do { (flags) |= (flag); } while(0)
#define FLAG_UNSET(flags, flag)   do { (flags) &= (~(flag)); } while(0)
#define FLAG_IS_SET(flags, flag)  ((flags) & (flag))

/* Given an 2 frame positions, update selection and repaint as needed. 
   Selection is between x1 and x2, but x1 is not necessary <= x2.
 */
static void
gtk_wave_view_update_selection (GtkWaveView *waveview, gint32 x1, gint32 x2)
{
  gint32 len, t, t2;

  len = gtk_wave_buffer_get_length (waveview->wavebuffer);
  /* Cannot do a selection if empty. */
  if (len == 0)
    return;

  /* sanity checking: before, because both params can be out of range */
  if (x1 >= len) x1 = len - 1;
  if (x2 >= len) x2 = len - 1;
  if (x1 < 0) x1 = 0;
  if (x2 < 0) x2 = 0;
  if (x2 < x1)
    {
      t = x1;
      x1 = x2;
      x2 = t;
    }

  /* new selection doesn't overlap with old selection */
  if ((x1 < waveview->select_left && x2 < waveview->select_left) ||
      (x1 > waveview->select_right && x2 > waveview->select_right))
    {
      t = waveview->select_left;
      t2 = waveview->select_right;
      waveview->select_left = x1;
      waveview->select_right = x2;
      gtk_wave_view_redraw_sample_area (waveview, t, t2);
      gtk_wave_view_redraw_sample_area (waveview, x1, x2);
      return;
    }

  /* new_selection is overlapping with old_selection */
  if (x1 < waveview->select_left)
    {
      t = waveview->select_left;
      waveview->select_left = x1;
      gtk_wave_view_redraw_sample_area (waveview, x1, t - 1);
    }
  if (x1 > waveview->select_left)
    {
      t = waveview->select_left;
      waveview->select_left = x1;
      gtk_wave_view_redraw_sample_area (waveview, t, x1 - 1);
    }
  if (x2 < waveview->select_right)
    {
      t = waveview->select_right;
      waveview->select_right = x2;
      gtk_wave_view_redraw_sample_area(waveview, x2 + 1, t);
    }
  if (x2 > waveview->select_right)
    {
      t = waveview->select_right;
      waveview->select_right = x2;
      gtk_wave_view_redraw_sample_area(waveview, t + 1, x2);
    }
}


#define NEAR_SENSIVITY   3


static int
is_near_marker (GtkWaveView *waveview, gint32 x)
{
  if (waveview->marker < 0) 
    return 0;
  else
    {
      gint32 win;
      win = calc_win_pel_pos(waveview, waveview->marker);
      return  (x >= win - NEAR_SENSIVITY) && 
              (x <= win + NEAR_SENSIVITY);
    }
}


static int
is_near_left_selection (GtkWaveView *waveview, gint32 x)
{
  if (waveview->select_right < waveview->select_left) 
    return 0;
  else
    {
      gint32 win;
      win = calc_win_pel_pos(waveview, waveview->select_left);
      return  (x >= win - NEAR_SENSIVITY) && 
              (x <= win + NEAR_SENSIVITY);
    }
}


static int
is_near_right_selection (GtkWaveView *waveview, gint32 x)
{
  if (waveview->select_right < waveview->select_left) 
    return 0;
  else
    {
      gint32 win;
      win = calc_win_pel_pos (waveview, waveview->select_right);
      return  (x >= win - NEAR_SENSIVITY) && 
              (x <= win + NEAR_SENSIVITY);
    }
}


static void
area_set_drag_cursor (GtkWaveView *waveview)
{
  if (!FLAG_IS_SET (waveview->drag_flags, FLAG_CURSOR_SET))
    {
      gdk_window_set_cursor (waveview->area->window, waveview->drag_cursor);
      FLAG_SET (waveview->drag_flags, FLAG_CURSOR_SET);
    }
}


static void
area_restore_normal_cursor (GtkWaveView *waveview)
{
  if (FLAG_IS_SET (waveview->drag_flags, FLAG_CURSOR_SET))
    {
      gdk_window_set_cursor (waveview->area->window, waveview->normal_cursor);
      FLAG_UNSET (waveview->drag_flags, FLAG_CURSOR_SET);
    }
}

 
static gint
gtk_wave_view_button_press_event (GtkWidget *widget,
                                  GdkEventButton *event,
                                  gpointer userdata)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (userdata);
  gint32 frame;

  if (waveview->wavebuffer == NULL)
    return TRUE;

  if (event->button != 1)
    return FALSE;

  gtk_grab_add (widget);

  frame = calc_frame_pos_win (waveview, event->x);
  FLAGS_CLEAR (waveview->drag_flags);
  if (is_near_marker (waveview, event->x) &&
      !FLAG_IS_SET (event->state, GDK_CONTROL_MASK))
    {
      FLAG_SET (waveview->drag_flags, FLAG_DRAGGING_MARKER);
    }
  else if (is_near_right_selection (waveview, event->x))
    {
      FLAG_SET (waveview->drag_flags, FLAG_DRAGGING_SELECTION);
      waveview->drag_start_point = waveview->select_left;
    }
  else if (is_near_left_selection (waveview, event->x))
    {
      FLAG_SET (waveview->drag_flags, FLAG_DRAGGING_SELECTION);
      waveview->drag_start_point = waveview->select_right;
    }
  else
    {
      FLAG_SET (waveview->drag_flags, FLAG_DRAGGING_SELECTION);
      if (FLAG_IS_SET (event->state, GDK_SHIFT_MASK) &&
          waveview->marker >= 0)
        {
          waveview->drag_start_point = waveview->marker;
          FLAG_SET (waveview->drag_flags, FLAG_DID_MOVE);
          gtk_wave_view_update_selection (waveview, waveview->drag_start_point,
                                          frame);
        }
      else
        waveview->drag_start_point = frame;
    }

  return TRUE;
}


static void
gtk_wave_view_button_release_event (GtkWidget *widget,
                                    GdkEventAny *event,
                                    gpointer userdata)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (userdata);
  gint32 frame, x;

  gtk_grab_remove (widget);

  if (waveview->scroll_timeout != 0)
    {
      gtk_timeout_remove (waveview->scroll_timeout);
      waveview->scroll_timeout = 0;
    }
 
  if (event->type == GDK_BUTTON_RELEASE)
    {
      x = ((GdkEventButton *)event)->x;
    }
  else 
    {
      x = ((GdkEventCrossing *)event)->x;
    }
 
  /* dragging outside the border */
  if (x < 0 && GTK_ADJUSTMENT (waveview->adjust)->value > 0.0)
      x = 0;
  else if (x >= waveview->area->allocation.width)
      x = waveview->area->allocation.width - 1;

  frame = calc_frame_pos_win (waveview, x);

  if (FLAGS_IS_CLEAR (waveview->drag_flags))
    return;
  if (waveview->wavebuffer == NULL)
    {
      FLAGS_CLEAR (waveview->drag_flags);
      return;
    }
  if (FLAG_IS_SET (waveview->drag_flags, FLAG_DRAGGING_MARKER))
    {
      gtk_wave_view_set_marker (waveview, frame);
    }
  else if (FLAG_IS_SET (waveview->drag_flags, FLAG_DRAGGING_SELECTION))
    {
      if (FLAG_IS_SET (waveview->drag_flags, FLAG_DID_MOVE))
        {
          gtk_wave_view_update_selection (waveview, waveview->drag_start_point,
                                         frame);
        }
      else
        {
          /* actually, it was a click */
          gtk_wave_view_set_marker (waveview, frame);
        }
    }  
  area_restore_normal_cursor (waveview);
  FLAGS_CLEAR (waveview->drag_flags);
}


static gboolean
motion_update (GtkWaveView *waveview, guint32 x)
{
  gint32 frame;

  frame = calc_frame_pos_win (waveview, x);

  if (FLAG_IS_SET (waveview->drag_flags, FLAG_DRAGGING_MARKER))
    {
      gtk_wave_view_set_marker (waveview, frame);
      area_set_drag_cursor (waveview);
    }
  else if (FLAG_IS_SET (waveview->drag_flags, FLAG_DRAGGING_SELECTION))
    {
      FLAG_SET (waveview->drag_flags, FLAG_DID_MOVE);
      gtk_wave_view_update_selection (waveview, waveview->drag_start_point,
                                               frame);
      area_set_drag_cursor (waveview);
    }
  else
    {
      if (is_near_marker (waveview, x) ||
          is_near_left_selection (waveview, x) ||
          is_near_right_selection (waveview, x))

        area_set_drag_cursor (waveview);
      else
        area_restore_normal_cursor (waveview);
    }

  return TRUE;
}


static gboolean
gtk_wave_view_scroll_notify (gpointer widget)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
  GtkAdjustment *adjust = GTK_ADJUSTMENT (waveview->adjust);
  gint32 width, pos, frames;

  width = GTK_WIDGET (waveview)->allocation.width;
  pos = waveview->mouse_x;

  if (pos < 0)
    {
      /* Scroll left (if possible). */

      frames = (gint32) (adjust->value - waveview->zoom * 32.0);
      frames = MAX (adjust->lower, frames);
      if (adjust->value > 0.0)
        pos = 0;
    }
  else if (pos >= width)
    {
      /* Scroll right (if possible). */
      frames = (gint32) (adjust->value + waveview->zoom * 32.0);
      frames = MIN (adjust->upper - adjust->page_size, frames);
      pos = width - 1;
    }
  else
    return TRUE;

  if (FLAG_IS_SET (waveview->drag_flags, FLAG_DRAGGING_MARKER))
    {
      /* and now, something completely ugly ... A SHRUBBERY!
       * turn off the marker to reduce flicker while scrolling
       */
      gtk_wave_view_set_marker (waveview, -1);
      gtk_adjustment_set_value (adjust, frames);
      /* motion update will turn the marker back on */
      motion_update (waveview, pos);
    }
  else
    {
      gtk_adjustment_set_value (adjust, frames);
      motion_update (waveview, pos);
    }

  return TRUE;
}


static gint
gtk_wave_view_motion_notify_event (GtkWidget *widget,
                                   GdkEventMotion *event,
                                   gpointer userdata)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (userdata);
  gint32 x;

  if (waveview->wavebuffer == NULL)
    return FALSE;

  GTK_WIDGET_CLASS (GTK_OBJECT (waveview->hruler)->klass)->motion_notify_event (GTK_WIDGET (waveview->hruler), event);

  /* Get x coordinate on hint. */
  if (event->is_hint)
    {
      gint xint, yint;
      GdkModifierType state;

      gdk_window_get_pointer (event->window, &xint, &yint, &state);
      x = xint;
    }
  else
    x = event->x;

  motion_update (waveview, x);

  if (FLAG_IS_SET (waveview->drag_flags, FLAG_DRAGGING_MARKER) ||
      FLAG_IS_SET (waveview->drag_flags, FLAG_DRAGGING_SELECTION))
    if (waveview->scroll_timeout == 0)
      waveview->scroll_timeout =
        gtk_timeout_add (50, gtk_wave_view_scroll_notify, waveview);

  waveview->mouse_x = x;

  return TRUE;
}

 
static void
gtk_wave_view_init (GtkWaveView *waveview)
{
  waveview->hruler = gtk_hruler_new ();
  gtk_ruler_set_metric (GTK_RULER (waveview->hruler), GTK_PIXELS);
  gtk_ruler_set_range (GTK_RULER (waveview->hruler), 7, 13, 0, 20);
  gtk_box_pack_start (GTK_BOX (waveview), waveview->hruler, FALSE, FALSE, 0);
  gtk_widget_show (waveview->hruler);

  waveview->area = gtk_drawing_area_new ();
  gtk_drawing_area_size (GTK_DRAWING_AREA (waveview->area), 150, 100);
  gtk_widget_show (waveview->area);
  gtk_box_pack_start (GTK_BOX (waveview), waveview->area, TRUE, TRUE, 0);
  gtk_widget_show (waveview->area);

  waveview->adjust = gtk_adjustment_new (0.0, 0.0, 10.0, 1.0, 1.0, 5.0);
  waveview->hscroll = gtk_hscrollbar_new (GTK_ADJUSTMENT (waveview->adjust));
  gtk_box_pack_start (GTK_BOX (waveview), waveview->hscroll, FALSE, FALSE, 0);
  gtk_widget_show (waveview->hscroll);

  gtk_signal_connect (GTK_OBJECT (waveview->adjust),
                      "value_changed", gtk_wave_view_scroll,
                      GTK_OBJECT (waveview));

  gtk_signal_connect (GTK_OBJECT (waveview),
                      "size_allocate", gtk_wave_view_resize_event,
                      GTK_OBJECT (waveview));

  gtk_signal_connect (GTK_OBJECT (waveview->area),
                      "realize", on_area_realize,
                      GTK_OBJECT (waveview));

  gtk_signal_connect (GTK_OBJECT (waveview->area),
                      "expose_event", on_area_expose_event,
                      GTK_OBJECT (waveview));

  gtk_signal_connect (GTK_OBJECT (waveview->area),
                      "button_press_event", GTK_SIGNAL_FUNC (gtk_wave_view_button_press_event),
                      GTK_OBJECT (waveview));

  gtk_signal_connect (GTK_OBJECT (waveview->area),
                      "button_release_event", gtk_wave_view_button_release_event,
                      GTK_OBJECT (waveview));

  gtk_signal_connect (GTK_OBJECT (waveview->area),
                      "motion_notify_event", GTK_SIGNAL_FUNC (gtk_wave_view_motion_notify_event),
                      GTK_OBJECT (waveview));

  gtk_widget_set_events (waveview->area, GDK_EXPOSURE_MASK |
                                         GDK_POINTER_MOTION_MASK |
                                         GDK_BUTTON_PRESS_MASK |
                                         GDK_BUTTON_RELEASE_MASK |
                                         GDK_BUTTON_MOTION_MASK |
                                         GDK_POINTER_MOTION_HINT_MASK);

  waveview->wavebuffer = NULL;

  waveview->select_left = 0;
  waveview->select_right = -1;
  waveview->select_channels = 0xffffffff;

  waveview->drawn_offset = 0;

  waveview->channels = NULL;
  waveview->n_channels = 0;
  waveview->cache_tag = NULL;
  waveview->invalidate = FALSE;

  waveview->zoom = 1.0;
  waveview->cache_size = 2048;
  waveview->ampl_zoom = 1.0;

  waveview->marker = -1;
  waveview->marker_scroll_stop = 0.0;
  waveview->marker_scroll_start = 0.0;

  waveview->drag_flags = 0;
  waveview->drag_start_point = -1;
  waveview->drag_cursor = gdk_cursor_new (GDK_SB_H_DOUBLE_ARROW);
  waveview->normal_cursor = gdk_cursor_new (GDK_LEFT_PTR);

  waveview->mouse_x = 0;
  waveview->scroll_timeout = 0;

  /* we can't initialize the gc here because the window's not realized yet */
  waveview->marker_gc = NULL;
}


void
gtk_wave_view_set_amplitude_zoom (GtkWaveView *waveview, gdouble amplzoom)
{
  if (amplzoom < 1.0)
    amplzoom = 1.0;

  if (waveview->ampl_zoom != amplzoom)
    {
      waveview->ampl_zoom = amplzoom;
      gtk_widget_queue_draw (GTK_WIDGET (waveview));
    }
}


gdouble
gtk_wave_view_get_amplitude_zoom (GtkWaveView *waveview)
{
  return waveview->ampl_zoom;
}


#if 0
/* Verify selection is within the data bounds, otherwise fix it and
   repaint as necessary. */
static void
gtk_wave_view_check_selection (GtkWaveView *waveview)
{
  gboolean modified;
  gint32   length;

  length = gtk_wave_buffer_get_length (waveview->wavebuffer);

  modified = FALSE;
  if (waveview->select_left >= length)
    {
      modified = TRUE;
      waveview->select_left = length - 1;
    }

  if (waveview->select_right >= length)
    {
      modified = TRUE;
      waveview->select_right = length - 1;
    }

  if (modified)
    gtk_widget_queue_draw (waveview->area);
}
#endif


static void
gtk_wave_view_invalidate_range (GtkWaveView *waveview, guint32 start, guint32 length)
{
  if (!waveview->invalidate)
    {
      waveview->invalidate = TRUE;
      waveview->invalidate_start = start;
      waveview->invalidate_stop = start + length - 1;
    }
  else
    {
      waveview->invalidate_start = MIN (waveview->invalidate_start, start);
      waveview->invalidate_stop = MAX (waveview->invalidate_stop, start + length - 1);
    }
}


static void
on_wave_buffer_modified (GtkWaveView *widget, GRange *range)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (widget);

  if (waveview->wavebuffer == NULL)
    return;

  gtk_wave_view_invalidate_range (waveview, range->left, range->right - range->left + 1);
  gtk_wave_view_redraw_sample_area (waveview, range->left, range->right);
}


static void
on_wave_buffer_insert_data (GtkWidget *widget, GRange *range)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
  gint32 width;

  width = range->right - range->left + 1;

  if (waveview->marker >= range->left)
    waveview->marker += width;

  if (waveview->select_right >= range->left)
    waveview->select_right += width;

  if (waveview->select_left >= range->left)
    waveview->select_left += width;
}


static void
on_wave_buffer_delete_data (GtkWidget *widget, GRange *range)
{
  GtkWaveView *waveview = GTK_WAVE_VIEW (widget);
  gint32 width;

  width = range->right - range->left + 1;

  if (waveview->marker > range->right)
    waveview->marker -= width;
  else if (waveview->marker >= range->left)
    waveview->marker = -1;

  if (waveview->select_left > range->right)
    waveview->select_left -= width;
  else if (waveview->select_right >= range->left)
    waveview->select_left = range->left;

  if (waveview->select_right > range->right)
    waveview->select_right -= width;
  else if (waveview->select_right >= range->left)
    waveview->select_right = range->left - 1;
}


GtkWaveBuffer *
gtk_wave_view_get_buffer (GtkWaveView *waveview)
{
  return waveview->wavebuffer;
}


/* Select a new data stream, ref() it, invalidate cache, and update screen. */
void
gtk_wave_view_set_buffer (GtkWaveView *waveview, GtkWaveBuffer *wavebuffer)
{
  if (waveview->wavebuffer == wavebuffer)
    return;

  gtk_wave_view_cache_free (waveview);
  g_free (waveview->channels);

  if (waveview->wavebuffer != NULL)
    {
      if (GTK_IS_EDITABLE_WAVE_BUFFER (waveview->wavebuffer))
        {
          gtk_signal_disconnect (GTK_OBJECT (waveview->wavebuffer),
                                 waveview->modified_connection);
          gtk_signal_disconnect (GTK_OBJECT (waveview->wavebuffer),
                                 waveview->insert_data_connection);
          gtk_signal_disconnect (GTK_OBJECT (waveview->wavebuffer),
                                 waveview->delete_data_connection);
        }

      gtk_object_unref (GTK_OBJECT (waveview->wavebuffer));
    }

  waveview->wavebuffer = wavebuffer;

  if (wavebuffer != NULL)
    {
      gtk_object_ref (GTK_OBJECT (waveview->wavebuffer));
      gtk_object_sink (GTK_OBJECT (waveview->wavebuffer));

      if (GTK_IS_EDITABLE_WAVE_BUFFER (waveview->wavebuffer))
        {
          waveview->modified_connection = 
            gtk_signal_connect_object (GTK_OBJECT (waveview->wavebuffer),
                                       "modified",
                                       GTK_SIGNAL_FUNC (on_wave_buffer_modified),
                                       GTK_OBJECT (waveview));

          waveview->insert_data_connection = 
            gtk_signal_connect_object (GTK_OBJECT (waveview->wavebuffer),
                                       "insert_data",
                                       GTK_SIGNAL_FUNC (on_wave_buffer_insert_data),
                                       GTK_OBJECT (waveview));

          waveview->delete_data_connection = 
            gtk_signal_connect_object (GTK_OBJECT (waveview->wavebuffer),
                                       "delete_data",
                                       GTK_SIGNAL_FUNC (on_wave_buffer_delete_data),
                                       GTK_OBJECT (waveview));
        }

      waveview->n_channels = gtk_wave_buffer_get_num_channels (waveview->wavebuffer);
      waveview->channels = g_new (GtkWaveViewChannelInfo, waveview->n_channels);

      waveview->select_left = 0;
      waveview->select_right = gtk_wave_buffer_get_length (waveview->wavebuffer) - 1;
      waveview->marker = 0;

      gtk_wave_view_cache_create (waveview);
    }
  else
    {
      waveview->channels = NULL;
      waveview->cache_tag = NULL;
      waveview->n_channels = 0;
      waveview->marker = -1;
    }

  gtk_widget_queue_draw (GTK_WAVE_VIEW (waveview)->area);
  gtk_wave_view_update_units (waveview);
  gtk_wave_view_calc_channel_locs (waveview);
}


/* return < 0 means no further action necessary
 * return > 0 means further action necessary
 */
static int
check_marker (GtkWaveView *waveview, 
              gint32 *frame, 
              gint32 *length,
              gint32 *win_pos_new_marker)
{
  gint32 win_pos_old_marker;

  if (waveview->wavebuffer == NULL)
    return -1;
  
  if (!GTK_WIDGET_REALIZED (waveview->area))
    {
      waveview->marker = *frame;
      return -1;
    }

  if (*frame < 0)
    {
      /* removal:
       * neat trick: because the marker is drawn with GDK_XOR
       * we can restore the original frame(s) by drawing it again.
       */
      if (waveview->marker >= 0)
        gtk_wave_view_draw_marker (waveview);
      waveview->marker = -1;
      return -1;
    }
  
  *length = gtk_wave_buffer_get_length (waveview->wavebuffer);
  if (*frame >= *length)
      *frame = *length;

  win_pos_old_marker = calc_win_pel_pos (waveview, waveview->marker);
  *win_pos_new_marker = calc_win_pel_pos (waveview, *frame);

  if (*win_pos_new_marker == win_pos_old_marker)
    {
      waveview->marker = *frame;
      return -1;
    }

  return 1;
}


void
gtk_wave_view_set_marker (GtkWaveView *waveview, gint32 frame)
{
  gint32 length, win_pos_new;

  if (check_marker (waveview, &frame, &length, &win_pos_new) > 0)
    {
      draw_marker_from_frame_pos (waveview, frame);
      if (waveview->marker >= 0)
        draw_marker_from_frame_pos (waveview, waveview->marker);
      waveview->marker = frame;
    }
}


void
gtk_wave_view_set_marker_scrolling_boundaries (GtkWaveView *waveview,
                                               gdouble      stop_threshold,
                                               gdouble      start_offset)
{
  /* sanity checking on data */
  if (stop_threshold < 0.0 || stop_threshold > 1.0)
    {
      /* scroll when arriving at the outermost visible frame */
      stop_threshold = 1.0;
    }
  if (start_offset < 0.0 || start_offset > 1.0)
    {
      /* set the new position at the first visible frame */
      start_offset = 0.0;
    }
  if (start_offset + stop_threshold > 1.0)
    {
      /* this will drive ya nuts! 
       * Should have provided some decent parameters!
       */
      start_offset = 1.0 - stop_threshold;
    }
  waveview->marker_scroll_stop = stop_threshold;
  waveview->marker_scroll_start = start_offset;
}

static void
scroll_and_draw_marker (GtkWaveView *waveview, 
                        gint32  frame, 
                        gint32  win_width,
                        gint32  wave_length,
                        gdouble start_offset)
{
  GtkAdjustment *adj = GTK_ADJUSTMENT (waveview->adjust);
  gint32 frame_offset, max_offset;

  max_offset = wave_length - (win_width * waveview->zoom) - 1;
  max_offset = MAX (max_offset, 0);

  frame_offset = win_width * waveview->zoom * start_offset;
  frame_offset = frame - frame_offset;
  frame_offset = MAX (frame_offset, 0);
  frame_offset = MIN (frame_offset, max_offset);

  /* this will do the scrolling for us */
  gtk_adjustment_set_value (adj, frame_offset);

  /* remove the old marker, and draw the new one */
  draw_marker_from_frame_pos (waveview, waveview->marker);
  draw_marker_from_frame_pos (waveview, frame);

  gtk_wave_view_update_units (waveview);
}


void
gtk_wave_view_set_marker_and_scroll (GtkWaveView *waveview,
                                     gint32      frame)
{
  gint32 win_pos_new, width, length;
  //static int count;

  if (check_marker (waveview, &frame, &length, &win_pos_new) < 0)
    {
      /* no action necessary */
      /* WRONG! we might still need to scroll... return; */
    }

  width = waveview->area->allocation.width;

  /* calculate the amount of scroll */
  if (win_pos_new < 0 ||
      (frame > waveview->marker && 
       win_pos_new > width * (1.0 - waveview->marker_scroll_stop))) 
    {
      /* we're moving to the right */
      scroll_and_draw_marker (waveview, frame, width, length,
                              waveview->marker_scroll_start);
    }
  else if (win_pos_new >= width ||
           (frame < waveview->marker && 
            win_pos_new < width * waveview->marker_scroll_stop)) 
    {
      /* we're moving to the left */
      scroll_and_draw_marker (waveview, frame, width, length,
                              1.0 - waveview->marker_scroll_start);
    }
  else
    {
      /* redraw the marker */
      draw_marker_from_frame_pos (waveview, waveview->marker);
      draw_marker_from_frame_pos (waveview, frame);
    }

  /* set the new marker */
  waveview->marker = frame;
}
  

gint32 gtk_wave_view_get_marker (GtkWaveView *waveview)
{
  return waveview->marker;
}


/* Return zoom factor. */
gdouble
gtk_wave_view_get_zoom (GtkWaveView *waveview)
{
  return waveview->zoom;
}


/* Set zoom.  zoom = # of samples per pixel, must be >= 1.0 */
void
gtk_wave_view_set_zoom (GtkWaveView *waveview,
                        gdouble      zoom)
{
  if (zoom < 1.0)
    zoom = 1.0;

  if (waveview->zoom != zoom)
    {
      waveview->zoom = zoom;

      gtk_wave_view_cache_invalidate (waveview);
      gtk_wave_view_update_units (waveview);
      gtk_widget_queue_draw (GTK_WIDGET (waveview));
    }
}


/* Zoom whole screen. */
void
gtk_wave_view_set_zoom_all (GtkWaveView *waveview)
{
  if (waveview->wavebuffer != NULL)
    {
      guint32 length;

      if (GTK_WIDGET_REALIZED (waveview->area))
        length = GTK_WIDGET (waveview->area)->allocation.width;
      else
        length = GTK_WIDGET (waveview->area)->requisition.width;

      gtk_wave_view_set_zoom (waveview, ((gdouble) gtk_wave_buffer_get_length (waveview->wavebuffer)) / length);
    }
}


/* Set zoom to selection (if there is one). */
void
gtk_wave_view_set_zoom_selection (GtkWaveView *waveview)
{
  if (waveview->select_left <= waveview->select_right)
    {
      gdouble zoom;
      GtkAdjustment *adj = GTK_ADJUSTMENT (waveview->adjust);

      zoom = ((gdouble) (waveview->select_right - waveview->select_left + 1)) / waveview->area->allocation.width;

      gtk_wave_view_set_zoom (waveview, zoom);

      if (adj->value != waveview->select_left)
        {
          adj->value = waveview->select_left;
          gtk_wave_view_update_units (waveview);
          gtk_widget_queue_draw (waveview->area);
        }
    }
}


/* Return selection.  Length = 0 if nothing is selected. */
void
gtk_wave_view_get_selection (GtkWaveView *waveview,
                             gint32      *start,
                             gint32      *length)
{
  *start = waveview->select_left;

  if (waveview->select_left <= waveview->select_right)
    *length = waveview->select_right - waveview->select_left + 1;
  else
    *length = 0;
}


/* Set selection and refresh screen. */
void
gtk_wave_view_set_selection (GtkWaveView *waveview,
                             gint32 start,
                             gint32 length)
{
  waveview->select_left = start;
  waveview->select_right = start + length - 1;

  gtk_widget_queue_draw (waveview->area);
}


guint32
gtk_wave_view_get_select_channels (GtkWaveView *waveview)
{
  return waveview->select_channels;
}


void
gtk_wave_view_set_select_channels (GtkWaveView *waveview,
                                   guint32      sel_mask)
{
  waveview->select_channels = sel_mask;
  gtk_widget_queue_draw (waveview->area);
}


void
gtk_wave_view_set_cache_size (GtkWaveView *waveview,
                              guint32      size)
{
  if (size <= 0)
    size = 1;

  if (waveview->cache_size != size)
    {
      gtk_wave_view_cache_free (waveview);
      waveview->cache_size = size;
      gtk_wave_view_cache_create (waveview);
      gtk_wave_view_cache_invalidate (waveview);
    }
}
