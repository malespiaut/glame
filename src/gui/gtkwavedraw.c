/*
 * gtkwavedraw.c
 *
 * $Id: gtkwavedraw.c,v 1.3 2000/04/11 01:22:46 navratil Exp $
 *
 * Copyright (C) 2000 Joe Navratil
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <gtk/gtk.h>
#include "gtkwavedraw.h"

static GtkWidgetClass *parent_class = NULL;

/* TODO: Add necessary enums for events (pressed, released, motion) */

/* 
 * Local forward declarations
 */
static void gtk_wave_draw_class_init(GtkWaveDrawClass *klass);
static void gtk_wave_draw_init(GtkWaveDraw *wavedraw);
static void gtk_wave_draw_realize(GtkWidget *widget);
static void gtk_wave_draw_draw(GtkWidget *widget, GdkRectangle *area);
static void gtk_wave_draw_size_request(GtkWidget *widget, GtkRequisition *req);
static gint gtk_wave_draw_configure(GtkWidget *widget, 
				    GdkEventConfigure *event);
static gint gtk_wave_draw_add_wave_backend(GtkWaveDraw *wavedraw, gfloat *data, 
					   glong n_samples, glong start,
					   gint by_ref);
static gint gtk_wave_draw_expose(GtkWidget *widget, GdkEventExpose *event);
static void gtk_wave_draw_destroy(GtkObject *object);


/**********************************************************************
 *    Function: gtk_wave_draw_get_type()
 *
 * Description: Returns a unique GtkType for the GtkWaveDraw widget.
 **********************************************************************/
GtkType
gtk_wave_draw_get_type(void)
{
  static GtkType wave_draw_type = 0;
  
  /* Generate it the first time this is called */
  if (!wave_draw_type) {
    GtkTypeInfo wave_draw_info = { "GtkWaveDraw",
				   sizeof(GtkWaveDraw),
				   sizeof(GtkWaveDrawClass),
				   (GtkClassInitFunc)  gtk_wave_draw_class_init,
				   (GtkObjectInitFunc) gtk_wave_draw_init,
				   (GtkArgSetFunc)     NULL,
				   (GtkArgGetFunc)     NULL };

    wave_draw_type = gtk_type_unique (gtk_widget_get_type(),
				      &wave_draw_info);
  }

  return wave_draw_type;
}  

/**********************************************************************
 *    Function: gtk_wave_draw_class_init()
 *
 * Description: Initializes the GtkWaveDrawClass.
 **********************************************************************/
static void
gtk_wave_draw_class_init(GtkWaveDrawClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  
  object_class = (GtkObjectClass *)klass;
  widget_class = (GtkWidgetClass *)klass;
  parent_class = gtk_type_class(gtk_widget_get_type());

  object_class->destroy = gtk_wave_draw_destroy;

  widget_class->realize = gtk_wave_draw_realize;
  widget_class->draw = gtk_wave_draw_draw;
  widget_class->size_request = gtk_wave_draw_size_request;
  widget_class->expose_event = gtk_wave_draw_expose;
  widget_class->configure_event = gtk_wave_draw_configure;
}

/**********************************************************************
 *    Function: gtk_wave_draw_init()
 *
 * Description: Initializes an instance of a GtkWaveDraw widget
 **********************************************************************/
static void
gtk_wave_draw_init(GtkWaveDraw *wavedraw)
{
  wavedraw->pixmap = NULL;
  wavedraw->wavedraw_gc = NULL;

  wavedraw->wavelist = NULL;

  wavedraw->n_waves = 0;

  wavedraw->n_samples_current = 0; 
  wavedraw->start_current = 0;
  wavedraw->n_points_per_pixel = 1;
  
  wavedraw->dirty = 1;
}

/**********************************************************************
 *    Function: gtk_wave_draw_new()
 *
 * Description: Returns a new GtkWaveDraw widget
 **********************************************************************/
GtkWidget *
gtk_wave_draw_new(void)
{
  return gtk_type_new(gtk_wave_draw_get_type());
}

/**********************************************************************
 *    Function: gtk_wave_draw_realize()
 *
 * Description: Realizes a GtkWaveDraw widget, and associates it with
 *              an X window.
 **********************************************************************/
static void
gtk_wave_draw_realize(GtkWidget *widget)
{
  GtkWaveDraw *darea;
  GdkWindowAttr attributes;
  gint attributes_mask;

  g_return_if_fail(widget != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(widget));

  darea = GTK_WAVE_DRAW(widget);

  GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED);

  /* Set the attributes we need */
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual(widget);
  attributes.colormap = gtk_widget_get_colormap(widget);
  /* The event mask is probably going to bave to change to include:
   *   pressed, released, motion */
  attributes.event_mask = gtk_widget_get_events(widget) | GDK_EXPOSURE_MASK;

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

  /* Create the window */
  widget->window = gdk_window_new(gtk_widget_get_parent_window(widget),
				  &attributes, attributes_mask);
  gdk_window_set_user_data(widget->window, darea);

  widget->style = gtk_style_attach(widget->style, widget->window);
  gtk_style_set_background(widget->style, widget->window, GTK_STATE_NORMAL);

  /* Create the initial background pixmap */
  darea->pixmap = gdk_pixmap_new(widget->window, 
				 widget->allocation.width,
				 widget->allocation.height,
				 -1);
  darea->wavedraw_gc = gdk_gc_new(darea->pixmap);

}
/**********************************************************************
 *    Function: gtk_wave_draw_zoom()
 *
 * Description: Rezooms a GtkWaveDraw widget.  Takes a starting and
 *              stopping position (in samples) as parameters.
 **********************************************************************/
void
gtk_wave_draw_zoom(GtkWaveDraw *wavedraw, glong start, glong stop)
{
  int i;

  g_return_if_fail(wavedraw != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(wavedraw));

  wavedraw->n_samples_current = stop-start;
  wavedraw->start_current = start;
  
  wavedraw->dirty = 1;
}

/**********************************************************************
 *    Function: gtk_wave_draw_add_wave()
 *
 * Description: Wrapper for gtk_wave_draw_add_wave_backend that
 *              allocates & copies (effectively caches) the data for
 *              the new wave
 **********************************************************************/
gint
gtk_wave_draw_add_wave(GtkWaveDraw *wavedraw,
		       gfloat *data,
		       glong  n_samples,
		       glong  start)
{
  gfloat *data_cache;

  data_cache = g_malloc(sizeof(gfloat) * n_samples);
  memcpy(data_cache, data, sizeof(gfloat) * n_samples);
  
  return gtk_wave_draw_add_wave_backend(wavedraw, data_cache,
					n_samples, start, 0);
}
/**********************************************************************
 *    Function: gtk_wave_draw_add_wave_by_reference()
 *
 * Description: Wrapper for gtk_wave_draw_add_wave_backend that
 *              doesn't locally cache the wave data
 **********************************************************************/
gint
gtk_wave_draw_add_wave_by_reference(GtkWaveDraw *wavedraw,
				    gfloat *data,
				    glong  n_samples,
				    glong  start)
{
  return gtk_wave_draw_add_wave_backend(wavedraw, data,
					n_samples, start, 1);
}

/**********************************************************************
 *    Function: CompareWaveIdx
 *
 * Description: Helper function for the GSList, compares two WaveData
 *              structs by their wave_idx values.
 **********************************************************************/
gint
CompareWaveIdx(gconstpointer sWaveData1,
	       gconstpointer sWaveData2)
{
  if (((WaveData *)sWaveData1)->wave_idx < 
      ((WaveData *)sWaveData2)->wave_idx)
    return -1;
  else if (((WaveData *)sWaveData1)->wave_idx >
	   ((WaveData *)sWaveData2)->wave_idx)
    return 1;
  else
    return 0;
}

/**********************************************************************
 *    Function: gtk_wave_draw_add_wave()
 *
 * Description: Adds a set of wave data to the current waves;
 *              returns a 'wave index" number for the new data set.
 **********************************************************************/
static gint
gtk_wave_draw_add_wave_backend(GtkWaveDraw *wavedraw,
			       gfloat *data,
			       glong  n_samples,
			       glong  start,
			       gint   by_ref)
{
  gint new_idx;
  WaveData *wavedata;
  GSList *node;

  g_return_if_fail(wavedraw != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(wavedraw));
  
  wavedata = (WaveData *)g_malloc(sizeof(WaveData));

  /* Loop through and find the first free number */
  for (node = wavedraw->wavelist, new_idx = 0; 
       node; 
       node = node->next, new_idx++) {
    if (WAVE_DATA_FROM_LIST(node)->wave_idx != new_idx) break;
  }

  wavedata->wave_idx = new_idx;

  /* 
   * First of all, set the data.  If it's to be cached, this function
   * should have been called via gtk_wave_draw_add_wave()
   */
  wavedata->data = data;
  wavedata->by_ref = by_ref;

  /* 
   * Secondly, create two new markers and put 'em at the start and end.
   */
  wavedata->marker = g_malloc(sizeof(glong) * 2);
  wavedata->marker[0] = start;
  wavedata->marker[1] = start + n_samples - 1;

  /* 
   * Third, let the widget know how many samples are in this wave
   */
  wavedata->n_samples = n_samples;

  /* 
   * Fourth, give this wave a default color
   */
  wavedata->color.red   = (new_idx % 3 == 1) ? 0xffff : 0;
  wavedata->color.green = (new_idx % 3 == 0) ? 0xffff : 0;
  wavedata->color.blue  = (new_idx % 3 == 2) ? 0xffff : 0;

  /*
   * Fifth and finally, let the widget know where the new wave fits
   * into the grand scheme.
   */
  wavedata->start = start;

  wavedraw->wavelist = g_slist_insert_sorted(wavedraw->wavelist,
					     wavedata,
					     CompareWaveIdx);

  wavedraw->n_waves++;
  wavedraw->dirty = 1;

  return new_idx;
}

/**********************************************************************
 *    Function: gtk_wave_draw_remove_wave()
 *
 * Description: Deletes a given wave from the current widget set.
 *              This will work quickest if the wave being removed is
 *              the most recently added wave; it will be slower under
 *              any other circumstance.
 **********************************************************************/
void
gtk_wave_draw_remove_wave(GtkWaveDraw *wavedraw,
			  gint wave_idx)
{
  WaveData *wavedata;
  GSList *node;

  g_return_if_fail(wavedraw != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(wavedraw));
  g_return_if_fail(wave_idx >= 0);

  /* Let's see if we can find it */
  for (node = wavedraw->wavelist; node; node = node->next) {
    if ((wavedata = WAVE_DATA_FROM_LIST(node))->wave_idx == wave_idx)
      break;
  }

  /* If we didn't find it, we can't do anything about removing it */
  if (node == NULL) return;

  /* Otherwise: 
   * First, free the two allocated arrays */
  if (!wavedata->by_ref) g_free(wavedata->data);
  g_free(wavedata->marker);

  /* Second, tell the list to remove this elt */
  wavedraw->wavelist = g_slist_remove(wavedraw->wavelist, wavedata);

  /* Now free the entire struct -- this could probably be handled before
   * the g_slist_remove() call, but I'd rather have the call point to 
   * valid data.  Color me paranoid. */
  g_free(wavedata);

  /* Finally, tell the widget there's one less wave, and that something's
   * changed. */
  wavedraw->n_waves--;
  wavedraw->dirty = 1;
}

/**********************************************************************
 *    Function: gtk_wave_draw_draw()
 *
 * Description: Draws the GtkWaveDraw widget.  Stupid name.
 **********************************************************************/
static void
gtk_wave_draw_draw(GtkWidget *widget,
		   GdkRectangle *area)
{
  GtkWaveDraw *wavedraw;
  GdkPixmap   *pixmap;
  GdkGC       *wavedraw_gc;
  GSList      *node;
  WaveData    *wavedata;
  gint   wave_idx;
  gint   midy;                 /* The Y midpoint, ie the axis */
  gfloat scale_x, stride_x;
  gfloat start_x, disp_x;
  glong  stop_x;
  gfloat cur_x, cur_x2;
  gfloat stride_samp, samp_offset;
  gfloat *p_val;
  glong  start;
  glong  n_samples;
  glong  stop;
  glong  start_disp, stop_disp;
  glong  start_sample, stop_sample, disp_samples;
  glong  cur_y, cur_y2;
  glong  samp;
  gint   cur_subsamp;

  /* Make sure we're drawing something we know how to draw */
  g_return_if_fail(widget != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(widget));
  g_return_if_fail(GTK_WIDGET_DRAWABLE(widget));

  /* Get the wavedraw so we can mess with it */
  wavedraw = GTK_WAVE_DRAW(widget);

  /* Set a few initial constant (more or less) values */
  /* Possible (minor) speed consideration: move everything but midy after 
   * the axis draw */
  midy = widget->allocation.height / 2;
  start_disp = wavedraw->start_current;
  stop_disp  = start_disp + wavedraw->n_samples_current - 1;
  scale_x = widget->allocation.width / (gfloat)(wavedraw->n_samples_current - 2);
  pixmap = wavedraw->pixmap;
  wavedraw_gc = wavedraw->wavedraw_gc;

  /* First and foremost: Draw the axis */
  gdk_draw_line(pixmap,
		widget->style->white_gc,
		0, midy,
		widget->allocation.width, midy);

  /* If the widget's got no waves, we're done */
  if (wavedraw->n_waves == 0)
    return;

  for (node = wavedraw->wavelist; node; node = node->next) {
    /* Get the struct and a few necessary values */
    wavedata = (WaveData *)node->data;
    start = wavedata->start;
    n_samples = wavedata->n_samples;

    /* If our current start is after the end of the display, continue */
    if (start > stop_disp)
      continue;
    
    /* If our current stop is before the beginning of the display, continue */
    if ((stop = (start + n_samples - 1)) < start_disp)
      continue;

    p_val = wavedata->data;

    /* Set the color from the widget */
    gdk_color_alloc(gdk_colormap_get_system(), &wavedata->color);
    gdk_gc_set_foreground(wavedraw->wavedraw_gc, &wavedata->color);

    start_sample = MAX(start, start_disp);
    stop_sample = MIN(stop, stop_disp);
    disp_samples = stop_sample - start_sample + 1;
    
    start_x = (start_sample - start_disp) * scale_x;
    stop_x = (glong)((stop_sample - start_disp) * scale_x);
    disp_x = stop_x - start_x;

    if (disp_samples <= disp_x * wavedraw->n_points_per_pixel) {
      /* We've got more pixels than samples, so we're going to plot 
       * every sample. */
      stride_x = disp_x / disp_samples;
      cur_x = start_x;
      cur_y = (int)(midy - *p_val*midy);
      
      for (samp = start_sample; samp < stop_sample; samp++) {
	cur_y2 = (int)(midy - (*++p_val)*midy);
	cur_x2 = cur_x + stride_x;
	gdk_draw_line(pixmap, wavedraw_gc,
		      (gint) cur_x, cur_y,
		      (gint) cur_x2, cur_y2);
	cur_y = cur_y2;
	cur_x = cur_x2;
		      
      }
    } else {
      /* We've gotta do some subpixel elimination */
      stride_samp = disp_samples / (disp_x * wavedraw->n_points_per_pixel);
      cur_x = start_x;
      samp_offset = start_sample;
      p_val = wavedata->data + (glong) samp_offset;
      cur_y = (int)(midy - (*p_val)*midy);

      for (cur_x = start_x; cur_x < stop_x; cur_x++) {
	for (cur_subsamp = 0; cur_subsamp < wavedraw->n_points_per_pixel-1;
	     cur_subsamp++) {
	  samp_offset += stride_samp;
	  p_val = wavedata->data + (glong) samp_offset;
	  cur_y2 = (int)(midy - (*p_val)*midy);
	  gdk_draw_line(pixmap,	wavedraw_gc,
			cur_x, cur_y,
			cur_x, cur_y2);
	  cur_y = cur_y2;
	} /* Subsample plotting */
	if (cur_x == stop_x-1) break;
	samp_offset += stride_samp;
	p_val = wavedata->data + (glong) samp_offset;
	cur_y2 = (int)(midy - (*p_val)*midy);
	gdk_draw_line(pixmap, wavedraw_gc,
		      cur_x, cur_y,
		      cur_x+1, cur_y2);
	cur_y = cur_y2;
      } /* For cur_x = start_x... */
      
    } /* if/else on plotting */
    
  } /* For each wave */

  wavedraw->dirty = 0;
  gdk_draw_pixmap(widget->window,
		  widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  pixmap,
		  0, 0, 0, 0,
		  widget->allocation.width,
		  widget->allocation.height);
}

/**********************************************************************
 *    Function: gtk_wave_draw_size_request()
 *
 * Description: Initial size request.  This can be modified
 **********************************************************************/
static void
gtk_wave_draw_size_request(GtkWidget *widget,
			   GtkRequisition *req)
{
  req->width  = 600;
  req->height = 200;
}

/**********************************************************************
 *    Function: gtk_wave_draw_configure()
 *
 * Description: Configure event, resized or something
 **********************************************************************/
static gint
gtk_wave_draw_configure(GtkWidget *widget, GdkEventConfigure *event)
{
  GtkWaveDraw *wavedraw;

  g_return_val_if_fail(widget != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_WAVE_DRAW(widget), FALSE);
  g_return_val_if_fail(event != NULL, FALSE);

  wavedraw = GTK_WAVE_DRAW(widget);
  if (wavedraw->pixmap) 
    gdk_pixmap_unref(wavedraw->pixmap);
  wavedraw->pixmap = gdk_pixmap_new(widget->window,
				    widget->allocation.width,
				    widget->allocation.height,
				    -1);

  wavedraw->dirty = 1;

  return TRUE;
}
/**********************************************************************
 *    Function: gtk_wave_draw_expose()
 *
 * Description: Expose event, need to repaint.
 *              NOTE: Right now, I'm redrawing the entire widget.
 *                    This can be better optimized by taking into 
 *                    account the x-values of the expose event and 
 *                    only redrawing that section of the waves.
 **********************************************************************/
static gint
gtk_wave_draw_expose(GtkWidget *widget,
		     GdkEventExpose *event)
{
  GtkWaveDraw *wavedraw;

  g_return_val_if_fail(widget != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_WAVE_DRAW(widget), FALSE);
  g_return_val_if_fail(event != NULL, FALSE);

  if (event->count > 0) {
    return FALSE;
  }

  wavedraw = GTK_WAVE_DRAW(widget);

  gdk_window_clear_area(widget->window, 0, 0,
			widget->allocation.width,
			widget->allocation.height);

  if (wavedraw->dirty)
    gtk_wave_draw_draw(widget, NULL);
  else 
    gdk_draw_pixmap(widget->window,
		    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		    wavedraw->pixmap,
		    0, 0, 0, 0,
		    widget->allocation.width,
		    widget->allocation.height);

}

/**********************************************************************
 *    Function: gtk_wave_draw_destroy()
 *
 * Description: Destroy the widget and free its memory.
 **********************************************************************/
static void
gtk_wave_draw_destroy(GtkObject *object)
{
  GtkWaveDraw *wavedraw;
  int i;

  g_return_if_fail(object != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(object));

  wavedraw = GTK_WAVE_DRAW(object);

  /* Loop through and delete the wave data */
  while (wavedraw->wavelist) {
    gtk_wave_draw_remove_wave(wavedraw, 
			      WAVE_DATA_FROM_LIST(wavedraw->wavelist)->wave_idx);
  }

  /* Destroy the parent */
  GTK_OBJECT_CLASS(parent_class)->destroy(object);
}

/**********************************************************************
 *    Function: gtk_wave_draw_set_color()
 *
 * Description: Assigns a color for a given wave
 **********************************************************************/
void       
gtk_wave_draw_set_color(GtkWaveDraw *wavedraw,
			gint wave_idx,
			GdkColor *color)
{
  GSList *node;

  g_return_if_fail(wavedraw != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(wavedraw));
  g_return_if_fail(wave_idx < 0);

  for (node = wavedraw->wavelist; node; node = node->next) {
    if (WAVE_DATA_FROM_LIST(node)->wave_idx == wave_idx) break;
  }
  if (node)
    memcpy(&(WAVE_DATA_FROM_LIST(node)->color), color, sizeof(GdkColor));
}

/**********************************************************************
 *    Function: gtk_wave_draw_set_resolution
 *
 * Description: Sets the number of samples to draw per pixel if we've
 *              got more than one sample per pixel to display.
 **********************************************************************/
void       
gtk_wave_draw_set_resolution (GtkWaveDraw *wavedraw,
			      gint resolution)
{
  g_return_if_fail(wavedraw != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(wavedraw));
  g_return_if_fail(resolution <= 0);

  wavedraw->n_points_per_pixel = resolution;
}
