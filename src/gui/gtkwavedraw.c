/*
 * gtkwavedraw.c
 *
 * $Id: gtkwavedraw.c,v 1.1 2000/04/09 21:10:59 navratil Exp $
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
static gint gtk_wave_draw_configure(GtkWidget *widget, GdkEventConfigure *event);
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

  wavedraw->data = NULL;
  wavedraw->marker = NULL;
  wavedraw->n_samples = NULL;
  wavedraw->start = NULL;

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
 * Description: Adds a set of wave data to the current waves;
 *              returns a 'wave index" number for the new data set.
 **********************************************************************/
gint
gtk_wave_draw_add_wave(GtkWaveDraw *wavedraw,
		       gfloat *data,
		       glong  n_samples,
		       glong  start)
{
  gint new_idx;
  g_return_if_fail(wavedraw != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(wavedraw));
  
  new_idx = wavedraw->n_waves++;

  /* 
   * First of all, we need to copy in (locally cache) the new data
   */
  wavedraw->data = g_realloc(wavedraw->data, 
			     sizeof(gfloat *) * wavedraw->n_waves);
  wavedraw->data[new_idx] = g_malloc(sizeof(gfloat) * n_samples);
  memcpy(wavedraw->data[new_idx], data, sizeof(gfloat) * n_samples);

  /* 
   * Secondly, create two new markers and put 'em at the start and end.
   */
  wavedraw->marker = g_realloc(wavedraw->marker, 
			       sizeof(glong *) * wavedraw->n_waves);
  wavedraw->marker[new_idx] = g_malloc(sizeof(glong) * 2);
  wavedraw->marker[new_idx][0] = start;
  wavedraw->marker[new_idx][1] = start + n_samples - 1;

  /* 
   * Third, let the widget know how many samples are in this wave
   */
  wavedraw->n_samples = g_realloc(wavedraw->n_samples, 
				  sizeof(glong) * wavedraw->n_waves);
  wavedraw->n_samples[new_idx] = n_samples;

  /*
   * Fourth and finally, let the widget know where the new wave fits
   * into the grand scheme.
   */
  wavedraw->start = g_realloc(wavedraw->start, 
			      sizeof(glong) * wavedraw->n_waves);
  wavedraw->start[new_idx] = start;

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
  g_return_if_fail(wavedraw != NULL);
  g_return_if_fail(GTK_IS_WAVE_DRAW(wavedraw));
  g_return_if_fail(wave_idx >= 0 && wave_idx < wavedraw->n_waves);

  if (wave_idx == wavedraw->n_waves - 1) {
    /* 
     * Before we begin: make a note, we've got one less wave
     * to work with now.
     */
    wavedraw->n_waves--;

    /*
     * First, free the data.
     */
    g_free(wavedraw->data[wave_idx]);
    g_realloc(wavedraw->data, sizeof(gfloat *) * wavedraw->n_waves);

    /* 
     * Second, free the markers.
     */
    g_free(wavedraw->marker[wave_idx]);
    g_realloc(wavedraw->marker, sizeof(glong *) * wavedraw->n_waves);

    /*
     * Third, realloc the n_samples array
     */
    g_realloc(wavedraw->n_samples, sizeof(glong) * wavedraw->n_waves);

    /* 
     * Fourth and finally, realloc the starting point array
     */
    g_realloc(wavedraw->start, sizeof(glong) * wavedraw->n_waves);
  } else {
    /* 
     * We're not dealing with the easy case, so we can't simply
     * realloc all the arrays.  Maybe we should just make this
     * impossible (g_return_if_fail(wave_idx == wavedata->n_waves-1)) ?
     */
    gint   n_waves        = wavedraw->n_waves - 1;
    gfloat **new_data     = NULL;
    glong  **new_marker   = NULL;
    glong  *new_n_samples = NULL;
    glong  *new_start     = NULL;
    int i;
    
    new_data      = g_malloc(sizeof(gfloat *) * n_waves);
    new_marker    = g_malloc(sizeof(glong *)  * n_waves);
    new_n_samples = g_malloc(sizeof(glong)    * n_waves);
    new_start     = g_malloc(sizeof(glong)    * n_waves);
    for (i=0; i<wave_idx; i++) {
      new_data[i]      = wavedraw->data[i];
      new_marker[i]    = wavedraw->marker[i];
      new_n_samples[i] = wavedraw->n_samples[i];
      new_start[i]     = wavedraw->start[i];
    }
    for (i=wave_idx+1; i<wavedraw->n_waves; i++) {
      new_data[i-1]      = wavedraw->data[i];
      new_marker[i-1]    = wavedraw->marker[i];
      new_n_samples[i-1] = wavedraw->n_samples[i];
      new_start[i-1]     = wavedraw->start[i];
    }

    /* Free the allocated arrays  -- first, free the old wave data */
    g_free(wavedraw->data[wave_idx]);
    g_free(wavedraw->marker[wave_idx]);
    
    /* Free the allocated arrays  -- second, free the old wavedraw arrays */
    g_free(wavedraw->data);
    g_free(wavedraw->marker);
    g_free(wavedraw->n_samples);
    g_free(wavedraw->start);

    /* Now assign everything to the new arrays */
    wavedraw->data      = new_data;
    wavedraw->marker    = new_marker;
    wavedraw->n_samples = new_n_samples;
    wavedraw->start     = new_start;

    /* Finally, let the wavedraw know it's smaller now */
    wavedraw->n_waves   = n_waves;
  }

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
  static GdkColor *color = NULL; 
  GdkPixmap   *pixmap;
  GdkGC       *wavedraw_gc;
  gint   wave_idx;
  gint   midy;                 /* The Y midpoint, ie the axis */
  gfloat scale_x, stride_x;
  gfloat start_x, disp_x;
  glong  stop_x;
  gfloat cur_x, cur_x2;
  gfloat stride_samp, samp_offset;
  gfloat *p_val;
  glong  *start;
  glong  *n_samples;
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
  if (color == NULL) {
    color = (GdkColor *)g_malloc(sizeof(GdkColor));
  }
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

  for (wave_idx=0,
	 start = wavedraw->start,
	 n_samples = wavedraw->n_samples;
       wave_idx<wavedraw->n_waves; 
       wave_idx++, start++, n_samples++) {
    /* If our current start is after the end of the display, continue */
    if (*start > stop_disp)
      continue;
    
    /* If our current stop is before the beginning of the display, continue */
    if ((stop = (*start + *n_samples - 1)) < start_disp)
      continue;

    p_val = wavedraw->data[wave_idx];

    /* Figure out what color we're going to use -- this is extremely 
     * inelegant (not to mention ugly, painful, and redundant) , but I 
     * plan to replace it RSN. */
    if (wave_idx %3 == 0) {
      color->red = 0; color->green = 0xffff; color->blue = 0;
    } else if (wave_idx %3 == 1) {
      color->red = 0xffff; color->green = 0; color->blue = 0;
    } else {
      color->red = 0; color->green = 0; color->blue = 0xffff;
    }
    gdk_color_alloc(gdk_colormap_get_system(), color);
    gdk_gc_set_foreground(wavedraw->wavedraw_gc, color);

    start_sample = MAX(*start, start_disp);
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
	printf("Graphing %d (%d) to %d (%d)\n", cur_y, samp, cur_y2, samp+1);
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
      p_val = wavedraw->data[wave_idx] + (glong) samp_offset;
      cur_y = (int)(midy - (*p_val)*midy);

      for (cur_x = start_x; cur_x < stop_x; cur_x++) {
	for (cur_subsamp = 0; cur_subsamp < wavedraw->n_points_per_pixel-1;
	     cur_subsamp++) {
	  samp_offset += stride_samp;
	  p_val = wavedraw->data[wave_idx] + (glong) samp_offset;
	  cur_y2 = (int)(midy - (*p_val)*midy);
#if 0
	  printf("2. Graphing %d (%d) to %d (%d)\n", cur_y, 
		 (int)(samp_offset - stride_samp), cur_y2, (int)samp_offset);
#endif
	  gdk_draw_line(pixmap,	wavedraw_gc,
			cur_x, cur_y,
			cur_x, cur_y2);
	  cur_y = cur_y2;
	} /* Subsample plotting */
	if (cur_x == stop_x-1) break;
	samp_offset += stride_samp;
	p_val = wavedraw->data[wave_idx] + (glong) samp_offset;
	cur_y2 = (int)(midy - (*p_val)*midy);
	printf("2. Graphing %d (%d) to %d (%d) (stop=%d)\n", cur_y, 
	       (int)cur_x, cur_y2, (int)cur_x+1, stop_x);
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

  /* Free the allocated arrays */
  for (i=0; i<wavedraw->n_waves; i++) {
    g_free(wavedraw->data[i]);
    g_free(wavedraw->marker[i]);
  }
  g_free(wavedraw->data);
  g_free(wavedraw->marker);
  g_free(wavedraw->n_samples);
  g_free(wavedraw->start);

  /* Destroy the parent */
  GTK_OBJECT_CLASS(parent_class)->destroy(object);
}
