#ifndef _GTK_WAVE_WIDGET_H
#define _GTK_WAVE_WIDGET_H

/*
 * GtkWaveWidget is supposed to present a stacked view of
 * GtkWaveViews. Control is done through a custom GtkAdjustment
 * that is used for the individual GtkWaveViews. The stack of
 * waves can be modified by adding, removing, raising and lowering
 * of waves.
 */

#include <gdk/gdk.h>
#include <gtk/gtkdrawingarea.h>
#include "gtkwaveview.h"


#ifdef __cplusplus
extern "C" {
#endif


#define GTK_TYPE_WAVEWIDGET            (gtk_wave_view_get_type ())
#define GTK_WAVE_WIDGET(obj)           (GTK_CHECK_CAST ((obj), GTK_TYPE_WAVEWIDGET, GtkWaveWidget))
#define GTK_WAVE_WIDGET_CLASS(klass)   (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_WAVEWIDGET, GtkWaveWidgetClass))
#define GTK_IS_WAVEWIDGET(obj)         (GTK_CHECK_TYPE ((obj), GTK_TYPE_WAVEWIDGET))
#define GTK_IS_WAVEWIDGET_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_WAVEWIDGET))


typedef struct _GtkWaveWidget            GtkWaveWidget;
typedef struct _GtkWaveWidgetClass       GtkWaveWidgetClass;


struct _GtkWaveWidget
{
	GtkDrawingArea parent;

	GList *wave_views;

	GtkAdjustment *adjustment;
};

struct _GtkWaveWidgetClass
{
	GtkDrawingAreaClass parent_class;
};


GtkType     gtk_wave_widget_get_type          (void);
GtkWidget  *gtk_wave_widget_new               (GtkAdjustment *adjustment);
void gtk_wave_widget_set_adjustment           (GtkWaveWidget *wavewidget,
				               GtkAdjustment *adjustment);
GtkAdjustment *gtk_wave_widget_get_adjustment (GtkWaveWidget *wavewidget);

int gtk_wave_widget_add_top      (GtkWaveWidget *wavewidget,
				  GtkWaveView *waveview);
int gtk_wave_widget_add_bottom   (GtkWaveWidget *wavewidget,
				  GtkWaveView *waveview);
void gtk_wave_widget_remove      (GtkWaveWidget *wavewidget,
				  GtkWaveView *waveview);
void gtk_wave_widget_raise       (GtkWaveWidget *wavewidget,
				  GtkWaveView *waveview);
void gtk_wave_widget_lower       (GtkWaveWidget *wavewidget,
				  GtkWaveView *waveview);
GList *gtk_wave_widget_get_views (GtkWaveWidget *wavewidget);


#ifdef __cplusplus
}
#endif


#endif
