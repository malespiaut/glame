#ifndef _G_WAVE_H
#define _G_WAVE_H

#include <gtk/gtk.h>
#include "grange.h"


/*
 * GWave is much like libgtkwaveforms GtkWaveBuffer - its the
 * data object.
 */

#ifdef __cplusplus
extern "C" {
#endif


#ifndef G_OBJECT_GET_CLASS
#define G_OBJECT_GET_CLASS(object) ((GTK_OBJECT(object))->klass)
#endif


typedef enum {
	G_WAVE_TYPE_NULL,
	G_WAVE_TYPE_S16_I,
	G_WAVE_TYPE_S16_NI,
	G_WAVE_TYPE_FLOAT_I,
	G_WAVE_TYPE_FLOAT_NI
} GWaveType;


#define G_TYPE_WAVE            (g_wave_get_type ())
#define G_WAVE(obj)            (GTK_CHECK_CAST ((obj), G_TYPE_WAVE, GWave))
#define G_WAVE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), G_TYPE_WAVE, GWaveClass))
#define G_IS_WAVE(obj)         (GTK_CHECK_TYPE ((obj), G_TYPE_WAVE))
#define G_IS_WAVE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), G_TYPE_WAVE))

typedef struct _GWave          GWave;
typedef struct _GWaveClass     GWaveClass;


struct _GWave {
	GtkObject object;
};

struct _GWaveClass {
	GtkObjectClass object_class;

	/* Signals */
	void (*invalidated) (GWave *object);
	void (*modified)    (GWave *object, GRange *range);
	void (*insert_data) (GWave *object, GRange *range);
	void (*delete_data) (GWave *object, GRange *range);

	/* Pure abstract functions. */
	guint32   (*get_rate)          (GWave *wave);
	GWaveType (*get_datatype)      (GWave *wave);
	guint32   (*get_length)        (GWave *wave);
	guint32   (*get_num_channels)  (GWave *wave);
	void      (*get_samples)       (GWave *wave,
					guint32 start, guint32 length,
					guint32 channel_mask,
					gpointer data);
};


GtkType g_wave_get_type();

guint32   g_wave_get_rate         (GWave *wave);
guint32   g_wave_get_num_channels (GWave *wave);
guint32   g_wave_get_length       (GWave *wave);
GWaveType g_wave_get_datatype     (GWave *wave);
void      g_wave_get_samples      (GWave *wave,
				   guint32 start, guint32 length,
				   guint32 channel_mask,
				   gpointer data);


#ifdef __cplusplus
}
#endif

#endif
