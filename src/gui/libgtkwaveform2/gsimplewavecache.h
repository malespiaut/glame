#ifndef _G_SIMPLE_WAVE_CACHE_H
#define _G_SIMPLE_WAVE_CACHE_H

#include "libgtkwaveform/gtkeditablewavebuffer.h"
#include "gwavecache.h"


#ifdef __cplusplus
extern "C" {
#endif


#define G_TYPE_SIMPLE_WAVE_CACHE            (g_simple_wave_cache_get_type ())
#define G_SIMPLE_WAVE_CACHE(obj)            (GTK_CHECK_CAST ((obj), G_TYPE_SIMPLE_WAVE_CACHE, GSimpleWaveCache))
#define G_SIMPLE_WAVE_CACHE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), G_TYPE_SIMPLE_WAVE_CACHE, GSimpleWaveCacheClass))
#define G_IS_SIMPLE_WAVE_CACHE(obj)         (GTK_CHECK_TYPE ((obj), G_TYPE_SIMPLE_WAVE_CACHE))
#define G_IS_SIMPLE_WAVE_CACHE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), G_TYPE_SIMPLE_WAVE_CACHE))

typedef struct _GSimpleWaveCache          GSimpleWaveCache;
typedef struct _GSimpleWaveCacheClass     GSimpleWaveCacheClass;


struct _GSimpleWaveCache {
	GWave parent_object;

	GtkEditableWaveBuffer *wave;
	GWaveType *type;
};

struct _GSimpleWaveCacheClass {
	GWaveClass parent_class;
};


GtkType g_simple_wave_cache_get_type();

GSimpleWaveCache *g_simple_wave_cache_new(GtkEditableWaveBuffer *wave);


#ifdef __cplusplus
}
#endif

#endif
