#ifndef _G_WAVE_CACHE_H
#define _G_WAVE_CACHE_H

/*
 * GWaveCache can be used to transparently handle different datatypes
 * from a GWave - GWaveCache is supposed to do type conversion to a
 * requested type.
 * GWaveCache can also be used to sample the actual data and such
 * handles different zoom levels transparently. Instead of specifying
 * a range of samples to get, you specify the number of samples you
 * want to get out of a range.
 * GWaveCache is supposed to do this sampling and type converting in
 * a efficient manner by caching and prefetching data. GWaveCache
 * could be implemented to merge multiple GWaves into a virtual
 * GWave.
 * GWaveCache will behave exactly like a GWave if used as such (i.e.
 * all requests will be transparently forwarded to the GWave object(s)).
 */

#include "gwave.h"


#ifdef __cplusplus
extern "C" {
#endif


#define G_TYPE_WAVE_CACHE            (g_wave_cache_get_type ())
#define G_WAVE_CACHE(obj)            (GTK_CHECK_CAST ((obj), G_TYPE_WAVE_CACHE, GWaveCache))
#define G_WAVE_CACHE_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), G_TYPE_WAVE_CACHE, GWaveCacheClass))
#define G_IS_WAVE_CACHE(obj)         (GTK_CHECK_TYPE ((obj), G_TYPE_WAVE_CACHE))
#define G_IS_WAVE_CACHE_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), G_TYPE_WAVE_CACHE))

typedef struct _GWaveCache          GWaveCache;
typedef struct _GWaveCacheClass     GWaveCacheClass;


struct _GWaveCache {
	GWave parent_object;
};

struct _GWaveCacheClass {
	GWaveClass parent_class;

	/* Pure abstract functions. */
	void      (*set_datatype)      (GWaveCache *wavecache,
					GWaveType type);
	void      (*get_samples_sample)(GWaveCache *wavecache,
					guint32 start, guint32 length,
					guint32 nr_samples,
					guint32 channel_mask,
					gpointer data);
};


GtkType g_wave_cache_get_type();
guint32 g_wave_cache_get_rate(GWaveCache *wavecache);
guint32 g_wave_cache_get_num_channels(GWaveCache *wavecache);
guint32 g_wave_cache_get_length(GWaveCache *wavecache);
void g_wave_cache_set_datatype(GWaveCache *wavecache,
			       GWaveType type);
void g_wave_cache_get_samples_sample(GWaveCache *wavecache,
				     guint32 start, guint32 length,
				     guint32 nr_samples,
				     guint32 channel_mask,
				     gpointer data);


#ifdef __cplusplus
}
#endif

#endif
