INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@top_srcdir@/src/filter -I@top_srcdir@/src/gui $(GLAME_DCFLAGS) $(GLAME_AUDIO_CFLAGS) $(GUILE_INCS) -D_REENTRANT $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS) $(XML_CFLAGS)


EXTRA_DIST = basic.c basic_sample.c arithmetic.c \
	swapfile_io.c audio_io.c file_io.c waveform.c \
	ssp.c normalize.c fft_plugins.c \
	echo.c debug.c tutorial.c \
	nold.c maggy.c noisegate.c pan.c pipe.c \
	iir.c flanger.c distortion.c\
	audio_io_oss.c audio_io_esd.c audio_io_alsa.c audio_io_alsa_v050.c \
	audio_io_alsa_v090.c audio_io_irix.c \
	echo2.scm resample.scm bassboost.scm read_mp3.scm \
	fader.c builtins.c

# macro filters
pkgscripts_DATA = echo2.scm resample.scm bassboost.scm read_mp3.scm

# builtin filters
noinst_LTLIBRARIES = libglamebuiltins.la


# for each optional plugin we can have build dependencies

if HAVE_FFTW
fft_plugins_plugin = fft_plugins.la
endif
if HAVE_OSS
audio_io_oss = audio_io_oss.la
endif
if HAVE_ALSA
audio_io_alsa = audio_io_alsa.la
endif
if HAVE_ESD
audio_io_esd = audio_io_esd.la
endif
if HAVE_SGI
audio_io_irix = audio_io_irix.la
endif
if HAVE_GNOME
normalize = normalize.la mixer.la
endif


# builtin plugins - everything is builtin that does not depend
# on optional libraries
libglamebuiltins_la_SOURCES = basic.c basic_sample.c \
	swapfile_io.c audio_io.c file_io.c \
	arithmetic.c waveform.c ssp.c echo.c noisegate.c \
	pan.c pipe.c iir.c flanger.c distortion.c \
	fader.c
libglamebuiltins_la_LDFLAGS = -export-dynamic $(GLAME_AUDIO_LIBS)
libglamebuiltins_la_DEPENDENCIES = builtins.c

# real plugins (depending on optional stuff)
pkglib_LTLIBRARIES = \
	debug.la tutorial.la \
	$(normalize) \
	$(audio_io_oss) $(audio_io_alsa) $(audio_io_esd) $(audio_io_irix) \
	$(fft_plugins_plugin)

debug_la_SOURCES = debug.c
debug_la_LDFLAGS = -module -avoid-version

tutorial_la_SOURCES = tutorial.c
tutorial_la_LDFLAGS = -module -avoid-version

if HAVE_FFTW
fft_plugins_la_SOURCES = fft_plugins.c
endif
fft_plugins_la_LDFLAGS = -module -avoid-version $(GLAME_FFT_LIBS)

if HAVE_OSS
audio_io_oss_la_SOURCES = audio_io_oss.c audio_io.h
endif
audio_io_oss_la_LDFLAGS = -module -avoid-version

if HAVE_ESD
audio_io_esd_la_SOURCES = audio_io_esd.c audio_io.h
endif
audio_io_esd_la_LDFLAGS = -module -avoid-version $(ESD_LIBS)

if HAVE_ALSA
audio_io_alsa_la_SOURCES = audio_io_alsa.c audio_io.h
endif
audio_io_alsa_la_LDFLAGS = -module -avoid-version $(ALSA_LIBS)

if HAVE_SGI
audio_io_irix_la_SOURCES = audio_io_irix.c audio_io.h
endif
audio_io_irix_la_LDFLAGS = -module -avoid-version $(SGIAUDIO_LIBS)

if HAVE_GNOME
normalize_la_SOURCES = normalize.c
mixer_la_SOURCES = mixer.c
endif
normalize_la_LDFLAGS = -module -avoid-version $(GNOME_LIBDIR) $(GNOME_LIBS) $(GNOMEUI_LIBS)
mixer_la_LDFLAGS = -module -avoid-version $(GNOME_LIBDIR) $(GNOME_LIBS) $(GNOMEUI_LIBS)


builtins.c: $(libglamebuiltins_la_SOURCES) Makefile.am
	rm -f builtins.c
	echo '#include "glplugin.h"' > builtins.c
	for i in $(libglamebuiltins_la_SOURCES); do grep '^int .*_register(' $$i | sed -e 's/int \(.*\)_register(.*)/int \1_register(plugin_t *p);/' >> builtins.c; done
	echo 'void register_builtins() {' >> builtins.c
	echo '	plugin_t *p;' >> builtins.c
	for i in $(libglamebuiltins_la_SOURCES); do grep '^int .*_register(' $$i | sed -e 's/int \(.*\)_register(.*)/	if ((p = plugin_add("\1"))) \1_register(p);/' >> builtins.c; done
	echo '}' >> builtins.c

maintainer-clean-local:
	rm -f builtins.c

