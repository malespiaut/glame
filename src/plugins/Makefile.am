INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@top_srcdir@/src/filter $(GLAME_DCFLAGS) $(GLAME_AUDIO_CFLAGS) -D_REENTRANT

EXTRA_DIST = basic.c basic_sample.c arithmetic.c \
	track_io.c audio_io.c file_io.c waveform.c \
	basic_midi.c midi_io.c midi_debug.c rms.c \
	echo.c debug.c tutorial.c \
	garrison.c nold.c maggy.c noisegate.c pan.c pipe.c \
	iir.c ladspa.c

# builtin filters
lib_LTLIBRARIES = libglamebuiltins.la

#libglamebuiltins_la_SOURCES = basic.c basic_sample.c arithmetic.c \
#	track_io.c audio_io.c file_io.c waveform.c \
#	basic_midi.c midi_io.c midi_debug.c rms.c 
libglamebuiltins_la_SOURCES = basic.c basic_sample.c arithmetic.c \
	track_io.c audio_io.c file_io.c waveform.c rms.c
#libglamebuiltins_la_SOURCES = basic.c
libglamebuiltins_la_LDFLAGS = -module $(GLAME_AUDIO_LIBS)


# "real" plugins
#pkglib_LTLIBRARIES = echo.la debug.la tutorial.la \
#	garrison.la nold.la maggy.la noisegate.la pan.la pipe.la \
#	iir.la
pkglib_LTLIBRARIES = echo.la debug.la tutorial.la \
	pan.la pipe.la iir.la noisegate.la
#pkglib_LTLIBRARIES = debug.la tutorial.la

echo_la_SOURCES = echo.c
echo_la_LDFLAGS = -module

debug_la_SOURCES = debug.c
debug_la_LDFLAGS = -module

tutorial_la_SOURCES = tutorial.c
tutorial_la_LDFLAGS = -module

#garrison_la_SOURCES = garrison.c
#garrison_la_LDFLAGS = -module

#nold_la_SOURCES = nold.c
#nold_la_LDFLAGS = -module

#maggy_la_SOURCES = maggy.c
#maggy_la_LDFLAGS = -module

noisegate_la_SOURCES = noisegate.c
noisegate_la_LDFLAGS = -module

pan_la_SOURCES = pan.c
pan_la_LDFLAGS = -module

pipe_la_SOURCES = pipe.c
pipe_la_LDFLAGS = -module

iir_la_SOURCES = iir.c
iir_la_LDFLAGS = -module

#ladspa_la_SOURCES = ladspa.c
#ladspa_la_LDFLAGS = -module
