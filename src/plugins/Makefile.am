INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@top_srcdir@/src/filter $(GLAME_DCFLAGS) $(GLAME_AUDIO_CFLAGS) -D_REENTRANT


EXTRA_DIST = basic.c basic_sample.c arithmetic.c \
	swapfile_io.c audio_io.c file_io.c waveform.c \
	rms.c fft.c \
	echo.c debug.c tutorial.c \
	garrison.c nold.c maggy.c noisegate.c pan.c pipe.c \
	iir.c \
	audio_io_oss.c audio_io_esd.c audio_io_alsa.c audio_io_irix.c \
	echo2.scm fft_resample.scm fft_bandpass.scm read_mp3.scm


# macro filters
pkgdata_DATA = echo2.scm fft_resample.scm fft_bandpass.scm read_mp3.scm

# builtin filters
lib_LTLIBRARIES = libglamebuiltins.la

if HAVE_FFTW
libglamebuiltins_la_SOURCES = basic.c basic_sample.c arithmetic.c \
	swapfile_io.c audio_io.c file_io.c waveform.c rms.c fft.c
else
libglamebuiltins_la_SOURCES = basic.c basic_sample.c arithmetic.c \
	swapfile_io.c audio_io.c file_io.c waveform.c rms.c
endif
libglamebuiltins_la_LDFLAGS = -module $(GLAME_AUDIO_LIBS) $(GLAME_FFT_LIBS)


# "real" plugins
#pkglib_LTLIBRARIES = echo.la debug.la tutorial.la \
#	garrison.la nold.la maggy.la noisegate.la pan.la pipe.la \
#	iir.la
pkglib_LTLIBRARIES = echo.la debug.la tutorial.la \
	pan.la pipe.la iir.la noisegate.la time.la \
	audio_io_oss.la audio_io_esd.la audio_io_alsa.la audio_io_irix.la
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

time_la_SOURCES = time.c
time_la_LDFLAGS = -module

audio_io_oss_la_SOURCES = audio_io_oss.c audio_io.h
audio_io_oss_la_LDFLAGS = -module

audio_io_esd_la_SOURCES = audio_io_esd.c audio_io.h
audio_io_esd_la_LDFLAGS = -module

audio_io_alsa_la_SOURCES = audio_io_alsa.c audio_io.h
audio_io_alsa_la_LDFLAGS = -module

audio_io_irix_la_SOURCES = audio_io_irix.c audio_io.h
audio_io_irix_la_LDFLAGS = -module

