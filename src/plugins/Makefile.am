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


# for each optional plugin we can have build dependencies

if HAVE_FFTW
fft_plugins += fft.la
endif
if HAVE_OSS
audio_io_plugins += audio_io_oss.la
endif
if HAVE_ALSA
audio_io_plugins += audio_io_alsa.la
endif
if HAVE_ESD
audio_io_plugins += audio_io_esd.la
endif
if HAVE_SGI
audio_io_plugins += audio_io_irix.la
endif


libglamebuiltins_la_SOURCES = basic.c basic_sample.c arithmetic.c \
	swapfile_io.c audio_io.c file_io.c waveform.c rms.c
libglamebuiltins_la_LDFLAGS = -module $(GLAME_AUDIO_LIBS) $(GLAME_FFT_LIBS)


# (optional) real plugins
pkglib_LTLIBRARIES = echo.la debug.la tutorial.la \
	pan.la pipe.la iir.la noisegate.la time.la \
	$(audio_io_plugins) $(fft_plugins)

echo_la_SOURCES = echo.c
echo_la_LDFLAGS = -module

debug_la_SOURCES = debug.c
debug_la_LDFLAGS = -module

tutorial_la_SOURCES = tutorial.c
tutorial_la_LDFLAGS = -module

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

if HAVE_FFTW
fft_la_SOURCES = fft.c
endif
fft_la_LDFLAGS = -module

if HAVE_OSS
audio_io_oss_la_SOURCES = audio_io_oss.c audio_io.h
endif
audio_io_oss_la_LDFLAGS = -module $(OSS_LIBS)

if HAVE_ESD
audio_io_esd_la_SOURCES = audio_io_esd.c audio_io.h
endif
audio_io_esd_la_LDFLAGS = -module $(ESD_LIBS)

if HAVE_ALSA
audio_io_alsa_la_SOURCES = audio_io_alsa.c audio_io.h
endif
audio_io_alsa_la_LDFLAGS = -module $(ALSA_LIBS)

if HAVE_SGI
audio_io_irix_la_SOURCES = audio_io_irix.c audio_io.h
endif
audio_io_irix_la_LDFLAGS = -module $(SGIAUDIO_LIBS)
