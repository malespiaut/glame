INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@top_srcdir@/src/filter $(GLAME_DCFLAGS) $(GLAME_AUDIO_CFLAGS) -D_REENTRANT


EXTRA_DIST = basic.c basic_sample.c arithmetic.c \
	swapfile_io.c audio_io.c file_io.c waveform.c \
	rms.c fft_plugins.c \
	echo.c debug.c tutorial.c \
	garrison.c nold.c maggy.c noisegate.c pan.c pipe.c \
	iir.c flanger.c\
	audio_io_oss.c audio_io_esd.c audio_io_alsa.c audio_io_irix.c \
	echo2.scm resample.scm bassboost.scm read_mp3.scm

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


libglamebuiltins_la_SOURCES = basic.c basic_sample.c arithmetic.c \
	swapfile_io.c audio_io.c file_io.c waveform.c rms.c
libglamebuiltins_la_LDFLAGS = -module $(GLAME_AUDIO_LIBS)


# (optional) real plugins
pkglib_LTLIBRARIES = echo.la debug.la tutorial.la \
	pan.la pipe.la iir.la noisegate.la time.la flanger.la \
	$(audio_io_oss) $(audio_io_alsa) $(audio_io_esd) $(audio_io_irix) \
	$(fft_plugins_plugin)

echo_la_SOURCES = echo.c
echo_la_LDFLAGS = -module -avoid-version

debug_la_SOURCES = debug.c
debug_la_LDFLAGS = -module -avoid-version

tutorial_la_SOURCES = tutorial.c
tutorial_la_LDFLAGS = -module -avoid-version

noisegate_la_SOURCES = noisegate.c
noisegate_la_LDFLAGS = -module -avoid-version

pan_la_SOURCES = pan.c
pan_la_LDFLAGS = -module -avoid-version

pipe_la_SOURCES = pipe.c
pipe_la_LDFLAGS = -module -avoid-version

iir_la_SOURCES = iir.c
iir_la_LDFLAGS = -module -avoid-version

time_la_SOURCES = time.c
time_la_LDFLAGS = -module -avoid-version

flanger_la_SOURCES = flanger.c
flanger_la_LDFLAGS = -module -avoid-version

if HAVE_FFTW
fft_plugins_la_SOURCES = fft_plugins.c
endif
fft_plugins_la_LDFLAGS = -module -avoid-version $(GLAME_FFT_LIBS)

if HAVE_OSS
audio_io_oss_la_SOURCES = audio_io_oss.c audio_io.h
endif
audio_io_oss_la_LDFLAGS = -module -avoid-version $(OSS_LIBS)

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
