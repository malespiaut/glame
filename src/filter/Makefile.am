INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../hash -I@srcdir@/../channel $(ESD_CFLAGS) -D_REENTRANT

noinst_PROGRAMS = test_latency test_audio test_play test_network
noinst_LTLIBRARIES = libfilter.la

EXTRA_DIST = filter.h filter_buffer.h


libfilter_la_SOURCES = filter.c filter.h filter_buffer.h \
	filter_buffer.c filter_network.c \
	basic.c debug.c channel_io.c audio_io.c \
	volume_adjust.c waveform.c read_file.c \
	echo.c


test_latency_SOURCES = test_latency.c
test_latency_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lm -lpthread $(DEBUG_LIBS) $(ESD_LIBS) $(AUDIOFILE_LIBS)

test_audio_SOURCES = test_audio.c
test_audio_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lm -lpthread $(DEBUG_LIBS) $(ESD_LIBS) $(AUDIOFILE_LIBS)

test_play_SOURCES = test_play.c
test_play_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lm -lpthread $(DEBUG_LIBS) $(ESD_LIBS) $(AUDIOFILE_LIBS)

test_network_SOURCES = test_network.c
test_network_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lm -lpthread $(DEBUG_LIBS) $(ESD_LIBS) $(AUDIOFILE_LIBS)
