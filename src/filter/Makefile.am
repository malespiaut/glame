INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../hash -I@srcdir@/../channel $(ESD_CFLAGS) -D_REENTRANT

noinst_PROGRAMS = test_latency test_audio
noinst_LTLIBRARIES = libfilter.la

EXTRA_DIST = filter.h


libfilter_la_SOURCES = filter.c filter.h \
	filter_buffer.c filter_network.c filter_node.c \
	basic.c debug.c channel_io.c audio_io.c \
	mix.c volume_adjust.c waveform.c


test_latency_SOURCES = test_latency.c
test_latency_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lpthread $(DEBUG_LIBS) $(ESD_LIBS)

test_audio_SOURCES = test_audio.c
test_audio_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lpthread $(DEBUG_LIBS) $(ESD_LIBS)
