INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../hash -I@srcdir@/../channel $(GLAME_CFLAGS) -D_REENTRANT

noinst_PROGRAMS = test_latency test_network
noinst_LTLIBRARIES = libfilter.la

EXTRA_DIST = filter.h filterI.h filter_methods.h filter_mm.h filter_ops.h \
	filter_tools.h filter_protocols.h


libfilter_la_SOURCES = filter.h filterI.h filter_methods.h \
	filter.c filter_buffer.c filter_network.c filter_methods.c \
	filter_mm.c filter_ops.c \
	basic.c tutorial.c debug.c channel_io.c audio_io.c \
	volume_adjust.c waveform.c file_io.c \
	echo.c garrison.c nold.c echo2.c \
	midi_basic.c midi_io.c


test_latency_SOURCES = test_latency.c
test_latency_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lm -lpthread $(GLAME_LIBS)

test_network_SOURCES = test_network.c
test_network_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lm -lpthread $(GLAME_LIBS)
