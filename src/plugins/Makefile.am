INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@top_srcdir@/src/filter $(GLAME_DCFLAGS) $(GLAME_AUDIO_CFLAGS) -D_REENTRANT

noinst_PROGRAMS = test_latency test_network
noinst_LTLIBRARIES = libbuiltins.la

EXTRA_DIST = net_test_play net_test_audio net_test_echo net_test_echo2 net_test_latency

libbuiltins_la_SOURCES = basic.c basic_sample.c tutorial.c debug.c track_io.c audio_io.c \
	waveform.c file_io.c \
	echo.c garrison.c nold.c \
	basic_midi.c midi_io.c midi_debug.c maggy.c


test_latency_SOURCES = test_latency.c
test_latency_LDADD = ../filter/libfilter.la libbuiltins.la ../swapfile/libswapfile.la ../hash/libhash.la ../glmid/libglmid.la -lm -lpthread $(GLAME_DLIBS) $(GLAME_AUDIO_LIBS)

test_network_SOURCES = test_network.c
test_network_LDADD = ../filter/libfilter.la libbuiltins.la ../swapfile/libswapfile.la ../hash/libhash.la ../glmid/libglmid.la -lm -lpthread $(GLAME_DLIBS) $(GLAME_AUDIO_LIBS)
