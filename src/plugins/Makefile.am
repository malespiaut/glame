INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@top_srcdir@/src/filter $(GLAME_DCFLAGS) $(GLAME_AUDIO_CFLAGS) -D_REENTRANT

EXTRA_DIST = net_test_play net_test_audio net_test_echo net_test_echo2

noinst_PROGRAMS = test_network
noinst_LTLIBRARIES = libbuiltins.la

# put "real" plugins here
lib_LTLIBRARIES = echo.la

# list the builtin ones here
libbuiltins_la_SOURCES = builtins.c \
	basic.c basic_sample.c tutorial.c debug.c \
	track_io.c audio_io.c file_io.c \
	waveform.c \
	garrison.c nold.c maggy.c \
	basic_midi.c midi_io.c midi_debug.c
libbuiltins_la_LDFLAGS = -module $(GLAME_AUDIO_LIBS)

# the plugin rules
echo_la_SOURCES = echo.c
echo_la_LDFLAGS = -module

test_network_SOURCES = test_network.c
test_network_LDADD = libbuiltins.la ../glmid/libglame.la \
	$(GLAME_DLIBS) $(GLAME_AUDIO_LIBS)
test_network_LDFLAGS = -export-dynamic
