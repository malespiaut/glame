INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../hash $(ESD_CFLAGS) -D_REENTRANT

noinst_PROGRAMS = test_latency
noinst_LTLIBRARIES = libfilter.la

EXTRA_DIST = filter.h


if ESD
ESD_SOURCE = esd.c
else
ESD_SOURCE =
endif

libfilter_la_SOURCES = filter.c filter.h \
	filter_buffer.c filter_network.c filter_node.c \
	drop.c one2n.c dup.c mix.c null.c ping.c \
	file_in.c file_out.c \
	volume_adjust.c $(ESD_SOURCE)


test_latency_SOURCES = test_latency.c
test_latency_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la -lpthread $(DEBUG_LIBS) $(ESD_LIBS)
