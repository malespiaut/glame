INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../hash $(ESD_CFLAGS) -D_REENTRANT

noinst_PROGRAMS = test_latency
noinst_LTLIBRARIES = libfilter.la

EXTRA_DIST = filter.h


libfilter_la_SOURCES = filter.c filter.h \
	filter_buffer.c filter_network.c filter_node.c \
	drop.c one2n.c dup.c mix.c null.c ping.c \
	file_in.c file_out.c \
	volume_adjust.c esd.c

#if test $(ESD_CONFIG) = "no"; then
#	libfilter_la_SOURCES += esd.c
libfilter_la_LIBADD = $(ESD_LIBS)
#fi

test_latency_SOURCES = test_latency.c
test_latency_LDADD = libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la -lpthread
