INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -D_REENTRANT

noinst_PROGRAMS = test_latency
noinst_LTLIBRARIES = libfilter.la

EXTRA_DIST = filter.h filter_hash.h


libfilter_la_SOURCES = filter.c filter.h filter_hash.h \
	filter_buffer.c filter_network.c filter_node.c \
	drop.c one2n.c dup.c mix.c null.c ping.c \
	file_in.c file_out.c \
	volume_adjust.c

test_latency_SOURCES = test_latency.c
test_latency_LDADD = libfilter.la ../swapfile/libswapfile.la -lpthread
