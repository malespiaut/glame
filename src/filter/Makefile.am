INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/hash $(GLAME_DCFLAGS) -D_REENTRANT

noinst_LTLIBRARIES = libfilter.la

EXTRA_DIST = filter.h filterI.h filter_methods.h filter_mm.h filter_ops.h \
	filter_tools.h filter_protocols.h

libfilter_la_SOURCES = filter.h filterI.h filter_methods.h \
	filter.c filter_buffer.c filter_network.c filter_methods.c \
	filter_mm.c filter_ops.c

