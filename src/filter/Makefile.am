INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_DCFLAGS) -D_REENTRANT -D_NO_FILTER_COMPATIBILITY

noinst_LTLIBRARIES = libfilter.la

EXTRA_DIST = filter_tools.h filter_protocols.h filter_types.h

libfilter_la_SOURCES = \
	filter.h filter.c \
	filter_methods.h filter_methods.c \
	filter_buffer.h filter_buffer.c \
	filter_ops.h filter_ops.c \
	filter_param.h filter_param.c \
	filter_port.h filter_port.c \
	filter_pipe.h filter_pipe.c
