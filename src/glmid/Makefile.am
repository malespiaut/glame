if LADSPA
LADSPA_FILES = ladspa.c
else
LADSPA_FILES =
endif

INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../filter -I@srcdir@/../hash $(XML_CFLAGS) $(LADSPA_CFLAGS) -DPKGSCRIPTSDIR=\"$(pkgscriptsdir)\" -DPKGLIBDIR=\"$(pkglibdir)\" $(GUILE_INCS) -D_NO_FILTER_COMPATIBILITY

EXTRA_DIST = glame.scm ladspa.c

lib_LTLIBRARIES = libglame.la
pkgscripts_DATA = glame.scm

libglame_la_SOURCES = \
	glmid.c glmid.h \
	glplugin.c glplugin.h \
	glscript.c glscript.h \
	glscript_swapfile.c glscript_filter.c glscript_gpsm.c \
	$(LADSPA_FILES) \
	gpsm.c gpsm.h clipboard.c clipboard.h \
	network_utils.c network_utils.h

libglame_la_LIBADD = ../swapfile/libswapfile.la ../filter/libfilter.la \
	../hash/libhash.la ../plugins/libglamebuiltins.la $(XML_LIBS)
#libglame_la_LIBADD = ../swapfile/libswapfile.la ../filter/libfilter.la \
#	../glmath/libglmath.la ../hash/libhash.la
libglame_la_LDFLAGS = -module $(GUILE_LIBS) $(PTHREAD_LIB) -lm

