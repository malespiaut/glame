INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../filter -I@srcdir@/../hash -DPKGDATADIR=\"$(pkgdatadir)\" -DPKGLIBDIR=\"$(pkglibdir)\" $(GUILE_INCS) -D_NO_FILTER_COMPATIBILITY

EXTRA_DIST = glscript.c glscript.h \
	glscript_swapfile.c glscript_filter.c glame.scm

lib_LTLIBRARIES = libglame.la
pkgdata_DATA = glame.scm

if GUILE
libglame_la_SOURCES = \
	glmid.c glmid.h \
	glplugin.c glplugin.h \
	glscript.c glscript.h glscript_swapfile.c glscript_filter.c \
	ladspa.c
else
libglame_la_SOURCES = \
	glmid.c glmid.h \
	glplugin.c glplugin.h \
	ladspa.c
endif

libglame_la_LIBADD = ../swapfile/libswapfile.la ../filter/libfilter.la \
	../hash/libhash.la
#libglame_la_LIBADD = ../swapfile/libswapfile.la ../filter/libfilter.la \
#	../glmath/libglmath.la ../hash/libhash.la
libglame_la_LDFLAGS = -module $(GUILE_LIBS) $(PTHREAD_LIB) -lm

