INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../filter -I@srcdir@/../hash -DPKGDATADIR=\"$(pkgdatadir)\" -DPKGLIBDIR=\"$(pkglibdir)\" $(GUILE_INCS)

EXTRA_DIST = glmid.h glame.scm

lib_LTLIBRARIES = libglame.la
pkgdata_DATA = glame.scm

libglame_la_SOURCES = glmid.c glmid.h glscript.c gltrack.c gltrack.h \
	glplugin.c glplugin.h
libglame_la_LIBADD = ../swapfile/libswapfile.la ../filter/libfilter.la \
	../hash/libhash.la
#libglame_la_LIBADD = ../swapfile/libswapfile.la ../filter/libfilter.la \
#	../glmath/libglmath.la ../hash/libhash.la
libglame_la_LDFLAGS = -module $(GUILE_LIBS) $(PTHREAD_LIB) -lm

