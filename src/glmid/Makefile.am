if LADSPA
LADSPA_FILES = ladspa.c
else
LADSPA_FILES =
endif

INCLUDES = -I@top_srcdir@/libltdl -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../filter -I@srcdir@/../hash $(XML_CFLAGS) $(LADSPA_CFLAGS) -DPKGSCRIPTSDIR=\"$(pkgscriptsdir)\" -DPKGLIBDIR=\"$(pkglibdir)\" $(GUILE_INCS)

EXTRA_DIST = glame.scm ladspa.c

noinst_LTLIBRARIES = libglmid.la
pkgscripts_DATA = glame.scm

libglmid_la_SOURCES = \
	glmid.c glmid.h \
	glplugin.c glplugin.h \
	glscript.c glscript.h \
	glscript_swapfile.c glscript_filter.c glscript_gpsm.c \
	$(LADSPA_FILES) \
	gpsm.c gpsm.h clipboard.c clipboard.h \
	network_utils.c network_utils.h
libglmid_la_LDFLAGS = -export-dynamic \
	$(GUILE_LIBS) $(PTHREAD_LIB) $(XML_LIBS) -lm

#libglame_la_LIBADD = ../swapfile/libswapfile.la ../filter/libfilter.la \
	../hash/libhash.la ../plugins/libglamebuiltins.la

