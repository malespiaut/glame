if LADSPA
LADSPA_FILES = ladspa.c
else
LADSPA_FILES =
endif

INCLUDES = $(LTDLINCL) -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../filter -I@srcdir@/../hash -DPKGSCRIPTSDIR=\"$(pkgscriptsdir)\" -DPKGLIBDIR=\"$(pkglibdir)\" $(GLAME_INCLUDEDIR)

EXTRA_DIST = glame.scm glame_testsuite.scm ladspa.c

noinst_LTLIBRARIES = libglmid.la libglameaudiofile.la
pkgscripts_DATA = glame.scm glame_testsuite.scm

libglmid_la_SOURCES = \
	glmid.c glmid.h \
	glplugin.c glplugin.h \
	glscript.c glscript.h \
	glscript_swapfile.c glscript_filter.c glscript_gpsm.c \
	glconfig.c glconfig.h \
	$(LADSPA_FILES) \
	gpsm.c gpsm.h clipboard.c clipboard.h \
	network_utils.c network_utils.h
libglmid_la_LDFLAGS = -export-dynamic \
	$(GLAME_LIBDIR) $(GLAME_LIBS) -lm

libglameaudiofile_la_SOURCES = \
	glame_audiofile.c
libglameaudiofile_la_LDFLAGS = -export-dynamic -lm

