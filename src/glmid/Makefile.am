INCLUDES = -I@srcdir@/../include -I@srcdir@/../swapfile -I@srcdir@/../filter -I@srcdir@/../hash $(GUILE_INCS)

EXTRA_DIST = glmid.h glame.scm

noinst_LTLIBRARIES = libglmid.la
data_DATA = glame.scm

libglmid_la_SOURCES = glmid.h glscript.c gltrack.c gltrack.h \
	glplugin.c glplugin.h

