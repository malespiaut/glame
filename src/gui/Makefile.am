INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_CFLAGS) -D_REENTRANT -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)


EXTRA_DIST = gui.h canvas.h

if HAVE_GNOME
bin_PROGRAMS = glame
noinst_LTLIBRARIES = 
endif

glame_SOURCES = gui.h gui.c main.c canvas.h canvas.c canvas_types.c

glame_LDADD = ../plugins/libglamebuiltins.la ../glmid/libglame.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS)
glame_LDFLAGS = -export-dynamic

