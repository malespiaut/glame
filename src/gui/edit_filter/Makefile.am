INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_CFLAGS) -D_REENTRANT -D_NO_FILTER_COMPATIBILITY -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)


EXTRA_DIST = gui.h canvas.h glame.desktop

Multimediadir = $(datadir)/gnome/apps/Multimedia

if HAVE_GNOME
bin_PROGRAMS = edit_filter
noinst_LTLIBRARIES = 
Multimedia_DATA = glame.desktop
endif

edit_filter_SOURCES = gui.h gui.c main.c canvas.h canvas.c canvas_types.c

edit_filter_LDADD = ../../plugins/libglamebuiltins.la ../../glmid/libglame.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
edit_filter_LDFLAGS = -export-dynamic

