INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_CFLAGS) -D_REENTRANT -D_NO_FILTER_COMPATIBILITY -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)


EXTRA_DIST = gui.h canvas.h glame.desktop

Multimediadir = $(datadir)/gnome/apps/Multimedia

if HAVE_GNOME
bin_PROGRAMS = edit_filter
#bin_PROGRAMS = glame wavetest
noinst_LTLIBRARIES = 
Multimedia_DATA = glame.desktop
endif

eglame_SOURCES = gui.h gui.c main.c canvas.h canvas.c canvas_types.c

eglame_LDADD = ../../plugins/libglamebuiltins.la ../../glmid/libglame.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
eglame_LDFLAGS = -export-dynamic


#wavetest_SOURCES = gtkwavedraw.c gtkwavedraw.h wavetest.c
#wavetest_LDADD = ../../plugins/libglamebuiltins.la ../../glmid/libglame.la \
#	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
#wavetest_LDFLAGS = -export-dynamic

