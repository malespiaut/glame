INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/gui -I@top_srcdir@/src/gui/edit_filter -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_CFLAGS) -D_REENTRANT -D_NO_FILTER_COMPATIBILITY -DPKGPIXMAPSDIR=\"$(pkgpixmapsdir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)


EXTRA_DIST = canvas.h main.c

Multimediadir = $(datadir)/gnome/apps/Multimedia

if HAVE_GNOME
#bin_PROGRAMS = edit_filter
noinst_LTLIBRARIES = libeditfilter.la
endif

libeditfilter_la_SOURCES = gui.c canvas.h canvas.c \
	canvas_types.h canvas_types.c filtergui.h

#edit_filter_SOURCES = main.c
#edit_filter_LDADD = ../../plugins/libglamebuiltins.la ../../glmid/libglame.la \
#	libeditfilter.la \
#	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
#edit_filter_LDFLAGS = -export-dynamic

