SUBDIRS = libgtkwaveform edit_filter

INCLUDES = -Iedit_filter -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@srcdir@/libgtkwaveform $(GLAME_CFLAGS) -D_REENTRANT -D_NO_FILTER_COMPATIBILITY -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS) $(XML_CFLAGS)


EXTRA_DIST = glame.desktop

Multimediadir = $(datadir)/gnome/apps/Multimedia

if HAVE_GNOME
bin_PROGRAMS = glame
#bin_PROGRAMS = wavetest timeline
noinst_LTLIBRARIES = 
Multimedia_DATA = glame.desktop
endif

glame_SOURCES = main.c \
	gltreeitem.c gltreeitem.h swapfilegui.c swapfilegui.h \
	waveeditgui.c waveeditgui.h gui_main.c
glame_LDADD = ../plugins/libglamebuiltins.la ../glmid/libglame.la edit_filter/libeditfilter.la \
        libgtkwaveform/libgtkwaveform.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS) $(XML_LIBS)
glame_LDFLAGS = -export-dynamic

#timeline_SOURCES = timeline.c
#timeline_LDADD = $(GNOME_LIBDIR) $(GNOME_LIBS) $(GNOMEUI_LIBS)

#wavetest_SOURCES = gtkwavedraw.c gtkwavedraw.h wavetest.c
#wavetest_LDADD = ../plugins/libglamebuiltins.la ../glmid/libglame.la \
#	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
#wavetest_LDFLAGS = -export-dynamic

