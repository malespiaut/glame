SUBDIRS = libgtkwaveform edit_filter timeline

INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/gui -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@srcdir@/libgtkwaveform -I@srcdir@/edit_filter -I@srcdir@/timeline $(GLAME_CFLAGS) -D_REENTRANT -DPKGPIXMAPSDIR=\"$(pkgpixmapsdir)\" -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS) $(XML_CFLAGS)


EXTRA_DIST = glame.desktop default-accels

Multimediadir = $(datadir)/gnome/apps/Multimedia

if HAVE_GNOME
bin_PROGRAMS = glame
#bin_PROGRAMS = wavetest timeline
noinst_LTLIBRARIES = libglameguiutils.la
Multimedia_DATA = glame.desktop
endif

pkgdata_DATA = default-accels

libglameguiutils_la_SOURCES = glame_gui_utils.c glame_gui_utils.h

glame_SOURCES = \
	glame_accelerator.c glame_accelerator.h \
	glame_console.c glame_console.h \
	main.c \
	gltreeitem.c gltreeitem.h swapfilegui.c swapfilegui.h \
	waveeditgui.c waveeditgui.h apply.c
glame_LDADD = ../glmid/libglame.la edit_filter/libeditfilter.la \
        libgtkwaveform/libgtkwaveform.la timeline/libtimeline.la \
	libglameguiutils.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(XML_LIBS)
glame_LDFLAGS = -export-dynamic

#wavetest_SOURCES = gtkwavedraw.c gtkwavedraw.h wavetest.c
#wavetest_LDADD = ../plugins/libglamebuiltins.la ../glmid/libglame.la \
#	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
#wavetest_LDFLAGS = -export-dynamic

