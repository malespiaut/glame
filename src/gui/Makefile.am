SUBDIRS = libgtkwaveform util edit_filter timeline

INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/gui -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@top_srcdir@/src/gui $(GLAME_CFLAGS) -D_REENTRANT -DPKGPIXMAPSDIR=\"$(pkgpixmapsdir)\" -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(XML_CFLAGS)


EXTRA_DIST = glame.desktop default-accels

Multimediadir = $(datadir)/gnome/apps/Multimedia

if HAVE_GNOME
bin_PROGRAMS = glame
#bin_PROGRAMS = wavetest timeline
Multimedia_DATA = glame.desktop
endif

pkgdata_DATA = default-accels

glame_SOURCES = \
	glame_accelerator.c glame_accelerator.h \
	glame_console.c glame_console.h \
	main.c \
	gltreeitem.c gltreeitem.h swapfilegui.c swapfilegui.h \
	waveeditgui.c waveeditgui.h apply.c importexport.c importexport.h
glame_LDADD =  \
	../glmid/libglmid.la \
	../plugins/libglamebuiltins.la \
	../glmid/libglameaudiofile.la \
	../filter/libfilter.la \
	../hash/libhash.la \
	../swapfile/libswapfile.la \
	edit_filter/libeditfilter.la \
        libgtkwaveform/libgtkwaveform.la timeline/libtimeline.la \
	util/libglameguiutils.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(XML_LIBS) -lm ../../libltdl/libltdlc.la
glame_LDFLAGS = -export-dynamic -dlopen self

#wavetest_SOURCES = gtkwavedraw.c gtkwavedraw.h wavetest.c
#wavetest_LDADD = ../plugins/libglamebuiltins.la ../glmid/libglame.la \
#	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
#wavetest_LDFLAGS = -export-dynamic

