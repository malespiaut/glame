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
	libgtkwaveform/grange.c libgtkwaveform/grange.h \
	libgtkwaveform/gwavefile.c libgtkwaveform/gwavefile.h \
	libgtkwaveform/gtkwavebuffer.c libgtkwaveform/gtkwavebuffer.h \
	libgtkwaveform/gtkeditablewavebuffer.c libgtkwaveform/gtkeditablewavebuffer.h \
	libgtkwaveform/gtkwaveview.c libgtkwaveform/gtkwaveview.h \
	libgtkwaveform/gtkswapfilebuffer.c libgtkwaveform/gtkswapfilebuffer.h \
	\
	util/glame_gui_utils.c util/glame_gui_utils.h \
	util/glame_curve.c util/glame_curve.h \
	util/glame_param.c util/glame_param.h \
	util/glame_ruler.c util/glame_ruler.h util/glame_hruler.c util/glame_hruler.h \
	util/gtknob.c util/gtknob.h \
	\
	edit_filter/canvasfilter.c edit_filter/canvaspipe.c edit_filter/canvasport.c \
	edit_filter/glamecanvas.c edit_filter/canvasitem.h edit_filter/glamecanvas.h \
	edit_filter/filtereditgui.c edit_filter/filtereditgui.h \
	\
	timeline/timeline_canvas.h timeline/timeline_canvas.c \
	timeline/timeline.h timeline/timeline.c \
	\
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
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) \
	$(XML_LIBS) -lm $(LIBLTDL)
glame_LDFLAGS = -export-dynamic -dlopen self

#wavetest_SOURCES = gtkwavedraw.c gtkwavedraw.h wavetest.c
#wavetest_LDADD = ../plugins/libglamebuiltins.la ../glmid/libglame.la \
#	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
#wavetest_LDFLAGS = -export-dynamic

