INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/gui -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid -I@top_srcdir@/src/gui $(GLAME_CFLAGS) -D_REENTRANT -DPKGPIXMAPSDIR=\"$(pkgpixmapsdir)\" -DPKGDATADIR=\"$(pkgdatadir)\" $(GLAME_GUI_INCLUDEDIR) $(XML_CFLAGS)

EXTRA_DIST = glame.desktop default-accels \
	util/gtknob.xpm libgtkwaveform/rec.xpm libgtkwaveform/mute.xpm \
	libgtkwaveform/sel.xpm \
	edit_filter/edit_filter_marshal.list

Multimediadir = $(datadir)/gnome/apps/Multimedia

if BUILD_GUI
bin_PROGRAMS = glame
Multimedia_DATA = glame.desktop
endif

pkgdata_DATA = default-accels

BUILT_SOURCES = edit_filter/edit_filter_marshal.h edit_filter/edit_filter_marshal.c

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
	util/glame_dnd.h \
	\
	edit_filter/canvasfilter.c edit_filter/canvaspipe.c edit_filter/canvasport.c \
	edit_filter/glamecanvas.c edit_filter/canvasitem.h edit_filter/glamecanvas.h \
	edit_filter/filtereditgui.c edit_filter/filtereditgui.h \
	edit_filter/edit_filter_marshal.h edit_filter/edit_filter_marshal.c \
	\
	glame_accelerator.c glame_accelerator.h \
	glame_console.c glame_console.h \
	main.c \
	tree/gltree.h tree/gltree.c \
	tree/gpsmstore.h tree/gpsmstore.c \
	waveeditgui.c waveeditgui.h apply.c importexport.c importexport.h

glame_LDADD =  \
	../glmid/libglmid.la \
	../plugins/libglamebuiltins.la \
	../glmid/libglameaudiofile.la \
	../filter/libfilter.la \
	../hash/libhash.la \
	../swapfile/libswapfile.la \
	$(GLAME_GUI_LIBDIR) $(GLAME_GUI_LIBS) \
	-lm $(LIBLTDL) $(LIBINTL)
glame_LDFLAGS = -export-dynamic -dlopen self

edit_filter/edit_filter_marshal.h: edit_filter/edit_filter_marshal.list
	( @GLIB_GENMARSHAL@ --prefix=edit_filter_marshal edit_filter/edit_filter_marshal.list --header > edit_filter_marshal.tmp \
	&& mv edit_filter_marshal.tmp edit_filter/edit_filter_marshal.h ) \
	|| ( rm -f edit_filter_marshal.tmp && exit 1 )

edit_filter/edit_filter_marshal.c: edit_filter/edit_filter_marshal.h
	( @GLIB_GENMARSHAL@ --prefix=edit_filter_marshal edit_filter/edit_filter_marshal.list --body > edit_filter_marshal.tmp \
	&& mv edit_filter_marshal.tmp edit_filter/edit_filter_marshal.c ) \
	|| ( rm -f edit_filter_marshal.tmp && exit 1 )
