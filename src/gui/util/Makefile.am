INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/glmid -I@top_srcdir@/src/hash -I@top_srcdir@/src/gui \
	-DPKGPIXMAPSDIR=\"$(pkgpixmapsdir)\" -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS) $(GNOME_INCLUDEDIR)

EXTRA_DIST = glame_ruler.c glame_ruler.h glame_hruler.c glame_hruler.h gtknob.xpm

if HAVE_GNOME
noinst_LTLIBRARIES = libglameguiutils.la
endif

libglameguiutils_la_SOURCES = \
	glame_gui_utils.c glame_gui_utils.h \
	glame_curve.c glame_curve.h \
	glame_param.c glame_param.h \
	glame_ruler.c glame_ruler.h glame_hruler.c glame_hruler.h \
	gtknob.c gtknob.h

