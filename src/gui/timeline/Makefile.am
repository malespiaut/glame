INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/gui -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_CFLAGS) -D_REENTRANT -DPKGPIXMAPSDIR=\"$(pkgpixmapsdir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)

EXTRA_DIST =

if HAVE_GNOME
noinst_LTLIBRARIES = libtimeline.la
endif

libtimeline_la_SOURCES = timeline.h timeline.c \
	canvas_types.h canvas_types.c

