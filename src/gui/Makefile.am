INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_CFLAGS) -D_REENTRANT $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)


EXTRA_DIST = gui.h canvas.h

if HAVE_GTK
if HAVE_GNOME
noinst_PROGRAMS = main
noinst_LTLIBRARIES = 
endif
endif

main_SOURCES = gui.h gui.c main.c canvas.h canvas.c canvas_types.c

main_LDADD = ../plugins/libbuiltins.la ../glmid/libglame.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS)
main_LDFLAGS = -export-dynamic

