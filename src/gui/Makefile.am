INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash $(GLAME_CFLAGS) -D_REENTRANT $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)


EXTRA_DIST = gui.h canvas.h

if HAVE_GTK
if HAVE_GNOME
noinst_PROGRAMS = main
noinst_LTLIBRARIES = 
endif
endif

main_SOURCES = gui.h gui.c main.c canvas.h canvas.c canvas_types.c

main_LDADD = ../filter/libfilter.la ../plugins/libbuiltins.la ../glmid/libglmid.la ../swapfile/libswapfile.la ../hash/libhash.la -lm -lpthread $(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS)
