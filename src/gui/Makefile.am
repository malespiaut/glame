INCLUDES = -I@srcdir@/../include -I@srcdir@/../filter -I@srcdir@/../swapfile -I@srcdir@/../hash -I@srcdir@/../channel $(GLAME_CFLAGS) -D_REENTRANT $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)


EXTRA_DIST =

if HAVE_GTK
if HAVE_GNOME
noinst_PROGRAMS = main
noinst_LTLIBRARIES = 
endif
endif

main_SOURCES = gui.c main.c

main_LDADD = ../filter/libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lm -lpthread $(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS)
