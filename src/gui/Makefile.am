INCLUDES = -I@srcdir@/../include -I@srcdir@/../filter -I@srcdir@/../swapfile -I@srcdir@/../hash -I@srcdir@/../channel $(GLAME_CFLAGS) -D_REENTRANT $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)

noinst_PROGRAMS = main
noinst_LTLIBRARIES = 

EXTRA_DIST = 


main_SOURCES = gui.c main.c

main_LDADD = ../filter/libfilter.la ../swapfile/libswapfile.la ../hash/libhash.la ../channel/libchannel.la -lm -lpthread $(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS)

