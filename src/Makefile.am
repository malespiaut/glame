SUBDIRS = include libfixp hash swapfile channel filter rw gui

INCLUDES = -I@srcdir@/include -I@srcdir@/filter -I@srcdir@/swapfile -I@srcdir@/rw -I@srcdir@/channel -I@srcdir@/hash $(GLAME_CFLAGS)

bin_PROGRAMS = glame cglame


glame_SOURCES = glame.c
glame_LDADD = channel/libchannel.la swapfile/libswapfile.la filter/libfilter.la rw/librw.la hash/libhash.la \
	$(GLAME_LIBS) -lm -lpthread

cglame_SOURCES = cglame.c
cglame_LDADD = channel/libchannel.la swapfile/libswapfile.la filter/libfilter.la rw/librw.la hash/libhash.la \
	$(GLAME_LIBS) -lm -lpthread




