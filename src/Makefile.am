SUBDIRS = include libfixp hash swapfile channel filter rw

INCLUDES = -I@srcdir@/include -I@srcdir@/filter -I@srcdir@/swapfile -I@srcdir@/rw -I@srcdir@/channel -I@srcdir@/hash

bin_PROGRAMS = glame cglame


glame_SOURCES = glame.c
glame_LDADD = channel/libchannel.la swapfile/libswapfile.la filter/libfilter.la rw/librw.la hash/libhash.la \
	$(DEBUG_LIBS) $(ESD_LIBS) $(AUDIOFILE_LIBS) -lm -lpthread

cglame_SOURCES = cglame.c
cglame_LDADD = channel/libchannel.la swapfile/libswapfile.la filter/libfilter.la rw/librw.la hash/libhash.la \
	$(DEBUG_LIBS) $(ESD_LIBS) $(AUDIOFILE_LIBS) -lm -lpthread




