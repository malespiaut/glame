SUBDIRS = include data hash swapfile channel filter plugin glmid gui glmath

INCLUDES = -I@srcdir@/include -I@srcdir@/filter -I@srcdir@/swapfile -I@srcdir@/channel -I@srcdir@/hash -I@srcdir@/glmid $(GUILE_INCS) $(GLAME_CFLAGS) $(GLAME_AUDIO_CFLAGS) $(GLAME_DCFLAGS)

if GUILE
bin_PROGRAMS = cglame
endif


#glame_SOURCES = glame.c
#glame_LDADD = channel/libchannel.la swapfile/libswapfile.la filter/libfilter.la hash/libhash.la \
#	$(GLAME_LIBS) -lm -lpthread

cglame_SOURCES = cglame.c
cglame_LDADD = channel/libchannel.la swapfile/libswapfile.la filter/libfilter.la hash/libhash.la glmid/libglmid.la \
	$(GLAME_LIBS) $(GLAME_DLIBS) $(GLAME_AUDIO_LIBS) \
	$(GUILE_LIBS) -lm -lpthread




