SUBDIRS = include data hash swapfile filter glmath glmid plugins gui

INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid \
	$(GUILE_INCS) $(GLAME_CFLAGS) $(GLAME_AUDIO_CFLAGS) $(GLAME_DCFLAGS)

if GUILE
bin_PROGRAMS = cglame
endif

cglame_SOURCES = cglame.c
cglame_LDADD = swapfile/libswapfile.la filter/libfilter.la hash/libhash.la glmid/libglmid.la \
	plugins/libbuiltins.la \
	$(GLAME_LIBS) $(GLAME_DLIBS) $(GLAME_AUDIO_LIBS) \
	$(GUILE_LIBS) -lm -lpthread

