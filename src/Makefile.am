SUBDIRS = include hash swapfile filter plugins glmid gui

INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter \
	-I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash \
	-I@top_srcdir@/src/glmid \
	$(GUILE_INCS) $(GLAME_CFLAGS) $(GLAME_AUDIO_CFLAGS) $(GLAME_DCFLAGS)

bin_PROGRAMS = cglame

cglame_SOURCES = cglame.c
cglame_LDADD = \
	glmid/libglmid.la \
	plugins/libglamebuiltins.la \
	filter/libfilter.la \
	hash/libhash.la \
	swapfile/libswapfile.la \
	$(GLAME_LIBS) $(GLAME_DLIBS) ../libltdl/libltdlc.la
cglame_LDFLAGS = -export-dynamic -dlopen self

