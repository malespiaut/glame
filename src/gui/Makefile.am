SUBDIRS = libgtkwaveform edit_filter

INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_CFLAGS) -D_REENTRANT -D_NO_FILTER_COMPATIBILITY -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS) $(XML_CFLAGS)


EXTRA_DIST = glame.desktop \
	timeline.c treetest.c tree.c tree.h wavetest.c

Multimediadir = $(datadir)/gnome/apps/Multimedia

if HAVE_GNOME
gnome_targets = timeline wavetest
gnome_xml_targets = treetest
else
gnome_targets =
gnome_xml_targets =
endif

if HAVE_XML
xml_targets = $(gnome_xml_targets)
else
xml_targets =
endif

bin_PROGRAMS = $(gnome_targets) $(xml_targets)
Multimedia_DATA = glame.desktop

timeline_SOURCES = timeline.c
timeline_LDADD = $(GNOME_LIBDIR) $(GNOME_LIBS) $(GNOMEUI_LIBS)

treetest_SOURCES = tree.h tree.c treetest.c
treetest_LDADD = ../swapfile/libswapfile.la \
	$(GNOME_LIBDIR) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(XML_LIBS)

wavetest_SOURCES = gtkwavedraw.c gtkwavedraw.h wavetest.c
wavetest_LDADD = ../plugins/libglamebuiltins.la ../glmid/libglame.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
wavetest_LDFLAGS = -export-dynamic


