INCLUDES = -I@top_srcdir@/src/include -I@top_srcdir@/src/filter -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/hash -I@top_srcdir@/src/glmid $(GLAME_CFLAGS) -D_REENTRANT -D_NO_FILTER_COMPATIBILITY -DPKGDATADIR=\"$(pkgdatadir)\" $(GNOME_INCLUDEDIR) $(GNOMEUI_CFLAGS)


EXTRA_DIST = 

Multimediadir = $(datadir)/gnome/apps/Multimedia

if HAVE_GNOME
bin_PROGRAMS = glame_editor
noinst_LTLIBRARIES = 
endif

glame_editor_SOURCES = glame_editor.h glame_editor.c main.c

glame_editor_LDADD = ../../plugins/libglamebuiltins.la ../../glmid/libglame.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS) $(GLAME_FFT_LIBS)
glame_editor_LDFLAGS = -export-dynamic

