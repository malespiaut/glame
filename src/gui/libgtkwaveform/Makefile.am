INCLUDES = -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/include \
	$(GNOME_INCLUDEDIR)


noinst_LTLIBRARIES = libgtkwaveform.la

libgtkwaveform_la_SOURCES = \
  gtkswapfilebuffer.c      gtkswapfilebuffer.h  \
  gwavefile.c              gwavefile.h          \
  gtkeditablewavebuffer.c  gtkeditablewavebuffer.h \
  gtkwavebuffer.c          gtkwavebuffer.h      \
  gtkwaveview.c            gtkwaveview.h        \
  grange.c                 grange.h             \
  gtkwave.c                gtkwave.h


noinst_PROGRAMS = test

test_SOURCES = test.c
test_LDADD = ../../swapfile/libswapfile.la libgtkwaveform.la \
	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS)

