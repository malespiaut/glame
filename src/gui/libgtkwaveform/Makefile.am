INCLUDES = -I@top_srcdir@/src/swapfile -I@top_srcdir@/src/include \
	-I@top_srcdir@/src/glmid -I@top_srcdir@/src/hash \
	$(GNOME_INCLUDEDIR)

if HAVE_GNOME
noinst_LTLIBRARIES = libgtkwaveform.la
#noinst_PROGRAMS = test
endif


libgtkwaveform_la_SOURCES = \
  gtkswapfilebuffer.c      gtkswapfilebuffer.h  \
  gwavefile.c              gwavefile.h          \
  gtkeditablewavebuffer.c  gtkeditablewavebuffer.h \
  gtkwavebuffer.c          gtkwavebuffer.h      \
  gtkwaveview.c            gtkwaveview.h        \
  grange.c                 grange.h             \
  gtkwave.c                gtkwave.h


#test_SOURCES = test.c
#test_LDADD = ../../swapfile/libswapfile.la libgtkwaveform.la \
#	$(GNOME_LIBDIR) $(GLAME_LIBS) $(GNOME_LIBS) $(GNOMEUI_LIBS)


