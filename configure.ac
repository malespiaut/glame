dnl Process this file with autoconf to produce a configure script.

AC_INIT(glame, 1.1CVS)
AC_CONFIG_SRCDIR(configure.ac)
AM_INIT_AUTOMAKE([subdir-objects])
AM_CONFIG_HEADER(config.h)

dnl Disable re-creation of *.in files by default
AM_MAINTAINER_MODE

dnl Recognize CC and CFLAGS
AC_ARG_VAR(CC, [compiler])
AC_ARG_VAR(CFLAGS, [compiler flags])


dnl
dnl There are two sets of include/libdir/libs variables, one for
dnl the general stuff, one for the gui stuff (which includes the
dnl general set). CFLAGS generally belong to the CFLAGS variable.
dnl
dnl  GLAME_INCLUDEDIR GLAME_LIBDIR GLAME_LIBS
dnl  GLAME_GUI_INCLUDEDIR GLAME_GUI_LIBDIR GLAME_GUI_LIBS
dnl

GLAME_INCLUDEDIR=""
GLAME_LIBDIR=""
GLAME_LIBS=""
GLAME_GUI_INCLUDEDIR=""
GLAME_GUI_LIBDIR=""
GLAME_GUI_LIBS=""
AC_SUBST(GLAME_INCLUDEDIR)
AC_SUBST(GLAME_LIBDIR)
AC_SUBST(GLAME_LIBS)
AC_SUBST(GLAME_GUI_INCLUDEDIR)
AC_SUBST(GLAME_GUI_LIBDIR)
AC_SUBST(GLAME_GUI_LIBS)


dnl
dnl Switches to configure debugging stuff - sane default for now.
dnl

dnl Swapfile debugginglevel is seperate (but enables other debugging)
AC_ARG_ENABLE(swapfiledebug,
[  --enable-swapfiledebug  include lots of timeconsuming checks to swapfile 
  --disable-swapfiledebug compile for maximum optimization], swdebug=$enableval, swdebug="no")
if test "$swdebug" = "yes"; then
	AC_DEFINE(SWDEBUG,, [swapfile debugging])
fi

dnl Global debug flag
AC_ARG_ENABLE(debug,
[  --enable-debug          include lots of debugging code 
  --disable-debug         compile for maximum optimization], glame_debug=$enableval, glame_debug="")
if test "$swdebug" = "yes" -o "$glame_debug" = "yes"; then
	AC_DEFINE(DEBUG,, [general debugging])
	glame_debug_gcc_cflags="-g -O0"
elif test "$glame_debug" = "no"; then
	AC_DEFINE(NDEBUG,, [disable debugging code])
	glame_debug_gcc_cflags="-O3 -ffast-math -fstrict-aliasing"
else
	if test -z "$CFLAGS"; then
		CFLAGS="-O2"
	fi
	glame_debug_gcc_cflags="$CFLAGS"
fi


dnl
dnl Checks for programs.
dnl

dnl Compiling without gcc is not supported but may succeed.
AC_PROG_CC
if test "$GCC" = "yes"; then
	AC_DEFINE(HAVE_GCC,, [using gcc for build])
	CFLAGS="-Wall $glame_debug_gcc_cflags"
else
  AC_MSG_ERROR([Compilers other than gcc are not supported])
fi
AM_PROG_CC_C_O

AC_PROG_INSTALL
AC_LIBLTDL_CONVENIENCE
dnl libtool pre-1.4 used INCLTDL, but LTDLINCL is the proper namespace.
if test -z "${LTDLINCL}" -a ! -z "${INCLTDL}"; then
	LTDLINCL="${INCLTDL}"
fi
AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL


dnl Translations so far
ALL_LINGUAS="de fr it da"

dnl Check for internationalization support
AM_GNU_GETTEXT
AM_GNU_GETTEXT_VERSION(0.11.5)


dnl FIXME: The next two checks break cross-compiling.
dnl        Unfortunately run-time checks would mean performance impacts or are
dnl        simply not an option. [dk]

dnl Check endianness
AC_C_BIGENDIAN
AC_CHECK_HEADER(byteswap.h,AC_DEFINE(HAVE_BYTESWAP_H,, [have byteswap.h header]),)

dnl Check width of basic types
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)

dnl Check if we have certain functions and #define them
dnl to fallbacks if not
ACG_CHECK_STUFF


dnl
dnl Switch to make SAMPLE a double - float is default.
dnl -- disabled.
dnl

dnl AC_ARG_ENABLE(double, [  --enable-double         use doubles instead of floats (not recommended)], sample=$enableval, sample=float)
dnl if test "$sample" = "no"; then
	sample=float
dnl elif test "$sample" = "yes"; then
dnl	sample=double
dnl	AC_MSG_WARN([Using double for SAMPLE which is not supported])
dnl fi
if test "$sample" = "float"; then
	AC_DEFINE(SAMPLE_FLOAT,, [use float as sample type])
fi
AC_DEFINE_UNQUOTED(SAMPLE, $sample, [type to use as sample])



dnl
dnl Checks for libraries.
dnl



dnl
dnl First check all _required_ stuff
dnl


dnl
dnl pthread lib - GLAME is threaded everywhere, so global
dnl LIBS and CFLAGS use is necessary
dnl

ACX_PTHREAD([
LIBS="$PTHREAD_LIBS"
dnl FIXME: do we need -D_REENTRANT here?
CFLAGS="$CFLAGS $PTHREAD_CFLAGS -D_REENTRANT"
CC="$PTHREAD_CC"
], [
AC_MSG_ERROR(You need pthreads to run glame!)
])

dnl
dnl GUILE check, we absolutely need it, so fail if its not there
dnl the check has AM_CONDITIONAL(GUILE, ...) and defines HAVE_GUILE
dnl

ACG_CHECK_GUILE(1.4.0, , [
AC_MSG_ERROR([You need at least guile 1.4.0 to run glame!
Hint: You also need to have the appropriate guile development package
installed, usually called libguile-dev, guile-devel, or similar.])
])
GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $GUILE_INCS"
GLAME_LIBS="$GLAME_LIBS $GUILE_LIBS"


dnl
dnl The rest is optional stuff
dnl


dnl
dnl GUI stuff - require GNOME for anything
dnl

dnl switch to disable gui
AC_ARG_ENABLE(gui, [  --disable-gui           GUI - needs recent GTK/GNOME ], gui=$enableval, gui=yes)

AM_CONDITIONAL(BUILD_GUI, test x$gui = xyes)

dnl central GUI includes/libs

if test x$gui = xyes; then
dnl GNOME2 check
dnl  packages="libgnome-2.0 libgnomeui-2.0 libgnomecanvas-2.0 libglade-2.0 gobject-2.0 glib-2.0"
  packages="libgnome-2.0 libgnomeui-2.0 libgnomecanvas-2.0 gobject-2.0 glib-2.0"
  AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
  if test x$PKG_CONFIG = xno; then
    AC_MSG_ERROR([You need pkg-config installed to use gnome2 libraries])
  fi
  AC_MSG_CHECKING(for required packages)
  if ! $PKG_CONFIG --exists $packages; then
    AC_MSG_ERROR([You need $packages installed and registered with pkg-config])
  fi
  AC_MSG_CHECKING(for dependencies installed correctly)
  if $PKG_CONFIG --uninstalled $packages; then
    AC_MSG_ERROR([You need further dependencies for $packages installed and registered with pkg-config])
  fi
  AC_MSG_CHECKING(for gnome2 includes)
  GLAME_GUI_INCLUDEDIR="$GLAME_GUI_INCLUDEDIR `$PKG_CONFIG --cflags $packages`"
  AC_MSG_RESULT($GLAME_GUI_INCLUDEDIR)
  AC_MSG_CHECKING(for gnome2 libraries)
  GLAME_GUI_LIBDIR="$GLAME_GUI_LIBDIR `$PKG_CONFIG --libs-only-L $packages`"
  GLAME_GUI_LIBS="$GLAME_GUI_LIBS `$PKG_CONFIG --libs-only-l $packages`"
  GLIB_GENMARSHAL="`$PKG_CONFIG --variable=glib_genmarshal glib-2.0`"
  AC_SUBST(GLIB_GENMARSHAL)
  AC_MSG_RESULT($GLAME_GUI_LIBDIR $GLAME_GUI_LIBS)
  AC_DEFINE(HAVE_GNOME2, 1, [define if you have gnome2 libs])
dnl disabled glade support until we fix it 
dnl AC_DEFINE(HAVE_LIBGLADE,, [wether we have libglade])
  AMG_PATH_XML2(2.0.0, , [
	AC_MSG_ERROR([You need at least libxml 2.0.0 to run glame!
Hint: You also need to have the appropriate xml development package
installed, usually called libxml-dev, libxmld, or similar.])
])
  GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $XML_CFLAGS"
  GLAME_LIBS="$GLAME_LIBS $XML_LIBS"
  CFLAGS="$CFLAGS -DGTK_ENABLE_BROKEN"

else
dnl no GUI
  AC_MSG_RESULT([GUI was disabled.])
fi

dnl check for libstroke
AC_ARG_ENABLE(libstroke, [  --enable-libstroke     enable libstroke support], strokes=$enableval, strokes=no)
if test x$strokes = xyes; then
dnl first check for normal positions
  AC_CHECK_HEADER(stroke.h, strokes=yes, strokes=no)
if test x$strokes = xno; then
dnl look for debian version
  stroke_save_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="-I/usr/X11R6/include"
	AC_CHECK_HEADERS([stroke.h X11/stroke.h], strokes=yes, strokes=no)
  CPPFLAGS="$stroke_save_CPPFLAGS"
	if test x$strokes = xyes; then
		GLAME_GUI_INCLUDEDIR="$GLAME_GUI_INCLUDEDIR -I/usr/X11R6/include"
		GLAME_GUI_LIBDIR="$GLAME_GUI_LIBDIR -L/usr/X11R6/lib"
		GLAME_GUI_LIBS="$GLAME_GUI_LIBS -lstroke"
	fi
  fi
  if test x$strokes = xyes; then
	AC_DEFINE(HAVE_LIBSTROKE,, [have/use libstroke])
  fi
fi



dnl
dnl We also need libxml -- dont check, if we build the GUI as the check
dnl for gui libs includes check for appropriate libxml
dnl

if test x$gui = xno; then
AMG_PATH_XML2(2.0.0, , [
	AC_MSG_ERROR([You need at least libxml 2.0.0 to run glame!
Hint: You also need to have the appropriate xml development package
installed, usually called libxml-dev, libxmld, or similar.])
])
	GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $XML_CFLAGS"
	GLAME_LIBS="$GLAME_LIBS $XML_LIBS"
fi


dnl
dnl check for single/double precision fftw
dnl
if test "$sample" = "float"; then
	AC_CHECK_LIB(sfftw, fftw2d_create_plan, fftw=yes, fftw=no)
	AC_CHECK_HEADER(sfftw.h,, fftw=no)
	if test $fftw = yes; then
		GLAME_LIBS="$GLAME_LIBS -lsfftw -lsrfftw"
		AC_DEFINE(HAVE_FFTW,, [have fftw])
	fi
elif test "$sample" = "double"; then
	AC_CHECK_LIB(fftw, fftw2d_create_plan, fftw=yes, fftw=no)
	AC_CHECK_HEADER(fftw.h,, fftw=no)
	if test $fftw = yes; then
		GLAME_LIBS="$GLAME_LIBS -lfftw -lrfftw"
		AC_DEFINE(HAVE_FFTW,, [have fftw])
	fi
fi
AM_CONDITIONAL(HAVE_FFTW, test x$fftw = xyes)


dnl
dnl Checks for various audio stuff.
dnl

dnl check for libaudiofile, use audiofile-config, if available
AC_CHECK_PROG(afconfig, audiofile-config, yes, no)
ac_saved_ldflags=$LDFLAGS
ac_saved_cppflags=$CPPFLAGS
if test $afconfig = yes; then
	LDFLAGS="$LDFLAGS `audiofile-config --libs`"
	CPPFLAGS="$CPPFLAGS `audiofile-config --cflags`"
fi
AC_CHECK_LIB(audiofile, afGetVirtualFrameSize, laf=yes, laf=no)
AC_CHECK_HEADER(audiofile.h,, laf=no)
LDFLAGS=$ac_saved_ldflags
CPPFLAGS=$ac_saved_cppflags
if test $laf = yes; then
        if test $afconfig = yes; then
		GLAME_LIBS="$GLAME_LIBS `audiofile-config --libs`"
		GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR `audiofile-config --cflags`"
	else
		GLAME_LIBS="$GLAME_LIBS $ac_audiofile_libs -laudiofile"
	fi
	AC_DEFINE(HAVE_AUDIOFILE,, [have audiofile])
fi


dnl check for OSS
dnl BSD keeps the header file
dnl in sys/, Linux in linux/, 
dnl other Unices in machine/
AC_CHECK_HEADER(linux/soundcard.h, oss=yes, oss=no)
if test $oss = yes; then
	AC_DEFINE(HAVE_OSS,, [support oss sound interface])
	AC_DEFINE(HAVE_OSS_LINUX,, [linux oss system])
else
	AC_CHECK_HEADER(sys/soundcard.h, oss=yes, oss=no)
	if test $oss = yes; then
		AC_DEFINE(HAVE_OSS,, [support oss sound interface])
		AC_DEFINE(HAVE_OSS_SYS,, [soundcard.h in sys])
	else 
		AC_CHECK_HEADER(machine/soundcard.h, oss=yes, oss=no)
		if test $oss = yes; then
			AC_DEFINE(HAVE_OSS,, [support oss sound interface])
			AC_DEFINE(HAVE_OSS_MACHINE,, [soundcard.h in machine])
		fi
	fi
fi
AM_CONDITIONAL(HAVE_OSS, test x$oss = xyes)

dnl check for esd
ACG_PATH_ESD(0.2.0, esd=yes, esd=no)
if test $esd = yes; then
	GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $ESD_CFLAGS"
	AC_DEFINE(HAVE_ESD,, [have esd daemon])
fi
AM_CONDITIONAL(HAVE_ESD, test x$esd = xyes)

dnl check for native IRIX audio
AC_CHECK_LIB(audio, alWriteBuffers, sgi=yes, sgi=no)
AC_CHECK_HEADER(dmedia/audio.h,, sgi=no)
if test $sgi = yes; then
	GLAME_LIBS="$GLAME_LIBS -laudio"
	AC_DEFINE(HAVE_SGIAUDIO,, [have irix audio interface])
fi
AM_CONDITIONAL(HAVE_SGI, test x$sgi = xyes)

dnl check for ALSA
ACG_PATH_ALSA(0.9.0, alsa="yes", alsa="no")
if test "x$alsa" = xyes; then
	GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $ALSA_CFLAGS"
	GLAME_LIBS="$GLAME_LIBS $ALSA_LIBS"
	AC_DEFINE(HAVE_ALSA,, [have alsa library])
fi
AM_CONDITIONAL(HAVE_ALSA, test x$alsa = xyes)

dnl check for Jack
AC_CHECK_HEADER(jack/jack.h, jack=yes, jack=no)
if test x$jack = xyes; then
	GLAME_CFLAGS="$GLAME_CFLAGS `pkg-config --cflags jack`"
	GLAME_LIBS="$GLAME_LIBS `pkg-config --libs jack`"
	AC_DEFINE(HAVE_JACK,, [have jack library])
fi
AM_CONDITIONAL(HAVE_JACK, test x$jack = xyes)

dnl
dnl Check for LADSPA support
dnl
ACG_PATH_LADSPA([AC_DEFINE(HAVE_LADSPA,, [have ladspa.h])])
GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $LADSPA_CFLAGS"


dnl
dnl UNIX flavours compatibility stuff
dnl

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_DIRENT
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(fcntl.h sys/file.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_OFF_T
AC_STRUCT_ST_BLOCKS
AC_TYPE_SIGNAL
AC_TYPE_PID_T


dnl Checks for library functions.
AC_FUNC_MMAP
AC_FUNC_MADVISE
AC_FUNC_ALLOCA


dnl special code hooks for os
case "$host_os" in
*linux*)
	AC_DEFINE(OS_LINUX,, [am linux]);;
*bsd*)
	AC_DEFINE(OS_BSD,, [am bsd]);;
*irix*)
	AC_DEFINE(OS_IRIX,, [am irix]);;
esac

dnl
dnl Checks for asm code support
dnl


dnl
dnl Disable fast assembler routines, use slower C code instead.
dnl

AC_ARG_ENABLE(asm,
[  --disable-asm           disable fast assembler routines ], asm=$enableval, asm=yes)

dnl special asm code hooks for cpu -- require GCC.
if test "$asm" = "yes" -a "x$GCC" = "xyes"; then
	case "$host_cpu" in
	i?86)
		AC_DEFINE(CPU_X86,, [x86 cpu]);;
	mips*)
		AC_DEFINE(CPU_MIPS,, [mips cpu])
		ACG_CHECK_MIPS_LL_SC([
			AC_DEFINE(HAVE_MIPS_LL_SC,, [modern mips cpu with ll sc])
		]);;
	powerpc)
		AC_DEFINE(CPU_PPC,, [ppc cpu]);;
	alpha*)
		AC_DEFINE(CPU_ALPHA,, [alpha cpu]);;
	*)
		AC_DEFINE(NO_ASM,, [no asm support]);;
	esac
else
	AC_DEFINE(NO_ASM,, [no asm support])
fi

dnl
dnl Check for gcc SIMD support
dnl

AC_ARG_ENABLE(simd,
[  --disable-simd          disable fast simd routines ], simd=$enableval, simd=yes)
if test x$simd = xyes; then
  ACG_CHECK_GCC_VECTOR_EXTENSION([AC_DEFINE(HAVE_GCC_SIMD,, [gcc SIMD extensions])])
else
  AC_MSG_NOTICE([SIMD support disabled])
fi


dnl At the very last, concat GLAME_* to GLAME_GUI_*
GLAME_GUI_INCLUDEDIR="$GLAME_INCLUDEDIR $GLAME_GUI_INCLUDEDIR"
GLAME_GUI_LIBDIR="$GLAME_LIBDIR $GLAME_GUI_LIBDIR"
GLAME_GUI_LIBS="$GLAME_LIBS $GLAME_GUI_LIBS"


dnl
AM_CONDITIONAL(STATICLIBS, test x$enable_static = xyes)

dnl Several subdirs to clean up our own data layout.
pkgpixmapsdir='$(pkgdatadir)/pixmaps'
pkgscriptsdir='$(pkgdatadir)/scripts'

dnl Define install targets pkgpixmaps_FOO and pkgscrips_FOO.
AC_SUBST(pkgpixmapsdir)
AC_SUBST(pkgscriptsdir)

AC_SUBST(LIBLTDL)
AC_SUBST(LTDLINCL)
AC_CONFIG_SUBDIRS(libltdl)

AC_OUTPUT(Makefile macros/Makefile doc/Makefile doc/pix/Makefile data/Makefile data/pixmaps/Makefile src/Makefile src/filter/Makefile src/swapfile/Makefile src/hash/Makefile src/plugins/Makefile src/include/Makefile src/gui/Makefile src/glmid/Makefile intl/Makefile po/Makefile.in)
