dnl Process this file with autoconf to produce a configure script.

AC_INIT(glame, 2.0CVS)
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
	AC_DEFINE(SWDEBUG, 1, [swapfile debugging])
fi

dnl Global debug flag
AC_ARG_ENABLE(debug,
[  --enable-debug          include lots of debugging code 
  --disable-debug         compile for maximum optimization], glame_debug=$enableval, glame_debug="")
if test "$swdebug" = "yes" -o "$glame_debug" = "yes"; then
	AC_DEFINE(DEBUG, 1, [general debugging])
	glame_debug_gcc_cflags="-O -g"
elif test "$glame_debug" = "no"; then
	AC_DEFINE(NDEBUG, 1, [disable debugging code])
	glame_debug_gcc_cflags="-O3 -funroll-loops -ffast-math -fomit-frame-pointer"
else
	glame_debug_gcc_cflags="-O2 -g -ffast-math"
fi


dnl
dnl Checks for programs.
dnl

dnl Compiling without gcc is not supported but may succeed.
AC_PROG_CC
if test "$GCC" = "yes"; then
	AC_DEFINE(HAVE_GCC, 1, [using gcc for build])
	if test "$ac_env_CFLAGS_set" = "set"; then
	  CFLAGS="$CFLAGS -Wall -Wno-sign-compare"
	else
	  CFLAGS="-Wall -Wno-sign-compare $glame_debug_gcc_cflags"
	fi
	extra_cflag_unroll="-funroll-loops"
	AC_SUBST(extra_cflag_unroll)
else
	AC_MSG_WARN([Compilers other than GCC may not be supported])
fi
AM_PROG_CC_C_O

dnl Apple's preprocessor on MacOS X causes headaches. Turning off
dnl preprocessing switches to a sane cpp.
AC_PROG_CPP
AC_MSG_CHECKING([how to mate preprocessor and vararg macros])
AC_PREPROC_IFELSE(
[
#define MACRO_WITH_VARARGS(dummy...)	/* nothing */
MACRO_WITH_VARARGS(foo, bar)
/* no body */
],
[
AC_MSG_RESULT([already in love])
],
[
CPPFLAGS="$CPPFLAGS -no-cpp-precomp"
AC_PREPROC_IFELSE(
[
#define MACRO_WITH_VARARGS(dummy...)	/* nothing */
MACRO_WITH_VARARGS(foo, bar)
/* no body */
],
[
AC_MSG_RESULT([-no-cpp-precomp])
],
[
AC_MSG_RESULT([no way])
AC_DEFINE([NO_VARARG_MACROS], 1, [GCC style macro vararg support])
dnl AC_MSG_ERROR([GLAME needs a C preprocessor that can handle macros with a variable number of arguments. Go get one...])
])
])


AC_MSG_CHECKING([whether we have restrict support in C])
acx_cv_c_restrict=unsupported
for acx_kw in restrict __restrict__ __restrict; do
  AC_TRY_COMPILE([], [float * $acx_kw x;], [acx_cv_c_restrict=$acx_kw; break])
done
if test "$acx_cv_c_restrict" = "unsupported"; then
  AC_MSG_RESULT([no])
dnl  CFLAGS="$CFLAGS -Drestrict="
  AC_DEFINE(restrict, 1, [No restrict support])
else
  AC_MSG_RESULT([$acx_cv_c_restrict])
  if test "$acx_cv_c_restrict" != "restrict"; then
dnl    CFLAGS="$CFLAGS -Drestrict=$acx_cv_c_restrict"
    AC_DEFINE_UNQUOTED(restrict, $acx_cv_c_restrict, [Restrict support])
  fi
fi


AC_PROG_INSTALL
AC_LIBLTDL_CONVENIENCE
dnl libtool pre-1.4 used INCLTDL, but LTDLINCL is the proper namespace.
if test -z "${LTDLINCL}" -a ! -z "${INCLTDL}"; then
	LTDLINCL="${INCLTDL}"
fi
AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL


dnl Translations so far
dnl ALL_LINGUAS="de fr it da"
ALL_LINGUAS="de fr"

dnl Check for internationalization support
AM_GNU_GETTEXT
AM_GNU_GETTEXT_VERSION(0.11.5)


dnl FIXME: The next two checks break cross-compiling.
dnl        Unfortunately run-time checks would mean performance impacts or are
dnl        simply not an option. [dk]

dnl Check endianness
AC_C_BIGENDIAN
AC_CHECK_HEADER(byteswap.h,AC_DEFINE(HAVE_BYTESWAP_H, 1, [have byteswap.h header]),)

dnl Check width of basic types
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)

dnl Check if we have certain functions and #define them
dnl to fallbacks if not
ACG_CHECK_STUFF


dnl Only float is supported as SAMPLE type.
AC_DEFINE(SAMPLE_FLOAT, 1, [use float as sample type])
AC_DEFINE_UNQUOTED(SAMPLE, float, [type to use as sample])



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
  packages="libgnome-2.0 libgnomeui-2.0 libgnomecanvas-2.0 libglade-2.0 gobject-2.0 glib-2.0 gtk+-2.0 >= 2.6.0"
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
  AC_DEFINE(HAVE_LIBGLADE, 1, [wether we have libglade])
  AMG_PATH_XML2(2.0.0, , [
	AC_MSG_ERROR([You need at least libxml 2.0.0 to run glame!
Hint: You also need to have the appropriate xml development package
installed, usually called libxml-dev, libxmld, or similar.])
])
  GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $XML_CFLAGS"
  GLAME_LIBS="$GLAME_LIBS $XML_LIBS"

else
dnl no GUI
  AC_MSG_RESULT([GUI was disabled.])
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
AC_CHECK_LIB(sfftw, fftw2d_create_plan, fftw=yes, fftw=no)
AC_CHECK_HEADER(sfftw.h,, fftw=no)
if test $fftw = yes; then
	FFTW_LIBS="-lsfftw -lsrfftw"
	AC_DEFINE(HAVE_FFTW, 1, [have fftw])
fi
AM_CONDITIONAL(HAVE_FFTW, test x$fftw = xyes)
AC_SUBST(FFTW_LIBS)


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
		AUDIOFILE_LIBS="`audiofile-config --libs`"
		AUDIOFILE_CFLAGS="`audiofile-config --cflags`"
	else
		AUDIOFILE_LIBS="$ac_audiofile_libs -laudiofile"
	fi
	AC_DEFINE(HAVE_AUDIOFILE, 1, [have audiofile])
fi
AC_SUBST(AUDIOFILE_LIBS)
AC_SUBST(AUDIOFILE_CFLAGS)


dnl check for OSS
dnl BSD keeps the header file
dnl in sys/, Linux in linux/, 
dnl other Unices in machine/
AC_CHECK_HEADER(linux/soundcard.h, oss=yes, oss=no)
if test $oss = yes; then
	AC_DEFINE(HAVE_OSS, 1, [support oss sound interface])
	AC_DEFINE(HAVE_OSS_LINUX, 1, [linux oss system])
else
	AC_CHECK_HEADER(sys/soundcard.h, oss=yes, oss=no)
	if test $oss = yes; then
		AC_DEFINE(HAVE_OSS, 1, [support oss sound interface])
		AC_DEFINE(HAVE_OSS_SYS, 1, [soundcard.h in sys])
	else 
		AC_CHECK_HEADER(machine/soundcard.h, oss=yes, oss=no)
		if test $oss = yes; then
			AC_DEFINE(HAVE_OSS, 1, [support oss sound interface])
			AC_DEFINE(HAVE_OSS_MACHINE, 1, [soundcard.h in machine])
		fi
	fi
fi
AM_CONDITIONAL(HAVE_OSS, test x$oss = xyes)

dnl check for esd
ACG_PATH_ESD(0.2.0, esd=yes, esd=no)
if test $esd = yes; then
	GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $ESD_CFLAGS"
	AC_DEFINE(HAVE_ESD, 1, [have esd daemon])
fi
AM_CONDITIONAL(HAVE_ESD, test x$esd = xyes)

dnl check for native IRIX audio
AC_CHECK_LIB(audio, alWriteBuffers, sgi=yes, sgi=no)
AC_CHECK_HEADER(dmedia/audio.h,, sgi=no)
if test $sgi = yes; then
	GLAME_LIBS="$GLAME_LIBS -laudio"
	AC_DEFINE(HAVE_SGIAUDIO, 1, [have irix audio interface])
fi
AM_CONDITIONAL(HAVE_SGI, test x$sgi = xyes)

dnl check for ALSA, first 1.x.x, then 0.9.x
ACG_PATH_ALSA(1.0.0, alsa="yes", alsa="no")
if test "x$alsa" = xno; then
	ACG_PATH_ALSA(0.9.0, alsa="yes", alsa="no")
fi
if test "x$alsa" = xyes; then
	AC_DEFINE(HAVE_ALSA, 1, [have alsa library])
fi
AM_CONDITIONAL(HAVE_ALSA, test x$alsa = xyes)

dnl Jack support is not ready
dnl dnl check for Jack
dnl AC_CHECK_HEADER(jack/jack.h, jack=yes, jack=no)
jack="no"
dnl if test x$jack = xyes; then
dnl 	JACK_CFLAGS="`pkg-config --cflags jack`"
dnl 	JACK_LIBS="`pkg-config --libs jack`"
dnl 	AC_DEFINE(HAVE_JACK, 1, [have jack library])
dnl fi
AM_CONDITIONAL(HAVE_JACK, test x$jack = xyes)
dnl AC_SUBST(JACK_CFLAGS)
dnl AC_SUBST(JACK_LIBS)

dnl
dnl Check for LADSPA support
dnl

dnl LADSPA header
ACG_PATH_LADSPA([AC_DEFINE(HAVE_LADSPA, 1, [have ladspa.h])])
GLAME_INCLUDEDIR="$GLAME_INCLUDEDIR $LADSPA_CFLAGS"

dnl LADSPA RDF -- disabled, we don't use it ATM
dnl ACG_PKG_CHECK_MODULES(LRDF, lrdf,
dnl [
dnl   AC_DEFINE(HAVE_LRDF, 1, [have the lrdf library])
dnl   GLAME_CFLAGS="$GLAME_CFLAGS $LRDF_CFLAGS"
dnl   GLAME_LIBS="$GLAME_LIBS $LRDF_LIBS"
dnl ], [])

dnl check for libmad
ACG_PKG_CHECK_MODULES(MAD, mad,
[
  AC_DEFINE(HAVE_LIBMAD, 1, [have mad library])
],
[
  AC_CHECK_HEADER([mad.h],
  [
    acg_save_LIBS="$LIBS"
    AC_CHECK_LIB(mad, mad_stream_buffer,
    [
      AC_DEFINE(HAVE_LIBMAD, 1, [have mad library])
      MAD_LIBS="-lmad"
    ])
    LIBS="$acg_save_LIBS"
  ])
])
AC_SUBST(MAD_CFLAGS)
AC_SUBST(MAD_LIBS)


dnl check for libmp3lame
acg_save_LIBS="$LIBS"
AC_CHECK_LIB(mp3lame, lame_init_params, mp3lame=yes, mp3lame=no, -lm)
AC_CHECK_HEADER([lame/lame.h],, mp3lame=no)
if test $mp3lame = yes; then
        AC_DEFINE(HAVE_LIBMP3LAME, 1, [have mp3lame library])
        LAME_LIBS="-lmp3lame -lm"
fi
LIBS="$acg_save_LIBS"
AM_CONDITIONAL(HAVE_LIBMP3LAME, test x$mp3lame = xyes)
AC_SUBST(LAME_LIBS)


dnl check for libvorbisfile
have_vorbisfile=no
AC_CHECK_HEADERS([vorbis/codec.h],
[
  AC_CHECK_HEADERS([vorbis/vorbisfile.h],
  [
    acg_save_LIBS="$LIBS"
    AC_CHECK_LIB(vorbisfile, ov_open,
    [ 
      AC_MSG_CHECKING([for number of arguments in ov_read_float])
      AC_COMPILE_IFELSE(
      [
        #include <vorbis/vorbisfile.h>
	void acg_check_func(void)
	{
	  OggVorbis_File *f;
	  float ***ch;
	  int *bs;
	  ov_read_float(f, ch, bs);
	}
      ],
      [
        AC_MSG_RESULT([3])
        AC_DEFINE(GL_OV_READ_FLOAT_ARGS, 3, [Define to the number of arguments ov_read_float() from libvorbisfile takes])
        AC_DEFINE(HAVE_LIBVORBISFILE, 1, [have libvorbisfile library])
	have_vorbisfile=yes
        VORBIS_LIBS="-lvorbisfile -lvorbis -logg -lm -lvorbisenc"
      ],
      [
        AC_COMPILE_IFELSE(
        [
          #include <vorbis/vorbisfile.h>
  	  void acg_check_func(void)
  	  {
  	    OggVorbis_File *f;
  	    float ***ch;
	    int s;
  	    int *bs;
  	    ov_read_float(f, ch, s, bs);
  	  }
        ],
        [
          AC_MSG_RESULT([4])
	  AC_DEFINE(GL_OV_READ_FLOAT_ARGS, 4, [Define to the number of arguments ov_read_float() from libvorbisfile takes])
          AC_DEFINE(HAVE_LIBVORBISFILE, 1, [have libvorbisfile library])
	  have_vorbisfile=yes
          VORBIS_LIBS="-lvorbisfile -lvorbis -logg -lm -lvorbisenc"
	],
	[
	  AC_MSG_RESULT([failed])
	  AC_MSG_WARN([Unsupported version of ov_read_float(). Vorbis support disabled.])
	])
      ])
    ],,[-lvorbis -logg -lm])
    LIBS="$acg_save_LIBS"
  ])
])
AM_CONDITIONAL(HAVE_VORBISFILE, test x$have_vorbisfile = xyes)
AC_SUBST(VORBIS_LIBS)


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
	AC_DEFINE(OS_LINUX, 1, [am linux]);;
*bsd*)
	AC_DEFINE(OS_BSD, 1, [am bsd]);;
*irix*)
	AC_DEFINE(OS_IRIX, 1, [am irix]);;
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
		AC_DEFINE(CPU_X86, 1, [x86 cpu]);;
	mips*)
		AC_DEFINE(CPU_MIPS, 1, [mips cpu])
		ACG_CHECK_MIPS_LL_SC([
			AC_DEFINE(HAVE_MIPS_LL_SC, 1, [modern mips cpu with ll sc])
		]);;
	powerpc)
		AC_DEFINE(CPU_PPC, 1, [ppc cpu]);;
	alpha*)
		AC_DEFINE(CPU_ALPHA, 1, [alpha cpu]);;
	*)
		AC_DEFINE(NO_ASM, 1, [no asm support]);;
	esac
else
	AC_DEFINE(NO_ASM, 1, [no asm support])
fi

dnl
dnl Check for gcc SIMD support
dnl

AC_ARG_ENABLE(simd,
[  --disable-simd          disable fast simd routines ], simd=$enableval, simd=yes)
if test x$simd = xyes; then
  ACG_CHECK_GCC_VECTOR_EXTENSION([AC_DEFINE(HAVE_GCC_SIMD, 1, [gcc SIMD extensions])])
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
