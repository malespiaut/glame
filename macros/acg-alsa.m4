dnl Configure Paths for Alsa
dnl Christopher Lansdown (lansdoct@cs.alfred.edu)
dnl 29/10/1998
dnl ACG_PATH_ALSA(MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for libasound, and define ALSA_CFLAGS and ALSA_LIBS as appropriate.
dnl enables arguments --with-alsa-prefix= --with-alsa-enc-prefix= --disable-alsatest
dnl
AC_DEFUN(ACG_PATH_ALSA,
[dnl
dnl Get the clfags and libraries for alsa
dnl
AC_ARG_WITH(alsa-prefix,[ --with-alsa-prefix=PFX  Prefix where Alsa library is installed(optional)],
	[alsa_prefix="$withval"], [alsa_prefix=""])
AC_ARG_WITH(alsa-inc-prefix, [ --with-alsa-inc-prefix=PFX  Prefix where include libraries are (optional)],
	[alsa_inc_prefix="$withval"], [alsa_inc_prefix=""])
AC_ARG_ENABLE(alsatest, [ --disable-alsatest       Do not try to compile and run a test Alsa program], [enable_alsatest=no], [enable_alsatest=yes])

dnl Add any special include directories
if test "$alsa_inc_prefix" != "" ; then
	ALSA_CFLAGS="$ALSA_CFLAGS -I$alsa_inc_prefix"
fi

dnl add any special lib dirs
if test "$alsa_prefix" != "" ; then
	ALSA_LIBS="$ALSA_LIBS -L$alsa_prefix"
fi

dnl add the alsa library
ALSA_LIBS="$ALSA_LIBS -lasound"

ac_save_CPPFLAGS=$CPPFLAGS
ac_save_LDFLAGS=$LDFLAGS
CPPFLAGS=$ALSA_CFLAGS
LDFLAGS=$ALSA_LIBS

AC_CHECK_HEADER(alsa/asoundlib.h, ac_have_alsa=yes, ac_have_alsa=no)
if test x$ac_have_alsa = xno; then
  AC_CHECK_HEADER(sys/asoundlib.h, ac_have_alsa=yes, ac_have_alsa=no)
  if test x$ac_have_alsa = xyes; then
    AC_DEFINE(ALSA_H_IN_SYS,, [alsa.h in sys])
    ac_have_alsa_include="#include <sys/asoundlib.h>"
  fi
else
  ac_have_alsa_include="#include <alsa/asoundlib.h>"
fi
if test x$ac_have_alsa = xyes; then
  AC_CHECK_LIB(asound, snd_pcm_open, ac_have_alsa=yes, ac_have_alsa=no)
fi

AC_MSG_CHECKING([for alsa version $1 or greater])
alsa_min_major_version=`echo $1 | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
alsa_min_minor_version=`echo $1 | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
alsa_min_subminor_version=`echo $1 | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
AC_TRY_COMPILE(
[
$ac_have_alsa_include
],[
#if SND_LIB_MAJOR != $alsa_min_major_version
#error wrong major version
#endif
#if SND_LIB_MINOR < $alsa_min_minor_version
#error wrong minor version
#endif
#if (SND_LIB_MINOR == $alsa_min_minor_version) && (SND_LIB_SUBMINOR < $alsa_min_subminor_version)
#error wrong subminor version
#endif
],[
AC_MSG_RESULT(yes)
],[
AC_MSG_RESULT(no)
ac_have_alsa=no
])

CPPFLAGS=$ac_save_CPPFLAGS
LDFLAGS=$ac_save_LDFLAGS

if test x$ac_have_alsa = xyes; then
	ifelse([$2], , :, [$2])
else
	ALSA_CFLAGS=""
	ALSA_LIBS=""
	ifelse([$3], , :, [$3])
fi

dnl That should be it.  Now just export out symbols:
AC_SUBST(ALSA_CFLAGS)
AC_SUBST(ALSA_LIBS)
])

