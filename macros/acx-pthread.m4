dnl @synopsis ACX_PTHREAD([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro figures out how to build C programs using POSIX
dnl threads.  It sets the PTHREAD_LIBS output variable to the threads
dnl library and linker flags, and the PTHREAD_CFLAGS output variable
dnl to any special C compiler flags that are needed.  (The user can also
dnl force certain compiler flags/libs to be tested by setting these
dnl environment variables.)
dnl
dnl Also sets PTHREAD_CC to any special C compiler that is needed for
dnl multi-threaded programs (defaults to the value of CC otherwise).
dnl (This is necessary on AIX to use the special cc_r compiler alias.)
dnl
dnl If you are only building threads programs, you may wish to
dnl use these variables in your default LIBS, CFLAGS, and CC:
dnl
dnl        LIBS="$PTHREAD_LIBS $LIBS"
dnl        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
dnl        CC="$PTHREAD_CC"
dnl
dnl In addition, if the PTHREAD_CREATE_JOINABLE thread-attribute
dnl constant has a nonstandard name, defines PTHREAD_CREATE_JOINABLE
dnl to that name (e.g. PTHREAD_CREATE_UNDETACHED on AIX).
dnl
dnl ACTION-IF-FOUND is a list of shell commands to run if a threads
dnl library is found, and ACTION-IF-NOT-FOUND is a list of commands
dnl to run it if it is not found.  If ACTION-IF-FOUND is not specified,
dnl the default action will define HAVE_PTHREAD.
dnl
dnl Please let the authors know if this macro fails on any platform,
dnl or if you have any other suggestions or comments.  This macro was
dnl based on work by SGJ on autoconf scripts for FFTW (www.fftw.org)
dnl (with help from M. Frigo), as well as ac_pthread and hb_pthread
dnl macros posted by AFC to the autoconf macro repository.  We are also
dnl grateful for the helpful feedback of numerous users.
dnl
dnl @version $Id: acx-pthread.m4,v 1.1 2000/06/25 14:52:59 richi Exp $
dnl @author Steven G. Johnson <stevenj@alum.mit.edu> and Alejandro Forero Cuervo <bachue@bachue.com>

AC_DEFUN([ACX_PTHREAD], [

acx_pthread_ok=no

dnl variable to keep track of whether we have already added -D_THREAD_SAFE
using_THREAD_SAFE=no

dnl First, check if the POSIX threads header, pthread.h, is available.
dnl If it isn't, don't bother looking for the threads libraries.
AC_CHECK_HEADER(pthread.h, , acx_pthread_ok=noheader)

dnl We must check for the threads library under a number of different
dnl names; the ordering is very important because some systems
dnl (e.g. DEC) have both -lpthread and -lpthreads, where one of the
dnl libraries is broken (non-POSIX).

dnl First of all, check if the user has set any of the PTHREAD_LIBS,
dnl etcetera environment variables, and if threads linking works using
dnl them:
if test x"$PTHREAD_LIBS$PTHREAD_CFLAGS" != x; then
        save_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
        save_LIBS="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        AC_MSG_CHECKING([for pthread_join in LIBS=$PTHREAD_LIBS with CFLAGS=$PTHREAD_CFLAGS])
        AC_TRY_LINK_FUNC(pthread_join, acx_pthread_ok=yes)
        AC_MSG_RESULT($acx_pthread_ok)
        if test x"$acx_pthread_ok" = xno; then
                PTHREAD_LIBS=""
                PTHREAD_CFLAGS=""
        fi
        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"
fi

dnl POSIX threads library is plural on AIX (need to check for
dnl this *first* due to AIX brokenness; also, need to check
dnl for pthread_attr_init instead of pthread_create due to
dnl DEC craziness):
if test x"$acx_pthread_ok" = xno; then
        AC_CHECK_LIB(pthreads, pthread_attr_init,
                     [PTHREAD_LIBS="-lpthreads"
                      acx_pthread_ok=yes])
fi

dnl Check if no explicit threads library is needed; this should be
dnl done before -kthread/-Kthread, since otherwise those may work but
dnl simply produce continual annoying compiler warnings.  Also, include
dnl pthread.h since on the Sequent -Kthread is needed for this header
dnl file to work (see below).
if test x"$acx_pthread_ok" = xno; then
        dnl Check for pthread_join because of Irix, which has pthread_create
        dnl in libc.so and pthread_join in libpthread.so.  Lose, lose, lose.
        AC_MSG_CHECKING(whether threads work without any explicit flags)
        AC_TRY_LINK([#include <pthread.h>],
                    [pthread_t th; pthread_join(th, 0);],
                    [acx_pthread_ok=yes])
        AC_MSG_RESULT($acx_pthread_ok)
fi

dnl Try -Kthread for Sequent systems: it is required to parse pthread.h,
dnl although not for linking (threads are in libc).  (Thanks to Chris
dnl Lattner of Sequent for his help with this machine.)
if test x"$acx_pthread_ok" = xno; then
        AC_MSG_CHECKING(for Sequent Kthread compiler flag)
        save_CFLAGS="$CFLAGS"
        PTHREAD_CFLAGS="-Kthread"
        CFLAGS="$save_CFLAGS $PTHREAD_CFLAGS"
        AC_TRY_LINK([#include <pthread.h>],
                    [pthread_t th; pthread_join(th, 0);],
                    [acx_pthread_ok=yes])
        if  test x"$acx_pthread_ok" = xyes; then
                AC_MSG_RESULT($PTHREAD_CFLAGS)
        else
                PTHREAD_CFLAGS=""
                AC_MSG_RESULT(no)
        fi
        CFLAGS="$save_CFLAGS"
fi

dnl Now, try -kthread, for FreeBSD kernel threads.  We should do
dnl this before checking for -lpthread, because -kthread actually
dnl uses -lpthread.
if test x"$acx_pthread_ok" = xno; then
        AC_MSG_CHECKING(for bsd kthread compiler flags)
        save_CFLAGS="$CFLAGS"
        PTHREAD_CFLAGS="-kthread -D_THREAD_SAFE"
        CFLAGS="$save_CFLAGS $PTHREAD_CFLAGS"
        dnl Use AC_TRY_LINK_FUNC instead of AC_CHECK_FUNC, to prevent
        dnl the latter's results-caching from screwing us.  We check for
        dnl pthread_join and not pthread_create because of Irix; see above.
        AC_TRY_LINK_FUNC(pthread_join, acx_pthread_ok=yes)
        if  test x"$acx_pthread_ok" = xyes; then
                AC_MSG_RESULT($PTHREAD_CFLAGS)
                using_THREAD_SAFE=yes
        else
                PTHREAD_CFLAGS=""
                AC_MSG_RESULT(no)
        fi
        CFLAGS="$save_CFLAGS"
fi

dnl Normally (e.g. on Linux), POSIX threads are in -lpthread.
dnl We can't just use AC_CHECK_LIB, though, because DEC lossage
dnl requires that pthread.h be included for linking to work.
if test x"$acx_pthread_ok" = xno; then
        AC_MSG_CHECKING([for pthread_create in -lpthread])
        save_LIBS="$LIBS"
        LIBS="-lpthread $LIBS"
        AC_TRY_LINK([#include <pthread.h>],
                    [pthread_create(0,0,0,0);],
                    [PTHREAD_LIBS="-lpthread"
                     acx_pthread_ok=yes])
        LIBS="$save_LIBS"
        AC_MSG_RESULT(${acx_pthread_ok})
fi

dnl Now, try -llthread, for LinuxThreads (e.g. the LinuxThreads port
dnl on FreeBSD).  Do this before -pthread, below, because LinuxThreads
dnl are kernel threads and take advantage of SMP, unlike userland threads.
if test x"$acx_pthread_ok" = xno; then
        AC_CHECK_LIB(lthread, pthread_create,
                     [PTHREAD_LIBS="-llthread"
                      acx_pthread_ok=yes])
fi

dnl Next, try -pthread for FreeBSD userland threads.  We do this
dnl after checking for -lpthread, because -pthread actually is a
dnl valid gcc flag on several systems and links the threads library,
dnl but I prefer -lpthread because -pthread seems to be undocumented
dnl (and thus, to my mind, untrustworthy) on non-BSD systems.
if test x"$acx_pthread_ok" = xno; then
        AC_MSG_CHECKING(for bsd pthread compiler flags)
        save_CFLAGS="$CFLAGS"
        PTHREAD_CFLAGS="-pthread -D_THREAD_SAFE"
        CFLAGS="$save_CFLAGS $PTHREAD_CFLAGS"
        AC_TRY_LINK_FUNC(pthread_join, acx_pthread_ok=yes)
        if  test x"$acx_pthread_ok" = xyes; then
                AC_MSG_RESULT($PTHREAD_CFLAGS)
                using_THREAD_SAFE=yes
        else
                PTHREAD_CFLAGS=""
                AC_MSG_RESULT(no)
        fi
        CFLAGS="$save_CFLAGS"
fi

dnl Various other checks:
if test x"$acx_pthread_ok" = xyes; then
        save_LIBS="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        save_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"

        dnl Detect AIX lossage: threads are created detached by default
        dnl and the JOINABLE attribute has a nonstandard name (UNDETACHED).
        AC_MSG_CHECKING([for joinable pthread attribute])
        AC_TRY_LINK([#include <pthread.h>],
                    [int attr=PTHREAD_CREATE_JOINABLE;],
                    ok=PTHREAD_CREATE_JOINABLE, ok=unknown)
        if test x"$ok" = xunknown; then
                AC_TRY_LINK([#include <pthread.h>],
                            [int attr=PTHREAD_CREATE_UNDETACHED;],
                            ok=PTHREAD_CREATE_UNDETACHED, ok=unknown)
        fi
        if test x"$ok" != xPTHREAD_CREATE_JOINABLE; then
                AC_DEFINE(PTHREAD_CREATE_JOINABLE, $ok,
                          [Define to the necessary symbol if this constant
                           uses a non-standard name on your system.])
        fi
        AC_MSG_RESULT(${ok})
        if test x"$ok" = xunknown; then
                AC_MSG_WARN([we do not know how to create joinable pthreads])
        fi

        dnl More AIX/DEC lossage: must compile with -D_THREAD_SAFE
        dnl (also on FreeBSD) or -D_REENTRANT: (cc_r subsumes this on AIX,
        dnl but it doesn't hurt to -D as well, esp. if cc_r is not available.)
        AC_MSG_CHECKING([if more special flags are required for pthreads])
        ok=no
        AC_REQUIRE([AC_CANONICAL_HOST])
        case "${host_cpu}-${host_os}" in
                *-aix* | *-freebsd*)
                if test x"$using_THREAD_SAFE" = xno; then
                        PTHREAD_CFLAGS="-D_THREAD_SAFE $PTHREAD_CFLAGS"
                        ok="-D_THREAD_SAFE"
                fi;;
                alpha*-osf*)  PTHREAD_CFLAGS="-D_REENTRANT $PTHREAD_CFLAGS"
                        ok="-D_REENTRANT";;
        esac
        AC_MSG_RESULT(${ok})

        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"

        dnl More AIX lossage: must compile with cc_r
        AC_CHECK_PROG(PTHREAD_CC, cc_r, cc_r, ${CC})
else
        PTHREAD_CC="$CC"
fi

AC_SUBST(PTHREAD_LIBS)
AC_SUBST(PTHREAD_CFLAGS)
AC_SUBST(PTHREAD_CC)

dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_pthread_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_PTHREAD,1,[Define if you have POSIX threads libraries and header files.]),[$1])
        :
else
        acx_pthread_ok=no
        $2
fi

])dnl ACX_PTHREADS
