dnl
dnl ACG_CHECK_GUILE (MINIMUM-VERSION, [ACTION-IF-FOUND, [ACTION-IF-NOT-FOUND]])
dnl

AC_DEFUN(ACG_CHECK_GUILE,
[
	saved_ldflags="$LDFLAGS"
	saved_LIBS="$LIBS"
	saved_cppflags="$CPPFLAGS"
	LDFLAGS="$LDFLAGS $GNOME_LIBDIR"

	if test "x$cross_compiling" = "xyes" ; then
	  name_build_guile="$target_alias-guile-config"
	else
	  name_build_guile="guile-config"
	fi

	AC_CHECK_PROG(BUILD_GUILE, $name_build_guile, yes, no)

	if test "x$BUILD_GUILE" = "xyes"; then
	    AC_MSG_CHECKING(whether $name_build_guile works)
	    if test x`$name_build_guile --version >/dev/null 2>&1 || \
		echo no` = xno; then
		BUILD_GUILE=no
	    fi
	    AC_MSG_RESULT($BUILD_GUILE)
	else

	    if test "x$cross_compiling" = "xyes" ; then
		name_build_guile="$target_alias-build-guile"
	    else	
		name_build_guile="build-guile"
	    fi

	    AC_CHECK_PROG(BUILD_GUILE, $name_build_guile, yes, no)

	    if test "x$BUILD_GUILE" = "xyes"; then
		AC_MSG_CHECKING(whether $name_build_guile works)
		if test x`$name_build_guile --version >/dev/null 2>&1 || \
	 	    echo no` = xno; then
		    BUILD_GUILE=no
		fi
		AC_MSG_RESULT($BUILD_GUILE)
	    fi
	fi


	if test "x$BUILD_GUILE" = "xyes"; then
		AC_MSG_CHECKING(for guile libraries)
		GUILE_LIBS="`$name_build_guile link`"
		AC_MSG_RESULT($GUILE_LIBS)
		AC_MSG_CHECKING(for guile headers)
		GUILE_INCS="`$name_build_guile compile`"
		AC_MSG_RESULT($GUILE_INCS)
dnl FIXME - version check missing
	else
		AC_MSG_WARN([No guile-config was found, trying to continue anyway.])
		GUILE_LIBS="$GNOME_LIBDIR"
		GUILE_INCS="$GNOME_INCLUDEDIR"
		AC_CHECK_LIB(qthreads,qt_null,[
			QTTHREADS_LIB="-lqthreads"
		],[
			AC_CHECK_LIB(qt, qt_null, QTTHREADS_LIB="-lqt")
		],$LIBS)

		AC_CHECK_LIB(termcap,main,TERMCAP_LIB="-ltermcap")
		AC_CHECK_LIB(readline,main,READLINE_LIB="-lreadline",,$TERMCAP_LIB)
		AC_CHECK_LIB(m, sin)
		AC_CHECK_LIB(rx, main, GUILE_LIBS="-lrx $GUILE_LIBS")
		AC_CHECK_LIB(qt, qt_null, GUILE_LIBS="-lqt $GUILE_LIBS")
		AC_CHECK_LIB(dl, dlopen, GUILE_LIBS="-ldl $GUILE_LIBS")
		AC_CHECK_LIB(nsl, t_accept, GUILE_LIBS="$GUILE_LIBS -lnsl")
		AC_CHECK_LIB(socket, socket, GUILE_LIBS="$GUILE_LIBS -lsocket")
		GUILE_LIBS="-lguile $GUILE_LIBS $QTTHREADS_LIB $READLINE_LIB $TERMCAP_LIB"
	fi

	LIBS="$LIBS $GUILE_LIBS"
	CPPFLAGS="$saved_cppflags $GUILE_INCS"

	AC_MSG_CHECKING(whether guile works)
	AC_TRY_LINK([
		#include <libguile.h>
		#include <guile/gh.h>
	],[
		float *d;
		gh_floats2fvect(d, 5);
		scm_make_smob_type("Blah", 50);
		gh_eval_str("(newline)");
		scm_boot_guile(0,NULL,NULL,NULL);
	],[
		ac_cv_guile_found=yes
		AC_DEFINE(HAVE_GUILE)
	],[
		ac_cv_guile_found=no
	])
	AC_MSG_RESULT($ac_cv_guile_found)

	if test x$ac_cv_guile_found = xno ; then
		GUILE_LIBS= GUILE_INCS=
		ifelse([$3], , :, [$3])
	else
		ifelse([$2], , :, [$2])
	fi

	LIBS="$saved_LIBS"
	LDFLAGS="$saved_ldflags"
	CPPFLAGS="$saved_cppflags"

dnl	AC_SUBST(QTTHREADS_LIB)
dnl	AC_SUBST(TERMCAP_LIB)
dnl	AC_SUBST(READLINE_LIB)
	AC_SUBST(GUILE_INCS)
	AC_SUBST(GUILE_LIBS)
	AM_CONDITIONAL(GUILE, test x$ac_cv_guile_found = xyes)
])
