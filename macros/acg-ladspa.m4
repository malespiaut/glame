dnl $Id: acg-ladspa.m4,v 1.2 2001/05/28 14:58:52 richi Exp $
dnl Autoconf check for LADSPA header.
dnl Daniel Kobras <kobras@linux.de>
dnl
dnl ACG_PATH_LADSPA([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl
dnl Enables switches --enable-ladspa, and --with-ladspa-dir=DIR.
dnl Substitutes LADSPA_CFLAGS as appropriate.
dnl Exports AM_CONDITIONAL called LADSPA.
dnl NOTE: Without --enable-ladspa, defaults to NOT-FOUND!
AC_DEFUN(ACG_PATH_LADSPA,
[
	AC_ARG_ENABLE(ladspa, [  --enable-ladspa         Try to compile with LADSPA support],
		[
		case "$enableval" in
			yes)	;;
			no)	;;
			*)
				AC_MSG_ERROR([bad value $enableval, use yes/no instead])
				;;
		esac
		do_ladspa="$enableval"
		],	
		[do_ladspa="yes"])
	AC_ARG_WITH(ladspa-dir, [  --with-ladspa-dir=DIR   Directory where LADSPA header is installed],
		ladspa_dir="$withval", ladspa_dir="")
	AC_MSG_CHECKING([whether to build with ladspa support])
	AC_MSG_RESULT("$do_ladspa")
	if test "$do_ladspa" = "yes"; then
		if test "$ladspa_dir" != ""; then
			LADSPA_CFLAGS="-I$ladspa_dir"
			ac_save_CPPFLAGS="$CPPFLAGS"
			CPPFLAGS="$CPPFLAGS $LADSPA_CFLAGS"
		fi
		AC_CHECK_HEADER(ladspa.h, ac_have_ladspa=yes, ac_have_ladspa=no)
		if test "$ladspa_dir" != ""; then
			CPPFLAGS="$ac_save_CPPFLAGS"
		fi
	else
		ac_have_ladspa="no"
	fi
	if test "$ac_have_ladspa" = "yes"; then
		ifelse([$1], , :, [$1])
	else
		LADSPA_CFLAGS=""
		ifelse([$2], , :, [$2])
	fi
	AC_SUBST(LADSPA_CFLAGS)
	AM_CONDITIONAL(LADSPA, test "$ac_have_ladspa" = "yes")
])
