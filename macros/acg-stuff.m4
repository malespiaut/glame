dnl
dnl ACG_CHECK_STUFF
dnl

AC_DEFUN([ACG_CHECK_STUFF],
[
	ac_saved_libs="$LIBS"
	LIBS="$LIBS -lm"
	AC_CHECK_FUNCS(sqrtf sinf)
	LIBS="$ac_saved_libs"

])
