dnl
dnl ACG_CHECK_STUFF
dnl

AC_DEFUN([ACG_CHECK_STUFF],
[

	AC_MSG_CHECKING([for sqrtf])
	ac_saved_libs="$LIBS"
	LIBS="$LIBS -lm"
	AC_TRY_LINK(
[
	#include <math.h>
],[
	float f, g = 1.26;
	f = sqrtf(g);
],[
	AC_MSG_RESULT([yes])
],[
	AC_MSG_RESULT([no - defining to sqrt])
	AC_DEFINE_UNQUOTED(sqrtf, sqrt)
])
	LIBS="$ac_saved_libs"

])
