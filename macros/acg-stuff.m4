dnl
dnl ACG_CHECK_STUFF
dnl

AC_DEFUN([ACG_CHECK_STUFF],
[
	ac_saved_libs="$LIBS"
	LIBS="$LIBS -lm"
	AC_CHECK_FUNCS(sqrtf sinf)
	LIBS="$ac_saved_libs"

	AC_MSG_CHECKING([wether stdarg.h has va_copy support])
	AC_TRY_LINK(
	[
#include <stdarg.h>
int foo(int a, ...)
{
   va_list va, vax;
   int res;
   va_start(va, a);
   va_copy(vax, va);
   res = va_arg(vax, int);
   va_end(vax);
   va_end(va);
   return res;
}
	],[
   return foo(1, 0);
	],[
		AC_MSG_RESULT([yes])
	],[
		AC_MSG_RESULT([no])
		AC_MSG_ERROR([No fallback for missing va_copy implemented.])
	])

])
