# Check for madvise function (present in glibc2.?) and
# on some other unixes.
# Richard Guenther 00-4-10

AC_DEFUN(AC_FUNC_MADVISE,
[
AC_MSG_CHECKING(for madvise)
AC_TRY_COMPILE(
[
#include <sys/mman.h>
],
[
	int i = madvise((void *)0, 0, 0);
],
[
AC_MSG_RESULT(yes)
HAVE_MADVISE=1
AC_DEFINE(HAVE_MADVISE)
],
[
AC_MSG_RESULT(no)
]
)
])
