dnl On MIPS machines, check whether ll and sc opcodes are available
dnl (i.e. if we are running on a machine > R3000).
dnl Daniel Kobras 01-10-2001
dnl Usage: ACG_CHECK_MIPS_LL_SC([ACTION-IF-AVAILABLE, [ACTION-IF-UNAVAILABLE])

AC_DEFUN([ACG_CHECK_MIPS_LL_SC],
[
AC_MSG_CHECKING(for ll and sc opcodes)
AC_TRY_COMPILE(
[
/* No includes */
],
[
int i,j;
unsigned long k;
__asm__("ll %0,%1\n"
       	"sc %0,%1\n"
       	: "=&r" (k), "=m" (i)
       	: "Ir" (j), "m" (i));
],
[
AC_MSG_RESULT(yes)
ifelse([$1], , :, [$1])
],
[
AC_MSG_RESULT(no)
ifelse([$2], , :, [$2])
])
])
