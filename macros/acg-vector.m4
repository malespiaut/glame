dnl
dnl ACG_CHECK_GCC_VECTOR_EXTENSION([ACTION_IF_FOUND, [ACTION_IF_NOT_FOUND]])
dnl

AC_DEFUN([ACG_CHECK_GCC_VECTOR_EXTENSION],
[
	AC_MSG_CHECKING([whether we support gcc SIMD extensions])
	AC_TRY_LINK([
		typedef float v4sf __attribute__((vector_size(16)));
	],[
		v4sf a, b, c;
		a = b + c;
	],[
		AC_MSG_RESULT([yes])
		ifelse([$1], , :, [$1])
	],[
		AC_MSG_RESULT([no])
		ifelse([$2], , :, [$2])
	])
])
