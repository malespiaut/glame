dnl
dnl ACG_CHECK_POSIX_SEMAPHORES
dnl
dnl checks for working posix semaphore support - requires
dnl pthreads fixed LDFLAGS and CPPFLAGS/CFLAGS
dnl

AC_DEFUN([ACG_CHECK_POSIX_SEMAPHORES],
[
	AC_CHECK_HEADER(semaphore.h, posixsem_h=yes, posixsem_h=)
	AC_CHECK_FUNC(sem_init, posixsem_init=yes, posixsem_init=)
	if test "$posixsem_h"="yes" -a "$posixsem_init"="yes"; then
		AC_MSG_CHECKING(whether POSIX semaphores work) 
		AC_TRY_RUN(
[
#include <semaphore.h>
int main(int argc, char **argv)
{
	sem_t sem;
	int val;
	if (sem_init(&sem, 0, 1) == -1) exit(1);
	if (sem_wait(&sem) == -1) exit(1);
	if (sem_trywait(&sem) != -1) exit(1);
	if (sem_post(&sem) == -1) exit(1);
	if (sem_getvalue(&sem, &val), val != 1) exit(1);
	if (sem_destroy(&sem) == -1) exit(1);
	exit(0);
}
], posixsem_work=yes, posixsem_work=, posixsem_work=yes)
		if test "$posixsem_work"="yes"; then
			AC_MSG_RESULT(yes)
			AC_DEFINE(HAVE_POSIXSEM)
			POSIX_SEMAPHORES=1
		else
			AC_MSG_RESULT(no)
			POSIX_SEMAPHORES=0
		fi
	fi
])

