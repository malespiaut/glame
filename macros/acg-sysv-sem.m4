# Check if we need to define union semun ourselves.
# Daniel Kobras   00-5-4

AC_DEFUN(ACG_CHECK_SEMUN,
[
AC_MSG_CHECKING(whether union semun is already defined)
AC_TRY_COMPILE(
[
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
],
[
	union semun mysem;
],
[
AC_MSG_RESULT(yes)
HAVE_SEMUN=1
AC_DEFINE(HAVE_SEMUN)
]
,
[
HAVE_SEMUN=0
AC_MSG_RESULT(no)
]
)
])



# Check if semctl() call works, IRIX & gcc break wrt passing
# union semun.
# Richard Guenther 22-06-2000

AC_DEFUN(ACG_CHECK_SEMCTL,
[
AC_REQUIRE([ACG_CHECK_SEMUN])
AC_MSG_CHECKING(whether semctl does work)
saved_cflags="$CFLAGS"
CFLAGS="$CFLAGS -DHAVE_SEMUN=$HAVE_SEMUN"
AC_TRY_RUN(
[
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#if HAVE_SEMUN == 1
#else
union semun {
        int val;                    /* value for SETVAL */
        struct semid_ds *buf;       /* buffer for IPC_STAT, IPC_SET */
        unsigned short int *array;  /* array for GETALL, SETALL */
        struct seminfo *__buf;      /* buffer for IPC_INFO */
};
#endif
int main(int argc, char **argv)
{
	union semun su;
	int id;
	if ((id = semget(IPC_PRIVATE, 1, IPC_CREAT|0666)) == -1)
		exit(1);
	/* try, set the semval to 31 */
	su.val = 31;
	if (semctl(id, 0, SETVAL, su) == 1)
		goto err;
	/* read the val back and compare with 31 */
	if (semctl(id, 0, GETVAL, su) != 31)
		goto err;
	semctl(id, 0, IPC_RMID, su);
	exit(0);
err:
	semctl(id, 0, IPC_RMID, su);
	exit(1);
}
],
[
AC_MSG_RESULT(yes)
SEMCTL_OK=1
AC_DEFINE(SEMCTL_OK)
],
[
AC_MSG_RESULT(no - enabling workaround)
],
[
AC_MSG_RESULT(unknown - enabling workaround)
]
)
CFLAGS="$saved_cflags"
])
