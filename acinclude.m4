# Check for POSIX realtime signals (present in glibc2.1)
# Daniel Kobras   00-3-14

AC_DEFUN(AC_CHECK_POSIX_RTSIG,
[
AC_MSG_CHECKING(whether POSIX realtime signals are supported)
AC_TRY_COMPILE(
[
#include <signal.h>
],
[
int main()
{
	struct sigaction sa;
	sa.sa_sigaction(0, (siginfo_t *)0, (void *)0);
	return 0;
}
],
[
AC_MSG_RESULT(yes)
HAVE_POSIX_RTSIG=1
AC_DEFINE(HAVE_POSIX_RTSIG)
]
,
[
AC_MSG_RESULT(no)
]
)
])

# Configure paths for ESD
# Manish Singh    98-9-30
# stolen back from Frank Belew
# stolen from Manish Singh
# Shamelessly stolen from Owen Taylor

dnl AM_PATH_ESD([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for ESD, and define ESD_CFLAGS and ESD_LIBS
dnl
AC_DEFUN(AM_PATH_ESD,
[dnl 
dnl Get the cflags and libraries from the esd-config script
dnl
AC_ARG_WITH(esd-prefix,[  --with-esd-prefix=PFX   Prefix where ESD is installed (optional)],
            esd_prefix="$withval", esd_prefix="")
AC_ARG_WITH(esd-exec-prefix,[  --with-esd-exec-prefix=PFX Exec prefix where ESD is installed (optional)],
            esd_exec_prefix="$withval", esd_exec_prefix="")
AC_ARG_ENABLE(esdtest, [  --disable-esdtest       Do not try to compile and run a test ESD program],
		    , enable_esdtest=yes)

  if test x$esd_exec_prefix != x ; then
     esd_args="$esd_args --exec-prefix=$esd_exec_prefix"
     if test x${ESD_CONFIG+set} != xset ; then
        ESD_CONFIG=$esd_exec_prefix/bin/esd-config
     fi
  fi
  if test x$esd_prefix != x ; then
     esd_args="$esd_args --prefix=$esd_prefix"
     if test x${ESD_CONFIG+set} != xset ; then
        ESD_CONFIG=$esd_prefix/bin/esd-config
     fi
  fi

  AC_PATH_PROG(ESD_CONFIG, esd-config, no)
  min_esd_version=ifelse([$1], ,0.2.7,$1)
  AC_MSG_CHECKING(for ESD - version >= $min_esd_version)
  no_esd=""
  if test "$ESD_CONFIG" = "no" ; then
    no_esd=yes
  else
    AC_LANG_SAVE
    AC_LANG_C
    ESD_CFLAGS=`$ESD_CONFIG $esdconf_args --cflags`
    ESD_LIBS=`$ESD_CONFIG $esdconf_args --libs`

    esd_major_version=`$ESD_CONFIG $esd_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    esd_minor_version=`$ESD_CONFIG $esd_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    esd_micro_version=`$ESD_CONFIG $esd_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_esdtest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $ESD_CFLAGS"
      LIBS="$LIBS $ESD_LIBS"
dnl
dnl Now check if the installed ESD is sufficiently new. (Also sanity
dnl checks the results of esd-config to some extent
dnl
      rm -f conf.esdtest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <esd.h>

char*
my_strdup (char *str)
{
  char *new_str;
  
  if (str)
    {
      new_str = malloc ((strlen (str) + 1) * sizeof(char));
      strcpy (new_str, str);
    }
  else
    new_str = NULL;
  
  return new_str;
}

int main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.esdtest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = my_strdup("$min_esd_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_esd_version");
     exit(1);
   }

   if (($esd_major_version > major) ||
      (($esd_major_version == major) && ($esd_minor_version > minor)) ||
      (($esd_major_version == major) && ($esd_minor_version == minor) && ($esd_micro_version >= micro)))
    {
      return 0;
    }
  else
    {
      printf("\n*** 'esd-config --version' returned %d.%d.%d, but the minimum version\n", $esd_major_version, $esd_minor_version, $esd_micro_version);
      printf("*** of ESD required is %d.%d.%d. If esd-config is correct, then it is\n", major, minor, micro);
      printf("*** best to upgrade to the required version.\n");
      printf("*** If esd-config was wrong, set the environment variable ESD_CONFIG\n");
      printf("*** to point to the correct copy of esd-config, and remove the file\n");
      printf("*** config.cache before re-running configure\n");
      return 1;
    }
}

],, no_esd=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
       AC_LANG_RESTORE
     fi
  fi
  if test "x$no_esd" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$ESD_CONFIG" = "no" ; then
       echo "*** The esd-config script installed by ESD could not be found"
       echo "*** If ESD was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the ESD_CONFIG environment variable to the"
       echo "*** full path to esd-config."
     else
       if test -f conf.esdtest ; then
        :
       else
          echo "*** Could not run ESD test program, checking why..."
          CFLAGS="$CFLAGS $ESD_CFLAGS"
          LIBS="$LIBS $ESD_LIBS"
          AC_LANG_SAVE
          AC_LANG_C
          AC_TRY_LINK([
#include <stdio.h>
#include <esd.h>
],      [ return 0; ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding ESD or finding the wrong"
          echo "*** version of ESD. If it is not finding ESD, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means ESD was incorrectly installed"
          echo "*** or that you have moved ESD since it was installed. In the latter case, you"
          echo "*** may want to edit the esd-config script: $ESD_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
          AC_LANG_RESTORE
       fi
     fi
     ESD_CFLAGS=""
     ESD_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(ESD_CFLAGS)
  AC_SUBST(ESD_LIBS)
  rm -f conf.esdtest
])
dnl
dnl GNOME_INIT_HOOK (script-if-gnome-enabled, [failflag], [additional-inits])
dnl
dnl if failflag is "fail" then GNOME_INIT_HOOK will abort if gnomeConf.sh
dnl is not found. 
dnl

AC_DEFUN([GNOME_INIT_HOOK],[
	AC_SUBST(GNOME_LIBS)
	AC_SUBST(GNOMEUI_LIBS)
	AC_SUBST(GNOMEGNORBA_LIBS)
	AC_SUBST(GTKXMHTML_LIBS)
	AC_SUBST(ZVT_LIBS)
	AC_SUBST(GNOME_LIBDIR)
	AC_SUBST(GNOME_INCLUDEDIR)

	AC_ARG_WITH(gnome-includes,
	[  --with-gnome-includes   Specify location of GNOME headers],[
	CFLAGS="$CFLAGS -I$withval"
	])
	
	AC_ARG_WITH(gnome-libs,
	[  --with-gnome-libs       Specify location of GNOME libs],[
	LDFLAGS="$LDFLAGS -L$withval"
	gnome_prefix=$withval
	])

	AC_ARG_WITH(gnome,
	[  --with-gnome            Specify prefix for GNOME files],
		if test x$withval = xyes; then
	    		want_gnome=yes
	    		dnl Note that an empty true branch is not
			dnl valid sh syntax.
	    		ifelse([$1], [], :, [$1])
        	else
	    		if test "x$withval" = xno; then
	        		want_gnome=no
	    		else
	        		want_gnome=yes
	    			LDFLAGS="$LDFLAGS -L$withval/lib"
	    			CFLAGS="$CFLAGS -I$withval/include"
	    			gnome_prefix=$withval/lib
	    		fi
  		fi,
		want_gnome=yes)

	if test "x$want_gnome" = xyes; then

	    AC_PATH_PROG(GNOME_CONFIG,gnome-config,no)
	    if test "$GNOME_CONFIG" = "no"; then
	      no_gnome_config="yes"
	    else
	      AC_MSG_CHECKING(if $GNOME_CONFIG works)
	      if $GNOME_CONFIG --libs-only-l gnome >/dev/null 2>&1; then
	        AC_MSG_RESULT(yes)
	        GNOME_GNORBA_HOOK([],$2)
	        GNOME_LIBS="`$GNOME_CONFIG --libs-only-l gnome`"
	        GNOMEUI_LIBS="`$GNOME_CONFIG --libs-only-l gnomeui`"
	        GNOMEGNORBA_LIBS="`$GNOME_CONFIG --libs-only-l gnorba gnomeui`"
	        GTKXMHTML_LIBS="`$GNOME_CONFIG --libs-only-l gtkxmhtml`"
		ZVT_LIBS="`$GNOME_CONFIG --libs-only-l zvt`"
	        GNOME_LIBDIR="`$GNOME_CONFIG --libs-only-L gnorba gnomeui`"
	        GNOME_INCLUDEDIR="`$GNOME_CONFIG --cflags gnorba gnomeui`"
                $1
	      else
	        AC_MSG_RESULT(no)
	        no_gnome_config="yes"
              fi
            fi

	    if test x$exec_prefix = xNONE; then
	        if test x$prefix = xNONE; then
		    gnome_prefix=$ac_default_prefix/lib
	        else
 		    gnome_prefix=$prefix/lib
	        fi
	    else
	        gnome_prefix=`eval echo \`echo $libdir\``
	    fi
	
	    if test "$no_gnome_config" = "yes"; then
              AC_MSG_CHECKING(for gnomeConf.sh file in $gnome_prefix)
	      if test -f $gnome_prefix/gnomeConf.sh; then
	        AC_MSG_RESULT(found)
	        echo "loading gnome configuration from" \
		     "$gnome_prefix/gnomeConf.sh"
	        . $gnome_prefix/gnomeConf.sh
	        $1
	      else
	        AC_MSG_RESULT(not found)
 	        if test x$2 = xfail; then
	          AC_MSG_ERROR(Could not find the gnomeConf.sh file that is generated by gnome-libs install)
 	        fi
	      fi
            fi
	fi

	if test -n "$3"; then
	  n="$3"
	  for i in $n; do
	    AC_MSG_CHECKING(extra library \"$i\")
	    case $i in 
	      applets)
		AC_SUBST(GNOME_APPLETS_LIBS)
		GNOME_APPLETS_LIBS=`$GNOME_CONFIG --libs-only-l applets`
		AC_MSG_RESULT($GNOME_APPLETS_LIBS);;
	      capplet)
		AC_SUBST(GNOME_CAPPLET_LIBS)
		GNOME_CAPPLET_LIBS=`$GNOME_CONFIG --libs-only-l capplet`
		AC_MSG_RESULT($GNOME_CAPPLET_LIBS);;
	      *)
		AC_MSG_RESULT(unknown library)
	    esac
	  done
	fi
])

dnl
dnl GNOME_INIT ([additional-inits])
dnl

AC_DEFUN([GNOME_INIT],[
	GNOME_INIT_HOOK([],fail,$1)
])
# Configure paths for Bonobo
# Miguel de Icaza, 99-04-12
# Stolen from Chris Lahey	99-2-5
# stolen from Manish Singh again
# stolen back from Frank Belew
# stolen from Manish Singh
# Shamelessly stolen from Owen Taylor

dnl AM_PATH_BONOBO ([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for Bonobo, and define BONOBO_CFLAGS and BONOBO_LIBS
dnl
AC_DEFUN([AM_PATH_BONOBO],
[
dnl 
dnl Get the cflags and libraries from the gnome-config script
dnl
AC_ARG_WITH(bonobo-prefix,[  --with-bonobo-prefix=PFX   Prefix where Bonobo is installed (optional)],
            bonobo_prefix="$withval", bonobo_prefix="")
AC_ARG_WITH(bonobo-exec-prefix,[  --with-bonobo-exec-prefix=PFX Exec prefix where Bonobo is installed (optional)],
            bonobo_exec_prefix="$withval", bonobo_exec_prefix="")
AC_ARG_ENABLE(bonobotest, [  --disable-bonobotest       Do not try to compile and run a test Bonobo program],
		    , enable_bonobotest=yes)

  if test x$bonobo_exec_prefix != x ; then
     bonobo_args="$bonobo_args --exec-prefix=$bonobo_exec_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$bonobo_exec_prefix/bin/gnome-config
     fi
  fi
  if test x$bonobo_prefix != x ; then
     bonobo_args="$bonobo_args --prefix=$bonobo_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$bonobo_prefix/bin/gnome-config
     fi
  fi

  AC_PATH_PROG(GNOME_CONFIG, gnome-config, no)
  min_bonobo_version=ifelse([$1], ,0.1.0,$1)
  AC_MSG_CHECKING(for BONOBO - version >= $min_bonobo_version)
  no_bonobo=""
  if test "$GNOME_CONFIG" = "no" ; then
    no_bonobo=yes
  else
    BONOBO_CFLAGS=`$GNOME_CONFIG $bonoboconf_args --cflags bonobo`
    BONOBO_LIBS=`$GNOME_CONFIG $bonoboconf_args --libs bonobo`

    bonobo_major_version=`$GNOME_CONFIG $bonobo_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    bonobo_minor_version=`$GNOME_CONFIG $bonobo_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    bonobo_micro_version=`$GNOME_CONFIG $bonobo_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_bonobotest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $BONOBO_CFLAGS"
      LIBS="$LIBS $BONOBO_LIBS"
dnl
dnl Now check if the installed BONOBO is sufficiently new. (Also sanity
dnl checks the results of gnome-config to some extent
dnl
      rm -f conf.bonobotest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bonobo/gnome-object.h>

static char*
my_strdup (char *str)
{
  char *new_str;
  
  if (str)
    {
      new_str = malloc ((strlen (str) + 1) * sizeof(char));
      strcpy (new_str, str);
    }
  else
    new_str = NULL;
  
  return new_str;
}

int main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.bonobotest");
  gnome_object_get_type ();
  return 0;
}

],, no_bonobo=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_bonobo" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GNOME_CONFIG" = "no" ; then
       echo "*** The gnome-config script installed by GNOME-LIBS could not be found"
       echo "*** If BONOBO was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GNOME_CONFIG environment variable to the"
       echo "*** full path to gnome-config."
     else
       if test -f conf.bonobotest ; then
        :
       else
          echo "*** Could not run BONOBO test program, checking why..."
          CFLAGS="$CFLAGS $BONOBO_CFLAGS"
          LIBS="$LIBS $BONOBO_LIBS"
          AC_TRY_LINK([
#include <stdio.h>
#include <bonobo/gnome-object.h>
],      [ return 0; ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding BONOBO or finding the wrong"
          echo "*** version of BONOBO. If it is not finding BONOBO, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means BONOBO was incorrectly installed"
          echo "*** or that you have moved BONOBO since it was installed. In the latter case, you"
          echo "*** may want to edit the gnome-config script: $GNOME_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     BONOBO_CFLAGS=""
     BONOBO_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(BONOBO_CFLAGS)
  AC_SUBST(BONOBO_LIBS)
  rm -f conf.bonobotest
])

AC_DEFUN([BONOBO_CHECK], [
	AM_PATH_BONOBO(0.1.0,,[AC_MSG_ERROR(BONOBO not found)])
])

# gnome-common.m4
# 
# This only for packages that are not in the GNOME CVS tree.

dnl GNOME_COMMON_INIT

AC_DEFUN([GNOME_COMMON_INIT],
[
	GNOME_ACLOCAL_DIR=`$ACLOCAL --print-ac-dir`/gnome
	AC_SUBST(GNOME_ACLOCAL_DIR)

	ACLOCAL="$ACLOCAL -I $GNOME_ACLOCAL_DIR"
])

dnl
dnl GNOME_FILEUTILS_CHECKS
dnl
dnl checks that are needed for the diskusage applet.
dnl

AC_DEFUN([GNOME_FILEUTILS_CHECKS],
[	
AC_CHECK_HEADERS(fcntl.h sys/param.h sys/statfs.h sys/fstyp.h \
mnttab.h mntent.h sys/statvfs.h sys/vfs.h sys/mount.h \
sys/filsys.h sys/fs_types.h sys/fs/s5param.h)

AC_CHECK_FUNCS(bcopy endgrent endpwent fchdir ftime ftruncate \
getcwd getmntinfo gettimeofday isascii lchown \
listmntent memcpy mkfifo strchr strerror strrchr vprintf)

dnl Set some defaults when cross-compiling

if test x$cross_compiling = xyes ; then
	case "$host_os" in
	linux*)
	  fu_cv_sys_mounted_getmntent1=yes
	  fu_cv_sys_stat_statfs2_bsize=yes
	  ;;
	sunos*)
	  fu_cv_sys_stat_statfs4=yes
	  ;;
	freebsd*)
	  fu_cv_sys_stat_statfs2_bsize=yes
	  ;;
	osf*)
	  fu_cv_sys_stat_statfs3_osf1=yes
	  ;;
	esac
fi

# Determine how to get the list of mounted filesystems.
list_mounted_fs=

# If the getmntent function is available but not in the standard library,
# make sure LIBS contains -lsun (on Irix4) or -lseq (on PTX).
AC_FUNC_GETMNTENT

# This test must precede the ones for getmntent because Unicos-9 is
# reported to have the getmntent function, but its support is incompatible
# with other getmntent implementations.

# NOTE: Normally, I wouldn't use a check for system type as I've done for
# `CRAY' below since that goes against the whole autoconf philosophy.  But
# I think there is too great a chance that some non-Cray system has a
# function named listmntent to risk the false positive.

if test -z "$list_mounted_fs"; then
# Cray UNICOS 9
AC_MSG_CHECKING([for listmntent of Cray/Unicos-9])
AC_CACHE_VAL(fu_cv_sys_mounted_cray_listmntent,
[fu_cv_sys_mounted_cray_listmntent=no
AC_EGREP_CPP(yes,
[#ifdef _CRAY
yes
#endif
], [test $ac_cv_func_listmntent = yes \
&& fu_cv_sys_mounted_cray_listmntent=yes]
)
]
)
AC_MSG_RESULT($fu_cv_sys_mounted_cray_listmntent)
if test $fu_cv_sys_mounted_cray_listmntent = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_LISTMNTENT)
fi
fi

if test $ac_cv_func_getmntent = yes; then

# This system has the getmntent function.
# Determine whether it's the one-argument variant or the two-argument one.

if test -z "$list_mounted_fs"; then
# 4.3BSD, SunOS, HP-UX, Dynix, Irix
AC_MSG_CHECKING([for one-argument getmntent function])
AC_CACHE_VAL(fu_cv_sys_mounted_getmntent1,
[test $ac_cv_header_mntent_h = yes \
&& fu_cv_sys_mounted_getmntent1=yes \
|| fu_cv_sys_mounted_getmntent1=no])
AC_MSG_RESULT($fu_cv_sys_mounted_getmntent1)
if test $fu_cv_sys_mounted_getmntent1 = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETMNTENT1)
fi
fi

if test -z "$list_mounted_fs"; then
# SVR4
AC_MSG_CHECKING([for two-argument getmntent function])
AC_CACHE_VAL(fu_cv_sys_mounted_getmntent2,
[AC_EGREP_HEADER(getmntent, sys/mnttab.h,
fu_cv_sys_mounted_getmntent2=yes,
fu_cv_sys_mounted_getmntent2=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_getmntent2)
if test $fu_cv_sys_mounted_getmntent2 = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETMNTENT2)
fi
fi

if test -z "$list_mounted_fs"; then
AC_MSG_ERROR([could not determine how to read list of mounted filesystems])
fi

fi

if test -z "$list_mounted_fs"; then
# DEC Alpha running OSF/1.
AC_MSG_CHECKING([for getfsstat function])
AC_CACHE_VAL(fu_cv_sys_mounted_getsstat,
[AC_TRY_LINK([
#include <sys/types.h>
#include <sys/mount.h>
#include <sys/fs_types.h>],
[struct statfs *stats;
int numsys = getfsstat ((struct statfs *)0, 0L, MNT_WAIT); ],
fu_cv_sys_mounted_getsstat=yes,
fu_cv_sys_mounted_getsstat=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_getsstat)
if test $fu_cv_sys_mounted_getsstat = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETFSSTAT)
fi
fi

if test -z "$list_mounted_fs"; then
# AIX.
AC_MSG_CHECKING([for mntctl function and struct vmount])
AC_CACHE_VAL(fu_cv_sys_mounted_vmount,
[AC_TRY_CPP([#include <fshelp.h>],
fu_cv_sys_mounted_vmount=yes,
fu_cv_sys_mounted_vmount=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_vmount)
if test $fu_cv_sys_mounted_vmount = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_VMOUNT)
fi
fi

if test -z "$list_mounted_fs"; then
# SVR3
AC_MSG_CHECKING([for FIXME existence of three headers])
AC_CACHE_VAL(fu_cv_sys_mounted_fread_fstyp,
[AC_TRY_CPP([
#include <sys/statfs.h>
#include <sys/fstyp.h>
#include <mnttab.h>],
fu_cv_sys_mounted_fread_fstyp=yes,
fu_cv_sys_mounted_fread_fstyp=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_fread_fstyp)
if test $fu_cv_sys_mounted_fread_fstyp = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_FREAD_FSTYP)
fi
fi

if test -z "$list_mounted_fs"; then
# 4.4BSD and DEC OSF/1.
AC_MSG_CHECKING([for getmntinfo function])
AC_CACHE_VAL(fu_cv_sys_mounted_getmntinfo,
[
ok=
if test $ac_cv_func_getmntinfo = yes; then
AC_EGREP_HEADER(f_type;, sys/mount.h,
ok=yes)
fi
test -n "$ok" \
&& fu_cv_sys_mounted_getmntinfo=yes \
|| fu_cv_sys_mounted_getmntinfo=no
])
AC_MSG_RESULT($fu_cv_sys_mounted_getmntinfo)
if test $fu_cv_sys_mounted_getmntinfo = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETMNTINFO)
fi
fi

# FIXME: add a test for netbsd-1.1 here

if test -z "$list_mounted_fs"; then
# Ultrix
AC_MSG_CHECKING([for getmnt function])
AC_CACHE_VAL(fu_cv_sys_mounted_getmnt,
[AC_TRY_CPP([
#include <sys/fs_types.h>
#include <sys/mount.h>],
fu_cv_sys_mounted_getmnt=yes,
fu_cv_sys_mounted_getmnt=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_getmnt)
if test $fu_cv_sys_mounted_getmnt = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_GETMNT)
fi
fi

if test -z "$list_mounted_fs"; then
# SVR2
AC_MSG_CHECKING([whether it is possible to resort to fread on /etc/mnttab])
AC_CACHE_VAL(fu_cv_sys_mounted_fread,
[AC_TRY_CPP([#include <mnttab.h>],
fu_cv_sys_mounted_fread=yes,
fu_cv_sys_mounted_fread=no)])
AC_MSG_RESULT($fu_cv_sys_mounted_fread)
if test $fu_cv_sys_mounted_fread = yes; then
list_mounted_fs=found
AC_DEFINE(MOUNTED_FREAD)
fi
fi

if test -z "$list_mounted_fs"; then
AC_MSG_ERROR([could not determine how to read list of mounted filesystems])
# FIXME -- no need to abort building the whole package
# Can't build mountlist.c or anything that needs its functions
fi

AC_CHECKING(how to get filesystem space usage)
space=no

# Perform only the link test since it seems there are no variants of the
# statvfs function.  This check is more than just AC_CHECK_FUNCS(statvfs)
# because that got a false positive on SCO OSR5.  Adding the declaration
# of a `struct statvfs' causes this test to fail (as it should) on such
# systems.  That system is reported to work fine with STAT_STATFS4 which
# is what it gets when this test fails.
if test $space = no; then
# SVR4
AC_CACHE_CHECK([statvfs function (SVR4)], fu_cv_sys_stat_statvfs,
[AC_TRY_LINK([#include <sys/types.h>
#include <sys/statvfs.h>],
[struct statvfs fsd; statvfs (0, &fsd);],
fu_cv_sys_stat_statvfs=yes,
fu_cv_sys_stat_statvfs=no)])
if test $fu_cv_sys_stat_statvfs = yes; then
space=yes
AC_DEFINE(STAT_STATVFS)
fi
fi

if test $space = no; then
# DEC Alpha running OSF/1
AC_MSG_CHECKING([for 3-argument statfs function (DEC OSF/1)])
AC_CACHE_VAL(fu_cv_sys_stat_statfs3_osf1,
[AC_TRY_RUN([
#include <sys/param.h>
#include <sys/types.h>
#include <sys/mount.h>
main ()
{
struct statfs fsd;
fsd.f_fsize = 0;
exit (statfs (".", &fsd, sizeof (struct statfs)));
}],
fu_cv_sys_stat_statfs3_osf1=yes,
fu_cv_sys_stat_statfs3_osf1=no,
fu_cv_sys_stat_statfs3_osf1=no)])
AC_MSG_RESULT($fu_cv_sys_stat_statfs3_osf1)
if test $fu_cv_sys_stat_statfs3_osf1 = yes; then
space=yes
AC_DEFINE(STAT_STATFS3_OSF1)
fi
fi

if test $space = no; then
# AIX
AC_MSG_CHECKING([for two-argument statfs with statfs.bsize dnl
member (AIX, 4.3BSD)])
AC_CACHE_VAL(fu_cv_sys_stat_statfs2_bsize,
[AC_TRY_RUN([
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
main ()
{
struct statfs fsd;
fsd.f_bsize = 0;
exit (statfs (".", &fsd));
}],
fu_cv_sys_stat_statfs2_bsize=yes,
fu_cv_sys_stat_statfs2_bsize=no,
fu_cv_sys_stat_statfs2_bsize=no)])
AC_MSG_RESULT($fu_cv_sys_stat_statfs2_bsize)
if test $fu_cv_sys_stat_statfs2_bsize = yes; then
space=yes
AC_DEFINE(STAT_STATFS2_BSIZE)
fi
fi

if test $space = no; then
# SVR3
AC_MSG_CHECKING([for four-argument statfs (AIX-3.2.5, SVR3)])
AC_CACHE_VAL(fu_cv_sys_stat_statfs4,
[AC_TRY_RUN([#include <sys/types.h>
#include <sys/statfs.h>
main ()
{
struct statfs fsd;
exit (statfs (".", &fsd, sizeof fsd, 0));
}],
fu_cv_sys_stat_statfs4=yes,
fu_cv_sys_stat_statfs4=no,
fu_cv_sys_stat_statfs4=no)])
AC_MSG_RESULT($fu_cv_sys_stat_statfs4)
if test $fu_cv_sys_stat_statfs4 = yes; then
space=yes
AC_DEFINE(STAT_STATFS4)
fi
fi

if test $space = no; then
# 4.4BSD and NetBSD
AC_MSG_CHECKING([for two-argument statfs with statfs.fsize dnl
member (4.4BSD and NetBSD)])
AC_CACHE_VAL(fu_cv_sys_stat_statfs2_fsize,
[AC_TRY_RUN([#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
main ()
{
struct statfs fsd;
fsd.f_fsize = 0;
exit (statfs (".", &fsd));
}],
fu_cv_sys_stat_statfs2_fsize=yes,
fu_cv_sys_stat_statfs2_fsize=no,
fu_cv_sys_stat_statfs2_fsize=no)])
AC_MSG_RESULT($fu_cv_sys_stat_statfs2_fsize)
if test $fu_cv_sys_stat_statfs2_fsize = yes; then
space=yes
AC_DEFINE(STAT_STATFS2_FSIZE)
fi
fi

if test $space = no; then
# Ultrix
AC_MSG_CHECKING([for two-argument statfs with struct fs_data (Ultrix)])
AC_CACHE_VAL(fu_cv_sys_stat_fs_data,
[AC_TRY_RUN([#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_FS_TYPES_H
#include <sys/fs_types.h>
#endif
main ()
{
struct fs_data fsd;
/* Ultrix's statfs returns 1 for success,
0 for not mounted, -1 for failure.  */
exit (statfs (".", &fsd) != 1);
}],
fu_cv_sys_stat_fs_data=yes,
fu_cv_sys_stat_fs_data=no,
fu_cv_sys_stat_fs_data=no)])
AC_MSG_RESULT($fu_cv_sys_stat_fs_data)
if test $fu_cv_sys_stat_fs_data = yes; then
space=yes
AC_DEFINE(STAT_STATFS2_FS_DATA)
fi
fi

if test $space = no; then
# SVR2
AC_TRY_CPP([#include <sys/filsys.h>],
AC_DEFINE(STAT_READ_FILSYS) space=yes)
fi

if test -n "$list_mounted_fs" && test $space != no; then
DF_PROG="df"
# LIBOBJS="$LIBOBJS fsusage.o"
# LIBOBJS="$LIBOBJS mountlist.o"
fi

# Check for SunOS statfs brokenness wrt partitions 2GB and larger.
# If <sys/vfs.h> exists and struct statfs has a member named f_spare,
# enable the work-around code in fsusage.c.
AC_MSG_CHECKING([for statfs that truncates block counts])
AC_CACHE_VAL(fu_cv_sys_truncating_statfs,
[AC_TRY_COMPILE([
#if !defined(sun) && !defined(__sun)
choke -- this is a workaround for a Sun-specific problem
#endif
#include <sys/types.h>
#include <sys/vfs.h>],
[struct statfs t; long c = *(t.f_spare);],
fu_cv_sys_truncating_statfs=yes,
fu_cv_sys_truncating_statfs=no,
)])
if test $fu_cv_sys_truncating_statfs = yes; then
AC_DEFINE(STATFS_TRUNCATES_BLOCK_COUNTS)
fi
AC_MSG_RESULT($fu_cv_sys_truncating_statfs)

AC_CHECKING(for AFS)
test -d /afs && AC_DEFINE(AFS)
])
# Macro to add for using GNU gettext.
# Ulrich Drepper <drepper@cygnus.com>, 1995.
#
# Modified to never use included libintl. 
# Owen Taylor <otaylor@redhat.com>, 12/15/1998
#
#
# This file can be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.

# serial 5

AC_DEFUN(AM_GNOME_WITH_NLS,
  [AC_MSG_CHECKING([whether NLS is requested])
    dnl Default is enabled NLS
    AC_ARG_ENABLE(nls,
      [  --disable-nls           do not use Native Language Support],
      USE_NLS=$enableval, USE_NLS=yes)
    AC_MSG_RESULT($USE_NLS)
    AC_SUBST(USE_NLS)

    USE_INCLUDED_LIBINTL=no

    dnl If we use NLS figure out what method
    if test "$USE_NLS" = "yes"; then
      AC_DEFINE(ENABLE_NLS)
#      AC_MSG_CHECKING([whether included gettext is requested])
#      AC_ARG_WITH(included-gettext,
#        [  --with-included-gettext use the GNU gettext library included here],
#        nls_cv_force_use_gnu_gettext=$withval,
#        nls_cv_force_use_gnu_gettext=no)
#      AC_MSG_RESULT($nls_cv_force_use_gnu_gettext)
      nls_cv_force_use_gnu_gettext="no"

      nls_cv_use_gnu_gettext="$nls_cv_force_use_gnu_gettext"
      if test "$nls_cv_force_use_gnu_gettext" != "yes"; then
        dnl User does not insist on using GNU NLS library.  Figure out what
        dnl to use.  If gettext or catgets are available (in this order) we
        dnl use this.  Else we have to fall back to GNU NLS library.
	dnl catgets is only used if permitted by option --with-catgets.
	nls_cv_header_intl=
	nls_cv_header_libgt=
	CATOBJEXT=NONE

	AC_CHECK_HEADER(libintl.h,
	  [AC_CACHE_CHECK([for gettext in libc], gt_cv_func_gettext_libc,
	    [AC_TRY_LINK([#include <libintl.h>], [return (int) gettext ("")],
	       gt_cv_func_gettext_libc=yes, gt_cv_func_gettext_libc=no)])

	   if test "$gt_cv_func_gettext_libc" != "yes"; then
	     AC_CHECK_LIB(intl, bindtextdomain,
	       [AC_CACHE_CHECK([for gettext in libintl],
		 gt_cv_func_gettext_libintl,
		 [AC_CHECK_LIB(intl, gettext,
		  gt_cv_func_gettext_libintl=yes,
		  gt_cv_func_gettext_libintl=no)],
		 gt_cv_func_gettext_libintl=no)])
	   fi

	   if test "$gt_cv_func_gettext_libc" = "yes" \
	      || test "$gt_cv_func_gettext_libintl" = "yes"; then
	      AC_DEFINE(HAVE_GETTEXT)
	      AM_PATH_PROG_WITH_TEST(MSGFMT, msgfmt,
		[test -z "`$ac_dir/$ac_word -h 2>&1 | grep 'dv '`"], no)dnl
	      if test "$MSGFMT" != "no"; then
		AC_CHECK_FUNCS(dcgettext)
		AC_PATH_PROG(GMSGFMT, gmsgfmt, $MSGFMT)
		AM_PATH_PROG_WITH_TEST(XGETTEXT, xgettext,
		  [test -z "`$ac_dir/$ac_word -h 2>&1 | grep '(HELP)'`"], :)
		AC_TRY_LINK(, [extern int _nl_msg_cat_cntr;
			       return _nl_msg_cat_cntr],
		  [CATOBJEXT=.gmo
		   DATADIRNAME=share],
		  [CATOBJEXT=.mo
		   DATADIRNAME=lib])
		INSTOBJEXT=.mo
	      fi
	    fi

	    # Added by Martin Baulig 12/15/98 for libc5 systems
	    if test "$gt_cv_func_gettext_libc" != "yes" \
	       && test "$gt_cv_func_gettext_libintl" = "yes"; then
	       INTLLIBS=-lintl
	       LIBS=`echo $LIBS | sed -e 's/-lintl//'`
	    fi
	])

        if test "$CATOBJEXT" = "NONE"; then
	  AC_MSG_CHECKING([whether catgets can be used])
	  AC_ARG_WITH(catgets,
	    [  --with-catgets          use catgets functions if available],
	    nls_cv_use_catgets=$withval, nls_cv_use_catgets=no)
	  AC_MSG_RESULT($nls_cv_use_catgets)

	  if test "$nls_cv_use_catgets" = "yes"; then
	    dnl No gettext in C library.  Try catgets next.
	    AC_CHECK_LIB(i, main)
	    AC_CHECK_FUNC(catgets,
	      [AC_DEFINE(HAVE_CATGETS)
	       INTLOBJS="\$(CATOBJS)"
	       AC_PATH_PROG(GENCAT, gencat, no)dnl
#	       if test "$GENCAT" != "no"; then
#		 AC_PATH_PROG(GMSGFMT, gmsgfmt, no)
#		 if test "$GMSGFMT" = "no"; then
#		   AM_PATH_PROG_WITH_TEST(GMSGFMT, msgfmt,
#		    [test -z "`$ac_dir/$ac_word -h 2>&1 | grep 'dv '`"], no)
#		 fi
#		 AM_PATH_PROG_WITH_TEST(XGETTEXT, xgettext,
#		   [test -z "`$ac_dir/$ac_word -h 2>&1 | grep '(HELP)'`"], :)
#		 USE_INCLUDED_LIBINTL=yes
#		 CATOBJEXT=.cat
#		 INSTOBJEXT=.cat
#		 DATADIRNAME=lib
#		 INTLDEPS='$(top_builddir)/intl/libintl.a'
#		 INTLLIBS=$INTLDEPS
#		 LIBS=`echo $LIBS | sed -e 's/-lintl//'`
#		 nls_cv_header_intl=intl/libintl.h
#		 nls_cv_header_libgt=intl/libgettext.h
#              fi
            ])
	  fi
        fi

        if test "$CATOBJEXT" = "NONE"; then
	  dnl Neither gettext nor catgets in included in the C library.
	  dnl Fall back on GNU gettext library.
	  nls_cv_use_gnu_gettext=yes
        fi
      fi

      if test "$nls_cv_use_gnu_gettext" != "yes"; then
        AC_DEFINE(ENABLE_NLS)
      else
         # Unset this variable since we use the non-zero value as a flag.
         CATOBJEXT=
#        dnl Mark actions used to generate GNU NLS library.
#        INTLOBJS="\$(GETTOBJS)"
#        AM_PATH_PROG_WITH_TEST(MSGFMT, msgfmt,
#	  [test -z "`$ac_dir/$ac_word -h 2>&1 | grep 'dv '`"], msgfmt)
#        AC_PATH_PROG(GMSGFMT, gmsgfmt, $MSGFMT)
#        AM_PATH_PROG_WITH_TEST(XGETTEXT, xgettext,
#	  [test -z "`$ac_dir/$ac_word -h 2>&1 | grep '(HELP)'`"], :)
#        AC_SUBST(MSGFMT)
#	USE_INCLUDED_LIBINTL=yes
#        CATOBJEXT=.gmo
#        INSTOBJEXT=.mo
#        DATADIRNAME=share
#	INTLDEPS='$(top_builddir)/intl/libintl.a'
#	INTLLIBS=$INTLDEPS
#	LIBS=`echo $LIBS | sed -e 's/-lintl//'`
#        nls_cv_header_intl=intl/libintl.h
#        nls_cv_header_libgt=intl/libgettext.h
      fi

      dnl Test whether we really found GNU xgettext.
      if test "$XGETTEXT" != ":"; then
	dnl If it is no GNU xgettext we define it as : so that the
	dnl Makefiles still can work.
	if $XGETTEXT --omit-header /dev/null 2> /dev/null; then
	  : ;
	else
	  AC_MSG_RESULT(
	    [found xgettext program is not GNU xgettext; ignore it])
	  XGETTEXT=":"
	fi
      fi

      # We need to process the po/ directory.
      POSUB=po
    else
      DATADIRNAME=share
      nls_cv_header_intl=intl/libintl.h
      nls_cv_header_libgt=intl/libgettext.h
    fi
    AC_LINK_FILES($nls_cv_header_libgt, $nls_cv_header_intl)
    AC_OUTPUT_COMMANDS(
     [case "$CONFIG_FILES" in *po/Makefile.in*)
        sed -e "/POTFILES =/r po/POTFILES" po/Makefile.in > po/Makefile
      esac])


#    # If this is used in GNU gettext we have to set USE_NLS to `yes'
#    # because some of the sources are only built for this goal.
#    if test "$PACKAGE" = gettext; then
#      USE_NLS=yes
#      USE_INCLUDED_LIBINTL=yes
#    fi

    dnl These rules are solely for the distribution goal.  While doing this
    dnl we only have to keep exactly one list of the available catalogs
    dnl in configure.in.
    for lang in $ALL_LINGUAS; do
      GMOFILES="$GMOFILES $lang.gmo"
      POFILES="$POFILES $lang.po"
    done

    dnl Make all variables we use known to autoconf.
    AC_SUBST(USE_INCLUDED_LIBINTL)
    AC_SUBST(CATALOGS)
    AC_SUBST(CATOBJEXT)
    AC_SUBST(DATADIRNAME)
    AC_SUBST(GMOFILES)
    AC_SUBST(INSTOBJEXT)
    AC_SUBST(INTLDEPS)
    AC_SUBST(INTLLIBS)
    AC_SUBST(INTLOBJS)
    AC_SUBST(POFILES)
    AC_SUBST(POSUB)
  ])

AC_DEFUN(AM_GNOME_GETTEXT,
  [AC_REQUIRE([AC_PROG_MAKE_SET])dnl
   AC_REQUIRE([AC_PROG_CC])dnl
   AC_REQUIRE([AC_PROG_RANLIB])dnl
   AC_REQUIRE([AC_ISC_POSIX])dnl
   AC_REQUIRE([AC_HEADER_STDC])dnl
   AC_REQUIRE([AC_C_CONST])dnl
   AC_REQUIRE([AC_C_INLINE])dnl
   AC_REQUIRE([AC_TYPE_OFF_T])dnl
   AC_REQUIRE([AC_TYPE_SIZE_T])dnl
   AC_REQUIRE([AC_FUNC_ALLOCA])dnl
   AC_REQUIRE([AC_FUNC_MMAP])dnl

   AC_CHECK_HEADERS([argz.h limits.h locale.h nl_types.h malloc.h string.h \
unistd.h sys/param.h])
   AC_CHECK_FUNCS([getcwd munmap putenv setenv setlocale strchr strcasecmp \
strdup __argz_count __argz_stringify __argz_next])

   if test "${ac_cv_func_stpcpy+set}" != "set"; then
     AC_CHECK_FUNCS(stpcpy)
   fi
   if test "${ac_cv_func_stpcpy}" = "yes"; then
     AC_DEFINE(HAVE_STPCPY)
   fi

   AM_LC_MESSAGES
   AM_GNOME_WITH_NLS

   if test "x$CATOBJEXT" != "x"; then
     if test "x$ALL_LINGUAS" = "x"; then
       LINGUAS=
     else
       AC_MSG_CHECKING(for catalogs to be installed)
       NEW_LINGUAS=
       if test "x$LINGUAS" = "x"; then
           LINGUAS=$ALL_LINGUAS
       fi
       for lang in $LINGUAS; do
         case "$ALL_LINGUAS" in
          *$lang*) NEW_LINGUAS="$NEW_LINGUAS $lang" ;;
         esac
       done
       LINGUAS=$NEW_LINGUAS
       AC_MSG_RESULT($LINGUAS)
     fi

     dnl Construct list of names of catalog files to be constructed.
     if test -n "$LINGUAS"; then
       for lang in $LINGUAS; do CATALOGS="$CATALOGS $lang$CATOBJEXT"; done
     fi
   fi

   dnl The reference to <locale.h> in the installed <libintl.h> file
   dnl must be resolved because we cannot expect the users of this
   dnl to define HAVE_LOCALE_H.
   if test $ac_cv_header_locale_h = yes; then
     INCLUDE_LOCALE_H="#include <locale.h>"
   else
     INCLUDE_LOCALE_H="\
/* The system does not provide the header <locale.h>.  Take care yourself.  */"
   fi
   AC_SUBST(INCLUDE_LOCALE_H)

   dnl Determine which catalog format we have (if any is needed)
   dnl For now we know about two different formats:
   dnl   Linux libc-5 and the normal X/Open format
   test -d intl || mkdir intl
   if test "$CATOBJEXT" = ".cat"; then
     AC_CHECK_HEADER(linux/version.h, msgformat=linux, msgformat=xopen)

     dnl Transform the SED scripts while copying because some dumb SEDs
     dnl cannot handle comments.
     sed -e '/^#/d' $srcdir/intl/$msgformat-msg.sed > intl/po2msg.sed
   fi
   dnl po2tbl.sed is always needed.
   sed -e '/^#.*[^\\]$/d' -e '/^#$/d' \
     $srcdir/intl/po2tbl.sed.in > intl/po2tbl.sed

   dnl In the intl/Makefile.in we have a special dependency which makes
   dnl only sense for gettext.  We comment this out for non-gettext
   dnl packages.
   if test "$PACKAGE" = "gettext"; then
     GT_NO="#NO#"
     GT_YES=
   else
     GT_NO=
     GT_YES="#YES#"
   fi
   AC_SUBST(GT_NO)
   AC_SUBST(GT_YES)

   dnl If the AC_CONFIG_AUX_DIR macro for autoconf is used we possibly
   dnl find the mkinstalldirs script in another subdir but ($top_srcdir).
   dnl Try to locate is.
   MKINSTALLDIRS=
   if test -n "$ac_aux_dir"; then
     MKINSTALLDIRS="$ac_aux_dir/mkinstalldirs"
   fi
   if test -z "$MKINSTALLDIRS"; then
     MKINSTALLDIRS="\$(top_srcdir)/mkinstalldirs"
   fi
   AC_SUBST(MKINSTALLDIRS)

   dnl *** For now the libtool support in intl/Makefile is not for real.
   l=
   AC_SUBST(l)

   dnl Generate list of files to be processed by xgettext which will
   dnl be included in po/Makefile.
   test -d po || mkdir po
   if test "x$srcdir" != "x."; then
     if test "x`echo $srcdir | sed 's@/.*@@'`" = "x"; then
       posrcprefix="$srcdir/"
     else
       posrcprefix="../$srcdir/"
     fi
   else
     posrcprefix="../"
   fi
   rm -f po/POTFILES
   sed -e "/^#/d" -e "/^\$/d" -e "s,.*,	$posrcprefix& \\\\," -e "\$s/\(.*\) \\\\/\1/" \
	< $srcdir/po/POTFILES.in > po/POTFILES
  ])

AC_DEFUN([GNOME_GHTTP_CHECK],[
	AC_REQUIRE([GNOME_INIT_HOOK])
	GHTTP_LIB=
	AC_CHECK_FUNC(connect,,[
	  AC_CHECK_LIB(socket,connect,
		GHTTP_LIB="-lsocket $GHTTP_LIB",,$GHTTP_LIB)])
	AC_CHECK_FUNC(gethostbyname,,[
 	  AC_CHECK_LIB(nsl,gethostbyname,
		GHTTP_LIB="-lnsl $GHTTP_LIB",,$GHTTP_LIB)])
	AC_CHECK_LIB(ghttp, ghttp_request_new, 
		GHTTP_LIB="-lghttp $GHTTP_LIB",,-L$gnome_prefix $GHTTP_LIB)
	AC_SUBST(GHTTP_LIB)
	AC_PROVIDE([GNOME_GHTTP_CHECK])
])
dnl
dnl GNOME_GNORBA_HOOK (script-if-gnorba-found, failflag)
dnl
dnl if failflag is "failure" it aborts if gnorba is not found.
dnl

AC_DEFUN([GNOME_GNORBA_HOOK],[
	GNOME_ORBIT_HOOK([],$2)
	AC_CACHE_CHECK([for gnorba libraries],gnome_cv_gnorba_found,[
		gnome_cv_gnorba_found=no
		if test x$gnome_cv_orbit_found = xyes; then
			GNORBA_CFLAGS="`gnome-config --cflags gnorba gnomeui`"
			GNORBA_LIBS="`gnome-config --libs gnorba gnomeui`"
			if test -n "$GNORBA_LIBS"; then
				gnome_cv_gnorba_found=yes
			fi
		fi
	])
	AM_CONDITIONAL(HAVE_GNORBA, test x$gnome_cv_gnorba_found = xyes)
	if test x$gnome_cv_orbit_found = xyes; then
		$1
		GNORBA_CFLAGS="`gnome-config --cflags gnorba gnomeui`"
		GNORBA_LIBS="`gnome-config --libs gnorba gnomeui`"
		AC_SUBST(GNORBA_CFLAGS)
		AC_SUBST(GNORBA_LIBS)
	else
	    	if test x$2 = xfailure; then
			AC_MSG_ERROR(gnorba library not installed or installation problem)
	    	fi
	fi
])

AC_DEFUN([GNOME_GNORBA_CHECK], [
	GNOME_GNORBA_HOOK([],failure)
])
dnl
dnl GNOME_CHECK_GUILE (failflag)
dnl
dnl if failflag is "fail" then GNOME_CHECK_GUILE will abort if guile is not found.
dnl

AC_DEFUN([GNOME_CHECK_GUILE],
[
	saved_ldflags="$LDFLAGS"
	saved_cppflags="$CPPFLAGS"
	LDFLAGS="$LDFLAGS $GNOME_LIBDIR"

	AC_CHECK_LIB(qthreads,qt_null,[
		QTTHREADS_LIB="-lqthreads"
	],[
		AC_CHECK_LIB(qt, qt_null, QTTHREADS_LIB="-lqt")
	],$LIBS)
	AC_SUBST(QTTHREADS_LIB)

	AC_CHECK_LIB(termcap,main,TERMCAP_LIB="-ltermcap")
	AC_CHECK_LIB(readline,main,READLINE_LIB="-lreadline",,$TERMCAP_LIB)

	AC_SUBST(TERMCAP_LIB)
	AC_SUBST(READLINE_LIB)

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

	AC_CHECK_LIB(m, sin)

	if test "x$BUILD_GUILE" = "xyes"; then
		AC_MSG_CHECKING(for guile libraries)
		GUILE_LIBS="`$name_build_guile link`"
		AC_MSG_RESULT($GUILE_LIBS)
		AC_MSG_CHECKING(for guile headers)
		GUILE_INCS="`$name_build_guile compile`"
		AC_MSG_RESULT($GUILE_INCS)
	else
		GUILE_LIBS="$GNOME_LIBDIR"
		GUILE_INCS="$GNOME_INCLUDEDIR"
		AC_CHECK_LIB(rx, main, GUILE_LIBS="-lrx $GUILE_LIBS")
		AC_CHECK_LIB(qt, qt_null, GUILE_LIBS="-lqt $GUILE_LIBS")
		AC_CHECK_LIB(dl, dlopen, GUILE_LIBS="-ldl $GUILE_LIBS")
		AC_CHECK_LIB(nsl, t_accept, GUILE_LIBS="$GUILE_LIBS -lnsl")
		AC_CHECK_LIB(socket, socket, GUILE_LIBS="$GUILE_LIBS -lsocket")
		GUILE_LIBS="-lguile $GUILE_LIBS $QTTHREADS_LIB $READLINE_LIB $TERMCAP_LIB"
	fi

	AC_SUBST(GUILE_LIBS)
	AC_SUBST(GUILE_INCS)

	saved_LIBS="$LIBS"
	LIBS="$LIBS $GUILE_LIBS"
	CPPFLAGS="$saved_cppflags $GUILE_INCS"

	AC_MSG_CHECKING(whether guile works)
	AC_TRY_LINK([
		#include <libguile.h>
		#include <guile/gh.h>
	],[
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
		if test x$1 = xfail ; then
		  AC_MSG_ERROR(Can not find Guile on this system)
		else
		  AC_MSG_WARN(Can not find Guile on this system)
		fi
		ac_cv_guile_found=no
		GUILE_LIBS= GUILE_INCS=
	fi

	LIBS="$saved_LIBS"
	LDFLAGS="$saved_ldflags"
	CPPFLAGS="$saved_cppflags"

	AC_SUBST(GUILE_LIBS)
	AM_CONDITIONAL(GUILE, test x$ac_cv_guile_found = xyes)
])
dnl
dnl GNOME_LIBGTOP_TYPES
dnl
dnl some typechecks for libgtop.
dnl

AC_DEFUN([GNOME_LIBGTOP_TYPES],
[
	AC_CHECK_TYPE(u_int64_t, unsigned long long int)
	AC_CHECK_TYPE(int64_t, long long int)
])

dnl
dnl GNOME_LIBGTOP_HOOK (minversion, script-if-libgtop-enabled, failflag)
dnl
dnl if failflag is "fail" then GNOME_LIBGTOP_HOOK will abort if LibGTop
dnl is not found. 
dnl

AC_DEFUN([GNOME_LIBGTOP_HOOK],
[	
	AC_REQUIRE([GNOME_LIBGTOP_TYPES])

	AC_SUBST(LIBGTOP_LIBDIR)
	AC_SUBST(LIBGTOP_INCLUDEDIR)
	AC_SUBST(LIBGTOP_EXTRA_LIBS)
	AC_SUBST(LIBGTOP_LIBS)
	AC_SUBST(LIBGTOP_INCS)
	AC_SUBST(LIBGTOP_NAMES_LIBS)
	AC_SUBST(LIBGTOP_NAMES_INCS)
	AC_SUBST(LIBGTOP_GUILE_INCS)
	AC_SUBST(LIBGTOP_GUILE_LIBS)
	AC_SUBST(LIBGTOP_GUILE_NAMES_INCS)
	AC_SUBST(LIBGTOP_GUILE_NAMES_LIBS)
	AC_SUBST(LIBGTOP_MAJOR_VERSION)
	AC_SUBST(LIBGTOP_MINOR_VERSION)
	AC_SUBST(LIBGTOP_MICRO_VERSION)
	AC_SUBST(LIBGTOP_VERSION)
	AC_SUBST(LIBGTOP_VERSION_CODE)
	AC_SUBST(LIBGTOP_SERVER_VERSION)
	AC_SUBST(LIBGTOP_INTERFACE_AGE)
	AC_SUBST(LIBGTOP_BINARY_AGE)
	AC_SUBST(LIBGTOP_BINDIR)
	AC_SUBST(LIBGTOP_SERVER)

	dnl Get the cflags and libraries from the libgtop-config script
	dnl
	AC_ARG_WITH(libgtop,
	[  --with-libgtop=PFX      Prefix where LIBGTOP is installed (optional)],
	libgtop_config_prefix="$withval", libgtop_config_prefix="")
	AC_ARG_WITH(libgtop-exec,
	[  --with-libgtop-exec=PFX Exec prefix where LIBGTOP is installed (optional)],
	libgtop_config_exec_prefix="$withval", libgtop_config_exec_prefix="")

	if test x$libgtop_config_exec_prefix != x ; then
	  libgtop_config_args="$libgtop_config_args --exec-prefix=$libgtop_config_exec_prefix"
	  if test x${LIBGTOP_CONFIG+set} != xset ; then
	    LIBGTOP_CONFIG=$libgtop_config_exec_prefix/bin/libgtop-config
	  fi
	fi
	if test x$libgtop_config_prefix != x ; then
	  libgtop_config_args="$libgtop_config_args --prefix=$libgtop_config_prefix"
	  if test x${LIBGTOP_CONFIG+set} != xset ; then
	    LIBGTOP_CONFIG=$libgtop_config_prefix/bin/libgtop-config
	  fi
	fi

	AC_PATH_PROG(LIBGTOP_CONFIG, libgtop-config, no)
	dnl IMPORTANT NOTICE:
	dnl   If you increase this number here, this means that *ALL*
	dnl   modules will require the new version, even if they explicitly
	dnl   give a lower number in their `configure.in' !!!
	real_min_libgtop_version=1.0.0
	min_libgtop_version=ifelse([$1], ,$real_min_libgtop_version,$1)
	dnl I know, the following code looks really ugly, but if you want
	dnl to make changes, please test it with a brain-dead /bin/sh and
	dnl with a brain-dead /bin/test (not all shells/tests support the
	dnl `<' operator to compare strings, that's why I convert everything
	dnl into numbers and test them).
	min_libgtop_major=`echo $min_libgtop_version | \
	  sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
	min_libgtop_minor=`echo $min_libgtop_version | \
	  sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
	min_libgtop_micro=`echo $min_libgtop_version | \
	  sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
	test x$min_libgtop_micro = x && min_libgtop_micro=0
	real_min_libgtop_major=`echo $real_min_libgtop_version | \
	  sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
	real_min_libgtop_minor=`echo $real_min_libgtop_version | \
	  sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
	real_min_libgtop_micro=`echo $real_min_libgtop_version | \
	  sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
	test x$real_min_libgtop_micro = x && real_min_libgtop_micro=0
	dnl You cannot require a version less then $real_min_libgtop_version,
	dnl so you don't need to update each `configure.in' when it's increased.
	if test $real_min_libgtop_major -gt $min_libgtop_major ; then
	  min_libgtop_major=$real_min_libgtop_major
	  min_libgtop_minor=$real_min_libgtop_minor
	  min_libgtop_micro=$real_min_libgtop_micro
	elif test $real_min_libgtop_major = $min_libgtop_major ; then
	  if test $real_min_libgtop_minor -gt $min_libgtop_minor ; then
	    min_libgtop_minor=$real_min_libgtop_minor
	    min_libgtop_micro=$real_min_libgtop_micro
	  elif test $real_min_libgtop_minor = $min_libgtop_minor ; then
	    if test $real_min_libgtop_micro -gt $min_libgtop_micro ; then
	      min_libgtop_micro=$real_min_libgtop_micro
	    fi
	  fi
	fi
	min_libgtop_version="$min_libgtop_major.$min_libgtop_minor.$min_libgtop_micro"
	AC_MSG_CHECKING(for libgtop - version >= $min_libgtop_version)
	no_libgtop=""
	if test "$LIBGTOP_CONFIG" = "no" ; then
	  no_libgtop=yes
	else
	  configfile=`$LIBGTOP_CONFIG --config`
	  libgtop_major_version=`$LIBGTOP_CONFIG --version | \
	    sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
	  libgtop_minor_version=`$LIBGTOP_CONFIG --version | \
	    sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
	  libgtop_micro_version=`$LIBGTOP_CONFIG --version | \
	    sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
	  test $libgtop_major_version != $min_libgtop_major && no_libgtop=yes
	  test $libgtop_minor_version -lt $min_libgtop_minor && no_libgtop=yes
	  if test $libgtop_minor_version = $min_libgtop_minor ; then
	    test $libgtop_micro_version -lt $min_libgtop_micro && no_libgtop=yes
	  fi
	  . $configfile
	fi
	if test x$no_libgtop = x ; then
	  AC_DEFINE(HAVE_LIBGTOP)
	  AC_DEFINE_UNQUOTED(LIBGTOP_VERSION, "$LIBGTOP_VERSION")
	  AC_DEFINE_UNQUOTED(LIBGTOP_VERSION_CODE, $LIBGTOP_VERSION_CODE)
	  AC_DEFINE_UNQUOTED(LIBGTOP_MAJOR_VERSION, $LIBGTOP_MAJOR_VERSION)
	  AC_DEFINE_UNQUOTED(LIBGTOP_MINOR_VERSION, $LIBGTOP_MINOR_VERSION)
	  AC_DEFINE_UNQUOTED(LIBGTOP_MICRO_VERSION, $LIBGTOP_MICRO_VERSION)
	  AC_DEFINE_UNQUOTED(LIBGTOP_SERVER_VERSION, $LIBGTOP_SERVER_VERSION)
	  AC_MSG_RESULT(yes)
	  dnl Note that an empty true branch is not valid sh syntax.
	  ifelse([$2], [], :, [$2])
	else
	  AC_MSG_RESULT(no)
	  if test "x$3" = "xfail"; then
	    AC_MSG_ERROR(LibGTop >= $min_libgtop_version not found)
	  else
	    AC_MSG_WARN(LibGTop >= $min_libgtop_version not found)
	  fi
	fi

	AM_CONDITIONAL(HAVE_LIBGTOP, test x$no_libgtop != xyes)
])

AC_DEFUN([GNOME_INIT_LIBGTOP],[
	GNOME_LIBGTOP_HOOK($1,[ifelse([$3], [], :, [$3])],$2)
])

dnl
dnl GNOME_LIBGTOP_DOCU
dnl
dnl checks whether the documentation of LibGTop is installed
dnl

AC_DEFUN([GNOME_LIBGTOP_DOCU],
[
	AC_REQUIRE([GNOME_LIBGTOP_HOOK])

	helpdir="$LIBGTOP_DATADIR/gnome/help/libgtop"

	AC_MSG_CHECKING(whether you have the LibGTop Documentation)

	if test -f "$helpdir/C/topic.dat" ; then
	  have_libgtop_docu=yes
	  AC_DEFINE(HAVE_LIBGTOP_DOCU)
	else
	  have_libgtop_docu=no
	fi

	AC_MSG_RESULT($have_libgtop_docu)

	AM_CONDITIONAL(HAVE_LIBGTOP_DOCU, test x$have_libgtop_docu = xyes)
])

AC_DEFUN([GNOME_CHECK_OBJC],
[
dnl Look for an ObjC compiler.
dnl FIXME: extend list of possible names of ObjC compilers.
  AC_CHECK_PROGS(OBJC, $OBJC egcs, "")
  if test "x$OBJC" = "x" ; then
    AC_CHECK_PROGS(OBJC, $OBJC egcc, "")
    if test "x$OBJC" = "x" ; then
      AC_CHECK_PROGS(OBJC, $OBJC gcc, "")
    fi
  fi

  AC_REQUIRE([GNOME_PTHREAD_CHECK])

  OBJC_LIBS="-lobjc $PTHREAD_LIB"
  AC_CHECK_FUNC(sched_yield,,[
    AC_CHECK_LIB(posix4,sched_yield,
		OBJC_LIBS="$OBJC_LIBS -lposix4",, $OBJC_LIBS)])
  AC_SUBST(OBJC_LIBS)

  AC_CACHE_CHECK([if Objective C compiler ($OBJC) works],
		 ac_cv_prog_objc_works, [
    if test -n "$OBJC"; then
      cat > conftest.m <<EOF
#include <objc/Object.h>
@interface myRandomObj : Object
{
}
@end
@implementation myRandomObj
@end
int main () {
  /* No, you are not seeing double.  Remember that square brackets
     are the autoconf m4 quotes.  */
  id myid = [[myRandomObj alloc]];
  [[myid free]];
  return 0;
}
EOF

      $OBJC $CFLAGS -o conftest $LDFLAGS conftest.m $OBJC_LIBS 1>&AC_FD_CC 2>&1
      result=$?
      rm -f conftest*

      if test $result -eq 0; then
        ac_cv_prog_objc_works=yes
      fi
    else
      ac_cv_prog_objc_works=no
    fi
  ])

  AM_CONDITIONAL(OBJECTIVE_C, test x$ac_cv_prog_objc_works = xyes)
  dnl Also set the shell variable OBJECTIVE_C to "yes" or "no".
  OBJECTIVE_C=$ac_cv_prog_objc_works
])

AC_DEFUN([GNOME_INIT_OBJC],
[
	AC_MSG_CHECKING(for an obGnomeConf.sh)
	my_gnome_libdir=`$GNOME_CONFIG --libdir`
	if test -f $my_gnome_libdir/obGnomeConf.sh; then
	    . $my_gnome_libdir/obGnomeConf.sh
	    AC_MSG_RESULT(found $my_gnome_libdir)
	    ac_cv_have_gnome_objc=yes
	else
	    AC_MSG_RESULT(not found)
	    AC_MSG_WARN(Could not find the obGnomeConf.sh file that is generated by gnome-objc install)
	    ac_cv_have_gnome_objc=no
	fi
	
	dnl Add a conditional on whether or not we have gnome-objc
	AM_CONDITIONAL(HAVE_GNOME_OBJC, test x$ac_cv_have_gnome_objc = xyes)
	HAVE_GNOME_OBJC=$ac_cv_have_gnome_objc

	AC_SUBST(OBGNOME_INCLUDEDIR)
	AC_SUBST(OBGNOME_LIBS)
	AC_SUBST(OBGTK_LIBS)
])
dnl
dnl GNOME_ORBIT_HOOK (script-if-orbit-found, failflag)
dnl
dnl if failflag is "failure" it aborts if orbit is not found.
dnl

AC_DEFUN([GNOME_ORBIT_HOOK],[
	AC_PATH_PROG(ORBIT_CONFIG,orbit-config,no)
	AC_PATH_PROG(ORBIT_IDL,orbit-idl,no)
	AC_CACHE_CHECK([for working ORBit environment],gnome_cv_orbit_found,[
		if test x$ORBIT_CONFIG = xno -o x$ORBIT_IDL = xno; then
			gnome_cv_orbit_found=no
		else
			gnome_cv_orbit_found=yes
		fi
	])
	AM_CONDITIONAL(HAVE_ORBIT, test x$gnome_cv_orbit_found = xyes)
	if test x$gnome_cv_orbit_found = xyes; then
		$1
		ORBIT_CFLAGS=`orbit-config --cflags client server`
		ORBIT_LIBS=`orbit-config --use-service=name --libs client server`
		AC_SUBST(ORBIT_CFLAGS)
		AC_SUBST(ORBIT_LIBS)
	else
    		if test x$2 = xfailure; then
			AC_MSG_ERROR(ORBit not installed or installation problem)
    		fi
	fi
])

AC_DEFUN([GNOME_ORBIT_CHECK], [
	GNOME_ORBIT_HOOK([],failure)
])
# Configure paths for GNOME-PRINT
# Chris Lahey	99-2-5
# stolen from Manish Singh again
# stolen back from Frank Belew
# stolen from Manish Singh
# Shamelessly stolen from Owen Taylor

dnl AM_PATH_GNOME_PRINT([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for GNOME-PRINT, and define GNOME_PRINT_CFLAGS and GNOME_PRINT_LIBS
dnl
AC_DEFUN(AM_PATH_GNOME_PRINT,
[dnl 
dnl Get the cflags and libraries from the gnome-config script
dnl
AC_ARG_WITH(gnome-print-prefix,[  --with-gnome-print-prefix=PFX   Prefix where GNOME-PRINT is installed (optional)],
            gnome_print_prefix="$withval", gnome_print_prefix="")
AC_ARG_WITH(gnome-print-exec-prefix,[  --with-gnome-print-exec-prefix=PFX Exec prefix where GNOME-PRINT is installed (optional)],
            gnome_print_exec_prefix="$withval", gnome_print_exec_prefix="")
AC_ARG_ENABLE(gnome-printtest, [  --disable-gnome-printtest       Do not try to compile and run a test GNOME-PRINT program],
		    , enable_gnome_printtest=yes)

  if test x$gnome_print_exec_prefix != x ; then
     gnome_print_args="$gnome_print_args --exec-prefix=$gnome_print_exec_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$gnome_print_exec_prefix/bin/gnome-config
     fi
  fi
  if test x$gnome_print_prefix != x ; then
     gnome_print_args="$gnome_print_args --prefix=$gnome_print_prefix"
     if test x${GNOME_CONFIG+set} != xset ; then
        GNOME_CONFIG=$gnome_print_prefix/bin/gnome-config
     fi
  fi

  AC_PATH_PROG(GNOME_CONFIG, gnome-config, no)
  min_gnome_print_version=ifelse([$1], ,0.1.0,$1)
  AC_MSG_CHECKING(for GNOME-PRINT - version >= $min_gnome_print_version)
  no_gnome_print=""
  if test "$GNOME_CONFIG" = "no" ; then
    no_gnome_print=yes
  else
    GNOME_PRINT_CFLAGS=`$GNOME_CONFIG $gnome_printconf_args --cflags print`
    GNOME_PRINT_LIBS=`$GNOME_CONFIG $gnome_printconf_args --libs print`

    gnome_print_major_version=`$GNOME_CONFIG $gnome_print_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gnome_print_minor_version=`$GNOME_CONFIG $gnome_print_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gnome_print_micro_version=`$GNOME_CONFIG $gnome_print_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gnome_printtest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GNOME_PRINT_CFLAGS"
      LIBS="$LIBS $GNOME_PRINT_LIBS"
dnl
dnl Now check if the installed GNOME-PRINT is sufficiently new. (Also sanity
dnl checks the results of gnome-config to some extent
dnl
      rm -f conf.gnome_printtest
      AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgnomeprint/gnome-print.h>

static char*
my_strdup (char *str)
{
  char *new_str;
  
  if (str)
    {
      new_str = malloc ((strlen (str) + 1) * sizeof(char));
      strcpy (new_str, str);
    }
  else
    new_str = NULL;
  
  return new_str;
}

int main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gnome_printtest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = my_strdup("$min_gnome_print_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gnome_print_version");
     exit(1);
   }
  return 0;
#if 0
   if (($gnome_print_major_version > major) ||
      (($gnome_print_major_version == major) && ($gnome_print_minor_version > minor)) ||
      (($gnome_print_major_version == major) && ($gnome_print_minor_version == minor) && ($gnome_print_micro_version >= micro)))
    {
      return 0;
    }
  else
    {
      printf("\n*** 'gnome-config print --version' returned %d.%d.%d, but the minimum version\n", $gnome_print_major_version, $gnome_print_minor_version, $gnome_print_micro_version);
      printf("*** of GNOME-PRINT required is %d.%d.%d. If gnome-config is correct, then it is\n", major, minor, micro);
      printf("*** best to upgrade to the required version.\n");
      printf("*** If gnome-config was wrong, set the environment variable GNOME_CONFIG\n");
      printf("*** to point to the correct copy of gnome-config, and remove the file\n");
      printf("*** config.cache before re-running configure\n");
      return 1;
    }
#endif
}

],, no_gnome_print=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gnome_print" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GNOME_CONFIG" = "no" ; then
       echo "*** The gnome-config script installed by GNOME-LIBS could not be found"
       echo "*** If GNOME-PRINT was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GNOME_CONFIG environment variable to the"
       echo "*** full path to gnome-config."
     else
       if test -f conf.gnome_printtest ; then
        :
       else
          echo "*** Could not run GNOME-PRINT test program, checking why..."
          CFLAGS="$CFLAGS $GNOME_PRINT_CFLAGS"
          LIBS="$LIBS $GNOME_PRINT_LIBS"
          AC_TRY_LINK([
#include <stdio.h>
#include <libgnomeprint/gnome-print.h>
],      [ return 0; ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GNOME-PRINT or finding the wrong"
          echo "*** version of GNOME-PRINT. If it is not finding GNOME-PRINT, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GNOME-PRINT was incorrectly installed"
          echo "*** or that you have moved GNOME-PRINT since it was installed. In the latter case, you"
          echo "*** may want to edit the gnome-config script: $GNOME_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GNOME_PRINT_CFLAGS=""
     GNOME_PRINT_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GNOME_PRINT_CFLAGS)
  AC_SUBST(GNOME_PRINT_LIBS)
  rm -f conf.gnome_printtest
])

AC_DEFUN([GNOME_PRINT_CHECK], [
	AM_PATH_GNOME_PRINT(0.1.0,,[AC_MSG_ERROR(GNOME-PRINT not found)])
])
dnl
dnl And better, use gthreads instead...
dnl

AC_DEFUN([GNOME_PTHREAD_CHECK],[
	PTHREAD_LIB=""
	AC_CHECK_LIB(pthread, pthread_create, PTHREAD_LIB="-lpthread",
		[AC_CHECK_LIB(pthreads, pthread_create, PTHREAD_LIB="-lpthreads",
		    [AC_CHECK_LIB(c_r, pthread_create, PTHREAD_LIB="-lc_r",
			[AC_CHECK_FUNC(pthread_create)]
		    )]
		)]
	)
	AC_SUBST(PTHREAD_LIB)
	AC_PROVIDE([GNOME_PTHREAD_CHECK])
])
dnl GNOME_SUPPORT_CHECKS
dnl    Check for various support functions needed by the standard
dnl    Gnome libraries.  Sets LIBOBJS, might define some macros.
dnl    This should only be used when building the Gnome libs; 
dnl    Gnome clients should not need this macro.
AC_DEFUN([GNOME_SUPPORT_CHECKS],[
  # we need an `awk' to build `gnomesupport.h'
  AC_REQUIRE([AC_PROG_AWK])

  # this should go away soon
  need_gnome_support=yes

  save_LIBOBJS="$LIBOBJS"
  LIBOBJS=

  AC_CHECK_FUNCS(getopt_long,,LIBOBJS="$LIBOBJS getopt.o getopt1.o")

  # for `scandir'
  AC_HEADER_DIRENT

  # copied from `configure.in' of `libiberty'
  vars="program_invocation_short_name program_invocation_name sys_errlist"
  for v in $vars; do
    AC_MSG_CHECKING([for $v])
    AC_CACHE_VAL(gnome_cv_var_$v,
      [AC_TRY_LINK([int *p;], [extern int $v; p = &$v;],
		   [eval "gnome_cv_var_$v=yes"],
		   [eval "gnome_cv_var_$v=no"])])
    if eval "test \"`echo '$gnome_cv_var_'$v`\" = yes"; then
      AC_MSG_RESULT(yes)
      n=HAVE_`echo $v | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
      AC_DEFINE_UNQUOTED($n)
    else
      AC_MSG_RESULT(no)
    fi
  done

  AC_REPLACE_FUNCS(memmove mkstemp scandir strcasecmp strerror strndup strnlen)
  AC_REPLACE_FUNCS(strtok_r strtod strtol strtoul vasprintf vsnprintf)

  AC_CHECK_FUNCS(realpath,,LIBOBJS="$LIBOBJS canonicalize.o")

  # to include `error.c' error.c has some HAVE_* checks
  AC_CHECK_FUNCS(vprintf doprnt strerror_r)
  AM_FUNC_ERROR_AT_LINE

  # This is required if we declare setreuid () and setregid ().
  AC_TYPE_UID_T

  # see if we need to declare some functions.  Solaris is notorious for
  # putting functions into the `libc' but not listing them in the headers
  AC_CHECK_HEADERS(string.h strings.h stdlib.h unistd.h dirent.h)
  GCC_NEED_DECLARATIONS(gethostname setreuid setregid getpagesize)
  GCC_NEED_DECLARATION(scandir,[
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
])

  # Turn our LIBOBJS into libtool objects.  This is gross, but it
  # requires changes to autoconf before it goes away.
  LTLIBOBJS=`echo "$LIBOBJS" | sed 's/\.o/.lo/g'`
  AC_SUBST(need_gnome_support)
  AC_SUBST(LTLIBOBJS)

  LIBOBJS="$save_LIBOBJS"
  AM_CONDITIONAL(BUILD_GNOME_SUPPORT, test "$need_gnome_support" = yes)
])
dnl GNOME_UNDELFS_CHECKS
dnl    Check for ext2fs undel support.
dnl    Set shell variable ext2fs_undel to "yes" if we have it,
dnl    "no" otherwise.  May define USE_EXT2FSLIB for cpp.
dnl    Will set EXT2FS_UNDEL_LIBS to required libraries.

AC_DEFUN([GNOME_UNDELFS_CHECKS], [
  AC_CHECK_HEADERS(ext2fs/ext2fs.h linux/ext2_fs.h)
  ext2fs_undel=no
  EXT2FS_UNDEL_LIBS=
  if test x$ac_cv_header_ext2fs_ext2fs_h = xyes
  then
    if test x$ac_cv_header_linux_ext2_fs_h = xyes
    then
      AC_DEFINE(USE_EXT2FSLIB)
      ext2fs_undel=yes
      EXT2FS_UNDEL_LIBS="-lext2fs -lcom_err"
    fi
  fi
])
dnl GNOME_VFS_CHECKS
dnl   Check for various functions needed by libvfs.
dnl   This has various effects:
dnl     Sets GNOME_VFS_LIBS to libraries required
dnl     Sets termnet  to true or false depending on whether it is required.
dnl        If yes, defines USE_TERMNET.
dnl     Sets vfs_flags to "pretty" list of vfs implementations we include.
dnl     Sets shell variable use_vfs to yes (default, --with-vfs) or
dnl        "no" (--without-vfs).
dnl     Calls AC_SUBST(mcserv), which is either empty or "mcserv".

dnl Private define
AC_DEFUN([GNOME_WITH_VFS],[
  dnl FIXME: network checks should probably be in their own macro.
  AC_CHECK_LIB(nsl, t_accept)
  AC_CHECK_LIB(socket, socket)

  have_socket=no
  AC_CHECK_FUNCS(socket, have_socket=yes)
  if test $have_socket = no; then
    # socket is not in the default libraries.  See if it's in some other.
    for lib in bsd socket inet; do
      AC_CHECK_LIB($lib, socket, [
	  LIBS="$LIBS -l$lib"
	  have_socket=yes
	  AC_DEFINE(HAVE_SOCKET)
	  break])
    done
  fi

  have_gethostbyname=no
  AC_CHECK_FUNC(gethostbyname, have_gethostbyname=yes)
  if test $have_gethostbyname = no; then
    # gethostbyname is not in the default libraries.  See if it's in some other.
    for lib in bsd socket inet; do
      AC_CHECK_LIB($lib, gethostbyname, [LIBS="$LIBS -l$lib"; have_gethostbyname=yes; break])
    done
  fi

  vfs_flags="tarfs"
  use_net_code=false
  if test $have_socket = yes; then
      AC_STRUCT_LINGER
      AC_CHECK_FUNCS(pmap_set, , [
	 AC_CHECK_LIB(rpc, pmap_set, [
	   LIBS="-lrpc $LIBS"
	  AC_DEFINE(HAVE_PMAP_SET)
	  ])])
      AC_CHECK_FUNCS(pmap_getport pmap_getmaps rresvport)
      dnl add for source routing support setsockopt
      AC_CHECK_HEADERS(rpc/pmap_clnt.h)
      vfs_flags="$vfs_flags, mcfs, ftpfs, fish"
      use_net_code=true
  fi

  dnl
  dnl Samba support
  dnl
  smbfs=""
  SAMBAFILES=""
  AC_ARG_WITH(samba,
  	  [--with-samba	            Support smb virtual file system],[
  	  if test "x$withval != xno"; then
  		  AC_DEFINE(WITH_SMBFS)
	          vfs_flags="$vfs_flags, smbfs"
		  smbfs="smbfs.o"
		  SAMBAFILES="\$(SAMBAFILES)"
  	  fi
  ])
  AC_SUBST(smbfs)
  AC_SUBST(SAMBAFILES)
  
  dnl
  dnl The termnet support
  dnl
  termnet=false
  AC_ARG_WITH(termnet,
	  [--with-termnet             If you want a termified net support],[
	  if test x$withval = xyes; then
		  AC_DEFINE(USE_TERMNET)
		  termnet=true		
	  fi
  ])

  TERMNET=""
  AC_DEFINE(USE_VFS)
  if $use_net_code; then
     AC_DEFINE(USE_NETCODE)
  fi
  mcserv=
  if test $have_socket = yes; then
     mcserv="mcserv"
     if $termnet; then
	TERMNET="-ltermnet"
     fi
  fi

  AC_SUBST(TERMNET)
  AC_SUBST(mcserv)

dnl FIXME:
dnl GNOME_VFS_LIBS=

])

AC_DEFUN([GNOME_VFS_CHECKS],[
	use_vfs=yes
	AC_ARG_WITH(vfs,
		[--with-vfs		   Compile with the VFS code],
		use_vfs=$withval
	)
	case $use_vfs in
		yes) 	GNOME_WITH_VFS;;
		no) 	use_vfs=no;;
		*)   	use_vfs=no;;
			dnl Should we issue a warning?
	esac
])


dnl GNOME_X_CHECKS
dnl
dnl Basic X11 related checks for X11.  At the end, the following will be
dnl defined/changed:
dnl   GTK_{CFLAGS,LIBS}      From AM_PATH_GTK
dnl   CPPFLAGS		     Will include $X_CFLAGS
dnl   GNOME_HAVE_SM	     `true' or `false' depending on whether session
dnl                          management is available.  It is available if
dnl                          both -lSM and X11/SM/SMlib.h exist.  (Some
dnl                          Solaris boxes have the library but not the header)
dnl   XPM_LIBS               -lXpm if Xpm library is present, otherwise ""
dnl
dnl The following configure cache variables are defined (but not used):
dnl   gnome_cv_passdown_{x_libs,X_LIBS,X_CFLAGS}
dnl
AC_DEFUN([GNOME_X_CHECKS],
[
	AM_PATH_GTK(1.2.0,,AC_MSG_ERROR(GTK not installed, or gtk-config not in path))
	dnl Hope that GTK_CFLAGS have only -I and -D.  Otherwise, we could
	dnl   test -z "$x_includes" || CPPFLAGS="$CPPFLAGS -I$x_includes"
	dnl
	dnl Use CPPFLAGS instead of CFLAGS because AC_CHECK_HEADERS uses
	dnl CPPFLAGS, not CFLAGS
        CPPFLAGS="$CPPFLAGS $GTK_CFLAGS"

        saved_ldflags="$LDFLAGS"
        LDFLAGS="$LDFLAGS $GTK_LIBS"

	gnome_cv_passdown_x_libs="$GTK_LIBS"
	gnome_cv_passdown_X_LIBS="$GTK_LIBS"
	gnome_cv_passdown_X_CFLAGS="$GTK_CFLAGS"
	gnome_cv_passdown_GTK_LIBS="$GTK_LIBS"

        LDFLAGS="$saved_ldflags $GTK_LIBS"

dnl We are requiring GTK >= 1.1.1, which means this will be fine anyhow.
	USE_DEVGTK=true

dnl	AC_MSG_CHECKING([whether to use features from (unstable) GTK+ 1.1.x])
dnl	AC_EGREP_CPP(answer_affirmatively,
dnl	[#include <gtk/gtkfeatures.h>
dnl	#ifdef GTK_HAVE_FEATURES_1_1_0
dnl	   answer_affirmatively
dnl	#endif
dnl	], dev_gtk=yes, dev_gtk=no)
dnl	if test "$dev_gtk" = "yes"; then
dnl	   USE_DEVGTK=true
dnl	fi
dnl	AC_MSG_RESULT("$dev_gtk")

	GNOME_HAVE_SM=true
	case "$GTK_LIBS" in
	 *-lSM*)
	    dnl Already found it.
	    ;;
	 *)
	    dnl Assume that if we have -lSM then we also have -lICE.
	    AC_CHECK_LIB(SM, SmcSaveYourselfDone,
	        [GTK_LIBS="-lSM -lICE $GTK_LIBS"],GNOME_HAVE_SM=false,
		$x_libs -lICE)
	    ;;
	esac

	if test "$GNOME_HAVE_SM" = true; then
	   AC_CHECK_HEADERS(X11/SM/SMlib.h,,GNOME_HAVE_SM=false)
	fi

	if test "$GNOME_HAVE_SM" = true; then
	   AC_DEFINE(HAVE_LIBSM)
	fi

	XPM_LIBS=""
	AC_CHECK_LIB(Xpm, XpmFreeXpmImage, [XPM_LIBS="-lXpm"], , $x_libs)
	AC_SUBST(XPM_LIBS)

	AC_REQUIRE([GNOME_PTHREAD_CHECK])
        LDFLAGS="$saved_ldflags"

	AC_PROVIDE([GNOME_X_CHECKS])
])
dnl
dnl GNOME_XML_HOOK (script-if-xml-found, failflag)
dnl
dnl If failflag is "failure", script aborts due to lack of XML
dnl 
dnl Check for availability of the libxml library
dnl the XML parser uses libz if available too
dnl

AC_DEFUN([GNOME_XML_HOOK],[
	AC_PATH_PROG(GNOME_CONFIG,gnome-config,no)
	if test "$GNOME_CONFIG" = no; then
		if test x$2 = xfailure; then
			AC_MSG_ERROR(Could not find gnome-config)
		fi
	fi
	AC_CHECK_LIB(xml, xmlNewDoc, [
		$1
		AC_SUBST(GNOME_XML_LIB)
		GNOME_XML_LIB=`$GNOME_CONFIG --libs xml`
	], [
		if test x$2 = xfailure; then 
			AC_MSG_ERROR(Could not link sample xml program)
		fi
	], `$GNOME_CONFIG --libs xml`)
])

AC_DEFUN([GNOME_XML_CHECK], [
	GNOME_XML_HOOK([],failure)
])

# Check whether LC_MESSAGES is available in <locale.h>.
# Ulrich Drepper <drepper@cygnus.com>, 1995.
#
# This file can be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.

# serial 1

AC_DEFUN(AM_LC_MESSAGES,
  [if test $ac_cv_header_locale_h = yes; then
    AC_CACHE_CHECK([for LC_MESSAGES], am_cv_val_LC_MESSAGES,
      [AC_TRY_LINK([#include <locale.h>], [return LC_MESSAGES],
       am_cv_val_LC_MESSAGES=yes, am_cv_val_LC_MESSAGES=no)])
    if test $am_cv_val_LC_MESSAGES = yes; then
      AC_DEFINE(HAVE_LC_MESSAGES)
    fi
  fi])

# Search path for a program which passes the given test.
# Ulrich Drepper <drepper@cygnus.com>, 1996.
#
# This file can be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.

# serial 1

dnl AM_PATH_PROG_WITH_TEST(VARIABLE, PROG-TO-CHECK-FOR,
dnl   TEST-PERFORMED-ON-FOUND_PROGRAM [, VALUE-IF-NOT-FOUND [, PATH]])
AC_DEFUN(AM_PATH_PROG_WITH_TEST,
[# Extract the first word of "$2", so it can be a program name with args.
set dummy $2; ac_word=[$]2
AC_MSG_CHECKING([for $ac_word])
AC_CACHE_VAL(ac_cv_path_$1,
[case "[$]$1" in
  /*)
  ac_cv_path_$1="[$]$1" # Let the user override the test with a path.
  ;;
  *)
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in ifelse([$5], , $PATH, [$5]); do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$ac_word; then
      if [$3]; then
	ac_cv_path_$1="$ac_dir/$ac_word"
	break
      fi
    fi
  done
  IFS="$ac_save_ifs"
dnl If no 4th arg is given, leave the cache variable unset,
dnl so AC_PATH_PROGS will keep looking.
ifelse([$4], , , [  test -z "[$]ac_cv_path_$1" && ac_cv_path_$1="$4"
])dnl
  ;;
esac])dnl
$1="$ac_cv_path_$1"
if test -n "[$]$1"; then
  AC_MSG_RESULT([$]$1)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST($1)dnl
])
# Configure paths for GTK+
# Owen Taylor     97-11-3

dnl AM_PATH_GTK([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]]])
dnl Test for GTK, and define GTK_CFLAGS and GTK_LIBS
dnl
AC_DEFUN(AM_PATH_GTK,
[dnl 
dnl Get the cflags and libraries from the gtk-config script
dnl
AC_ARG_WITH(gtk-prefix,[  --with-gtk-prefix=PFX   Prefix where GTK is installed (optional)],
            gtk_config_prefix="$withval", gtk_config_prefix="")
AC_ARG_WITH(gtk-exec-prefix,[  --with-gtk-exec-prefix=PFX Exec prefix where GTK is installed (optional)],
            gtk_config_exec_prefix="$withval", gtk_config_exec_prefix="")
AC_ARG_ENABLE(gtktest, [  --disable-gtktest       Do not try to compile and run a test GTK program],
		    , enable_gtktest=yes)

  for module in . $4
  do
      case "$module" in
         gthread) 
             gtk_config_args="$gtk_config_args gthread"
         ;;
      esac
  done

  if test x$gtk_config_exec_prefix != x ; then
     gtk_config_args="$gtk_config_args --exec-prefix=$gtk_config_exec_prefix"
     if test x${GTK_CONFIG+set} != xset ; then
        GTK_CONFIG=$gtk_config_exec_prefix/bin/gtk-config
     fi
  fi
  if test x$gtk_config_prefix != x ; then
     gtk_config_args="$gtk_config_args --prefix=$gtk_config_prefix"
     if test x${GTK_CONFIG+set} != xset ; then
        GTK_CONFIG=$gtk_config_prefix/bin/gtk-config
     fi
  fi

  AC_PATH_PROG(GTK_CONFIG, gtk-config, no)
  min_gtk_version=ifelse([$1], ,0.99.7,$1)
  AC_MSG_CHECKING(for GTK - version >= $min_gtk_version)
  no_gtk=""
  if test "$GTK_CONFIG" = "no" ; then
    no_gtk=yes
  else
    GTK_CFLAGS=`$GTK_CONFIG $gtk_config_args --cflags`
    GTK_LIBS=`$GTK_CONFIG $gtk_config_args --libs`
    gtk_config_major_version=`$GTK_CONFIG $gtk_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$GTK_CONFIG $gtk_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$GTK_CONFIG $gtk_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gtktest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GTK_CFLAGS"
      LIBS="$GTK_LIBS $LIBS"
dnl
dnl Now check if the installed GTK is sufficiently new. (Also sanity
dnl checks the results of gtk-config to some extent
dnl
      rm -f conf.gtktest
      AC_TRY_RUN([
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>

int 
main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gtktest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = g_strdup("$min_gtk_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
   }

  if ((gtk_major_version != $gtk_config_major_version) ||
      (gtk_minor_version != $gtk_config_minor_version) ||
      (gtk_micro_version != $gtk_config_micro_version))
    {
      printf("\n*** 'gtk-config --version' returned %d.%d.%d, but GTK+ (%d.%d.%d)\n", 
             $gtk_config_major_version, $gtk_config_minor_version, $gtk_config_micro_version,
             gtk_major_version, gtk_minor_version, gtk_micro_version);
      printf ("*** was found! If gtk-config was correct, then it is best\n");
      printf ("*** to remove the old version of GTK+. You may also be able to fix the error\n");
      printf("*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n");
      printf("*** /etc/ld.so.conf. Make sure you have run ldconfig if that is\n");
      printf("*** required on your system.\n");
      printf("*** If gtk-config was wrong, set the environment variable GTK_CONFIG\n");
      printf("*** to point to the correct copy of gtk-config, and remove the file config.cache\n");
      printf("*** before re-running configure\n");
    } 
#if defined (GTK_MAJOR_VERSION) && defined (GTK_MINOR_VERSION) && defined (GTK_MICRO_VERSION)
  else if ((gtk_major_version != GTK_MAJOR_VERSION) ||
	   (gtk_minor_version != GTK_MINOR_VERSION) ||
           (gtk_micro_version != GTK_MICRO_VERSION))
    {
      printf("*** GTK+ header files (version %d.%d.%d) do not match\n",
	     GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
      printf("*** library (version %d.%d.%d)\n",
	     gtk_major_version, gtk_minor_version, gtk_micro_version);
    }
#endif /* defined (GTK_MAJOR_VERSION) ... */
  else
    {
      if ((gtk_major_version > major) ||
        ((gtk_major_version == major) && (gtk_minor_version > minor)) ||
        ((gtk_major_version == major) && (gtk_minor_version == minor) && (gtk_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of GTK+ (%d.%d.%d) was found.\n",
               gtk_major_version, gtk_minor_version, gtk_micro_version);
        printf("*** You need a version of GTK+ newer than %d.%d.%d. The latest version of\n",
	       major, minor, micro);
        printf("*** GTK+ is always available from ftp://ftp.gtk.org.\n");
        printf("***\n");
        printf("*** If you have already installed a sufficiently new version, this error\n");
        printf("*** probably means that the wrong copy of the gtk-config shell script is\n");
        printf("*** being found. The easiest way to fix this is to remove the old version\n");
        printf("*** of GTK+, but you can also set the GTK_CONFIG environment to point to the\n");
        printf("*** correct copy of gtk-config. (In this case, you will have to\n");
        printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
        printf("*** so that the correct libraries are found at run-time))\n");
      }
    }
  return 1;
}
],, no_gtk=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GTK_CONFIG" = "no" ; then
       echo "*** The gtk-config script installed by GTK could not be found"
       echo "*** If GTK was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GTK_CONFIG environment variable to the"
       echo "*** full path to gtk-config."
     else
       if test -f conf.gtktest ; then
        :
       else
          echo "*** Could not run GTK test program, checking why..."
          CFLAGS="$CFLAGS $GTK_CFLAGS"
          LIBS="$LIBS $GTK_LIBS"
          AC_TRY_LINK([
#include <gtk/gtk.h>
#include <stdio.h>
],      [ return ((gtk_major_version) || (gtk_minor_version) || (gtk_micro_version)); ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GTK or finding the wrong"
          echo "*** version of GTK. If it is not finding GTK, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"
          echo "***"
          echo "*** If you have a RedHat 5.0 system, you should remove the GTK package that"
          echo "*** came with the system with the command"
          echo "***"
          echo "***    rpm --erase --nodeps gtk gtk-devel" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GTK was incorrectly installed"
          echo "*** or that you have moved GTK since it was installed. In the latter case, you"
          echo "*** may want to edit the gtk-config script: $GTK_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GTK_CFLAGS=""
     GTK_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  rm -f conf.gtktest
])

dnl Configure Paths for Alsa
dnl Christopher Lansdown (lansdoct@cs.alfred.edu)
dnl 29/10/1998
dnl AM_PATH_ALSA(MINIMUM-VERSION)
dnl Test for libasound, and define ALSA_CFLAGS and ALSA_LIBS as appropriate.
dnl enables arguments --with-alsa-prefix= --with-alsa-enc-prefix= --disable-alsatest
dnl
AC_DEFUN(AM_PATH_ALSA,
[dnl
dnl Get the clfags and libraries for alsa
dnl
AC_ARG_WITH(alsa-prefix,[ --with-alsa-prefix=PFX  Prefix where Alsa library is installed(optional)],
	[alsa_prefix="$withval"], [alsa_prefix=""])
AC_ARG_WITH(alsa-inc-prefix, [ --with-alsa-inc-prefix=PFX  Prefix where include libraries are (optional)],
	[alsa_inc_prefix="$withval"], [alsa_inc_prefix=""])
AC_ARG_ENABLE(alsatest, [ --disable-alsatest       Do not try to compile and run a test Alsa program], [enable_alsatest=no], [enable_alsatest=yes])

dnl Add any special include directories
AC_MSG_CHECKING(for ALSA CFLAGS)
if test "$alsa_inc_prefix" != "" ; then
	ALSA_CFLAGS="$ALSA_CFLAGS -I$alsa_inc_prefix"
	CFLAGS="-I$alsa_inc_prefix"
fi
AC_MSG_RESULT($ALSA_CFLAGS)

dnl add any special lib dirs
AC_MSG_CHECKING(for ALSA LDFLAGS)
if test "$alsa_prefix" != "" ; then
	ALSA_LIBS="$ALSA_LIBS -L$alsa_prefix"
	LIBS="-L$alsa_prefix"
fi

dnl add the alsa library
ALSA_LIBS="$ALSA_LIBS -lasound"
LDFLAGS="$ALSA_LIBS -lasound"
AC_MSG_RESULT($ALSA_LIBS)

dnl Check for the presence of the library
dnl if test $enable_alsatest = yes; then
dnl   AC_MSG_CHECKING(for working libasound)
dnl   AC_TRY_RUN([
dnl #include <sys/asoundlib.h>
dnl void main(void)
dnl {
dnl   snd_cards();
dnl   exit(0);
dnl }
dnl ],
dnl    [AC_MSG_RESULT("present")],
dnl    [AC_MSG_RESULT("not found. ")
dnl    AC_MSG_ERROR(Fatal error: Install alsa-lib package or use --with-alsa-prefix option...)],
dnl    [AC_MSG_RESULT(unsopported)
dnl     AC_MSG_ERROR(Cross-compiling isn't supported...)]
dnl  )
dnl fi

dnl Check for a working version of libasound that is of the right version.
min_alsa_version=ifelse([$1], ,0.1.1,$1)
AC_MSG_CHECKING(for libasound headers version >= $min_alsa_version)
no_alsa=""
    alsa_min_major_version=`echo $min_alsa_version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    alsa_min_minor_version=`echo $min_alsa_version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    alsa_min_micro_version=`echo $min_alsa_version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

AC_LANG_SAVE
AC_LANG_C
AC_TRY_COMPILE([
#include <sys/asoundlib.h>
], [
void main(void)
{
/* ensure backward compatibility */
#if !defined(SND_LIB_MAJOR) && defined(SOUNDLIB_VERSION_MAJOR)
#define SND_LIB_MAJOR SOUNDLIB_VERSION_MAJOR
#endif
#if !defined(SND_LIB_MINOR) && defined(SOUNDLIB_VERSION_MINOR)
#define SND_LIB_MINOR SOUNDLIB_VERSION_MINOR
#endif
#if !defined(SND_LIB_SUBMINOR) && defined(SOUNDLIB_VERSION_SUBMINOR)
#define SND_LIB_SUBMINOR SOUNDLIB_VERSION_SUBMINOR
#endif

#  if(SND_LIB_MAJOR > $alsa_min_major_version)
  exit(0);
#  else
#    if(SND_LIB_MAJOR < $alsa_min_major_version)
#       error not present
#    endif

#   if(SND_LIB_MINOR > $alsa_min_minor_version)
  exit(0);
#   else
#     if(SND_LIB_MINOR < $alsa_min_minor_version)
#          error not present
#      endif

#      if(SND_LIB_SUBMINOR < $alsa_min_micro_version)
#        error not present
#      endif
#    endif
#  endif
exit(0);
}
],
  [AC_MSG_RESULT(found.)],
  [AC_MSG_RESULT(not present.)
   AC_MSG_ERROR(Sufficiently new version of libasound not found.)]
)
AC_LANG_RESTORE

dnl Now that we know that we have the right version, let's see if we have the library and not just the headers.
AC_CHECK_LIB([asound], [snd_cards],,
	[AC_MSG_ERROR(No linkable libasound was found.)]
)

dnl That should be it.  Now just export out symbols:
AC_SUBST(ALSA_CFLAGS)
AC_SUBST(ALSA_LIBS)
])
