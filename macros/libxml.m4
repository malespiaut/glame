dnl Code shamelessly stolen from glib-config by Sebastian Rittau
dnl AMG_PATH_XML([MINIMUM-VERSION [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
AC_DEFUN(AMG_PATH_XML,[
AC_ARG_WITH(xml-prefix,
            [  --with-xml-prefix=PFX    Prefix where libxml is installed (optional)],
            xml_config_prefix="$withval", xml_config_prefix="")
AC_ARG_ENABLE(xmltest,
              [  --disable-xmltest        Do not try to compile and run a test XML program],,
              enable_xmltest=yes)

  if test x$xml_config_prefix != x ; then
    xml_config_args="$xml_config_args --prefix=$xml_config_prefix"
    if test x${XML_CONFIG+set} != xset ; then
      XML_CONFIG=$xml_config_prefix/bin/xml-config
    fi
  fi

  AC_PATH_PROG(XML_CONFIG, xml-config, no)
  min_xml_version=ifelse([$1], ,2.0.0, [$1])
  AC_MSG_CHECKING(for libxml - version >= $min_xml_version)
  no_xml=""
  if test "$XML_CONFIG" = "no" ; then
    no_xml=yes
  else
    XML_CFLAGS=`$XML_CONFIG $xml_config_args --cflags`
    XML_LIBS=`$XML_CONFIG $xml_config_args --libs`
    xml_config_major_version=`$XML_CONFIG $xml_config_args --version | \
      sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    xml_config_minor_version=`$XML_CONFIG $xml_config_args --version | \
      sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    xml_config_micro_version=`$XML_CONFIG $xml_config_args --version | \
      sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_xmltest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $XML_CFLAGS"
      LIBS="$XML_LIBS $LIBS"
dnl
dnl Now check if the installed libxml is sufficiently new.
dnl
      rm -f conf.xmltest
      AC_TRY_RUN([
#include <stdlib.h>
#include <stdio.h>
#include <parser.h>

int
main()
{
  int xml_major_version, xml_minor_version, xml_micro_version;
  int major, minor, micro;
  char *tmp_version;

  system("touch conf.xmltest");

  tmp_version = xmlStrdup("$min_xml_version");
  if(sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
    printf("%s, bad version string\n", "$min_xml_version");
    exit(1);
  }

  xml_major_version = $xml_config_major_version;
  xml_minor_version = $xml_config_minor_version;
  xml_micro_version = $xml_config_micro_version;

      if ((xml_major_version > major) ||
          ((xml_major_version == major) && (xml_minor_version > minor)) ||
          ((xml_major_version == major) && (xml_minor_version == minor) &&
           (xml_micro_version >= micro)))
        {
          return 0;
        }
      else
        {
          printf("\n*** An old version of libxml (%d.%d.%d) was found.\n",
            xml_major_version, xml_minor_version, xml_micro_version);
          printf("*** You need a version of libxml newer than %d.%d.%d. The latest version of\n",
            major, minor, micro);
          printf("*** libxml is always available from ftp://ftp.gnome.org.\n");
          printf("***\n");
          printf("*** If you have already installed a sufficiently new version, this error\n");
          printf("*** probably means that the wrong copy of the xml-config shell script is\n");
          printf("*** being found. The easiest way to fix this is to remove the old version\n");
          printf("*** of libxml, but you can also set the XML_CONFIG environment to point to the\n");
          printf("*** correct copy of xml-config. (In this case, you will have to\n");
          printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
          printf("*** so that the correct libraries are found at run-time))\n");
        }
  return 1;
}
],, no_xml=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])

      CFLAGS="$ac_save_CFLAGS"
      LIBS="$ac_save_LIBS"
    fi
  fi

  if test "x$no_xml" = x ; then
    AC_MSG_RESULT(yes)
    ifelse([$2], , :, [$2])
  else
    AC_MSG_RESULT(no)
    if test "$XML_CONFIG" = "no" ; then
      echo "*** The xml-config script installed by libxml could not be found"
      echo "*** If libxml was installed in PREFIX, make sure PREFIX/bin is in"
      echo "*** your path, or set the XML_CONFIG environment variable to the"
      echo "*** full path to xml-config."
    else
      if test -f conf.xmltest ; then
        :
      else
        echo "*** Could not run libxml test program, checking why..."
        dnl CFLAGS="$CFLAGS $XML_CFLAGS"
        dnl LIBS="$LIBS $XML_LIBS"
        dnl FIXME: AC_TRY_LINK
      fi
    fi

    XML_CFLAGS=""
    XML_LIBS=""
    ifelse([$3], , :, [$3])
  fi
  AC_SUBST(XML_CFLAGS)
  AC_SUBST(XML_LIBS)
  rm -f conf.xmltest
])