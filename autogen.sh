#!/bin/sh

rm -f config.cache libtool ltconfig ltmain.sh po/Makefile.in.in
rm -rf intl/ ABOUT-NLS libltdl
yes n | gettextize --copy
aclocal -I . -I macros
libtoolize --copy --automake --ltdl
autoheader
touch ltconfig
automake --copy --add-missing
autoconf
