#!/bin/sh

# glame requires at least automake 1.6 and autoconf 2.5

rm -f config.cache libtool ltconfig ltmain.sh po/Makefile.in.in
rm -rf intl/ m4/ ABOUT-NLS libltdl
autopoint
aclocal -I . -I macros -I m4
libtoolize --copy --automake --ltdl
autoheader
touch ltconfig
automake --copy --add-missing
autoconf
