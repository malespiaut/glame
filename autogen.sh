#!/bin/sh

# glame requires at least automake 1.9 and autoconf 2.59

rm -f config.cache libtool ltconfig ltmain.sh po/Makefile.in.in
rm -rf intl/ ABOUT-NLS libltdl
autopoint --force
aclocal -I . -I macros
libtoolize --copy --automake --ltdl
autoheader
touch ltconfig
automake --copy --add-missing
autoconf

cd libltdl
aclocal -I .
autoheader
automake --copy --add-missing
autoconf
cd ..
