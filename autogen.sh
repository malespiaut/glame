#!/bin/sh

rm -f config.cache libtool ltconfig ltmain.sh po/Makefile.in.in
rm -rf intl/
gettextize --copy >/dev/null
aclocal -I . -I macros
libtoolize --copy --automake --ltdl
autoheader
autoconf
touch ltconfig
automake --copy --add-missing
