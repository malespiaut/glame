#!/bin/sh

rm -f config.cache libtool ltconfig ltmain.sh
aclocal -I . -I macros
libtoolize --copy --automake --ltdl
autoheader
autoconf
touch ltconfig
automake --copy --add-missing
