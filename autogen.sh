#!/bin/sh

rm -f config.cache libtool ltconfig ltmain.sh
aclocal -I . -I macros
libtoolize --copy --automake
autoheader
autoconf
touch ltconfig
automake --add-missing
