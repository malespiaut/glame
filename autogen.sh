#!/bin/sh

rm -f config.cache
aclocal -I . -I macros
libtoolize --copy --automake
#--ltdl
autoheader
autoconf
automake --add-missing
