#!/bin/sh

rm -f config.cache
aclocal -I .
libtoolize --copy --automake
#--ltdl
autoheader
autoconf
automake --add-missing
