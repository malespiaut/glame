#!/bin/sh

aclocal -I .
libtoolize --copy --automake
#--ltdl
autoheader
autoconf
automake --add-missing
