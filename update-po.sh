#!/bin/sh

LANGS="de fr"

for l in $LANGS; do
	cp po/$l.po messages.po
	for s in `cat po/POTFILES.in`; do
		xgettext -a --debug --exclude-file=po/ignore -j -F $s
	done
	mv messages.po po/$l.po.new
done

