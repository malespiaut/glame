#!/bin/sh

SOURCES="src/gui/main.c src/gui/swapfilegui.c"
LANGS="de fr"

for l in $LANGS; do
	cp po/$l.po messages.po
	for s in $SOURCES; do
# should remove bogous source references
		xgettext --debug --no-location -a -j $s
		xgettext --debug -a -j -F $s
	done
	mv messages.po po/$l.po.new
done

