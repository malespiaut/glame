#!/bin/sh

SOURCES="src/gui/main.c src/gui/swapfilegui.c"
LANGS="de fr"

for l in $LANGS; do
	cp po/$l.po messages.po
	for s in $SOURCES; do
		xgettext -a --debug -j -F $s
	done
	mv messages.po po/$l.po.new
done

