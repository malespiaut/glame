#!/bin/sh

SOURCES="src/gui/main.c src/gui/swapfilegui.c src/gui/apply.c src/gui/glame_accelerator.c src/gui/glame_console.c src/gui/importexport.c src/gui/waveeditgui.c src/gui/edit_filter/canvasfilter.c src/gui/edit_filter/canvaspipe.c src/gui/edit_filter/canvasport.c src/gui/edit_filter/filtereditgui.c src/gui/edit_filter/glamecanvas.c"
LANGS="de fr"

for l in $LANGS; do
	cp po/$l.po messages.po
	for s in $SOURCES; do
		xgettext -a --debug --exclude-file=po/ignore -j -F $s
	done
	mv messages.po po/$l.po.new
done

