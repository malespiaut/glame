EXTRA_DIST = coding-style.txt
SUBDIRS = pix
info_TEXINFOS = glame.texi glame-dev.texi
glame_TEXINFOS = plugins.texi gui.texi cglame.texi quickstart.texi version.texi
glame_dev_TEXINFOS = filter.texi filter-tutorial.texi swapfile.texi \
		 gldb.texi glsignal.texi conversion.texi gpsm.texi \
		 plugin.texi version-dev.texi
%.html:	%.texi	
	@rm -f $@
	@test -e pix || ln -sf @srcdir@/pix .
	$(MAKEINFO) -P @srcdir@ --html --no-split $<

clean-local:
	-test -L pix && rm -f pix
	rm -f glame.html glame-dev.html
	rm -f glame-dev.info glame.info glame.info-1 glame.info-2 glame.info-3

