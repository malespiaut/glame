EXTRA_DIST = coding-style.txt filter.fig
info_TEXINFOS = glame.texi glame-dev.texi
glame_TEXINFOS = plugins.texi gui.texi cglame.texi quickstart.texi
glame_dev_TEXINFOS = filter.texi filter-tutorial.texi swapfile.texi \
		 gldb.texi glsignal.texi conversion.texi gpsm.texi \
		 plugin.texi 
%.html:	%.texi	
	@cd @srcdir@ && rm -f $@
	cd @srcdir@ && $(MAKEINFO) --html $<

