EXTRA_DIST = coding-style.txt development.txt masterplan.txt \
	meisterplan_de.txt filter.fig
info_TEXINFOS = glame.texi
glame_TEXINFOS = filter.texi filter-tutorial.texi swapfile.texi \
                 plugin.texi

%.html:	%.texi	
	@cd @srcdir@ && rm -f $@
	cd @srcdir@ && $(MAKEINFO) --html $<

