SUBDIRS = macros src doc data

EXTRA_DIST = TODO MAINTAINERS CREDITS autogen.sh

update-changelog:
	rcs2log -u "xwolf	Johannes Hirche	jhirche@gris.uni-tuebingen.de" -u "nold	Daniel Kobras	kobras@linux.de" -u "richi	Richard Guenther	zxmpm11@student.uni-tuebingen.de" -u "mag	Alexander Ehlert	ehlert@linux.de" -u "garrison	Jim Garrison	garrison@users.sourceforge.net" -u "mainzelm	Martin Gasbichler	gasbichl@informatik.uni-tuebingen.de" -u "navratil	Joe Navratil	navratil@umich.edu" > $(top_srcdir)/ChangeLog

dist-hook:
	mkdir $(distdir)/debian
	cp -p $(srcdir)/debian/[^CS]* $(distdir)/debian/

