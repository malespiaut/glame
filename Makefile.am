SUBDIRS = intl libltdl macros src doc data po

ACLOCAL_AMFLAGS = -I . -I m4 -I macros

EXTRA_DIST = TODO MAINTAINERS CREDITS BUGS autogen.sh glame.spec

update-changelog:
	rcs2log -u "xwolf	Johannes Hirche	jhirche@gris.uni-tuebingen.de" -u "nold	Daniel Kobras	kobras@linux.de" -u "richi	Richard Guenther	rguenth@tat.physik.uni-tuebingen.de" -u "mag	Alexander Ehlert	ehlert@linux.de" -u "garrison	Jim Garrison	garrison@users.sourceforge.net" -u "mainzelm	Martin Gasbichler	gasbichl@informatik.uni-tuebingen.de" -u "navratil	Joe Navratil	navratil@umich.edu" > $(top_srcdir)/ChangeLog

dist-hook:
	mkdir $(distdir)/debian
	cp -p `find $(srcdir)/debian/ -type f -maxdepth 1` $(distdir)/debian/

