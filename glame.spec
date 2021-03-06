# $Id: glame.spec,v 1.9 2005/03/28 16:52:17 richi Exp $
# RPM spec file for GLAME.
#
# This file is adapted from the Mandrake spec for their GLAME rpms.
# Thanks to Lenny Cartier <lenny@mandrakesoft.com> and 
# Renaud Chaillat <rchaillat@mandrakesoft.com>.
#
%define name glame
%define version   2.0.1
%define release   1

Summary:   A sound editor and synthesis tool
Name:      %{name}
Version:   %{version}
Release:   %{release}
Copyright: GPL
Group:     Applications/Multimedia
Source0:   %{name}-%{version}.tar.gz
URL:       http://glame.sourceforge.net/ 
Buildroot: %{_tmppath}/%{name}-buildroot


%description
GLAME is meant to be the GIMP of audio processing. It is designed to be
a powerful, fast, stable, and easily extensible sound editor for Linux
and compatible systems. Supported platforms are Linux and IRIX. 

%prep
rm -rf $RPM_BUILD_ROOT

%setup

%build

%configure
make 

%install
%makeinstall

%post
/sbin/ldconfig
/sbin/install-info --info-dir=%{_infodir} %{_infodir}/glame.info
/sbin/install-info --info-dir=%{_infodir} %{_infodir}/glame-dev.info

%postun
/sbin/ldconfig
/sbin/install-info --delete --info-dir=%{_infodir} %{_infodir}/glame.info
/sbin/install-info --delete --info-dir=%{_infodir} %{_infodir}/glame-dev.info

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr (-,root,root)
%doc AUTHORS BUGS COPYING CREDITS ChangeLog NEWS README TODO
%{_bindir}/*
%{_libdir}/glame/*
%{_datadir}/%{name}/
%{_infodir}/glame*

%changelog
* Mon Mar 28 2005 Richard G?nther <richi@users.sf.net> 2.0.1

- New upstream version.

* Thu Oct 28 2004 Daniel Kobras <kobras@linux.de> 1.0.3

- New upstream version.

* Fri Jun 27 2003 Richard G?nther <richi@users.sf.net> 1.0.1

- New upstream version.

* Sat Apr 19 2003 Richard G?nther <richi@users.sf.net> 1.0.0

- New upstream version.

* Thu Nov 21 2002 Richard G?nther <richi@users.sf.net> 0.6.4

- New upstream version.

* Sun Mar 24 2002 Daniel Kobras <kobras@linux.de> 0.6.2

- New upstream version.

* Sun Jan 27 2002 Daniel Kobras <kobras@linux.de> 0.6.1

- New upstream version.

* Sun Dec 23 2001 Daniel Kobras <kobras@linux.de> 0.6.0

- New stable release.

* Fri Nov 09 2001 Daniel Kobras <kobras@linux.de> 0.5.3CVS

- New upstream version.
- Remove references to obsolete libglame.
- Register devel manual.

* Tue Jul 03 2001 Daniel Kobras <kobras@linux.de> 0.5.1

- New upstream version.

* Thu May 17 2001 Daniel Kobras <kobras@linux.de> 0.5.0CVS-3

- Better fix for /usr/info/dir issue, kudos to Pavel Polischouk.

* Fri May 11 2001 Daniel Kobras <kobras@linux.de> 0.5.0CVS-2

- Apply install-info fix thanks to Ronald Cole.
- Do not package /usr/info/dir.

* Thu May 03 2001 Daniel Kobras <kobras@linux.de> 0.5.0CVS-1

- Merge with Mandrake's spec file for GLAME 0.4.0.
- Compile with low-latency enabled.
- Don't use mp3lame support in packages.
