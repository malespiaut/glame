# $Id: glame.spec,v 1.3 2001/05/11 08:27:52 nold Exp $
# RPM spec file for GLAME.
#
# This file is adapted from the Mandrake spec for their GLAME rpms.
# Thanks to Lenny Cartier <lenny@mandrakesoft.com> and 
# Renaud Chaillat <rchaillat@mandrakesoft.com>.
#
%define name glame
%define version   0.5.0CVS
%define release   2

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

%configure --enable-low-latency --disable-mp3lame
make 

%install
%makeinstall
# Don't be gross--let other info entries live. Postinst will fixup.
rm -f "$RPM_BUILD_ROOT/%{_infodir}/dir"

%post
/sbin/ldconfig
/sbin/install-info --info-dir=%{_infodir} %{_infodir}/glame.info

%postun
/sbin/ldconfig
/sbin/install-info --delete --info-dir=%{_infodir} %{_infodir}/glame.info

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr (-,root,root)
%doc AUTHORS BUGS COPYING CREDITS ChangeLog NEWS README TODO
%{_bindir}/*
%{_libdir}/libgla*
%{_libdir}/glame/*
%{_datadir}/%{name}/
%{_infodir}/*

%changelog
* Fri May 11 2001 Daniel Kobras <kobras@linux.de> 0.5.0CVS-2

- Apply install-info fix thanks to Ronald Cole.
- Do not package /usr/info/dir.

* Thu May 03 2001 Daniel Kobras <kobras@linux.de> 0.5.0CVS-1

- Merge with Mandrake's spec file for GLAME 0.4.0.
- Compile with low-latency enabled.
- Don't use mp3lame support in packages.
