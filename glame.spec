%define name glame
%define version   0.4.0beta
%define release   glame

Summary:   A sound editor and synthesis tool
Name:      %{name}
Version:   %{version}
Release:   %{release}
Copyright: GPL
Group:     Sound
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

%make

%install
%makeinstall

(cd $RPM_BUILD_ROOT
mkdir -p ./usr/lib/menu
cat > ./usr/lib/menu/%{name} <<EOF
?package(%{name}):\
command="/usr/bin/glame"\
title="Glame"\
longtitle="GNU/Linux Audio Mechanics"\
needs="x11"\
section="Multimedia/Sound"
EOF
)
 
%post
/sbin/ldconfig
/sbin/install-info %{_infodir}/glame.info.bz2 %{_infodir}/dir
/sbin/install-info %{_infodir}/glame.info-1.bz2 %{_infodir}/dir
/sbin/install-info %{_infodir}/glame.info-2.bz2 %{_infodir}/dir
/sbin/install-info %{_infodir}/glame.info-3.bz2 %{_infodir}/dir
%{update_menus}

%postun
/sbin/ldconfig
/sbin/install-info --delete %{_infodir}/glame.info.bz2 %{_infodir}/dir
/sbin/install-info --delete %{_infodir}/glame.info-1.bz2 %{_infodir}/dir
/sbin/install-info --delete %{_infodir}/glame.info-2.bz2 %{_infodir}/dir
/sbin/install-info --delete %{_infodir}/glame.info-3.bz2 %{_infodir}/dir
%{clean_menus}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr (-,root,root)
%doc AUTHORS COPYING ChangeLog INSTALL NEWS README TODO
%{_bindir}/*
%{_libdir}/libgla*
%{_libdir}/glame/*
%{_datadir}/%{name}/
%{_infodir}/*
%{_menudir}/*

%changelog
