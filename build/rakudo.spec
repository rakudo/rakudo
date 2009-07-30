%define parrot_version 1.4.0

Name:           rakudo
Version:        2009-08
Release:        1
Summary:        Rakudo Perl 6
License:        Artistic 2.0
Group:          Development/Libraries
URL:            http://www.rakudo.org/
Source0:        http://www.pmichaud.com/perl6/rakudo-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
BuildRequires:  parrot           >= %parrot_version
BuildRequires:  parrot-devel     >= %parrot_version

%define parrot_versiondirname %{parrot_version}-devel
%define parrot_versiondirpath  %{_libdir}/parrot/%{parrot_versiondirname}

%define parrot_dynext %{parrot_versiondirpath}/dynext

%define rakudo_libs %{parrot_versiondirpath}/languages/perl6/lib

# Versions don't go easily in install_files.pl yet

%description
Rakudo Perl 6 is an implementation of the Perl 6 language for
the Parrot virtual machine.  More information about Perl 6 is
available from http://perl6-projects.org/ .

%prep
%setup -q

%build
echo Building with root $RPM_BUILD_ROOT
%{__perl} Configure.pl
make

%install
rm -rf $RPM_BUILD_ROOT

make install DESTDIR=$RPM_BUILD_ROOT

%check
make test < /dev/null

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%doc CREDITS README
%doc docs
%{parrot_dynext}/perl6_group.so
%{parrot_dynext}/perl6_ops.so
%{parrot_dynext}/perl6_ops_cg.so
%{parrot_dynext}/perl6_ops_cgp.so
%{parrot_dynext}/perl6_ops_switch.so
%{_bindir}/perl6
%{parrot_versiondirpath}/languages/perl6/perl6.pbc
%{rakudo_libs}/Test.pm
%{rakudo_libs}/Safe.pm

%changelog
* Wed Jul 22 2009 wayland <wayland@wayland.id.au> 0.20
- Updated to latest version

* Fri Mar  6 2009 wayland <wayland@wayland.id.au> 0.17
- created from parrot.spec
- Didn't redo any of the files stuff
- Played with things 'til it worked
