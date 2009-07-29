%define parrot_version 1.4.0

Name:           rakudo
Version:        build20
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

#%define rakudo_libs %{_libdir}/perl6/rakudo
%define rakudo_libs %{parrot_versiondirpath}/languages/perl6/lib

# Versions don't go easily in install_files.pl yet
#%define relative_rakudo_dynext %{name}/%{version}
%define relative_rakudo_dynext %{name}/dynext
%define rakudo_dynext %{parrot_dynext}/%{relative_rakudo_dynext}

%description
Rakudo Perl 6 is an implementation of the Perl 6 specification which
runs on the Parrot virtual machine.  Perl 6 is a programming language
which supersedes earlier versions of Perl.  

%prep
%setup -q

%build
echo Building with root $RPM_BUILD_ROOT
%{__perl} Configure.pl
make

%install
rm -rf $RPM_BUILD_ROOT

make install DESTDIR=$RPM_BUILD_ROOT
#echo first find
#find $RPM_BUILD_ROOT

#pushd $RPM_BUILD_ROOT/%{parrot_dynext}
#for i in %{relative_rakudo_dynext}/*.so; do 
#	ln -s $i
#done
#popd

#echo second find
#find $RPM_BUILD_ROOT

%check
# make test < /dev/null
# %{?_with_fulltest:make fulltest < /dev/null}
# make test || :
# %{?_with_fulltest:make fulltest || :}

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
#%{rakudo_dynext}/perl6_group.so
#%{rakudo_dynext}/perl6_ops.so
#%{rakudo_dynext}/perl6_ops_cg.so
#%{rakudo_dynext}/perl6_ops_cgp.so
#%{rakudo_dynext}/perl6_ops_switch.so
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
