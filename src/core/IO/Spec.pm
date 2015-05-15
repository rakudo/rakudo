my class IO::Spec {

    my %module =                # only list the non-Unix ones in lowercase
        'mswin32' => 'Win32',
        'os2' =>     'Win32',
        'dos'     => 'Win32',
        'symbian' => 'Win32',
        'netware' => 'Win32',
        'win32'   => 'Win32',
        'cygwin'  => 'Cygwin',
        'qnx'     => 'QNX',
        'nto'     => 'QNX',
        # <MacOS Mac>  »=>» 'Mac',
        # 'VMS'     => 'VMS'
    ;

    method select(IO::Spec:U: $token? is copy) {

        # really just a way of getting $*DISTRO.name before we have %*ENV
        $token //=
#?if jvm
          nqp::p6box_s(nqp::atkey(nqp::jvmgetproperties(), 'os.name'));
#?endif
#?if moar
          nqp::p6box_s(nqp::atkey(nqp::backendconfig(), 'osname'));
#?endif
        IO::Spec::{%module{ lc $token } // 'Unix'};
    }

    method MODULE(IO::Spec:U:) {
       DEPRECATED('$*SPEC', |<2014.10 2015.09>);
       $*SPEC;
    }

    method FSTYPE(IO::Spec:U: $OS?) {
        DEPRECATED('$*SPEC.select', |<2014.10 2015.09>);
        self.select($OS);
    }

    method os (Str $OS?) {
       DEPRECATED('$*SPEC.select', |<2014.10 2015.09>);
       self.select($OS);
    }

    method tmpdir() { # people seem to expect IO::Spec.tmpdir to return a Str
        DEPRECATED('$*TMPDIR', |<2014.10 2015.09>);
        $*SPEC.tmpdir.path;
    }

    method canonpath(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.canonpath', |<2014.10 2015.09>);
        $*SPEC.canonpath( |c );
    }
    method curdir(IO::Spec:U:) {
        DEPRECATED('$*SPEC.curdir', |<2014.10 2015.09>);
        $*SPEC.curdir();
    }
    method updir(IO::Spec:U:) {
        DEPRECATED('$*SPEC.updir', |<2014.10 2015.09>);
        $*SPEC.updir();
    }
    method rootdir(IO::Spec:U:) {
        DEPRECATED('$*SPEC.rootdir', |<2014.10 2015.09>);
        $*SPEC.rootdir();
    }
    method devnull(IO::Spec:U:) {
        DEPRECATED('$*SPEC.devnull', |<2014.10 2015.09>);
        $*SPEC.devnull();
    }
    method is-absolute(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.is-absolute', |<2014.10 2015.09>);
        $*SPEC.is-absolute( |c );
    }
    method no-parent-or-current-test(IO::Spec:U:) {
        DEPRECATED('$*SPEC.curupdir', |<2014.10 2015.09>);
        $*SPEC.curupdir;
    }
    method path(IO::Spec:U:) {
        DEPRECATED('$*SPEC.path', |<2014.10 2015.09>);
        $*SPEC.path();
    }
    method split(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.split', |<2014.10 2015.09>);
        $*SPEC.split( |c );
    }
    method join(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.join', |<2014.10 2015.09>);
        $*SPEC.join( |c );
    }
    method splitpath(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.splitpath', |<2014.10 2015.09>);
        $*SPEC.splitpath( |c );
    }
    method catpath(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.catpath', |<2014.10 2015.09>);
        $*SPEC.catpath( |c );
    }
    method catfile(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.catfile', |<2014.10 2015.09>);
        $*SPEC.catfile( |c );
    }
    method splitdir(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.splitdir', |<2014.10 2015.09>);
        $*SPEC.splitdir( |c );
    }
    method catdir(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.catdir', |<2014.10 2015.09>);
        $*SPEC.catdir( |c );
    }
    method abs2rel(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.abs2rel', |<2014.10 2015.09>);
        $*SPEC.abs2rel( |c );
    }
    method rel2abs(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.rel2abs', |<2014.10 2015.09>);
        $*SPEC.rel2abs( |c );
    }
}

# temporary non-lazy initialization of $*SPEC
PROCESS::<$SPEC> = IO::Spec.select;

nqp::gethllsym('perl6', 'ModuleLoader').register_absolute_path_func(
    sub ($path) { return $*SPEC.rel2abs($path); }
);

# vim: ft=perl6 expandtab sw=4
