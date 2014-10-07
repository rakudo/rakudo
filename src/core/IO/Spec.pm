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
#?if parrot
          nqp::p6box_s(nqp::atkey(nqp::atpos(pir::getinterp__P, pir::const::IGLOBALS_CONFIG_HASH), 'osname'));
#?endif
#?if jvm
          nqp::p6box_s(nqp::atkey(nqp::jvmgetproperties(), 'os.name'));
#?endif
#?if moar
          nqp::p6box_s(nqp::atkey(nqp::backendconfig(), 'osname'));
#?endif
        ::('IO::Spec::' ~ ( %module{ lc $token } // 'Unix' ));
    }

    method MODULE(IO::Spec:U:) {
       DEPRECATED('$*SPEC');
       $*SPEC;
    }

    method FSTYPE(IO::Spec:U: $OS?) {
        DEPRECATED('$*SPEC.select');
        self.select($OS);
    }

    method os (Str $OS?) {
       DEPRECATED('$*SPEC.select');
       self.select($OS);
    }

    method tmpdir() { # people seem to expect IO::Spec.tmpdir to return a Str
        DEPRECATED('$*TMPDIR');
        $*SPEC.tmpdir.path;
    }

    method canonpath(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.canonpath');
        $*SPEC.canonpath( |c );
    }
    method curdir(IO::Spec:U:) {
        DEPRECATED('$*SPEC.curdir');
        $*SPEC.curdir();
    }
    method updir(IO::Spec:U:) {
        DEPRECATED('$*SPEC.updir');
        $*SPEC.updir();
    }
    method rootdir(IO::Spec:U:) {
        DEPRECATED('$*SPEC.rootdir');
        $*SPEC.rootdir();
    }
    method devnull(IO::Spec:U:) {
        DEPRECATED('$*SPEC.devnull');
        $*SPEC.devnull();
    }
    method is-absolute(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.is-absolute');
        $*SPEC.is-absolute( |c );
    }
    method no-parent-or-current-test(IO::Spec:U:) {
        DEPRECATED('$*SPEC.curupdir');
        $*SPEC.curupdir;
    }
    method path(IO::Spec:U:) {
        DEPRECATED('$*SPEC.path');
        $*SPEC.path();
    }
    method split(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.split');
        $*SPEC.split( |c );
    }
    method join(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.join');
        $*SPEC.join( |c );
    }
    method splitpath(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.splitpath');
        $*SPEC.splitpath( |c );
    }
    method catpath(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.catpath');
        $*SPEC.catpath( |c );
    }
    method catfile(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.catfile');
        $*SPEC.catfile( |c );
    }
    method splitdir(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.splitdir');
        $*SPEC.splitdir( |c );
    }
    method catdir(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.catdir');
        $*SPEC.catdir( |c );
    }
    method abs2rel(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.abs2rel');
        $*SPEC.abs2rel( |c );
    }
    method rel2abs(IO::Spec:U: |c ) {
        DEPRECATED('$*SPEC.rel2abs');
        $*SPEC.rel2abs( |c );
    }
}

# temporary non-lazy initialization of $*SPEC
PROCESS::<$SPEC> = IO::Spec.select;

nqp::gethllsym('perl6', 'ModuleLoader').register_absolute_path_func(
    sub ($path) { return IO::Spec.rel2abs($path); }
);

# vim: ft=perl6 expandtab sw=4
