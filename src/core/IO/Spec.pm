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
#?if !jvm
          nqp::p6box_s(nqp::atkey(nqp::backendconfig(), 'osname'));
#?endif
        IO::Spec::{%module{ lc $token } // 'Unix'};
    }
}

# temporary non-lazy initialization of $*SPEC
PROCESS::<$SPEC> = IO::Spec.select;

# vim: ft=perl6 expandtab sw=4
