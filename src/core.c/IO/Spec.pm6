my class VM { ... }

my class IO::Spec {

#?if moar
    my constant $module = nqp::hash( # only list the non-Unix ones in lowercase
#?endif
#?if !moar
    my $module := nqp::hash( # only list the non-Unix ones in lowercase
#?endif
      'mswin32', 'Win32',
      'os2',     'Win32',
      'dos',     'Win32',
      'symbian', 'Win32',
      'netware', 'Win32',
      'win32',   'Win32',
      'cygwin',  'Cygwin',
      'qnx',     'QNX',
      'nto',     'QNX',
    # <MacOS Mac>  »=>» 'Mac',
     # 'VMS'     => 'VMS'
    );

    proto method select(|) {*}
    multi method select(IO::Spec:U:) {
        IO::Spec::{nqp::ifnull(nqp::atkey($module,VM.osname),'Unix')};
    }
    multi method select(IO::Spec:U: $token) {
        IO::Spec::{nqp::ifnull(nqp::atkey($module,$token.lc),'Unix')};
    }
}

# temporary non-lazy initialization of $*SPEC
PROCESS::<$SPEC> = IO::Spec.select;

# vim: expandtab shiftwidth=4
