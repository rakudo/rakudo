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
}

# temporary non-lazy initialization of $*SPEC
PROCESS::<$SPEC> = IO::Spec.select;

enum SeekType (
  :SeekFromBeginning(0),
  :SeekFromCurrent(1),
  :SeekFromEnd(2),
);
enum ProtocolFamily (
  :PF_LOCAL(0),
  :PF_UNIX(1),
  :PF_INET(2),
  :PF_INET6(3),
  :PF_MAX(4),
);
enum SocketType (
  :SOCK_PACKET(0),
  :SOCK_STREAM(1),
  :SOCK_DGRAM(2),
  :SOCK_RAW(3),
  :SOCK_RDM(4),
  :SOCK_SEQPACKET(5),
  :SOCK_MAX(6),
);
enum ProtocolType (
  :PROTO_TCP(6),
  :PROTO_UDP(17),
);

# vim: ft=perl6 expandtab sw=4
