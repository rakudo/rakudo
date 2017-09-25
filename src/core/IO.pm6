my role IO {
    # This role is empty and exists so that IO() coercers
    # that coerce to IO::Path type check the result values OK
}

enum SeekType (
  :SeekFromBeginning(0),
  :SeekFromCurrent(1),
  :SeekFromEnd(2),
);
enum ProtocolFamily (
  :PF_UNSPEC(0),
  :PF_LOCAL(1),
  :PF_UNIX(1),
  :PF_INET(2),
  :PF_INET6(10),
  :PF_MAX(44),
);
enum SocketType (
  :SOCK_STREAM(1),
  :SOCK_DGRAM(2),
  :SOCK_RAW(3),
  :SOCK_RDM(4),
  :SOCK_SEQPACKET(5),
  :SOCK_PACKET(10),
  :SOCK_MAX(11),
);
enum ProtocolType (
  :PROTO_TCP(6),
  :PROTO_UDP(17),
);

# vim: ft=perl6 expandtab sw=4
