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
  :PF_UNSPEC(nqp::p6box_i(nqp::const::SOCKET_FAMILY_UNSPEC)),
  :PF_INET(nqp::p6box_i(nqp::const::SOCKET_FAMILY_INET)),
  :PF_INET6(nqp::p6box_i(nqp::const::SOCKET_FAMILY_INET6)),
  :PF_LOCAL(nqp::p6box_i(nqp::const::SOCKET_FAMILY_UNIX)),
  :PF_UNIX(nqp::p6box_i(nqp::const::SOCKET_FAMILY_UNIX)),
  :PF_MAX(nqp::p6box_i(nqp::const::SOCKET_FAMILY_UNIX + 1)),
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

# vim: expandtab shiftwidth=4
