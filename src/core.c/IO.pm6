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
);

enum SocketType (
  :SOCK_STREAM(nqp::p6box_i(nqp::const::SOCKET_TYPE_STREAM)),
  :SOCK_DGRAM(nqp::p6box_i(nqp::const::SOCKET_TYPE_DGRAM)),
  :SOCK_RAW(nqp::p6box_i(nqp::const::SOCKET_TYPE_RAW)),
  :SOCK_RDM(nqp::p6box_i(nqp::const::SOCKET_TYPE_RDM)),
  :SOCK_SEQPACKET(nqp::p6box_i(nqp::const::SOCKET_TYPE_SEQPACKET)),
);

enum ProtocolType (
  :IPPROTO_IP(nqp::p6box_i(nqp::const::SOCKET_PROTOCOL_IP)),
  :IPPROTO_IPV6(nqp::p6box_i(nqp::const::SOCKET_PROTOCOL_IPV6)),
  :IPPROTO_TCP(nqp::p6box_i(nqp::const::SOCKET_PROTOCOL_TCP)),
  :IPPROTO_UDP(nqp::p6box_i(nqp::const::SOCKET_PROTOCOL_UDP)),
  :IPPROTO_RAW(nqp::p6box_i(nqp::const::SOCKET_PROTOCOL_RAW)),
);

# vim: ft=perl6 expandtab sw=4
