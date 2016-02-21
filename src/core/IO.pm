my role IO {
    method umask { state $ = :8( qx/umask/.chomp ) }
}

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
