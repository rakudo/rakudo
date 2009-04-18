class IO::Socket::INET {

    has Int $.Version = 4; # Whether to use IPv4 or IPv6
    has Str $.Protocol = 'TCP';
    has Str $.RemoteHost;
    has Int $.RemotePort;
    has Str $.LocalHost;
    has Int $.LocalPort;
}
